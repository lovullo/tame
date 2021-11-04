// xmlo object file reader
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
//
//  This file is part of TAME.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

//! `xmlo` object file reader.
//!
//! This defines a lower-level event-based [`XmloReader`] similar to that of
//!   [`quick_xml`] (see [`XmloEvent`]),
//!     where the events are a slightly higher-level abstraction over the
//!     types of nodes present in the file.
//!
//! _Note that a "symbol" in the `xmlo` sense differs slightly from
//!   [`SymbolId`];_
//!     the former is more akin to an identifier.
//!
//! For more information on `xmlo` files,
//!   see the [parent crate][super].A
//!
//! This reader will be used by both the compiler and linker,
//!   and so its [`SymbolId`] type is generalized.
//!
//!
//! How To Use
//! ==========
//! The event-based API for [`XmloReader`] is similar to that of
//!   [`quick_xml`].
//! There is minor overhead incurred from parsing if the emitted events are
//!   not used,
//!     but it is quite minimal.
//!
//! The next [`XmloEvent`] is retrieved using [`XmloReader::read_event`].
//! _You should stop reading at [`XmloEvent::Eoh`];_
//!   reading the remainder of the object file has not yet been implemented.

use super::super::{PackageAttrs, SymAttrs, SymType};
use super::{XmloError, XmloEvent, XmloResult};
use crate::sym::{GlobalSymbolInternUnchecked, GlobalSymbolResolve, SymbolId};
#[cfg(test)]
use crate::test::quick_xml::MockBytesStart as BytesStart;
#[cfg(test)]
use crate::test::quick_xml::MockXmlEvent as XmlEvent;
#[cfg(test)]
use crate::test::quick_xml::MockXmlReader as XmlReader;
#[cfg(not(test))]
use quick_xml::events::BytesStart;
#[cfg(not(test))]
use quick_xml::events::Event as XmlEvent;
#[cfg(not(test))]
use quick_xml::Reader as XmlReader;
use std::convert::TryInto;
use std::io::BufRead;
use std::iter::Iterator;
use std::result::Result;

/// Wrapper around [`quick_xml::Reader`] for reading and parsing `xmlo`
///   object files.
///
/// This reader performs interning (see [crate::sym]) for data that is
///   expected to be duplicated or compared.
/// Other data are converted into more concise representations where
///   possible,
///     or are returned as owned [`String`] values otherwise,
///     with the understanding that values will be persisted within an IR
///     anyway.
/// This reader stores symbol attributes in the Legacy IR's [`SymAttrs`].
///
/// See [module-level documentation](self) for more information and
///   examples.
pub struct XmloReader<B>
where
    B: BufRead,
{
    /// Source `xmlo` reader.
    reader: XmlReader<B>,

    /// Internal buffer for [`XmlReader`].
    buffer: Vec<u8>,

    /// Another internal buffer for [`XmlReader`].
    ///
    /// This buffer exists to work around ownership rules.
    /// TODO: It this worth removing?  If not, remove this TODO.
    sub_buffer: Vec<u8>,

    /// Whether the root has been validated.
    ///
    /// This is used to ensure that we provide an error early on if we try
    ///   to process something that isn't a package.
    seen_root: bool,

    /// Name of the package currently being read.
    ///
    /// This is known after processing the root `package` element,
    ///   provided that it's a proper root node.
    pkg_name: Option<SymbolId>,
}

impl<B> XmloReader<B>
where
    B: BufRead,
{
    /// Construct a new reader.
    pub fn new(reader: B) -> Self {
        let mut reader = XmlReader::from_reader(reader);

        // xmlo files are compiler output and should be trusted
        reader.check_end_names(false);

        Self {
            reader,
            // TODO: option to accept buffer
            buffer: Vec::new(),
            sub_buffer: Vec::new(),
            seen_root: false,
            pkg_name: None,
        }
    }

    /// Continue reading and produce the next event.
    ///
    /// An [`XmloEvent::Eoh`] event is emitted at the end of the header
    ///   (at the closing `preproc:fragment` node).
    ///
    /// Stack Warning
    /// =============
    /// The source file will be read until an event can be produced.
    /// This is recursive on the underlying [`XmlReader::read_event`],
    ///   and Rust dues not (at the time of writing) support tail call
    ///   optimization.
    /// This shouldn't be a concern for proper `xmlo` files as long as you
    ///   acknowledge [`XmloEvent::Eoh`] and do not continue reading
    ///   further.
    ///
    /// Errors
    /// ======
    /// - Any of [`XmloError`].
    ///   See private methods for more information.
    ///
    /// TODO: Augment failures with context
    pub fn read_event<'a>(&mut self) -> XmloResult<XmloEvent> {
        // Just to cut down on peak memory usage, cleaning up after a
        // previous run.  This does not affect behavior.
        self.buffer.clear();
        self.sub_buffer.clear();

        let event = self.reader.read_event(&mut self.buffer)?;

        // Ensure that the first encountered node is something we expect
        if !self.seen_root {
            match &event {
                // We don't process namespaces, so we have to guess what
                // they may be (map xmlo files differ, for example)
                XmlEvent::Start(ele) => {
                    if !(ele.name() == b"package"
                        || ele.name() == b"lv:package")
                    {
                        return Err(XmloError::UnexpectedRoot);
                    }

                    self.seen_root = true;
                }
                _ => return self.read_event(),
            }
        }

        match event {
            XmlEvent::Empty(ele) if ele.name() == b"preproc:sym" => {
                Self::process_sym(&self.pkg_name, &ele)
            }

            XmlEvent::Start(ele) => match ele.name() {
                b"package" | b"lv:package" => {
                    let attrs = Self::process_package(&ele)?;

                    self.pkg_name = attrs.name;

                    Ok(XmloEvent::Package(attrs))
                }

                b"preproc:sym-dep" => Self::process_dep(
                    &ele,
                    &mut self.reader,
                    &mut self.sub_buffer,
                ),

                b"preproc:fragment" => Self::process_fragment(
                    &ele,
                    &mut self.reader,
                    &mut self.sub_buffer,
                ),

                // `func` symbols include additional data for param
                // ordering, which we don't care about.  But `map` includes
                // source field information which we want to keep.  (We
                // don't care about `retmap` for our purposes.)
                b"preproc:sym" => {
                    let mut event = Self::process_sym(&self.pkg_name, &ele)?;

                    match &mut event {
                        XmloEvent::SymDecl(_, attrs)
                            if attrs.ty == Some(SymType::Map) =>
                        {
                            attrs.from = Self::process_map_from(
                                &mut self.reader,
                                &mut self.sub_buffer,
                            )?;

                            Ok(event)
                        }
                        _ => {
                            self.reader.read_to_end(
                                ele.name(),
                                &mut self.sub_buffer,
                            )?;

                            Ok(event)
                        }
                    }
                }

                // Just like the outer match, recurse
                _ => self.read_event(),
            },

            XmlEvent::End(ele) if ele.name() == b"preproc:fragments" => {
                Ok(XmloEvent::Eoh)
            }

            // Ignore and recurse, looking for something we can process
            _ => self.read_event(),
        }
    }

    /// Process `lv:package` element attributes.
    ///
    /// The result is an [`XmloEvent::Package`] containing each applicable
    ///   attribute,
    ///     parsed.
    fn process_package<'a>(
        ele: &'a BytesStart<'a>,
    ) -> XmloResult<PackageAttrs> {
        let mut program = false;
        let mut elig = None;
        let mut name = None;
        let mut relroot = None;

        for attr in ele.attributes().with_checks(false).filter_map(Result::ok) {
            match attr.key {
                b"name" => {
                    name =
                        Some(unsafe { (&attr.value).intern_utf8_unchecked() });
                }

                b"__rootpath" => {
                    relroot =
                        Some(unsafe { (&attr.value).intern_utf8_unchecked() });
                }

                b"program" => {
                    program = &*attr.value == b"true";
                }

                b"preproc:elig-class-yields" => {
                    elig =
                        Some(unsafe { (&attr.value).intern_utf8_unchecked() });
                }

                _ => (),
            }
        }

        // TODO: proper errors, no panic
        Ok(PackageAttrs {
            name,
            relroot,
            program,
            elig,
            ..Default::default()
        })
    }

    /// Process `preproc:sym` element attributes.
    ///
    /// The symbol name `preproc:sym/@name` is interned.
    /// All other known attributes are parsed
    ///   and unknown attributes are ignored.
    ///
    /// The result is a single [`XmloEvent::SymDecl`] with an interned
    ///   `preproc:sym/@name`.
    ///
    /// Errors
    /// ======
    /// - [`XmloError::UnassociatedSym`] if missing `preproc:sym/@name`.
    fn process_sym<'a>(
        pkg_name: &Option<SymbolId>,
        ele: &'a BytesStart<'a>,
    ) -> XmloResult<XmloEvent> {
        let mut name: Option<SymbolId> = None;
        let mut sym_attrs = SymAttrs::default();

        for attr in ele.attributes().with_checks(false).filter_map(Result::ok) {
            match attr.key {
                b"name" => {
                    name = Some(unsafe { attr.value.intern_utf8_unchecked() });
                }

                b"src" => {
                    sym_attrs.src =
                        Some(unsafe { attr.value.intern_utf8_unchecked() });
                }

                b"type" => {
                    sym_attrs.ty =
                        Some((*attr.value).try_into().map_err(|_| {
                            XmloError::InvalidType(unsafe {
                                String::from_utf8_unchecked(attr.value.to_vec())
                            })
                        })?);
                }

                b"dim" => {
                    sym_attrs.dim = Some(Self::dim_to_u8(&attr.value)?);
                }

                b"dtype" => {
                    sym_attrs.dtype =
                        Some((*attr.value).try_into().map_err(|_| {
                            XmloError::InvalidDtype(unsafe {
                                String::from_utf8_unchecked(attr.value.to_vec())
                            })
                        })?);
                }

                b"extern" => {
                    sym_attrs.extern_ = &*attr.value == b"true";
                }

                b"preproc:generated" => {
                    sym_attrs.generated = &*attr.value == b"true";
                }

                b"parent" => {
                    sym_attrs.parent =
                        Some(unsafe { attr.value.intern_utf8_unchecked() });
                }

                b"yields" => {
                    sym_attrs.yields =
                        Some(unsafe { attr.value.intern_utf8_unchecked() });
                }

                b"desc" => {
                    sym_attrs.desc =
                        Some(unsafe { attr.value.intern_utf8_unchecked() });
                }

                b"virtual" => {
                    sym_attrs.virtual_ = &*attr.value == b"true";
                }

                b"isoverride" => {
                    sym_attrs.override_ = &*attr.value == b"true";
                }

                // As this reader evolves, we may wish to provide an error
                // for unknown attributes so that we can be sure that we've
                // handled them all.
                _ => (),
            }
        }

        sym_attrs.pkg_name = *pkg_name;

        name.map(|name_sym| XmloEvent::SymDecl(name_sym, sym_attrs))
            .ok_or(XmloError::UnassociatedSym)
    }

    /// Process `preproc:from` for `preproc:sym[@type="map"]` elements.
    ///
    /// Map symbols contain additional information describing source
    ///   inputs external to the system.
    ///
    /// Errors
    /// ======
    /// - [`XmloError::InvalidMapFrom`] if `@name` missing or if unexpected
    ///   data (e.g. elements) are encountered.
    /// - [`XmloError::XmlError`] on XML parsing failure.
    fn process_map_from<'a>(
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<Option<SymbolId>> {
        let mut from = None;

        loop {
            match reader.read_event(buffer)? {
                XmlEvent::Empty(ele) if ele.name() == b"preproc:from" => {
                    if from.is_some() {
                        // This feature isn't actually utilized for the
                        //   input map.
                        return Err(XmloError::InvalidMapFrom(
                            "multiple preproc:from found for input map entry"
                                .into(),
                        ));
                    }

                    from.replace(
                        ele.attributes()
                            .with_checks(false)
                            .filter_map(Result::ok)
                            .find(|attr| attr.key == b"name")
                            .map_or(
                                Err(XmloError::InvalidMapFrom(
                                    "preproc:from/@name missing".into(),
                                )),
                                |attr| {
                                    Ok(unsafe {
                                        attr.value.intern_utf8_unchecked()
                                    })
                                },
                            )?,
                    );
                }

                XmlEvent::End(ele) if ele.name() == b"preproc:sym" => break,

                // Note that whitespace counts as text
                XmlEvent::Text(_) => (),

                _ => Err(XmloError::InvalidMapFrom("unexpected data".into()))?,
            };
        }

        Ok(from)
    }

    /// Process `preproc:sym-dep` element.
    ///
    /// This represents an adjacency list for a given identifier in the
    ///   dependency graph.
    /// The structure of this element is looks like this:
    ///
    /// ```xml
    /// <preproc:sym-dep name=":class:some-sym">
    ///   <preproc:sym-ref name="someOtherSym" />
    ///   <!-- ... -->
    /// </preproc:sym-dep>
    /// ```
    ///
    /// This function will read any number of `preproc:sym-ref` nodes and
    ///   produce a single [`XmloEvent::SymDeps`] containing a [`SymbolId`]
    ///   for `preproc:sym-dep/@name` and for each `preproc:sym-ref/@name`.
    ///
    /// Errors
    /// ======
    ///  - [`XmloError::UnassociatedSymDep`] if missing `preproc:sym-dep/@name`.
    ///  - [`XmloError::MalformedSymRef`] if missing `preproc:sym-ref/@name`
    ///    or if any `preproc:sym-dep/node()` is not a `prepreoc:sym-ref`.
    ///  - [`XmloError::XmlError`] on XML parsing failure.
    fn process_dep<'a>(
        ele: &'a BytesStart<'a>,
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<XmloEvent> {
        let name = ele
            .attributes()
            .with_checks(false)
            .filter_map(Result::ok)
            .find(|attr| attr.key == b"name")
            .map_or(Err(XmloError::UnassociatedSymDep), |attr| {
                Ok(unsafe { attr.value.intern_utf8_unchecked() })
            })?;

        let mut deps = Vec::new();

        loop {
            match reader.read_event(buffer)? {
                XmlEvent::Empty(symref)
                    if symref.name() == b"preproc:sym-ref" =>
                {
                    deps.push(
                        symref
                            .attributes()
                            .with_checks(false)
                            .filter_map(Result::ok)
                            .find(|attr| attr.key == b"name")
                            .map_or(
                                Err(XmloError::MalformedSymRef(
                                    "preproc:sym-ref/@name missing".into(),
                                )),
                                |attr| {
                                    Ok(unsafe {
                                        attr.value.intern_utf8_unchecked()
                                    })
                                },
                            )?,
                    );
                }

                // We assume that elements are properly nested, so this must
                // be the closing preproc:sym-dep tag.
                XmlEvent::End(_) => break,

                // Note that whitespace counts as text
                XmlEvent::Text(_) => (),

                _ => return Err(XmloError::MalformedSymRef(format!(
                    "preproc:sym-dep must contain only preproc:sym-ref children for `{}`",
                    name.lookup_str(),
                )))
            }
        }

        Ok(XmloEvent::SymDeps(name, deps))
    }

    /// Process `preproc:fragment` element.
    ///
    /// This element represents the compiled code for the given symbol.
    /// The result is a single [`XmloEvent::Fragment`] with an interned
    ///   fragment and an interned `preproc:fragment/@id`.
    /// The fragment is _left escaped_,
    ///   since it is assumed that it will be written back out verbatim
    ///   without further modification;
    ///     this save us from having to spend time re-escaping on output
    ///     down the line.
    ///
    /// Errors
    /// ======
    /// - [`XmloError::UnassociatedFragment`] if missing `preproc:fragment/@id`.
    /// - [`XmloError::MissingFragmentText`] if missing
    ///   `preproc:fragment/text()`.
    /// - [`XmloError::XmlError`] for XML parsing errors.
    fn process_fragment<'a>(
        ele: &'a BytesStart<'a>,
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<XmloEvent> {
        let mut src_attrs = ele.attributes();
        let mut filtered = src_attrs.with_checks(false).filter_map(Result::ok);

        let id = filtered
            .find(|attr| attr.key == b"id")
            .filter(|attr| &*attr.value != b"")
            .map_or(Err(XmloError::UnassociatedFragment), |attr| {
                Ok(unsafe { attr.value.intern_utf8_unchecked() })
            })?;

        let text = match reader.read_event(buffer)? {
            XmlEvent::Text(ev) => {
                // It is wasteful to unescape only to have to re-escape
                // again on write, so keep the text raw (escaped), and also
                // trust that it's valid UTF-8, having come from the
                // compiler.
                Ok(unsafe { ev.escaped().clone_uninterned_utf8_unchecked() })
            }
            _ => Err(XmloError::MissingFragmentText(id)),
        }?;

        Ok(XmloEvent::Fragment(id, text))
    }

    /// Convert single-character `@dim` to a [`u8`].
    ///
    /// Errors
    /// ======
    /// - [`XmloError::InvalidDim`] if first character is not an ASCII
    ///   digit,
    ///     or if there is more than one character.
    fn dim_to_u8(value: &[u8]) -> XmloResult<u8> {
        // Technically this could allow for incorrect inputs (we only take
        // the first character)
        if value.len() > 1 || !value[0].is_ascii_digit() {
            return Err(XmloError::InvalidDim(unsafe {
                String::from_utf8_unchecked(value.to_vec())
            }));
        }

        Ok(value[0] - b'0')
    }
}

impl<B> Iterator for XmloReader<B>
where
    B: BufRead,
{
    type Item = XmloResult<XmloEvent>;

    /// Invoke [`XmloReader::read_event`] and yield the result via an
    ///   [`Iterator`] API.
    ///
    /// *Warning*: This will always return [`Some`] for now.
    /// Future changes may alter this behavior.
    /// To terminate the iterator,
    ///   it's recommended that you use [`Iterator::take_while`] to filter
    ///   on the desired predicate,
    ///     such as [`XmloEvent::Eoh`].
    fn next(&mut self) -> Option<Self::Item> {
        Some(self.read_event())
    }
}

impl<B> From<B> for XmloReader<B>
where
    B: BufRead,
{
    fn from(buf: B) -> Self {
        Self::new(buf)
    }
}

#[cfg(test)]
mod test;
