// xmlo object file reader
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
//!   [`Symbol`];_
//!     the former is more akin to an identifier.
//!
//! For more information on `xmlo` files,
//!   see the [parent crate][super].
//!
//!
//! How To Use
//! ==========
//! The event-based API for [`XmloReader`] is similar to that of
//!   [`quick_xml`],
//!     except that the [`XmloResult`] produces
//!     [Legacy IR](crate::ir::legacyir).
//! There is minor overhead incurred from parsing if the emitted events are
//!   not used,
//!     but it is quite minimal.
//! The only lifetime one has to worry about is the lifetime of the
//!   [`Interner`] used to produce symbols.
//!
//! The next [`XmloEvent`] is retrieved using [`XmloReader::read_event`].
//! _You should stop reading at [`XmloEvent::Eoh`];_
//!   reading the remainder of the object file has not yet been implemented.
//!
//! ```
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use tamer::obj::xmlo::{XmloEvent, XmloReader};
//! use tamer::ir::legacyir::SymType;
//! use tamer::sym::{DefaultInterner, Interner};
//!
//! let xmlo = br#"<package name="foo">
//!       <preproc:symtable>
//!         <preproc:sym name="syma" type="class" />
//!         <preproc:sym name="symb" type="cgen" />
//!       </preproc:symtable>
//!
//!       <preproc:sym-deps>
//!         <preproc:sym-dep name="syma">
//!           <preproc:sym-ref name="depa-1" />
//!           <preproc:sym-ref name="depa-2" />
//!         </preproc:sym-dep>
//!         <preproc:sym-dep name="symb">
//!           <preproc:sym-ref name="depb-1" />
//!         </preproc:sym-dep>
//!       </preproc:sym-deps>
//!
//!       <preproc:fragments>
//!         <preproc:fragment id="syma">syma text</preproc:fragment>
//!         <preproc:fragment id="symb">symb text</preproc:fragment>
//!       </preproc:fragments>
//!     </package>"#;
//!
//! let interner = DefaultInterner::new();
//! let mut reader = XmloReader::new(xmlo as &[u8], &interner);
//!
//! let mut pkgname = None;
//! let mut syms = Vec::new();
//! let mut deps = Vec::new();
//! let mut fragments = Vec::new();
//!
//! loop {
//!     match reader.read_event()? {
//!         XmloEvent::Package(attrs) => pkgname = attrs.name,
//!         XmloEvent::SymDecl(sym, attrs) => syms.push((sym, attrs.ty)),
//!         XmloEvent::SymDeps(sym, symdeps) => deps.push((sym, symdeps)),
//!         XmloEvent::Fragment(sym, text) => fragments.push((sym, text)),
//!
//!         // Do not read past end of header.
//!         XmloEvent::Eoh => break,
//!     }
//! }
//!
//! assert_eq!(Some(interner.intern("foo")), pkgname);
//!
//! assert_eq!(
//!     vec![
//!         (interner.intern("syma"), Some(SymType::Class)),
//!         (interner.intern("symb"), Some(SymType::Cgen)),
//!     ],
//!     syms
//! );
//!
//! assert_eq!(
//!     vec![
//!         (interner.intern("syma"), vec![
//!             interner.intern("depa-1"),
//!             interner.intern("depa-2"),
//!         ]),
//!         (interner.intern("symb"), vec![
//!             interner.intern("depb-1"),
//!         ]),
//!     ],
//!     deps
//! );
//!
//! assert_eq!(
//!     vec![
//!         (interner.intern("syma"), "syma text".into()),
//!         (interner.intern("symb"), "symb text".into()),
//!     ],
//!     fragments
//! );
//!
//! # Ok(())
//! # }
//! ```

use crate::ir::legacyir::{PackageAttrs, SymAttrs, SymType};
use crate::sym::{Interner, Symbol};
#[cfg(test)]
use crate::test::quick_xml::MockBytesStart as BytesStart;
#[cfg(test)]
use crate::test::quick_xml::MockXmlEvent as XmlEvent;
#[cfg(test)]
use mock::MockXmlReader as XmlReader;
#[cfg(not(test))]
use quick_xml::events::BytesStart;
#[cfg(not(test))]
use quick_xml::events::Event as XmlEvent;
use quick_xml::Error as XmlError;
#[cfg(not(test))]
use quick_xml::Reader as XmlReader;
use std::convert::TryInto;
use std::fmt::Display;
use std::io::BufRead;
use std::iter::Iterator;
use std::result::Result;

/// A [`Result`] with a hard-coded [`XmloError`] error type.
///
/// This is the result of every [`XmloReader`] operation that could
///   potentially fail in error.
pub type XmloResult<T> = Result<T, XmloError>;

/// Wrapper around [`quick_xml::Reader`] for reading and parsing `xmlo`
///   object files.
///
/// This reader performs interning (see [`Interner`]) for data that is
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
pub struct XmloReader<'i, B, I>
where
    B: BufRead,
    I: Interner<'i>,
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

    /// String internment system.
    interner: &'i I,

    /// Whether the root has been validated.
    ///
    /// This is used to ensure that we provide an error early on if we try
    ///   to process something that isn't a package.
    seen_root: bool,

    /// Name of the package currently being read.
    ///
    /// This is known after processing the root `package` element,
    ///   provided that it's a proper root node.
    pkg_name: Option<&'i Symbol<'i>>,
}

impl<'i, B: BufRead, I: Interner<'i>> XmloReader<'i, B, I> {
    /// Construct a new reader.
    pub fn new(reader: B, interner: &'i I) -> Self {
        let mut reader = XmlReader::from_reader(reader);

        // xmlo files are compiler output and should be trusted
        reader.check_end_names(false);

        Self {
            reader,
            // TODO: option to accept buffer
            buffer: Vec::new(),
            sub_buffer: Vec::new(),
            interner,
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
    pub fn read_event<'a>(&mut self) -> XmloResult<XmloEvent<'i>> {
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
                Self::process_sym(&self.pkg_name, &ele, self.interner)
            }

            XmlEvent::Start(ele) => match ele.name() {
                b"package" | b"lv:package" => {
                    let attrs = Self::process_package(&ele, self.interner)?;

                    self.pkg_name = attrs.name;

                    Ok(XmloEvent::Package(attrs))
                }

                b"preproc:sym-dep" => Self::process_dep(
                    &ele,
                    self.interner,
                    &mut self.reader,
                    &mut self.sub_buffer,
                ),

                b"preproc:fragment" => Self::process_fragment(
                    &ele,
                    self.interner,
                    &mut self.reader,
                    &mut self.sub_buffer,
                ),

                // `func` symbols include additional data for param
                // ordering, which we don't care about.  But `map` includes
                // source field information which we want to keep.  (We
                // don't care about `retmap` for our purposes.)
                b"preproc:sym" => {
                    let mut event =
                        Self::process_sym(&self.pkg_name, &ele, self.interner)?;

                    match &mut event {
                        XmloEvent::SymDecl(_, attrs)
                            if attrs.ty == Some(SymType::Map) =>
                        {
                            attrs.from = Some(Self::process_map_from(
                                self.interner,
                                &mut self.reader,
                                &mut self.sub_buffer,
                            )?);

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
        interner: &'i I,
    ) -> XmloResult<PackageAttrs<'i>> {
        let mut program = false;
        let mut elig: Option<&'i Symbol<'i>> = None;
        let mut name: Option<&'i Symbol<'i>> = None;
        let mut relroot: Option<String> = None;

        for attr in ele.attributes().with_checks(false).filter_map(Result::ok) {
            match attr.key {
                b"name" => {
                    name = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
                }

                b"__rootpath" => {
                    relroot = Some(unsafe {
                        String::from_utf8_unchecked(attr.value.to_vec())
                    });
                }

                b"program" => {
                    program = &*attr.value == b"true";
                }

                b"preproc:elig-class-yields" => {
                    elig = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
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
        pkg_name: &Option<&'i Symbol<'i>>,
        ele: &'a BytesStart<'a>,
        interner: &'i I,
    ) -> XmloResult<XmloEvent<'i>> {
        let mut name: Option<&'i Symbol<'i>> = None;
        let mut sym_attrs = SymAttrs::default();

        for attr in ele.attributes().with_checks(false).filter_map(Result::ok) {
            match attr.key {
                b"name" => {
                    name = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
                }

                b"src" => {
                    sym_attrs.src = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
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
                    sym_attrs.parent = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
                }

                b"yields" => {
                    sym_attrs.yields = Some(unsafe {
                        interner.intern_utf8_unchecked(&attr.value)
                    });
                }

                b"desc" => {
                    sym_attrs.desc = Some(unsafe {
                        String::from_utf8_unchecked(attr.value.to_vec())
                    });
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
        interner: &'i I,
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<Vec<&'i Symbol<'i>>> {
        let mut froms = Vec::new();

        loop {
            match reader.read_event(buffer)? {
                XmlEvent::Empty(ele) if ele.name() == b"preproc:from" => froms
                    .push(
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
                                        interner
                                            .intern_utf8_unchecked(&attr.value)
                                    })
                                },
                            )?,
                    ),

                XmlEvent::End(ele) if ele.name() == b"preproc:sym" => break,

                // Note that whitespace counts as text
                XmlEvent::Text(_) => (),

                _ => Err(XmloError::InvalidMapFrom("unexpected data".into()))?,
            };
        }

        Ok(froms)
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
    ///   produce a single [`XmloEvent::SymDeps`] containing a [`Symbol`]
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
        interner: &'i I,
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<XmloEvent<'i>> {
        let name = ele
            .attributes()
            .with_checks(false)
            .filter_map(Result::ok)
            .find(|attr| attr.key == b"name")
            .map_or(Err(XmloError::UnassociatedSymDep), |attr| {
                Ok(unsafe { interner.intern_utf8_unchecked(&attr.value) })
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
                                        interner.intern_utf8_unchecked(&attr.value)
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
                    name,
                )))
            }
        }

        Ok(XmloEvent::SymDeps(name, deps))
    }

    /// Process `preproc:fragment` element.
    ///
    /// This element represents the compiled code for the given symbol.
    /// The result is a single [`XmloEvent::Fragment`] with an owned
    ///   [`String`] fragment and an interned `preproc:fragment/@id`.
    ///
    /// Errors
    /// ======
    /// - [`XmloError::UnassociatedFragment`] if missing `preproc:fragment/@id`.
    /// - [`XmloError::MissingFragmentText`] if missing
    ///   `preproc:fragment/text()`.
    /// - [`XmloError::XmlError`] for XML parsing errors.
    fn process_fragment<'a>(
        ele: &'a BytesStart<'a>,
        interner: &'i I,
        reader: &mut XmlReader<B>,
        buffer: &mut Vec<u8>,
    ) -> XmloResult<XmloEvent<'i>> {
        let mut src_attrs = ele.attributes();
        let mut filtered = src_attrs.with_checks(false).filter_map(Result::ok);

        let id = filtered
            .find(|attr| attr.key == b"id")
            .filter(|attr| &*attr.value != b"")
            .map_or(Err(XmloError::UnassociatedFragment), |attr| {
                Ok(unsafe { interner.intern_utf8_unchecked(&attr.value) })
            })?;

        let text =
            reader
                .read_text(ele.name(), buffer)
                .map_err(|err| match err {
                    XmlError::TextNotFound => {
                        XmloError::MissingFragmentText(id.to_string())
                    }
                    _ => err.into(),
                })?;

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

impl<'i, B, I> Iterator for XmloReader<'i, B, I>
where
    B: BufRead,
    I: Interner<'i>,
{
    type Item = XmloResult<XmloEvent<'i>>;

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

impl<'i, B, I> From<(B, &'i I)> for XmloReader<'i, B, I>
where
    B: BufRead,
    I: Interner<'i>,
{
    fn from(args: (B, &'i I)) -> Self {
        Self::new(args.0, args.1)
    }
}

/// `xmlo` reader events.
///
/// All data are parsed rather than being returned as [`u8`] slices,
///   which avoids having to deal with awkward borrows or data copying since
///   these data will likely be persisted in memory anyway.
///
/// To avoid extra data copying,
///   we should instead prefer not to put data into object files that won't
///   be useful and can't be easily skipped without parsing.
#[derive(Debug, PartialEq, Eq)]
pub enum XmloEvent<'i> {
    /// Package declaration.
    ///
    /// This contains data gathered from the root `lv:package` node.
    Package(PackageAttrs<'i>),

    /// Symbol declaration.
    ///
    /// This represents an entry in the symbol table,
    ///   which includes a symbol along with its variable metadata as
    ///   [`SymAttrs`].
    SymDecl(&'i Symbol<'i>, SymAttrs<'i>),

    /// Dependencies of a given symbol.
    ///
    /// Note that, for simplicity, an owned vector is returned rather than a
    ///   slice into an internal buffer.
    SymDeps(&'i Symbol<'i>, Vec<&'i Symbol<'i>>),

    /// Text (compiled code) fragment for a given symbol.
    ///
    /// This contains the compiler output for a given symbol,
    ///   and is returned here as an owned value.
    /// Given that fragments can be quite large,
    ///   a caller not interested in these data should choose to skip
    ///   fragments entirely rather than simply ignoring fragment events.
    Fragment(&'i Symbol<'i>, String),

    /// End-of-header.
    ///
    /// The header of an `xmlo` file is defined as the symbol table;
    ///   dependency list; and fragments.
    /// This event is emitted at the closing `preproc:fragment` node.
    Eoh,
}

/// Error during `xmlo` processing.
///
/// Errors contain only owned values rather than references to original
///   data since they represent conditions requiring termination from
///   malformed compiler output,
///     and so should rarely occur.
/// This drastically simplifies the reader and [`Result`] chaining.
///
/// TODO: These errors provide no context (byte offset).
#[derive(Debug, PartialEq)]
pub enum XmloError {
    /// XML parsing error.
    XmlError(XmlParseError),
    /// The root node was not an `lv:package`.
    UnexpectedRoot,
    /// A `preproc:sym` node was found, but is missing `@name`.
    UnassociatedSym,
    /// The provided `preproc:sym/@type` is unknown or invalid.
    InvalidType(String),
    /// The provided `preproc:sym/@dtype` is unknown or invalid.
    InvalidDtype(String),
    /// The provided `preproc:sym/@dim` is invalid.
    InvalidDim(String),
    /// A `preproc:sym-dep` element was found, but is missing `@name`.
    UnassociatedSymDep,
    /// The `preproc:sym[@type="map"]` contains unexpected or invalid data.
    InvalidMapFrom(String),
    /// Invalid dependency in adjacency list
    ///   (`preproc:sym-dep/preproc:sym-ref`).
    MalformedSymRef(String),
    /// A `preproc:fragment` element was found, but is missing `@id`.
    UnassociatedFragment,
    /// A `preproc:fragment` element was found, but is missing `text()`.
    MissingFragmentText(String),
}

impl From<XmlError> for XmloError {
    fn from(e: XmlError) -> Self {
        XmloError::XmlError(e.into())
    }
}

impl Display for XmloError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            XmloError::XmlError(e) => e.fmt(fmt),
            XmloError::UnexpectedRoot => {
                write!(fmt, "unexpected package root (is this a package?)")
            }
            XmloError::UnassociatedSym => write!(
                fmt,
                "unassociated symbol table entry: preproc:sym/@name missing"
            ),
            XmloError::InvalidType(ty) => {
                write!(fmt, "invalid preproc:sym/@type `{}`", ty)
            }
            XmloError::InvalidDtype(dtype) => {
                write!(fmt, "invalid preproc:sym/@dtype `{}`", dtype)
            }
            XmloError::InvalidDim(dim) => {
                write!(fmt, "invalid preproc:sym/@dim `{}`", dim)
            }
            XmloError::InvalidMapFrom(msg) => {
                write!(fmt, "invalid preproc:sym[@type=\"map\"]: {}", msg)
            }
            XmloError::UnassociatedSymDep => write!(
                fmt,
                "unassociated dependency list: preproc:sym-dep/@name missing"
            ),
            XmloError::MalformedSymRef(msg) => {
                write!(fmt, "malformed dependency ref: {}", msg)
            }
            XmloError::UnassociatedFragment => write!(
                fmt,
                "unassociated fragment: preproc:fragment/@id missing"
            ),
            XmloError::MissingFragmentText(symname) => write!(
                fmt,
                "fragment found, but missing text for symbol `{}`",
                symname,
            ),
        }
    }
}

impl std::error::Error for XmloError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::XmlError(e) => Some(e),
            _ => None,
        }
    }
}

/// Thin wrapper around [`XmlError`] to implement [`PartialEq`].
///
/// This will always yield `false`,
///   but allows us to derive the trait on types using [`XmloError`];
///     otherwise, this madness propagates indefinitely.
#[derive(Debug)]
pub struct XmlParseError(XmlError);

impl PartialEq for XmlParseError {
    /// [`XmlError`] does not implement [`PartialEq`] and so this will
    ///   always yield `false`.
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl From<XmlError> for XmlParseError {
    fn from(e: XmlError) -> Self {
        Self(e)
    }
}

impl Display for XmlParseError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(fmt)
    }
}

impl std::error::Error for XmlParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.0)
    }
}

#[cfg(test)]
mod mock {
    use super::*;
    use quick_xml::Result as XmlResult;

    pub struct MockXmlReader<B: BufRead> {
        _reader: B,

        pub check_end: Option<bool>,

        /// Closure yielding the next event for `read_event`.
        ///
        /// This exists exclusively to avoid adding a lifetime parameter to
        ///   the mock when providing stub data.
        /// A closure must be set before calling `read_event` to avoid a
        ///   panic.
        pub next_event: Option<
            Box<dyn for<'a> Fn(&'a mut Vec<u8>, u8) -> XmlResult<XmlEvent<'a>>>,
        >,

        pub event_i: u8,

        /// Next string to yield for a text node.
        pub next_text: Option<XmlResult<String>>,

        pub given_text_ele: Option<String>,

        pub read_to_end_name: Option<String>,
    }

    impl<B: BufRead> MockXmlReader<B> {
        pub fn from_reader(reader: B) -> Self {
            Self {
                _reader: reader,
                check_end: None,
                next_event: None,
                event_i: 0,
                next_text: None,
                given_text_ele: None,
                read_to_end_name: None,
            }
        }

        pub fn check_end_names(&mut self, val: bool) -> &mut Self {
            self.check_end = Some(val);
            self
        }

        pub fn read_event<'a, 'b>(
            &'a mut self,
            buf: &'b mut Vec<u8>,
        ) -> XmlResult<XmlEvent<'b>> {
            let result =
                (self.next_event.as_ref().expect("missing mock next_event"))(
                    buf,
                    self.event_i,
                );

            self.event_i += 1;
            result
        }

        pub fn read_text<K: AsRef<[u8]>>(
            &mut self,
            end: K,
            _buf: &mut Vec<u8>,
        ) -> XmlResult<String> {
            self.given_text_ele =
                Some(String::from_utf8(end.as_ref().to_vec()).unwrap());

            self.next_text.take().expect("missing mock next_text")
        }

        pub fn read_to_end<K: AsRef<[u8]>>(
            &mut self,
            end: K,
            _buf: &mut Vec<u8>,
        ) -> XmlResult<()> {
            self.read_to_end_name =
                Some(String::from_utf8(end.as_ref().to_vec()).unwrap());

            Ok(())
        }
    }
}

#[cfg(test)]
pub mod test;
