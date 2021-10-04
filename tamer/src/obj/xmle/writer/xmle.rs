// Concrete xmle writer
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

use super::writer::{Result, WriterError};
use crate::ir::asg::{
    IdentKind, IdentObject, IdentObjectData, Sections, SectionsIter,
};
use crate::sym::{GlobalSymbolResolve, SymbolId};
#[cfg(test)]
use mock::MockXmlWriter as XmlWriter;
use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event};
#[cfg(not(test))]
use quick_xml::Writer as XmlWriter;
use std::borrow::Cow;
use std::io::Write;

/// Responsible for writing to the xmle files
pub struct XmleWriter<W: Write> {
    writer: XmlWriter<W>,
}

impl<W: Write> XmleWriter<W> {
    /// Create a new instance of `XmleWriter`
    /// ```
    /// use std::io::Cursor;
    /// use tamer::obj::xmle::writer::XmleWriter;
    ///
    /// let writer = Cursor::new(Vec::new());
    /// let xmle_writer = XmleWriter::new(writer);
    /// ```
    pub fn new(write: W) -> Self {
        let writer = XmlWriter::new_with_indent(write, b' ', 2);

        Self { writer }
    }

    /// Consume the `XmleWriter` and return the inner `Write` object
    ///
    /// ```
    /// use std::io::Cursor;
    /// use tamer::obj::xmle::writer::XmleWriter;
    ///
    /// let writer = Cursor::new(Vec::new());
    /// let xmle_writer = XmleWriter::new(writer);
    /// assert!(xmle_writer.into_inner().into_inner().is_empty());
    /// ```
    pub fn into_inner(self) -> W {
        self.writer.into_inner()
    }

    /// Write xmle
    ///
    /// Goes through each of the pre-ordered [`Sections`] and writes to the
    ///   buffer.
    ///
    /// ```
    /// use std::io::Cursor;
    /// use tamer::ir::asg::{Sections, IdentObject};
    /// use tamer::obj::xmle::writer::XmleWriter;
    /// use tamer::sym::GlobalSymbolIntern;
    ///
    /// let writer = Cursor::new(Vec::new());
    /// let mut xmle_writer = XmleWriter::new(writer);
    /// let sections = Sections::<IdentObject>::new();
    /// let name = "foo".intern();
    /// xmle_writer.write(
    ///     &sections,
    ///     name,
    ///     &String::from(""),
    /// );
    /// let buf = xmle_writer.into_inner().into_inner();
    /// assert!(!buf.is_empty(), "something was written to the buffer");
    /// ```
    pub fn write<T: IdentObjectData>(
        &mut self,
        sections: &Sections<T>,
        name: SymbolId,
        relroot: &str,
    ) -> Result {
        self.write_start_package(name, &relroot)?
            .write_element(b"l:dep", |writer| {
                writer.write_sections(&sections, &relroot)
            })?
            // This was not in the original linker, but we need to be able to
            //   convey this information for `standalones` (which has received
            //   some logic from the old linker for the time being).
            .write_element(b"l:map-from", |writer| {
                writer.write_froms(&sections)
            })?
            .write_element(b"l:map-exec", |writer| {
                writer.write_section(sections.iter_map())
            })?
            .write_element(b"l:retmap-exec", |writer| {
                writer.write_section(sections.iter_retmap())
            })?
            .write_element(b"l:static", |writer| {
                writer.write_section(sections.iter_static())
            })?
            .write_element(b"l:exec", |writer| {
                writer.write_section(sections.iter_exec())
            })?
            .write_end_tag(b"package")?;

        Ok(())
    }

    /// Write an element
    ///
    /// This writes the opening tag, the content, and the closing tag for a
    ///   given element. The callback is what will write the element's body.
    #[inline]
    fn write_element<F>(
        &mut self,
        name: &[u8],
        callback: F,
    ) -> Result<&mut XmleWriter<W>>
    where
        F: FnOnce(&mut Self) -> Result<&mut XmleWriter<W>>,
    {
        self.write_start_tag(name)?;
        (callback)(self)?;
        self.write_end_tag(name)?;

        Ok(self)
    }

    /// Open the `package` element
    ///
    /// The `package` element's opening tag needs attributes, so it cannot use
    ///   `write_start_tag` directly.
    fn write_start_package(
        &mut self,
        name: SymbolId,
        relroot: &str,
    ) -> Result<&mut XmleWriter<W>> {
        let name_str = name.lookup_str();

        let root =
            BytesStart::owned_name(b"package".to_vec()).with_attributes(vec![
                ("xmlns", "http://www.lovullo.com/rater"),
                ("xmlns:preproc", "http://www.lovullo.com/rater/preproc"),
                ("xmlns:l", "http://www.lovullo.com/rater/linker"),
                ("title", &name_str), // TODO
                ("program", "true"),
                ("name", &name_str),
                ("__rootpath", &relroot),
            ]);

        self.writer.write_event(Event::Start(root))?;

        Ok(self)
    }

    /// Open an element's tag
    fn write_start_tag(&mut self, name: &[u8]) -> Result<&mut XmleWriter<W>> {
        self.writer
            .write_event(Event::Start(BytesStart::borrowed_name(name)))?;

        Ok(self)
    }

    /// Close an element's tag
    fn write_end_tag(&mut self, name: &[u8]) -> Result<&mut XmleWriter<W>> {
        self.writer
            .write_event(Event::End(BytesEnd::borrowed(name)))?;

        Ok(self)
    }

    /// Write all [`Sections`]
    ///
    /// All the [`Sections`] found need to be written out using the `writer`
    ///   object.
    fn write_sections<T: IdentObjectData>(
        &mut self,
        sections: &Sections<T>,
        relroot: &str,
    ) -> Result<&mut XmleWriter<W>> {
        let all = sections.iter_all();

        for obj in all {
            let ident = obj
                .as_ident()
                .expect("internal error: encountered non-identifier object");

            match ident {
                IdentObject::Ident(sym, kind, src)
                | IdentObject::IdentFragment(sym, kind, src, _) => {
                    // this'll be formalized more sanely
                    let mut attrs = match kind {
                        IdentKind::Cgen(dim) => {
                            vec![("type", "cgen"), ("dim", dim.as_ref())]
                        }
                        IdentKind::Class(dim) => {
                            vec![("type", "class"), ("dim", dim.as_ref())]
                        }
                        IdentKind::Const(dim, dtype) => vec![
                            ("type", "const"),
                            ("dim", dim.as_ref()),
                            ("dtype", dtype.as_ref()),
                        ],
                        IdentKind::Func(dim, dtype) => vec![
                            ("type", "func"),
                            ("dim", dim.as_ref()),
                            ("dtype", dtype.as_ref()),
                        ],
                        IdentKind::Gen(dim, dtype) => vec![
                            ("type", "gen"),
                            ("dim", dim.as_ref()),
                            ("dtype", dtype.as_ref()),
                        ],
                        IdentKind::Lparam(dim, dtype) => vec![
                            ("type", "lparam"),
                            ("dim", dim.as_ref()),
                            ("dtype", dtype.as_ref()),
                        ],
                        IdentKind::Param(dim, dtype) => vec![
                            ("type", "param"),
                            ("dim", dim.as_ref()),
                            ("dtype", dtype.as_ref()),
                        ],
                        IdentKind::Rate(dtype) => {
                            vec![("type", "rate"), ("dtype", dtype.as_ref())]
                        }
                        IdentKind::Tpl => vec![("type", "tpl")],
                        IdentKind::Type(dtype) => {
                            vec![("type", "type"), ("dtype", dtype.as_ref())]
                        }
                        IdentKind::MapHead => vec![("type", "map:head")],
                        IdentKind::Map => vec![("type", "map")],
                        IdentKind::MapTail => vec![("type", "map:tail")],
                        IdentKind::RetMapHead => vec![("type", "retmap:head")],
                        IdentKind::RetMap => vec![("type", "retmap")],
                        IdentKind::RetMapTail => vec![("type", "retmap:tail")],
                        IdentKind::Meta => vec![("type", "meta")],
                        IdentKind::Worksheet => vec![("type", "worksheet")],
                    };

                    let name = &sym.lookup_str();
                    attrs.push(("name", name));

                    if src.generated {
                        attrs.push(("preproc:generated", "true"));
                    }

                    let srcpath: String;
                    if let Some(pkg_name) = src.pkg_name {
                        srcpath = format!("{}{}", relroot, pkg_name.lookup_str());
                        attrs.push(("src", &srcpath));
                    }

                    let parent_str = src.parent.map(|sym| sym.lookup_str());
                    if let Some(ref parent) = parent_str {
                        attrs.push(("parent", parent));
                    }

                    let yields_str = src.yields.map(|sym| sym.lookup_str());
                    if let Some(ref yields) = yields_str {
                        attrs.push(("yields", yields));
                    }
                    if let Some(desc) = &src.desc {
                        attrs.push(("desc", desc.lookup_str().as_str()));
                    }

                    let sym = BytesStart::owned_name(b"preproc:sym".to_vec())
                        .with_attributes(attrs);

                    self.writer.write_event(Event::Empty(sym))?;
                }
                _ => unreachable!(
                    "identifier should have been filtered out during sorting: {:?}",
                    ident,
                ),
            }
        }

        Ok(self)
    }

    /// Write the source `from`
    ///
    /// If a `map` object has a `from` attribute in its source, we need to
    ///   write them using the `writer`'s `write_event`.
    fn write_froms<T: IdentObjectData>(
        &mut self,
        sections: &Sections<T>,
    ) -> Result<&mut XmleWriter<W>> {
        for from in sections.iter_map_froms_uniq() {
            let name: &str = &from.lookup_str();

            self.writer.write_event(Event::Empty(
                BytesStart::borrowed_name(b"l:from")
                    .with_attributes(vec![("name", name)]),
            ))?;
        }

        Ok(self)
    }

    /// Write a ['Section`]
    ///
    /// Iterates through the parts of a `Section` and writes them using the
    ///   `writer`'s 'write_event`.
    fn write_section<T: IdentObjectData>(
        &mut self,
        idents: SectionsIter<T>,
    ) -> Result<&mut XmleWriter<W>> {
        for obj in idents {
            let ident = obj
                .as_ident()
                .expect("internal error: encountered non-identifier object");

            match ident {
                IdentObject::IdentFragment(_, _, _, frag) => {
                    self.writer.write_event(Event::Text(
                        BytesText::from_escaped_str(Cow::Borrowed(
                            &frag.lookup_str() as &str,
                        )),
                    ))?;
                }
                // Cgen, Gen, and Lparam are not expected to be present, so we
                //   can ignore them when we determeing when to return an Err.
                IdentObject::Ident(_, IdentKind::Cgen(_), _)
                | IdentObject::Ident(_, IdentKind::Gen(_, _), _)
                | IdentObject::Ident(_, IdentKind::Lparam(_, _), _) => (),
                obj => {
                    return Err(WriterError::ExpectedFragment(format!(
                        "fragment expected: {:?}",
                        obj
                    )));
                }
            }
        }

        Ok(self)
    }
}

#[cfg(test)]
mod mock {
    use super::*;

    pub struct MockXmlWriter<W: Write> {
        inner: W,
        pub write_callback: Option<Box<dyn for<'a> Fn(&Event<'a>) -> Result>>,
    }

    impl<W: Write> MockXmlWriter<W> {
        pub fn new(inner: W) -> Self {
            Self {
                inner,
                write_callback: None,
            }
        }

        pub fn new_with_indent(inner: W, _: u8, _: u8) -> Self {
            Self::new(inner)
        }

        pub fn write_event<'a, E: AsRef<Event<'a>>>(
            &mut self,
            event: E,
        ) -> Result<usize> {
            (self
                .write_callback
                .as_ref()
                .expect("missing mock write_callback"))(
                event.as_ref()
            )?;

            Ok(0)
        }

        pub fn into_inner(self) -> W {
            self.inner
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::asg::{Dim, FragmentText, Source};
    use crate::ir::legacyir::SymAttrs;
    use crate::sym::GlobalSymbolIntern;
    use std::str;

    type Sut<W> = XmleWriter<W>;

    #[test]
    fn writer_uses_inner_buffer() -> Result {
        let expected = vec![1, 2, 3];
        let buf = expected.clone();

        let sut = Sut::new(buf);

        assert_eq!(expected, sut.into_inner());

        Ok(())
    }

    #[test]
    fn write_start_package() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Start(bytes_start) => {
                let name = str::from_utf8(bytes_start.name());
                match name {
                    Ok("package") => {
                        let attributes = bytes_start.attributes();
                        assert_eq!(7, attributes.count());
                        Ok(())
                    }
                    _ => panic!("unreachable"),
                }
            }
            _ => panic!("did not match expected event"),
        }));

        sut.write_start_package("".intern(), &String::from(""))?;

        Ok(())
    }

    #[test]
    fn write_start_tag() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Start(bytes_start) => {
                let name = str::from_utf8(bytes_start.name());
                match name {
                    Ok("l:dep") => {
                        let attributes = bytes_start.attributes();
                        assert_eq!(0, attributes.count());
                        Ok(())
                    }
                    _ => panic!("unreachable"),
                }
            }
            _ => panic!("did not match expected event"),
        }));

        sut.write_start_tag(b"l:dep")?;

        Ok(())
    }

    #[test]
    fn write_end_tag() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::End(bytes_end) => {
                let name = str::from_utf8(bytes_end.name());
                assert_eq!("package", name?);
                Ok(())
            }
            _ => panic!("did not match expected event"),
        }));

        sut.write_end_tag(b"package")?;

        Ok(())
    }

    #[test]
    fn write_section() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Text(_) => (Ok(())),
            _ => panic!("did not trigger event"),
        }));

        let obj = IdentObject::IdentFragment(
            "sym".intern(),
            IdentKind::Meta,
            Source::default(),
            FragmentText::from(""),
        );

        let mut sections = Sections::new();
        sections.rater.push_body(&obj);
        sut.write_section(sections.iter_all())?;

        Ok(())
    }

    #[test]
    fn write_section_ignores_other_kinds() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|_| {
            panic!("callback should not have been called");
        }));

        let obj = IdentObject::Ident(
            "sym".intern(),
            IdentKind::Cgen(Dim::default()),
            Source::default(),
        );

        let mut sections = Sections::new();
        sections.rater.push_body(&obj);
        sut.write_section(sections.iter_all())?;

        Ok(())
    }

    #[test]
    fn write_section_catch_missing() -> Result {
        let mut sut = Sut::new(vec![]);
        sut.writer.write_callback = Some(Box::new(|_| {
            panic!("callback should not have been called");
        }));

        let obj = IdentObject::Missing("missing".intern());

        let mut sections = Sections::new();
        sections.rater.push_body(&obj);
        let result = sut.write_section(sections.iter_all());

        match result {
            Err(WriterError::ExpectedFragment(_)) => {}
            _ => panic!("expected Err"),
        }

        Ok(())
    }

    #[test]
    fn write_sections() -> Result {
        let mut sut = Sut::new(vec![]);

        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Empty(bytes_start) => {
                let name = str::from_utf8(bytes_start.name())?;
                assert_eq!("preproc:sym", name);
                let mut attributes = bytes_start.attributes();
                assert_eq!(2, attributes.clone().count());

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("type", str::from_utf8(attr.key)?);
                assert_eq!("worksheet", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("name", str::from_utf8(attr.key)?);
                assert_eq!("random_symbol", str::from_utf8(&attr.value)?);

                Ok(())
            }
            _ => panic!("unexpected event"),
        }));

        let object = IdentObject::Ident(
            "random_symbol".intern(),
            IdentKind::Worksheet,
            Source::default(),
        );
        let mut sections = Sections::new();
        sections.map.push_body(&object);
        sut.write_sections(&sections, &String::from(""))?;

        Ok(())
    }

    #[test]
    fn write_sections_with_sources() -> Result {
        let mut sut = Sut::new(vec![]);

        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Empty(bytes_start) => {
                let name = str::from_utf8(bytes_start.name())?;
                assert_eq!("preproc:sym", name);

                let mut attributes = bytes_start.attributes();
                assert_eq!(7, attributes.clone().count());

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("type", str::from_utf8(attr.key)?);
                assert_eq!("worksheet", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("name", str::from_utf8(attr.key)?);
                assert_eq!("name", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("preproc:generated", str::from_utf8(attr.key)?);
                assert_eq!("true", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("src", str::from_utf8(attr.key)?);
                assert_eq!("rootname", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("parent", str::from_utf8(attr.key)?);
                assert_eq!("parent", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("yields", str::from_utf8(attr.key)?);
                assert_eq!("yields", str::from_utf8(&attr.value)?);

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("desc", str::from_utf8(attr.key)?);
                assert_eq!("sym desc", str::from_utf8(&attr.value)?);

                Ok(())
            }
            _ => panic!("unexpected event"),
        }));

        let nsym = "name".intern();
        let ssym = "src".intern();
        let psym = "parent".intern();
        let ysym = "yields".intern();
        let fsym = "from".intern();

        let attrs = SymAttrs {
            pkg_name: Some(nsym),
            src: Some(ssym),
            generated: true,
            parent: Some(psym),
            yields: Some(ysym),
            desc: Some("sym desc".into()),
            from: Some(fsym),
            virtual_: true,
            ..Default::default()
        };
        let object =
            IdentObject::Ident(nsym, IdentKind::Worksheet, attrs.into());
        let mut sections = Sections::new();
        sections.map.push_body(&object);
        sut.write_sections(&sections, &String::from("root"))?;

        Ok(())
    }

    #[test]
    fn write_froms() -> Result {
        let mut sut = Sut::new(vec![]);

        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Empty(bytes_start) => {
                let name = str::from_utf8(bytes_start.name())?;
                assert_eq!("l:from", name);

                let mut attributes = bytes_start.attributes();
                assert_eq!(1, attributes.clone().count());

                let attr = attributes.next().expect("Expects attributes")?;
                assert_eq!("name", str::from_utf8(attr.key)?);
                assert_eq!("dest symbol", str::from_utf8(&attr.value)?);

                Ok(())
            }
            _ => panic!("unexpected event"),
        }));

        let sym = "source symbol".intern();
        let symb = "dest symbol".intern();

        let mut src = Source::default();
        src.from = Some(symb);
        let object = IdentObject::Ident(sym, IdentKind::Worksheet, src);
        let mut sections = Sections::new();
        sections.map.push_body(&object);
        sut.write_froms(&sections)?;

        Ok(())
    }

    #[test]
    fn write_froms_no_from_no_write() -> Result {
        let mut sut = Sut::new(vec![]);

        sut.writer.write_callback = Some(Box::new(|event| match event {
            _ => panic!("unexpected write"),
        }));

        let sym = "random_symbol".intern();

        let object =
            IdentObject::Ident(sym, IdentKind::Worksheet, Source::default());
        let mut sections = Sections::new();
        sections.map.push_body(&object);
        sut.write_froms(&sections)?;

        Ok(())
    }

    #[test]
    fn write_element() -> Result {
        let mut sut = Sut::new(vec![]);

        sut.writer.write_callback = Some(Box::new(|event| match event {
            Event::Start(bytes) => {
                let name = str::from_utf8(bytes.name());
                match name {
                    Ok("foo") => {
                        let attributes = bytes.attributes();
                        assert_eq!(0, attributes.count());
                        Ok(())
                    }
                    _ => panic!("unreachable"),
                }
            }
            Event::End(bytes) => {
                let name = str::from_utf8(bytes.name());
                match name {
                    Ok("foo") => Ok(()),
                    _ => panic!("unreachable"),
                }
            }
            _ => panic!("did not match expected event"),
        }));

        sut.write_element(b"foo", |writer| Ok(writer))?;

        Ok(())
    }
}
