// Tests for xmlo object file reader
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

use super::*;
use crate::obj::xmlo::{SymDtype, SymType};
use crate::sym::GlobalSymbolIntern;
use crate::test::quick_xml::*;
use crate::tpwrap::quick_xml::{Error as XmlError, InnerXmlError};

type Sut<B> = XmloReader<B>;

// Tests marked with "DONE" have been migrated to `super::test`.

macro_rules! xmlo_tests {
    ($(fn $fn:ident($sut:ident) $body:block)*) => {
        $(
            #[test]
            fn $fn() -> XmloResult<()> {
                let stub_data: &[u8] = &[];

                #[allow(unused_mut)]
                let mut $sut = Sut::new(stub_data);

                // We don't want to have to output a proper root node
                // for every one of our tests.
                $sut.seen_root = true;
                $sut.pkg_name = Some("pkg/name".intern());

                $body;

                Ok(())
            }
        )*
    };
}

xmlo_tests! {
    fn sets_parsing_options(sut) {
        assert_eq!(Some(false), sut.reader.check_end);
    }

    // DONE
    fn proxies_xml_failures(sut) {
        sut.reader.next_event =
            Some(Box::new(|_, _| Err(InnerXmlError::UnexpectedEof("test".into()))));

        match sut.read_event() {
            Err(XmloError::XmlError(XmlError(InnerXmlError::UnexpectedEof(_)))) => (),
            bad => panic!("expected XmlError: {:?}", bad),
        }
    }

    fn sym_fails_without_name(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::UnassociatedSym) => (),
            bad => panic!("expected XmloError::UnassociatedSym: {:?}", bad),
        }
    }

    // DONE
    fn fails_on_invalid_root(sut) {
        // xmlo_tests macro sets this for us, so we need to clear it to
        // be able to perform the check
        sut.seen_root = false;

        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"not-a-valid-package-node",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::UnexpectedRoot) => (),
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    // DONE
    fn recognizes_valid_roots(sut) {
        // xmlo_tests macro sets this for us, so we need to clear it to
        // be able to perform the check
        sut.seen_root = false;

        // First valid root
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"package",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        // Will fail if the above is not valid.  See below for actually
        // testing the package node.
        sut.read_event()?;

        // We don't process namespaces (to slow) so we have to handle
        // the difference explicitly.
        sut.seen_root = false;
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"lv:package",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        sut.read_event()?;
    }

    // DONE
    fn package_event_program(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"package",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(b"program", b"true"),
                    MockAttribute::new(
                        b"preproc:elig-class-yields", b"eligClassYields",
                    ),
                ])),
            )))
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::Package(PackageAttrs {
                program: true,
                elig: Some("eligClassYields".intern()),
                ..Default::default()
            }),
            result
        );
    }

    // DONE
    fn package_event_nonprogram(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"package",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::Package(PackageAttrs {
                program: false,
                ..Default::default()
            }),
            result
        );
    }

    // DONE
    fn package_event_name(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"package",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(b"name", b"pkg/name"),
                    MockAttribute::new(b"__rootpath", b"../../"),
                ])),
            )))
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::Package(PackageAttrs {
                name: Some("pkg/name".intern()),
                relroot: Some("../../".into()),
                program: false,
                ..Default::default()
            }),
            result
        );
    }

    fn sym_dep_event(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym-dep",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"depsym",
                )])),
            ))),
            1 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:sym-ref",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"dep1",
                )])),
            ))),
            2 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:sym-ref",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"dep2",
                )])),
            ))),
            3 => Ok(XmlEvent::End(MockBytesEnd::new(b"preproc:sym-dep"))),
            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::SymDeps(
                "depsym".intern(),
                vec!["dep1".intern(), "dep2".intern()]
            ),
            result
        );
    }

    fn sym_dep_fails_with_missing_name(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym-dep",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::UnassociatedSymDep) => (),
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn sym_dep_malformed_ref_missing_name(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym-dep",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"depsymbad",
                )])),
            ))),
            // no attributes
            1 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:sym-ref",
                Some(MockAttributes::new(vec![])),
            ))),
            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        match sut.read_event() {
            Err(XmloError::MalformedSymRef(msg)) => {
                assert!(msg.contains("preproc:sym-ref/@name"))
            },
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn sym_dep_malformed_ref_unexpected_element(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym-dep",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"depsym-unexpected",
                )])),
            ))),
            // text is okay (e.g. whitespace)
            1 => Ok(XmlEvent::Text(MockBytesText::new(
                b"      ",
            ))),
            // unexpected (not a preproc:sym-ref)
            2 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:unexpected",
                Some(MockAttributes::new(vec![])),
            ))),
            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        match sut.read_event() {
            Err(XmloError::MalformedSymRef(msg)) => {
                assert!(msg.contains("depsym-unexpected"))
            },
            bad => panic!("expected XmloError: {:?}", bad),
        }

        // We should have gotten past the text
        assert_eq!(3, sut.reader.event_i, "Did not ignore Text");
    }

    fn eoh_after_fragments(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::End(MockBytesEnd::new(b"preproc:fragments")))
        }));

        let result = sut.read_event()?;

        assert_eq!(XmloEvent::Eoh, result);
    }

    fn fragment_event(sut) {
        let expected = "fragment text";

        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:fragment",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"id", b"fragsym",
                )])),
            ))),
            1 => Ok(XmlEvent::Text(MockBytesText::new(
                b"fragment text"
            ))),
            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        let result = sut.read_event()?;

        assert!(matches!(
            result,
            XmloEvent::Fragment(sym, given)
                if sym == "fragsym".intern() && given.lookup_str() == expected
        ));
    }

    fn fragment_fails_with_missing_id(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:fragment",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::UnassociatedFragment) => (),
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    // Yes, this happened.
    fn fragment_fails_with_empty_id(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:fragment",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"id", b"",
                )])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::UnassociatedFragment) => (),
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn fragment_fails_with_missing_text(sut) {
        sut.reader.next_text = Some(Err(InnerXmlError::TextNotFound));

        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:fragment",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"id", b"fragsym",
                )])),
            )))
        }));

        match sut.read_event() {
            Err(XmloError::MissingFragmentText(symname)) => {
                assert_eq!("fragsym".intern(), symname)
            }
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn skips_unneeded_nodes(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            // Skip over this
            0 => Ok(XmlEvent::End(MockBytesEnd::new(
                b"preproc:ignore-me",
            ))),

            // And this
            1 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:symtable",
                Some(MockAttributes::new(vec![])),
            ))),

            // But process this
            2 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![MockAttribute::new(
                    b"name", b"sym-expected",
                )])),
            ))),

            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::SymDecl(
                "sym-expected".intern(),
                SymAttrs {
                    pkg_name: Some("pkg/name".intern()),
                    ..Default::default()
                },
            ),
            result
        );
    }

    // Some preproc:sym nodes have children (`func` symbols,
    // specifically) that we choose to ignore.  See next test for
    // data we do care about.
    fn sym_nonempty_element(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            // Notice Start, not Empty
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(
                        b"name", b"sym-nonempty",
                    ),
                    // Just to observe that processing works properly
                    MockAttribute::new(
                        b"dim", b"2",
                    ),
                ])),
            )))
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::SymDecl(
                "sym-nonempty".intern(),
                SymAttrs {
                    dim: Some(2),
                    pkg_name: Some("pkg/name".intern()),
                    ..Default::default()
                },
            ),
            result
        );

        // Ensure that we have skipped the remainder of this element
        // (all of its children) so that the next event will yield the
        // next symbol.
        assert_eq!(Some("preproc:sym".into()), sut.reader.read_to_end_name);
    }

    // `map` symbols include information about their source
    // fields.
    fn sym_map_from(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            // Notice Start, not Empty
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(
                        b"name", b"sym-map-from",
                    ),
                    MockAttribute::new(
                        b"type", b"map",
                    ),
                ])),
            ))),

            // make sure that whitespace is permitted
            1 => Ok(XmlEvent::Text(MockBytesText::new(
                b"      ",
            ))),

            2 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:from",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(
                        b"name", b"from-a",
                    ),
                ])),
            ))),

            3 => Ok(XmlEvent::End(MockBytesEnd::new(
                b"preproc:sym",
            ))),

            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        let result = sut.read_event()?;

        assert_eq!(
            XmloEvent::SymDecl(
                "sym-map-from".intern(),
                SymAttrs {
                    ty: Some(SymType::Map),
                    from: Some(
                        "from-a".intern(),
                    ),
                    pkg_name: Some("pkg/name".intern()),
                    ..Default::default()
                },
            ),
            result
        );

        // Should _not_ have read to the end.
        assert_eq!(None, sut.reader.read_to_end_name);
    }

    fn sym_map_from_missing_name(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            // Notice Start, not Empty
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(
                        b"name", b"sym-map-from-bad",
                    ),
                    MockAttribute::new(
                        b"type", b"map",
                    ),
                ])),
            ))),

            // missing @name
            1 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:from",
                Some(MockAttributes::new(vec![])),
            ))),

            2 => Ok(XmlEvent::End(MockBytesEnd::new(
                b"preproc:sym",
            ))),

            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        match sut.read_event() {
            Err(XmloError::InvalidMapFrom(msg)) => {
                assert!(msg.contains("preproc:from"))
            }
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn sym_map_from_unexpected_data(sut) {
        sut.reader.next_event = Some(Box::new(|_, event_i| match event_i {
            // Notice Start, not Empty
            0 => Ok(XmlEvent::Start(MockBytesStart::new(
                b"preproc:sym",
                Some(MockAttributes::new(vec![
                    MockAttribute::new(
                        b"name", b"sym-map-from-bad",
                    ),
                    MockAttribute::new(
                        b"type", b"map",
                    ),
                ])),
            ))),

            // garbage
            1 => Ok(XmlEvent::Empty(MockBytesStart::new(
                b"preproc:nonsense",
                Some(MockAttributes::new(vec![])),
            ))),

            _ => Err(InnerXmlError::UnexpectedEof(
                format!("MockXmlReader out of events: {}", event_i).into(),
            )),
        }));

        match sut.read_event() {
            Err(XmloError::InvalidMapFrom(_)) => (),
            bad => panic!("expected XmloError: {:?}", bad),
        }
    }

    fn read_events_via_iterator(sut) {
        sut.reader.next_event = Some(Box::new(|_, _| {
            Ok(XmlEvent::Start(MockBytesStart::new(
                b"package",
                Some(MockAttributes::new(vec![])),
            )))
        }));

        let result = sut.next().unwrap()?;

        assert_eq!(
            XmloEvent::Package(PackageAttrs {
                program: false,
                ..Default::default()
            }),
            result
        );
    }
}

macro_rules! sym_test_reader_event {
    ($sut:ident, $name:ident, $($key:ident=$val:literal),*) => {
        // See xmlo_tests macro for explanation
        $sut.seen_root = true;

        $sut.reader.next_event = Some(Box::new(|_, event_i| {
            match event_i {
                0 => Ok(XmlEvent::Start(MockBytesStart::new(
                    b"package",
                    Some(MockAttributes::new(vec![
                        MockAttribute::new(b"name", b"pkg/name")
                    ])),
                ))),

                1 => Ok(XmlEvent::Empty(MockBytesStart::new(
                    b"preproc:sym",
                    Some(MockAttributes::new(
                        vec![
                            MockAttribute::new(
                                b"name",
                                stringify!($name).as_bytes(),
                            ),
                            $(
                                MockAttribute::new(
                                    stringify!($key).as_bytes(),
                                    $val.as_bytes(),
                                ),
                            )*
                        ],
                    )),
                ))),

                _ => Err(InnerXmlError::UnexpectedEof(
                    format!("MockXmlReader out of events: {}", event_i).into(),
                )),
            }
        }));

        // consume the package to set the name
        let _ = $sut.read_event();
    }
}

macro_rules! sym_tests {
    ($($name:ident: [$($key:ident=$val:literal),*] => $expect:expr)*) => {
        $(
            #[test]
            fn $name() -> XmloResult<()> {
                let stub_data: &[u8] = &[];
                let mut sut = Sut::new(stub_data);

                sym_test_reader_event!(sut, $name, $( $key=$val ),*);

                let result = sut.read_event()?;

                let mut expected_attrs = $expect;
                expected_attrs.pkg_name = Some("pkg/name".intern());

                assert_eq!(
                    XmloEvent::SymDecl(
                        stringify!($name).intern(),
                        expected_attrs
                    ),
                    result
                );
                Ok(())
            }
        )*
    }
}

sym_tests! {
    src: [src="foo/bar/baz"] => SymAttrs {
        // see macro for src relpath
        src: Some("foo/bar/baz".intern()),
        ..Default::default()
    }

    // note that this doesn't test every type; we're not going to
    // duplicate the mapping for all of them here
    tycgen: [type="cgen"] => SymAttrs {
        ty: Some(SymType::Cgen),
        ..Default::default()
    }

    dim_0: [dim="0"] => SymAttrs {
        dim: Some(0),
        ..Default::default()
    }

    dim_1: [dim="1"] => SymAttrs {
        dim: Some(1),
        ..Default::default()
    }

    dtyboolean: [dtype="boolean"] => SymAttrs {
        dtype: Some(SymDtype::Boolean),
        ..Default::default()
    }

    dtyinteger: [dtype="integer"] => SymAttrs {
        dtype: Some(SymDtype::Integer),
        ..Default::default()
    }

    dtyfloat: [dtype="float"] => SymAttrs {
        dtype: Some(SymDtype::Float),
        ..Default::default()
    }

    dtyempty: [dtype="empty"] => SymAttrs {
        dtype: Some(SymDtype::Empty),
        ..Default::default()
    }

    extern_true: [extern="true"] => SymAttrs {
        extern_: true,
        ..Default::default()
    }

    // The compiler will never produce nonsense values, so we'll just
    // provide a sane default rather than adding extra checks (and
    // hopefully we don't regret this)
    extern_crap: [extern="nonsense"] => SymAttrs {
        extern_: false,
        ..Default::default()
    }

    parent: [parent="foo"] => SymAttrs {
        parent: Some("foo".intern()),
        ..Default::default()
    }

    yields: [yields="yield"] => SymAttrs {
        yields: Some("yield".intern()),
        ..Default::default()
    }

    desc: [desc="Description"] => SymAttrs {
        desc: Some("Description".into()),
        ..Default::default()
    }

    r#virtual: [virtual="true"] => SymAttrs {
        virtual_: true,
        ..Default::default()
    }

    r#override: [isoverride="true"] => SymAttrs {
        override_: true,
        ..Default::default()
    }

    // Multiple attributes at once
    multi: [src="foo", type="class", dim="1", dtype="float", extern="true"]
        => SymAttrs {
            // see macro for src relpath
            src: Some("foo".intern()),
            ty: Some(SymType::Class),
            dim: Some(1),
            dtype: Some(SymDtype::Float),
            extern_: true,
            ..Default::default()
        }
}

// can't be tested using the above
#[test]
fn generated_true() -> XmloResult<()> {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    // See xmlo_tests macro for explanation
    sut.seen_root = true;
    sut.pkg_name = Some("pkg/name".intern());

    sut.reader.next_event = Some(Box::new(|_, _| {
        Ok(XmlEvent::Empty(MockBytesStart::new(
            b"preproc:sym",
            Some(MockAttributes::new(vec![
                MockAttribute::new(b"name", b"generated_true"),
                MockAttribute::new(b"preproc:generated", b"true"),
            ])),
        )))
    }));

    let result = sut.read_event()?;

    let expected_attrs = SymAttrs {
        generated: true,
        pkg_name: Some("pkg/name".intern()),
        ..Default::default()
    };

    assert_eq!(
        XmloEvent::SymDecl("generated_true".intern(), expected_attrs),
        result
    );

    Ok(())
}

#[test]
fn fails_on_non_ascii_dim() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    sym_test_reader_event!(sut, fail_sym, dim = "X1");

    match sut.read_event() {
        Err(XmloError::InvalidDim(msg)) => assert!(msg.contains("X1")),
        bad => panic!("expected failure: {:?}", bad),
    }
}

#[test]
fn fails_on_multi_char_dim() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    sym_test_reader_event!(sut, fail_sym, dim = "11");

    match sut.read_event() {
        Err(XmloError::InvalidDim(msg)) => assert!(msg.contains("11")),
        bad => panic!("expected failure: {:?}", bad),
    }
}

#[test]
fn fails_on_invalid_type() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    sym_test_reader_event!(sut, fail_sym, type = "foo");

    match sut.read_event() {
        Err(XmloError::InvalidType(msg)) => assert!(msg.contains("foo")),
        bad => panic!("expected failure: {:?}", bad),
    }
}

#[test]
fn fails_on_invalid_dtype() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    sym_test_reader_event!(sut, fail_sym, dtype = "foo");

    match sut.read_event() {
        Err(XmloError::InvalidDtype(msg)) => assert!(msg.contains("foo")),
        bad => panic!("expected failure: {:?}", bad),
    }
}

#[test]
fn fails_when_missing_sym_name() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    sym_test_reader_event!(sut, fail_sym, dtype = "foo");

    match sut.read_event() {
        Err(XmloError::InvalidDtype(msg)) => assert!(msg.contains("foo")),
        bad => panic!("expected failure: {:?}", bad),
    }
}
