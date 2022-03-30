// Tests xmlo object file reader
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

use std::assert_matches::assert_matches;

use super::*;
use crate::{
    convert::ExpectInto,
    obj::xmlo::{SymDtype, SymType},
    parse::{ParseError, ParseState, Parsed},
    span::{Span, DUMMY_SPAN},
    sym::GlobalSymbolIntern,
    xir::{
        attr::Attr,
        flat::{Depth, Object as Xirf},
        QName,
    },
};

const S1: Span = DUMMY_SPAN;
const S2: Span = S1.offset_add(1).unwrap();
const S3: Span = S2.offset_add(1).unwrap();
const S4: Span = S3.offset_add(1).unwrap();
const S5: Span = S4.offset_add(1).unwrap();

type Sut = XmloReaderState;

#[test]
fn fails_on_invalid_root() {
    let mut sut = Sut::parse(
        [Xirf::Open(
            "not-a-valid-package-node".unwrap_into(),
            S1,
            Depth(0),
        )]
        .into_iter(),
    );

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(XmloError::UnexpectedRoot)))
    );
}

fn common_parses_package_attrs(package: QName) {
    let name = "pkgroot".into();
    let relroot = "../../".into();
    let elig = "elig-class-yields".into();

    let toks = [
        Xirf::Open(package, S1, Depth(0)),
        Xirf::Attr(Attr::new("name".unwrap_into(), name, (S2, S3))),
        Xirf::Attr(Attr::new("__rootpath".unwrap_into(), relroot, (S2, S3))),
        Xirf::Attr(Attr::new(
            "program".unwrap_into(),
            crate::sym::st::raw::L_TRUE,
            (S3, S4),
        )),
        Xirf::Attr(Attr::new(
            ("preproc", "elig-class-yields").unwrap_into(),
            elig,
            (S3, S4),
        )),
        Xirf::Close(Some(package), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,
            Parsed::Object(XmloEvent::PkgName(name)),
            Parsed::Object(XmloEvent::PkgRootPath(relroot)),
            Parsed::Object(XmloEvent::PkgProgramFlag),
            Parsed::Object(XmloEvent::PkgEligClassYields(elig)),
            Parsed::Incomplete,
        ]),
        sut.collect(),
    );
}

// The linker does not [yet] parse namespaces.
#[test]
fn parses_package_attrs_without_ns_prefix() {
    common_parses_package_attrs("package".unwrap_into());
}

// The linker does not [yet] parse namespaces.
#[test]
fn parses_package_attrs_with_ns_prefix() {
    common_parses_package_attrs(("lv", "package").unwrap_into());
}

// Maintains BC with existing system,
//   but this ought to reject in the future.
#[test]
fn ignores_unknown_package_attr() {
    let name = "pkgroot".into();

    let toks = [
        Xirf::Open(QN_PACKAGE, S1, Depth(0)),
        Xirf::Attr(Attr::new("name".unwrap_into(), name, (S2, S3))),
        // This is ignored.
        Xirf::Attr(Attr::new("unknown".unwrap_into(), name, (S2, S3))),
        Xirf::Close(Some(QN_PACKAGE), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete,
            Parsed::Object(XmloEvent::PkgName(name)),
            Parsed::Incomplete, // The unknown attribute
            Parsed::Incomplete,
        ]),
        sut.collect(),
    );
}

#[test]
fn symtable_err_missing_sym_name() {
    let toks = [
        Xirf::Open(QN_SYM, S1, Depth(0)),
        // No attributes, but importantly, no name.
        Xirf::Close(Some(QN_SYMTABLE), S2, Depth(0)),
    ]
    .into_iter();

    let mut sut = SymtableState::parse(toks);

    assert_eq!(sut.next(), Some(Ok(Parsed::Incomplete)),);

    assert_eq!(
        sut.next(),
        Some(Err(ParseError::StateError(XmloError::UnassociatedSym(S1)))),
    );
}

/// The span expected by the below `preproc:sym` tests for the emitted object.
const SSYM: Span = S1;
/// The span expected by failures associated with symbol attributes.
const SATTRVAL: Span = S4;

macro_rules! symtable_tests {
    ($($name:ident: [$($key:ident=$val:literal),*] => $expect:expr)*) => {
        $(
            #[test]
            fn $name() {
                let name = stringify!($name).intern();

                let toks = [
                    Xirf::Open(QN_SYM, SSYM, Depth(0)),
                    Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
                    $(
                        Xirf::Attr(Attr(
                            stringify!($key).unwrap_into(),
                            $val.unwrap_into(),
                            (S3, SATTRVAL)
                        )),
                    )*
                    Xirf::Close(Some(QN_SYM), S2, Depth(0)),
                ]
                    .into_iter();

                assert_eq!(
                    match $expect {
                        Ok(expected) =>
                            Ok(vec![
                                Parsed::Incomplete,  // Opening tag
                                Parsed::Incomplete,  // @name
                                $(
                                    // For each attribute ($key here is necessary
                                    //   for macro iteration).
                                    #[allow(unused)]
                                    #[doc=stringify!($key)]
                                    Parsed::Incomplete,
                                )*
                                Parsed::Object((name, expected, SSYM)),
                            ]),
                        Err(expected) => Err(ParseError::StateError(expected)),
                    },
                    SymtableState::parse(toks).collect(),
                );
            }
        )*
    }
}

symtable_tests! {
    src: [src="foo/bar/baz"] => Ok(SymAttrs {
        // see macro for src relpath
        src: Some("foo/bar/baz".intern()),
        ..Default::default()
    })

    // note that this doesn't test every type; we're not going to
    // duplicate the mapping for all of them here
    tycgen: [type="cgen"] => Ok(SymAttrs {
        ty: Some(SymType::Cgen),
        ..Default::default()
    })

    badtype: [type="bad"] => Err(
        XmloError::InvalidType("bad".into(), SATTRVAL)
    )

    dim_0: [dim="0"] => Ok(SymAttrs {
        dim: Some(Dim::Scalar),
        ..Default::default()
    })

    dim_1: [dim="1"] => Ok(SymAttrs {
        dim: Some(Dim::Vector),
        ..Default::default()
    })

    dim_2: [dim="2"] => Ok(SymAttrs {
        dim: Some(Dim::Matrix),
        ..Default::default()
    })

    dim_highnum: [dim="3"] => Err(
        XmloError::InvalidDim("3".into(), SATTRVAL)
    )

    dim_nonum: [dim="X1"] => Err(
        XmloError::InvalidDim("X1".into(), SATTRVAL)
    )

    dtyboolean: [dtype="boolean"] => Ok(SymAttrs {
        dtype: Some(SymDtype::Boolean),
        ..Default::default()
    })

    dtyinteger: [dtype="integer"] => Ok(SymAttrs {
        dtype: Some(SymDtype::Integer),
        ..Default::default()
    })

    dtyfloat: [dtype="float"] => Ok(SymAttrs {
        dtype: Some(SymDtype::Float),
        ..Default::default()
    })

    dtyempty: [dtype="empty"] => Ok(SymAttrs {
        dtype: Some(SymDtype::Empty),
        ..Default::default()
    })

    dtybad: [dtype="bad"] => Err(
        XmloError::InvalidDtype("bad".into(), SATTRVAL)
    )

    extern_true: [extern="true"] => Ok(SymAttrs {
        extern_: true,
        ..Default::default()
    })

    // The compiler will never produce nonsense values, so we'll just
    // provide a sane default rather than adding extra checks (and
    // hopefully we don't regret this)
    extern_crap: [extern="nonsense"] => Ok(SymAttrs {
        extern_: false,
        ..Default::default()
    })

    parent: [parent="foo"] => Ok(SymAttrs {
        parent: Some("foo".intern()),
        ..Default::default()
    })

    yields: [yields="yield"] => Ok(SymAttrs {
        yields: Some("yield".intern()),
        ..Default::default()
    })

    desc: [desc="Description"] => Ok(SymAttrs {
        desc: Some("Description".into()),
        ..Default::default()
    })

    r#virtual: [virtual="true"] => Ok(SymAttrs {
        virtual_: true,
        ..Default::default()
    })

    r#override: [isoverride="true"] => Ok(SymAttrs {
        override_: true,
        ..Default::default()
    })

    // Multiple attributes at once
    multi: [src="foo", type="class", dim="1", dtype="float", extern="true"]
        => Ok(SymAttrs {
            // see macro for src relpath
            src: Some("foo".intern()),
            ty: Some(SymType::Class),
            dim: Some(Dim::Vector),
            dtype: Some(SymDtype::Float),
            extern_: true,
            ..Default::default()
        })
}

// Can't be tested using the above macro because of the attr name.
#[test]
fn symtable_sym_generated_true() {
    let name = "generated_true".into();

    let toks = [
        Xirf::Open(QN_SYM, SSYM, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        Xirf::Attr(Attr(
            ("preproc", "generated").unwrap_into(),
            raw::L_TRUE,
            (S3, S4),
        )),
        Xirf::Close(Some(QN_SYM), S2, Depth(0)),
    ]
    .into_iter();

    let expected = SymAttrs {
        generated: true,
        ..Default::default()
    };

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete, // Opening tag
            Parsed::Incomplete, // @name
            Parsed::Incomplete, // @preproc:generated
            Parsed::Object((name, expected, SSYM)),
        ]),
        SymtableState::parse(toks).collect(),
    );
}

// `map` symbols include information about their source
// fields.
#[test]
fn symtable_map_from() {
    let name = "sym-map-from".into();
    let map_from = "from-a".into();

    let toks = [
        Xirf::Open(QN_SYM, SSYM, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        Xirf::Attr(Attr(QN_TYPE, raw::L_MAP, (S3, S4))),
        // <preproc:from>
        Xirf::Open(QN_FROM, S2, Depth(1)),
        Xirf::Attr(Attr(QN_NAME, map_from, (S2, S3))),
        Xirf::Close(None, S4, Depth(1)),
        // />
        Xirf::Close(Some(QN_SYM), S2, Depth(0)),
    ]
    .into_iter();

    let expected = SymAttrs {
        ty: Some(SymType::Map),
        from: Some(map_from),
        ..Default::default()
    };

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete, // Opening tag
            Parsed::Incomplete, // @name
            Parsed::Incomplete, // @type
            Parsed::Incomplete, //   <preproc:from
            Parsed::Incomplete, //   @name
            Parsed::Incomplete, //   />
            Parsed::Object((name, expected, SSYM)),
        ]),
        SymtableState::parse(toks).collect(),
    );
}

#[test]
fn symtable_map_from_missing_name() {
    let name = "sym-map-from-missing".into();

    let toks = [
        Xirf::Open(QN_SYM, SSYM, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        Xirf::Attr(Attr(QN_TYPE, raw::L_MAP, (S3, S4))),
        // <preproc:from>
        Xirf::Open(QN_FROM, S2, Depth(1)),
        // @name missing
        Xirf::Close(None, S4, Depth(1)),
        // />
        Xirf::Close(Some(QN_SYM), S2, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::MapFromNameMissing(name, S2))), // />
        SymtableState::parse(toks)
            .collect::<Result<Vec<Parsed<<SymtableState as ParseState>::Object>>, _>>(),
    );
}

// Multiple `from` nodes used to be a thing but are no longer utilized.
#[test]
fn symtable_map_from_multiple() {
    let name = "sym-map-from-missing".into();

    let toks = [
        Xirf::Open(QN_SYM, SSYM, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        Xirf::Attr(Attr(QN_TYPE, raw::L_MAP, (S3, S4))),
        // <preproc:from>
        Xirf::Open(QN_FROM, S2, Depth(1)),
        Xirf::Attr(Attr(QN_NAME, "ok".into(), (S2, S3))),
        Xirf::Close(None, S4, Depth(1)),
        // />
        // <preproc:from> again (err)
        Xirf::Open(QN_FROM, S3, Depth(1)),
        Xirf::Attr(Attr(QN_NAME, "bad".into(), (S2, S3))),
        Xirf::Close(None, S4, Depth(1)),
        // />
        Xirf::Close(Some(QN_SYM), S2, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::MapFromMultiple(name, S3))),
        SymtableState::parse(toks)
            .collect::<Result<Vec<Parsed<<SymtableState as ParseState>::Object>>, _>>(),
    );
}

#[test]
fn sym_dep_event() {
    let name = "depsym".into();
    let dep1 = "dep1".into();
    let dep2 = "dep2".into();

    let toks = [
        Xirf::Open(QN_SYM_DEP, S1, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        // <preproc:sym-ref
        Xirf::Open(QN_SYM_REF, S2, Depth(1)),
        Xirf::Attr(Attr(QN_NAME, dep1, (S3, S4))),
        Xirf::Close(None, S4, Depth(1)),
        // />
        // <preproc:sym-ref
        Xirf::Open(QN_SYM_REF, S3, Depth(1)),
        Xirf::Attr(Attr(QN_NAME, dep2, (S4, S5))),
        Xirf::Close(None, S4, Depth(1)),
        // />
        Xirf::Close(Some(QN_SYM_DEP), S5, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Ok(vec![
            Parsed::Incomplete, // <preproc:sym-ref
            Parsed::Object(XmloEvent::SymDepStart(name, S1)), // @name
            Parsed::Incomplete, // <preproc:sym-ref
            Parsed::Object(XmloEvent::Symbol(dep1, S4)), // @name
            Parsed::Incomplete, // />
            Parsed::Incomplete, // <preproc:sym-ref
            Parsed::Object(XmloEvent::Symbol(dep2, S5)), // @name
            Parsed::Incomplete, // />
            Parsed::Incomplete, // </preproc:sym-dep>
        ]),
        SymDepsState::parse(toks).collect()
    );
}

#[test]
fn sym_dep_missing_name() {
    let toks = [
        Xirf::Open(QN_SYM_DEP, S1, Depth(0)),
        // missing @name, causes error
        Xirf::Open(QN_SYM_REF, S2, Depth(1)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::UnassociatedSymDep(S1))),
        SymDepsState::parse(toks)
            .collect::<Result<Vec<Parsed<<SymDepsState as ParseState>::Object>>, _>>(),
    );
}

#[test]
fn sym_ref_missing_name() {
    let name = "depsym".into();

    let toks = [
        Xirf::Open(QN_SYM_DEP, S1, Depth(0)),
        Xirf::Attr(Attr(QN_NAME, name, (S2, S3))),
        Xirf::Open(QN_SYM_REF, S2, Depth(1)),
        // missing @name, causes error
        Xirf::Close(None, S3, Depth(1)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::MalformedSymRef(name, S2))),
        SymDepsState::parse(toks)
            .collect::<Result<Vec<Parsed<<SymDepsState as ParseState>::Object>>, _>>(),
    );
}

/// Very lightly test the default parser composition.
///
/// This test should do just enough to verify that parser state stitching has
///   occurred.
#[test]
fn xmlo_composite_parsers_header() {
    let sym_name = "sym".into();
    let symdep_name = "symdep".into();

    let toks_header = [
        Xirf::Open(QN_PACKAGE, S1, Depth(0)),
        // <preproc:symtable>
        Xirf::Open(QN_SYMTABLE, S2, Depth(1)),
        //   <preproc:sym
        Xirf::Open(QN_SYM, S3, Depth(2)),
        Xirf::Attr(Attr(QN_NAME, sym_name, (S2, S3))),
        Xirf::Close(None, S4, Depth(2)),
        //   />
        Xirf::Close(Some(QN_SYMTABLE), S4, Depth(1)),
        // </preproc:symtable>
        // <preproc:sym-deps>
        Xirf::Open(QN_SYM_DEPS, S2, Depth(1)),
        //   <preproc:sym-dep
        Xirf::Open(QN_SYM_DEP, S3, Depth(3)),
        Xirf::Attr(Attr(QN_NAME, symdep_name, (S2, S3))),
        Xirf::Close(Some(QN_SYM_DEP), S4, Depth(3)),
        //   </preproc:sym-dep>
        Xirf::Close(Some(QN_SYM_DEPS), S3, Depth(1)),
        // </preproc:sym-deps>
        // No closing root node:
        //   ensure that we can just end at the header without parsing further.
    ]
    .into_iter();

    let sut = Sut::parse(toks_header);

    assert_eq!(
        Ok(vec![
            Parsed::Object(XmloEvent::SymDecl(
                sym_name,
                Default::default(),
                S3
            )),
            Parsed::Object(XmloEvent::SymDepStart(symdep_name, S3)),
        ]),
        sut.filter(|parsed| match parsed {
            Ok(Parsed::Incomplete) => false,
            _ => true,
        })
        .collect(),
    );
}
