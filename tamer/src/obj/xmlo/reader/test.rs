// Tests xmlo object file reader
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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
    num::Dtype,
    obj::xmlo::SymType,
    parse::{ParseError, ParseState, Parsed},
    span::{dummy::*, Span},
    sym::GlobalSymbolIntern,
    xir::{
        flat::{
            test::{attr, close, close_empty, open},
            Depth, XirfToken as Xirf,
        },
        QName,
    },
};

type Sut = XmloReader;

use Parsed::{Incomplete, Object as O};
use XmloToken::*;

#[test]
fn fails_on_invalid_root() {
    let tok = open("not-a-valid-package-node", S1, Depth(0));

    let mut sut = Sut::parse([tok.clone()].into_iter());

    assert_matches!(
        sut.next(),
        Some(Err(ParseError::StateError(XmloError::UnexpectedRoot(etok)))) if etok == tok
    );
}

fn common_parses_package_attrs(package: QName) {
    let name = "/pkgroot".into();
    let relroot = "../../".into();
    let elig = "elig-class-yields".into();

    #[rustfmt::skip]
    let toks = [
        open(package, S1, Depth(0)),
          attr("name", name, (S2, S3)),
          attr("__rootpath", relroot, (S2, S3)),
          attr("program", crate::sym::st::raw::L_TRUE, (S3, S4)),
          attr(("preproc", "elig-class-yields"), elig, (S3, S4)),
        close(Some(package), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(PkgStart(S1)),
              O(PkgName(SPair(name, S3))),
              O(PkgRootPath(SPair(relroot, S3))),
              // Span for the program flag is the attr name,
              //   rather than the value,
              //   since the value is just a boolean and does not provide as
              //     useful of context.
              O(PkgProgramFlag(S3)),
              O(PkgEligClassYields(SPair(elig, S4))),
            Incomplete,
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

// For compatibility with XSLT-based compiler.
#[test]
fn adds_missing_leading_slash_to_canonical_name() {
    let name = "needs/leading".into();

    #[rustfmt::skip]
    let toks = [
        open(QN_PACKAGE, S1, Depth(0)),
          attr("name", name, (S2, S3)),
        close(Some(QN_PACKAGE), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(PkgStart(S1)),
              O(PkgName(SPair("/needs/leading".into(), S3))),
            Incomplete,
        ]),
        sut.collect(),
    );
}

// Maintains BC with existing system,
//   but this ought to reject in the future.
#[test]
fn ignores_unknown_package_attr() {
    let name = "/pkgroot".into();

    #[rustfmt::skip]
    let toks = [
        open(QN_PACKAGE, S1, Depth(0)),
          attr("name", name, (S2, S3)),
          // This is ignored.
          attr("unknown", name, (S2, S3)),
        close(Some(QN_PACKAGE), S2, Depth(0)),
    ]
    .into_iter();

    let sut = Sut::parse(toks);

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            O(PkgStart(S1)),
              O(PkgName(SPair(name, S3))),
              Incomplete, // The unknown attribute
            Incomplete,
        ]),
        sut.collect(),
    );
}

#[test]
fn symtable_err_missing_sym_name() {
    let toks = [
        open(QN_P_SYM, S1, Depth(0)),
        // No attributes, but importantly, no name.
        close(Some(QN_P_SYMTABLE), S2, Depth(0)),
    ]
    .into_iter();

    let mut sut = SymtableState::parse(toks);

    assert_eq!(sut.next(), Some(Ok(Incomplete)),);

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
                    open(QN_P_SYM, SSYM, Depth(0)),
                    attr(QN_NAME, name, (S2, S3)),
                    $(
                        attr(
                            stringify!($key),
                            $val.unwrap_into(),
                            (S3, SATTRVAL)
                        ),
                    )*
                    close(Some(QN_P_SYM), S2, Depth(0)),
                ]
                    .into_iter();

                assert_eq!(
                    match $expect {
                        Ok(expected) =>
                            Ok(vec![
                                Incomplete,  // Opening tag
                                Incomplete,  // @name
                                $(
                                    // For each attribute ($key here is necessary
                                    //   for macro iteration).
                                    #[allow(unused)]
                                    #[doc=stringify!($key)]
                                    Incomplete,
                                )*
                                O(SymDecl(
                                    SPair(name, S3),
                                    expected
                                )),
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
        dtype: Some(Dtype::Boolean),
        ..Default::default()
    })

    dtyinteger: [dtype="integer"] => Ok(SymAttrs {
        dtype: Some(Dtype::Integer),
        ..Default::default()
    })

    dtyfloat: [dtype="float"] => Ok(SymAttrs {
        dtype: Some(Dtype::Float),
        ..Default::default()
    })

    dtyempty: [dtype="empty"] => Ok(SymAttrs {
        dtype: Some(Dtype::Empty),
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
            dtype: Some(Dtype::Float),
            extern_: true,
            ..Default::default()
        })
}

// Can't be tested using the above macro because of the attr name.
#[test]
fn symtable_sym_generated_true() {
    let name = "generated_true".into();

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM, SSYM, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),
          attr(
              ("preproc", "generated"),
              raw::L_TRUE,
              (S3, S4),
          ),
        close(Some(QN_P_SYM), S2, Depth(0)),
    ]
    .into_iter();

    let expected = SymAttrs {
        generated: true,
        ..Default::default()
    };

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            Incomplete, // Opening tag
            Incomplete, // @name
            Incomplete, // @preproc:generated
            O(SymDecl(SPair(name, S3), expected)),
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

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM, SSYM, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),
          attr(QN_TYPE, raw::L_MAP, (S3, S4)),

          // <preproc:from>
          open(QN_P_FROM, S2, Depth(1)),
            attr(QN_NAME, map_from, (S2, S3)),
          close_empty(S4, Depth(1)),
          // />
        close(Some(QN_P_SYM), S2, Depth(0)),
    ]
    .into_iter();

    let expected = SymAttrs {
        ty: Some(SymType::Map),
        from: Some(map_from),
        ..Default::default()
    };

    assert_eq!(
        Ok(vec![
            Incomplete, // Opening tag
            Incomplete, // @name
            Incomplete, // @type
            Incomplete, //   <preproc:from
            Incomplete, //   @name
            Incomplete, //   />
            O(SymDecl(SPair(name, S3), expected)),
        ]),
        SymtableState::parse(toks).collect(),
    );
}

#[test]
fn symtable_map_from_missing_name() {
    let name = "sym-map-from-missing".into();

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM, SSYM, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),
          attr(QN_TYPE, raw::L_MAP, (S3, S4)),

          // <preproc:from>
          open(QN_P_FROM, S2, Depth(1)),
            // @name missing
          close_empty(S4, Depth(1)),
          // />
        close(Some(QN_P_SYM), S2, Depth(0)),
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

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM, SSYM, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),
          attr(QN_TYPE, raw::L_MAP, (S3, S4)),

          // <preproc:from>
          open(QN_P_FROM, S2, Depth(1)),
            attr(QN_NAME, "ok".into(), (S2, S3)),
          close_empty(S4, Depth(1)),
          // />

          // <preproc:from> again (err)
          open(QN_P_FROM, S3, Depth(1)),
            attr(QN_NAME, "bad".into(), (S2, S3)),
          close_empty(S4, Depth(1)),
          // />
        close(Some(QN_P_SYM), S2, Depth(0)),
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

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM_DEP, S1, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),

          // <preproc:sym-ref
          open(QN_P_SYM_REF, S2, Depth(1)),
            attr(QN_NAME, dep1, (S3, S4)),
          close_empty(S4, Depth(1)),
          // />

          // <preproc:sym-ref
          open(QN_P_SYM_REF, S3, Depth(1)),
            attr(QN_NAME, dep2, (S4, S5)),
          close_empty(S4, Depth(1)),
          // />
        close(Some(QN_P_SYM_DEP), S5, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Ok(vec![
            Incomplete,                      // <preproc:sym-ref
            O(SymDepStart(SPair(name, S1))), // @name
            Incomplete,                      // <preproc:sym-ref
            O(Symbol(SPair(dep1, S4))),      // @name
            Incomplete,                      // />
            Incomplete,                      // <preproc:sym-ref
            O(Symbol(SPair(dep2, S5))),      // @name
            Incomplete,                      // />
            Incomplete,                      // </preproc:sym-dep>
        ]),
        SymDepsState::parse(toks).collect()
    );
}

#[test]
fn sym_dep_missing_name() {
    let toks = [
        open(QN_P_SYM_DEP, S1, Depth(0)),
        // missing @name, causes error
        open(QN_P_SYM_REF, S2, Depth(1)),
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

    #[rustfmt::skip]
    let toks = [
        open(QN_P_SYM_DEP, S1, Depth(0)),
          attr(QN_NAME, name, (S2, S3)),

          open(QN_P_SYM_REF, S2, Depth(1)),
          // missing @name, causes error
          close_empty(S3, Depth(1)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::MalformedSymRef(name, S2))),
        SymDepsState::parse(toks)
            .collect::<Result<Vec<Parsed<XmloToken>>, _>>(),
    );
}

#[test]
fn sym_fragment_event() {
    let id1 = "fragsym1".into();
    let id2 = "fragsym2".into();
    let frag1 = "fragment text 1".into();
    let frag2 = "fragment text 2".into();

    #[rustfmt::skip]
    let toks = [
        // first
        open(QN_P_FRAGMENT, S1, Depth(0)),
          attr(QN_ID, id1, (S2, S3)),
          Xirf::Text(Text(frag1, S4), Depth(1)),
        close(Some(QN_P_FRAGMENT), S5, Depth(0)),

        // second
        open(QN_P_FRAGMENT, S2, Depth(0)),
          attr(QN_ID, id2, (S3, S4)),
          Xirf::Text(Text(frag2, S5), Depth(1)),
        close(Some(QN_P_FRAGMENT), S5, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Ok(vec![
            Incomplete,                         // <preproc:fragment
            Incomplete,                         // @id
            O(Fragment(SPair(id1, S1), frag1)), // text
            Incomplete,                         // </preproc:fragment>
            Incomplete,                         // <preproc:fragment
            Incomplete,                         // @id
            O(Fragment(SPair(id2, S2), frag2)), // text
            Incomplete,                         // </preproc:fragment>
        ]),
        FragmentsState::parse(toks).collect()
    );
}

#[test]
fn sym_fragment_missing_id() {
    let toks = [
        open(QN_P_FRAGMENT, S1, Depth(0)),
        // missing @id
        Xirf::Text(Text("text".into(), S4), Depth(1)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::UnassociatedFragment(S1))),
        FragmentsState::parse(toks)
            .collect::<Result<Vec<Parsed<XmloToken>>, _>>(),
    );
}

// Yes, this happened.
#[test]
fn sym_fragment_empty_id() {
    let toks = [
        open(QN_P_FRAGMENT, S1, Depth(0)),
        // empty @id
        attr(QN_ID, "".into(), (S3, S4)),
        Xirf::Text(Text("text".into(), S4), Depth(1)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::UnassociatedFragment(S1))),
        FragmentsState::parse(toks)
            .collect::<Result<Vec<Parsed<XmloToken>>, _>>(),
    );
}

// TODO: Re-enable after compiler bug is resolved.
// See implementation.
//#[test]
fn _sym_fragment_missing_text() {
    let id = "fragsym".into();

    let toks = [
        open(QN_P_FRAGMENT, S1, Depth(0)),
        attr(QN_ID, id, (S3, S4)),
        // missing text
        close(Some(QN_P_FRAGMENT), S5, Depth(0)),
    ]
    .into_iter();

    assert_eq!(
        Err(ParseError::StateError(XmloError::MissingFragmentText(
            id, S1
        ))),
        FragmentsState::parse(toks)
            .collect::<Result<Vec<Parsed<XmloToken>>, _>>(),
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
    let symfrag_id = "symfrag".into();
    let frag = "fragment text".into();

    #[rustfmt::skip]
    let toks_header = [
        open(QN_PACKAGE, S1, Depth(0)),
          open(QN_P_SYMTABLE, S2, Depth(1)),
            open(QN_P_SYM, S3, Depth(2)),
              attr(QN_NAME, sym_name, (S2, S3)),
            close_empty(S4, Depth(2)),
          close(Some(QN_P_SYMTABLE), S4, Depth(1)),

          open(QN_P_SYM_DEPS, S2, Depth(1)),
            open(QN_P_SYM_DEP, S3, Depth(3)),
              attr(QN_NAME, symdep_name, (S2, S3)),
            close(Some(QN_P_SYM_DEP), S4, Depth(3)),
          close(Some(QN_P_SYM_DEPS), S3, Depth(1)),

          open(QN_P_FRAGMENTS, S2, Depth(1)),
            open(QN_P_FRAGMENT, S4, Depth(2)),
              attr(QN_ID, symfrag_id, (S2, S3)),
              Xirf::Text(Text(frag, S5), Depth(3)),
            close(Some(QN_P_FRAGMENT), S4, Depth(2)),
          close(Some(QN_P_FRAGMENTS), S3, Depth(1)),
       // No closing root node:
       //   ensure that we can just end at the header without parsing further).
    ]
    .into_iter();

    let sut = Sut::parse(toks_header);

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(PkgStart(S1)),
              O(SymDecl(SPair(sym_name, S3), Default::default(),)),
              O(SymDepStart(SPair(symdep_name, S3))),
              O(SymDepEnd(S3)),
              O(Fragment(SPair(symfrag_id, S4), frag)),
              O(Eoh(S3)),
        ]),
        sut.filter(|parsed| match parsed {
            Ok(Incomplete) => false,
            _ => true,
        })
        .collect(),
    );
}

#[test]
fn xmlo_end_after_sym_deps_before_fragments() {
    let sym_name = "sym".into();
    let symdep_name = "symdep".into();

    #[rustfmt::skip]
    let toks_header = [
        open(QN_PACKAGE, S1, Depth(0)),
          open(QN_P_SYMTABLE, S2, Depth(1)),
            open(QN_P_SYM, S3, Depth(2)),
              attr(QN_NAME, sym_name, (S2, S3)),
            close_empty(S4, Depth(2)),
          close(Some(QN_P_SYMTABLE), S4, Depth(1)),

          open(QN_P_SYM_DEPS, S2, Depth(1)),
            open(QN_P_SYM_DEP, S3, Depth(3)),
              attr(QN_NAME, symdep_name, (S2, S3)),
            close(Some(QN_P_SYM_DEP), S4, Depth(3)),
          close(Some(QN_P_SYM_DEPS), S3, Depth(1)),

        // End before fragments.
    ]
    .into_iter();

    let sut = Sut::parse(toks_header);

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            O(PkgStart(S1)),
              O(SymDecl(SPair(sym_name, S3), Default::default(),)),
              O(SymDepStart(SPair(symdep_name, S3))),
              O(SymDepEnd(S3)),
        ]),
        sut.filter(|parsed| match parsed {
            Ok(Incomplete) => false,
            _ => true,
        })
        .collect(),
    );
}
