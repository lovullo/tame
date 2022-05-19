// Test lowering operations into XIR.
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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
use crate::ld::xmle::section::PushResult;
use crate::ld::xmle::Sections;
use crate::obj::xmlo::SymDtype;
use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve};
use crate::xir::tree::merge_attr_fragments;
use crate::{
    asg::{Dim, IdentKind, Source},
    xir::{
        pred::{not, open},
        tree::parser_from,
    },
};
use std::collections::HashSet;

type TestResult = Result<(), Box<dyn std::error::Error>>;

macro_rules! assert_attr{
    ($attrs:ident, $name:ident, $expected:expr, $($args:expr),*) => {
        assert_eq!(
            $attrs.find($name).map(|a| a.value()),
            $expected,
            $($args),*
        )
    }
}

#[test]
fn test_produces_header() -> TestResult {
    let empty = Sections::new();
    let name = "test-pkg".intern();
    let relroot = "rel/root/".intern();

    let result = lower_iter(empty, name, relroot)
        .take_while(|tok| match tok {
            // TODO
            Token::Close(_, _) => false,
            _ => true,
        })
        .collect::<Vec<Token>>();

    assert_eq!(Token::Open(QN_PACKAGE, LSPAN), result[0]);

    Ok(())
}

#[test]
fn test_closes_package() -> TestResult {
    let empty = Sections::new();

    let result = lower_iter(empty, "foo".intern(), "relroot".intern()).last();

    assert_eq!(Some(Token::Close(Some(QN_PACKAGE), LSPAN)), result);
    Ok(())
}

#[test]
fn test_writes_deps() -> TestResult {
    let relroot = "relroot-deps".intern();

    let objs = [
        IdentObject::Ident(
            "cgentest".intern(),
            IdentKind::Cgen(Dim::Vector),
            Source {
                yields: Some("yieldsValue".intern()),
                parent: Some("cparent".intern()),
                generated: true,
                pkg_name: Some("pkg/name".intern()),
                ..Default::default()
            },
        ),
        IdentObject::Ident(
            "classtest".intern(),
            IdentKind::Class(Dim::Matrix),
            Default::default(),
        ),
        IdentObject::Ident(
            "consttest".intern(),
            IdentKind::Const(Dim::Scalar, SymDtype::Boolean),
            Default::default(),
        ),
        IdentObject::Ident(
            "functest".intern(),
            IdentKind::Func(Dim::Matrix, SymDtype::Integer),
            Default::default(),
        ),
        IdentObject::Ident(
            "gentest".intern(),
            IdentKind::Gen(Dim::Matrix, SymDtype::Boolean),
            Default::default(),
        ),
        IdentObject::Ident(
            "lparamtest".intern(),
            IdentKind::Gen(Dim::Matrix, SymDtype::Float),
            Default::default(),
        ),
        IdentObject::Ident(
            "paramtest".intern(),
            IdentKind::Gen(Dim::Scalar, SymDtype::Integer),
            Default::default(),
        ),
        IdentObject::Ident(
            "ratetest".intern(),
            IdentKind::Rate(SymDtype::Integer),
            Default::default(),
        ),
        IdentObject::Ident(
            "tpltest".intern(),
            IdentKind::Tpl,
            Default::default(),
        ),
        IdentObject::Ident(
            "typetest".intern(),
            IdentKind::Type(SymDtype::Integer),
            Default::default(),
        ),
        IdentObject::Ident(
            "mapheadtest".intern(),
            IdentKind::MapHead,
            Default::default(),
        ),
        IdentObject::Ident(
            "maptest".intern(),
            IdentKind::Map,
            Default::default(),
        ),
        IdentObject::Ident(
            "maptailtest".intern(),
            IdentKind::MapTail,
            Default::default(),
        ),
        IdentObject::Ident(
            "retmapheadtest".intern(),
            IdentKind::RetMapHead,
            Default::default(),
        ),
        IdentObject::Ident(
            "retmaptest".intern(),
            IdentKind::RetMap,
            Default::default(),
        ),
        IdentObject::Ident(
            "retmaptailtest".intern(),
            IdentKind::RetMapTail,
            Default::default(),
        ),
        IdentObject::Ident(
            "metatest".intern(),
            IdentKind::Meta,
            Source {
                desc: Some("test desc".intern()),
                ..Default::default()
            },
        ),
        IdentObject::Ident(
            "worksheettest".intern(),
            IdentKind::Worksheet,
            Default::default(),
        ),
    ];

    // Creating a stub to return our deps prevents us from being obstructed
    // by changes to Sections' requirements.
    struct StubSections<'a> {
        deps: Vec<&'a IdentObject>,
    }

    impl<'a> XmleSections<'a> for StubSections<'a> {
        fn push(&mut self, _ident: &'a IdentObject) -> PushResult {
            unimplemented!()
        }

        fn take_deps(&mut self) -> Vec<&'a IdentObject> {
            self.deps.clone()
        }

        fn take_static(&mut self) -> Vec<SymbolId> {
            vec![]
        }

        fn take_map(&mut self) -> Vec<SymbolId> {
            vec![]
        }

        fn take_map_froms(&mut self) -> fxhash::FxHashSet<SymbolId> {
            Default::default()
        }

        fn take_retmap(&mut self) -> Vec<SymbolId> {
            vec![]
        }

        fn take_exec(&mut self) -> Vec<SymbolId> {
            vec![]
        }
    }

    let sections = StubSections {
        deps: objs.iter().collect(),
    };

    let mut lower_iter = lower_iter(sections, "pkg".intern(), relroot)
        .skip_while(not(open(QN_L_DEP)));

    let mut iter = parser_from(merge_attr_fragments(&mut lower_iter));

    let given = iter
        .next()
        .expect("tree object expected")
        .unwrap() // Tree
        .into_element()
        .expect("element expected");

    // Sanity check to ensure we have the element we're expecting.
    assert_eq!(QN_L_DEP, given.name());

    let children = given.children();

    assert_eq!(
        children.len(),
        objs.len(),
        "expected child node for each dependency"
    );

    let p_syms = children.into_iter().map(|child| {
        let ele = child.as_element().expect("child expected to be an element");

        assert_eq!(QN_P_SYM, ele.name());

        ele
    });

    p_syms.enumerate().for_each(|(i, ele)| {
        let ident = &objs[i];
        let attrs = ele.attrs();

        assert_eq!(attrs.find(QN_NAME).map(|a| a.value()), Some(ident.name()),);

        assert_eq!(
            attrs.find(QN_TYPE).map(|a| a.value()),
            Some(ident.kind().unwrap().as_sym())
        );

        let generated = attrs.find(QN_GENERATED).map(|a| a.value());

        if let Some(Source {
            generated: true, ..
        }) = ident.src()
        {
            assert_eq!(generated, Some("true".intern()));
        } else {
            assert_eq!(generated, None);
        }

        if let Some(Source { parent, .. }) = ident.src() {
            assert_attr!(attrs, QN_PARENT, *parent,);
        }

        if let Some(Source { yields, .. }) = ident.src() {
            assert_attr!(attrs, QN_YIELDS, *yields,);
        }

        if let Some(Source {
            desc: Some(desc), ..
        }) = ident.src()
        {
            assert_attr!(attrs, QN_DESC, Some(*desc),);
        }

        if let Some(Source {
            pkg_name: Some(pkg_name),
            ..
        }) = ident.src()
        {
            let expected = [relroot, *pkg_name]
                .iter()
                .map(GlobalSymbolResolve::lookup_str)
                .collect::<String>()
                .intern();

            assert_attr!(attrs, QN_SRC, Some(expected),);
        }

        // Object-specific attributes
        match ident.kind().unwrap() {
            IdentKind::Cgen(dim) | IdentKind::Class(dim) => {
                assert_attr!(
                    attrs,
                    QN_DIM,
                    Some(Into::<SymbolId>::into(*dim)),
                    "invalid {:?} @dim",
                    ident.kind()
                );
            }

            IdentKind::Const(dim, dtype)
            | IdentKind::Func(dim, dtype)
            | IdentKind::Gen(dim, dtype)
            | IdentKind::Lparam(dim, dtype)
            | IdentKind::Param(dim, dtype) => {
                assert_attr!(
                    attrs,
                    QN_DIM,
                    Some((*dim).into()),
                    "invalid {:?} @dim",
                    ident.kind()
                );

                assert_attr!(
                    attrs,
                    QN_DTYPE,
                    Some((*dtype).into()),
                    "invalid {:?} @dtype",
                    ident.kind()
                );
            }

            IdentKind::Rate(dtype) | IdentKind::Type(dtype) => {
                assert_attr!(
                    attrs,
                    QN_DTYPE,
                    Some((*dtype).into()),
                    "invalid {:?} @dim",
                    ident.kind()
                );
            }

            // The others have no additional attributes
            _ => {}
        }
    });

    Ok(())
}

#[test]
fn test_writes_map_froms() -> TestResult {
    let mut sections = Sections::new();
    let relroot = "relroot-deps".intern();

    let a = IdentObject::IdentFragment(
        "a".intern(),
        IdentKind::Map,
        Source {
            from: Some("froma".intern()),
            ..Default::default()
        },
        "fraga".intern(),
    );

    let b = IdentObject::IdentFragment(
        "a".intern(),
        IdentKind::Map,
        Source {
            from: Some("fromb".intern()),
            ..Default::default()
        },
        "fragb".intern(),
    );

    // Add a duplicate just to ensure that we're using the right method on
    // `Sections` for uniqueness.
    sections.push(&a)?;
    sections.push(&a)?;
    sections.push(&b)?;

    let mut iter = parser_from(
        lower_iter(sections, "pkg".intern(), relroot)
            .skip_while(not(open(QN_L_MAP_FROM))),
    );

    let given = iter
        .next()
        .expect("tree object expected")
        .unwrap() // Tree
        .into_element()
        .expect("element expected");

    // Sanity check to ensure we have the element we're expecting.
    assert_eq!(QN_L_MAP_FROM, given.name());

    let froms = given.children();

    let mut found = HashSet::new();

    froms.iter().for_each(|from| {
        assert_eq!(QN_L_FROM, from.as_element().unwrap().name());

        found.insert(
            from.as_element()
                .unwrap()
                .attrs()
                .find(QN_NAME)
                .expect("expecting @name")
                .value(),
        );
    });

    assert!(found.contains(&"froma".intern()));
    assert!(found.contains(&"fromb".intern()));

    Ok(())
}

macro_rules! test_exec_sec {
    ($name:ident, $qn:ident, $type:expr) => {
        #[test]
        fn $name() -> TestResult {
            let mut sections = Sections::new();
            let relroot = "relroot-exec".intern();

            let frag_a = "a fragment".intern();
            let frag_b = "b fragment".intern();

            let a = IdentObject::IdentFragment(
                "a".intern(),
                $type,
                Default::default(),
                frag_a,
            );

            let b = IdentObject::IdentFragment(
                "b".intern(),
                $type,
                Default::default(),
                frag_b,
            );

            sections.push(&a)?;
            sections.push(&b)?;

            let mut iter = parser_from(
                lower_iter(sections, "pkg".intern(), relroot)
                    .skip_while(not(open($qn))),
            );

            let given = iter
                .next()
                .expect("tree object expected")
                .unwrap() // Tree
                .into_element()
                .expect("element expected");

            // Sanity check to ensure we have the element we're expecting.
            assert_eq!($qn, given.name());

            let nodes = given.children();

            // The text is considered escaped since the fragment was already escaped
            //   within the xmlo file it was read from.
            // Order _absolutely_ matters,
            //   since the purpose of the linker is to put things into the correct
            //   order for execution.
            assert_eq!(Some(frag_a), nodes[0].as_sym());
            assert_eq!(Some(frag_b), nodes[1].as_sym());

            assert_eq!(nodes.len(), 2);

            Ok(())
        }
    };
}

test_exec_sec!(test_map_exec, QN_L_MAP_EXEC, IdentKind::Map);
test_exec_sec!(test_retmap_exec, QN_L_RETMAP_EXEC, IdentKind::RetMap);
test_exec_sec!(test_static, QN_L_STATIC, IdentKind::Worksheet);
test_exec_sec!(test_exec, QN_L_EXEC, IdentKind::Class(Dim::Vector));
