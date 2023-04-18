// Test ASG lowering into xmle sections
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

use super::*;
use crate::{
    asg::{FragmentText, Ident, Source},
    ld::xmle::{section::PushResult, Sections},
    num::{Dim, Dtype},
    parse::util::SPair,
    span::dummy::*,
    sym::GlobalSymbolIntern,
};

/// Create a graph with the expected {ret,}map head/tail identifiers.
fn make_asg() -> Asg {
    let mut asg = Asg::new();

    let text = "dummy fragment".intern();

    {
        let sym = SPair(st::L_MAP_UUUHEAD.into(), S1);
        asg.declare(sym, IdentKind::MapHead, Default::default())
            .unwrap();
        asg.set_fragment(sym, text).unwrap();
    }

    {
        let sym = SPair(st::L_MAP_UUUTAIL.into(), S2);
        asg.declare(sym, IdentKind::MapTail, Default::default())
            .unwrap();
        asg.set_fragment(sym, text).unwrap();
    }

    {
        let sym = SPair(st::L_RETMAP_UUUHEAD.into(), S3);
        asg.declare(sym, IdentKind::RetMapHead, Default::default())
            .unwrap();
        asg.set_fragment(sym, text).unwrap();
    }

    {
        let sym = SPair(st::L_RETMAP_UUUTAIL.into(), S4);
        asg.declare(sym, IdentKind::RetMapTail, Default::default())
            .unwrap();
        asg.set_fragment(sym, text).unwrap();
    }

    asg
}

#[test]
fn graph_sort() -> SortResult<()> {
    // We care only about the order of pushes, not the sections they end
    // up in.
    struct StubSections<'a> {
        pushed: Vec<&'a Ident>,
    }

    impl<'a> XmleSections<'a> for StubSections<'a> {
        fn push(&mut self, ident: &'a Ident) -> PushResult {
            self.pushed.push(ident);
            Ok(())
        }

        fn take_deps(&mut self) -> Vec<&'a Ident> {
            unimplemented!()
        }

        fn take_static(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_map(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_map_froms(&mut self) -> fxhash::FxHashSet<SymbolId> {
            unimplemented!()
        }

        fn take_retmap(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_exec(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }
    }

    let mut asg = make_asg();

    // Add them in an unsorted order.
    let adep = asg
        .declare(
            SPair("adep".into(), S1),
            IdentKind::Meta,
            Default::default(),
        )
        .unwrap();
    let a = asg
        .declare(SPair("a".into(), S2), IdentKind::Meta, Default::default())
        .unwrap();
    let adepdep = asg
        .declare(
            SPair("adepdep".into(), S3),
            IdentKind::Meta,
            Default::default(),
        )
        .unwrap();

    asg.add_dep(a, adep);
    asg.add_dep(adep, adepdep);

    asg.add_root(a);

    let sections = sort(&asg, StubSections { pushed: Vec::new() })?;

    assert_eq!(
        sections.pushed,
        vec![
            // Static head
            get_ident(&asg, st::L_MAP_UUUHEAD),
            get_ident(&asg, st::L_RETMAP_UUUHEAD),
            // Post-order
            asg.get_ident(adepdep).unwrap(),
            asg.get_ident(adep).unwrap(),
            asg.get_ident(a).unwrap(),
            // Static tail
            get_ident(&asg, st::L_MAP_UUUTAIL),
            get_ident(&asg, st::L_RETMAP_UUUTAIL),
        ]
        .into_iter()
        .collect::<Vec<_>>()
    );

    Ok(())
}

#[test]
fn graph_sort_missing_node() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);

    asg.add_root(sym_node);

    match sort(&asg, Sections::new()) {
        Ok(_) => panic!("Unexpected success - dependency is not in graph"),
        Err(SortError::SectionsError(SectionsError::UnresolvedObject(_))) => (),
        bad => {
            panic!(
                "Incorrect error result when dependency is not in graph: {:?}",
                bad
            )
        }
    }

    Ok(())
}

#[test]
fn graph_sort_no_roots_same_as_empty_graph() -> SortResult<()> {
    let mut asg_nonempty_no_roots = make_asg();

    // "empty" (it has the head/tail {ret,}map objects)
    let asg_empty = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    asg_nonempty_no_roots.add_dep_lookup_global(sym, dep);

    assert_eq!(
        sort(&asg_nonempty_no_roots, Sections::new()),
        sort(&asg_empty, Sections::new())
    );

    Ok(())
}

#[test]
fn graph_sort_simple_cycle() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let dep_node = asg
        .declare(
            dep,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();
    asg.set_fragment(dep, FragmentText::from("bar")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);
    let (_, _) = asg.add_dep_lookup_global(dep, sym);

    asg.add_root(sym_node);

    let result = sort(&asg, Sections::new());

    let expected = vec![[dep_node, sym_node]
        .into_iter()
        .map(|o| asg.get(o).unwrap().name())
        .collect::<Vec<_>>()];

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_two_simple_cycles() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let sym2 = SPair("sym2".into(), S2);
    let dep = SPair("dep".into(), S3);
    let dep2 = SPair("dep2".into(), S4);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym2_node = asg
        .declare(
            sym2,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let dep_node = asg
        .declare(
            dep,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let dep2_node = asg
        .declare(
            dep2,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();
    asg.set_fragment(sym2, FragmentText::from("bar")).unwrap();
    asg.set_fragment(dep, FragmentText::from("baz")).unwrap();
    asg.set_fragment(dep2, FragmentText::from("huh")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);
    let (_, _) = asg.add_dep_lookup_global(dep, sym);
    let (_, _) = asg.add_dep_lookup_global(sym2, dep2);
    let (_, _) = asg.add_dep_lookup_global(dep2, sym2);

    asg.add_root(sym_node);

    let result = sort(&asg, Sections::new());

    let expected = [[dep_node, sym_node], [dep2_node, sym2_node]]
        .into_iter()
        .map(|cycle| {
            cycle
                .into_iter()
                .map(|o| asg.get(o).unwrap().name())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_no_cycle_with_edge_to_same_node() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.declare(
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();
    asg.set_fragment(dep, FragmentText::from("bar")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);
    let (_, _) = asg.add_dep_lookup_global(sym, dep);

    asg.add_root(sym_node);

    let result = sort(&asg, Sections::new());

    match result {
        Ok(_) => (),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_cycle_with_a_few_steps() -> SortResult<()> {
    let mut asg = make_asg();

    let sym1 = SPair("sym1".into(), S1);
    let sym2 = SPair("sym2".into(), S2);
    let sym3 = SPair("sym3".into(), S3);

    let sym1_node = asg
        .declare(
            sym1,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym2_node = asg
        .declare(
            sym2,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym3_node = asg
        .declare(
            sym3,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym1, FragmentText::from("foo")).unwrap();
    asg.set_fragment(sym2, FragmentText::from("bar")).unwrap();
    asg.set_fragment(sym3, FragmentText::from("baz")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym1, sym2);
    let (_, _) = asg.add_dep_lookup_global(sym2, sym3);
    let (_, _) = asg.add_dep_lookup_global(sym3, sym1);

    asg.add_root(sym1_node);

    let result = sort(&asg, Sections::new());

    let expected = vec![[sym3_node, sym2_node, sym1_node]
        .into_iter()
        .map(|o| asg.get(o).unwrap().name())
        .collect::<Vec<_>>()];

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_cyclic_function_with_non_function_with_a_few_steps(
) -> SortResult<()> {
    let mut asg = make_asg();

    let sym1 = SPair("sym1".into(), S1);
    let sym2 = SPair("sym2".into(), S2);
    let sym3 = SPair("sym3".into(), S3);

    let sym1_node = asg
        .declare(
            sym1,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym2_node = asg
        .declare(
            sym2,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym3_node = asg
        .declare(
            sym3,
            IdentKind::Func(Dim::Scalar, Dtype::Empty),
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym1, FragmentText::from("foo")).unwrap();
    asg.set_fragment(sym2, FragmentText::from("bar")).unwrap();
    asg.set_fragment(sym3, FragmentText::from("baz")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym1, sym2);
    let (_, _) = asg.add_dep_lookup_global(sym2, sym3);
    let (_, _) = asg.add_dep_lookup_global(sym3, sym1);

    asg.add_root(sym1_node);

    let result = sort(&asg, Sections::new());

    let expected = vec![[sym3_node, sym2_node, sym1_node]
        .into_iter()
        .map(|o| asg.get(o).unwrap().name())
        .collect::<Vec<_>>()];

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_cyclic_bookended_by_functions() -> SortResult<()> {
    let mut asg = make_asg();

    let sym1 = SPair("sym1".into(), S1);
    let sym2 = SPair("sym2".into(), S2);
    let sym3 = SPair("sym3".into(), S3);

    let sym1_node = asg
        .declare(
            sym1,
            IdentKind::Func(Dim::Scalar, Dtype::Empty),
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym2_node = asg
        .declare(
            sym2,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym3_node = asg
        .declare(
            sym3,
            IdentKind::Func(Dim::Scalar, Dtype::Empty),
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym1, FragmentText::from("foo")).unwrap();
    asg.set_fragment(sym2, FragmentText::from("bar")).unwrap();
    asg.set_fragment(sym3, FragmentText::from("baz")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym1, sym2);
    let (_, _) = asg.add_dep_lookup_global(sym2, sym3);
    let (_, _) = asg.add_dep_lookup_global(sym3, sym1);

    asg.add_root(sym1_node);

    let result = sort(&asg, Sections::new());

    let expected = vec![[sym3_node, sym2_node, sym1_node]
        .into_iter()
        .map(|o| asg.get(o).unwrap().name())
        .collect::<Vec<_>>()];

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_cyclic_function_ignored() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Func(Dim::Scalar, Dtype::Empty),
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.declare(
        dep,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();
    asg.set_fragment(dep, FragmentText::from("bar")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);
    let (_, _) = asg.add_dep_lookup_global(dep, sym);

    asg.add_root(sym_node);

    let result = sort(&asg, Sections::new());

    match result {
        Ok(_) => (),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_cyclic_function_is_bookended() -> SortResult<()> {
    let mut asg = make_asg();

    let sym1 = SPair("sym1".into(), S1);
    let sym2 = SPair("sym2".into(), S2);
    let sym3 = SPair("sym3".into(), S3);

    let sym1_node = asg
        .declare(
            sym1,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym2_node = asg
        .declare(
            sym2,
            IdentKind::Func(Dim::Scalar, Dtype::Empty),
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    let sym3_node = asg
        .declare(
            sym3,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.set_fragment(sym1, FragmentText::from("foo")).unwrap();
    asg.set_fragment(sym2, FragmentText::from("bar")).unwrap();
    asg.set_fragment(sym3, FragmentText::from("baz")).unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym1, sym2);
    let (_, _) = asg.add_dep_lookup_global(sym2, sym3);
    let (_, _) = asg.add_dep_lookup_global(sym3, sym1);

    asg.add_root(sym1_node);

    let result = sort(&asg, Sections::new());

    let expected = vec![[sym3_node, sym2_node, sym1_node]
        .into_iter()
        .map(|o| asg.get(o).unwrap().name())
        .collect::<Vec<_>>()];

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}

#[test]
fn graph_sort_ignore_non_linked() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);
    let ignored = SPair("ignored".into(), S3);

    let sym_node = asg
        .declare(
            sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )
        .unwrap();

    asg.declare(
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap();

    asg.declare(
        ignored,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap();

    asg.set_fragment(sym, FragmentText::from("foo")).unwrap();
    asg.set_fragment(dep, FragmentText::from("bar")).unwrap();
    asg.set_fragment(ignored, FragmentText::from("baz"))
        .unwrap();

    let (_, _) = asg.add_dep_lookup_global(sym, dep);
    let (_, _) = asg.add_dep_lookup_global(ignored, sym);

    asg.add_root(sym_node);

    let result = sort(&asg, Sections::new());

    match result {
        Ok(_) => (),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}
