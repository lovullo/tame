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
    asg::{AsgError, FragmentText, Ident, Source},
    ld::xmle::{section::PushResult, Sections},
    num::{Dim, Dtype},
    parse::util::SPair,
    span::dummy::*,
    sym::GlobalSymbolIntern,
};

fn declare(
    asg: &mut Asg,
    name: SPair,
    kind: IdentKind,
    src: Source,
) -> Result<ObjectIndex<Ident>, AsgError> {
    let oi_root = asg.root(name);
    oi_root.declare(asg, name, kind, src)
}

fn lookup_or_missing(asg: &mut Asg, name: SPair) -> ObjectIndex<Ident> {
    let oi_root = asg.root(name);
    oi_root.lookup_or_missing(asg, name)
}

/// Create a graph with the expected {ret,}map head/tail identifiers.
fn make_asg() -> Asg {
    let mut asg = Asg::new();

    let text = "dummy fragment".intern();

    {
        let sym = SPair(st::L_MAP_UUUHEAD.into(), S1);
        declare(&mut asg, sym, IdentKind::MapHead, Default::default())
            .unwrap()
            .set_fragment(&mut asg, text)
            .unwrap();
    }

    {
        let sym = SPair(st::L_MAP_UUUTAIL.into(), S2);
        declare(&mut asg, sym, IdentKind::MapTail, Default::default())
            .unwrap()
            .set_fragment(&mut asg, text)
            .unwrap();
    }

    {
        let sym = SPair(st::L_RETMAP_UUUHEAD.into(), S3);
        declare(&mut asg, sym, IdentKind::RetMapHead, Default::default())
            .unwrap()
            .set_fragment(&mut asg, text)
            .unwrap();
    }

    {
        let sym = SPair(st::L_RETMAP_UUUTAIL.into(), S4);
        declare(&mut asg, sym, IdentKind::RetMapTail, Default::default())
            .unwrap()
            .set_fragment(&mut asg, text)
            .unwrap();
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
    let adep = declare(
        &mut asg,
        SPair("adep".into(), S1),
        IdentKind::Meta,
        Default::default(),
    )
    .unwrap();
    let a = declare(
        &mut asg,
        SPair("a".into(), S2),
        IdentKind::Meta,
        Default::default(),
    )
    .unwrap();
    let adepdep = declare(
        &mut asg,
        SPair("adepdep".into(), S3),
        IdentKind::Meta,
        Default::default(),
    )
    .unwrap();

    a.add_opaque_dep(&mut asg, adep);
    adep.add_opaque_dep(&mut asg, adepdep);

    a.root(&mut asg);

    let sections = sort(&asg, StubSections { pushed: Vec::new() })?;

    assert_eq!(
        sections.pushed,
        vec![
            // Static head
            get_ident(&asg, st::L_MAP_UUUHEAD),
            get_ident(&asg, st::L_RETMAP_UUUHEAD),
            // Post-order
            asg.get(adepdep).unwrap(),
            asg.get(adep).unwrap(),
            asg.get(a).unwrap(),
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

    let oi_dep = lookup_or_missing(&mut asg, dep);

    declare(
        &mut asg,
        sym,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap()
    .add_opaque_dep(&mut asg, oi_dep)
    .root(&mut asg);

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
    let mut asg = make_asg();

    // "empty" (it has the head/tail {ret,}map objects)
    let asg_empty = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let oi_sym = lookup_or_missing(&mut asg, sym);
    let oi_dep = lookup_or_missing(&mut asg, dep);
    oi_sym.add_opaque_dep(&mut asg, oi_dep);

    assert_eq!(
        sort(&asg, Sections::new()),
        sort(&asg_empty, Sections::new())
    );

    Ok(())
}

#[test]
fn graph_sort_simple_cycle() -> SortResult<()> {
    let mut asg = make_asg();

    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    let oi_sym = declare(
        &mut asg,
        sym,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let oi_dep = declare(
        &mut asg,
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    oi_sym.add_opaque_dep(&mut asg, oi_dep);
    oi_dep.add_opaque_dep(&mut asg, oi_sym);

    oi_sym.root(&mut asg);

    let result = sort(&asg, Sections::new());

    let expected = vec![[oi_dep, oi_sym]
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

    let oi_sym = declare(
        &mut asg,
        sym,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let oi_sym2 = declare(
        &mut asg,
        sym2,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let oi_dep = declare(
        &mut asg,
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    let oi_dep2 = declare(
        &mut asg,
        dep2,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("huh"))
    .unwrap();

    oi_sym.add_opaque_dep(&mut asg, oi_dep);
    oi_dep.add_opaque_dep(&mut asg, oi_sym);
    oi_sym2.add_opaque_dep(&mut asg, oi_dep2);
    oi_dep2.add_opaque_dep(&mut asg, oi_sym2);

    oi_sym.root(&mut asg);

    let result = sort(&asg, Sections::new());

    let expected = [[oi_dep, oi_sym], [oi_dep2, oi_sym2]]
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

    let sym_node = declare(
        &mut asg,
        sym,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let dep_node = declare(
        &mut asg,
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    sym_node.add_opaque_dep(&mut asg, dep_node);
    sym_node.add_opaque_dep(&mut asg, dep_node);

    sym_node.root(&mut asg);

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

    let sym1_node = declare(
        &mut asg,
        sym1,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let sym2_node = declare(
        &mut asg,
        sym2,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let sym3_node = declare(
        &mut asg,
        sym3,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    sym1_node.add_opaque_dep(&mut asg, sym2_node);
    sym2_node.add_opaque_dep(&mut asg, sym3_node);
    sym3_node.add_opaque_dep(&mut asg, sym1_node);

    sym1_node.root(&mut asg);

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

    let sym1_node = declare(
        &mut asg,
        sym1,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let sym2_node = declare(
        &mut asg,
        sym2,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let sym3_node = declare(
        &mut asg,
        sym3,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    sym1_node.add_opaque_dep(&mut asg, sym2_node);
    sym2_node.add_opaque_dep(&mut asg, sym3_node);
    sym3_node.add_opaque_dep(&mut asg, sym1_node);

    sym1_node.root(&mut asg);

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

    let sym1_node = declare(
        &mut asg,
        sym1,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let sym2_node = declare(
        &mut asg,
        sym2,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let sym3_node = declare(
        &mut asg,
        sym3,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    sym1_node.add_opaque_dep(&mut asg, sym2_node);
    sym2_node.add_opaque_dep(&mut asg, sym3_node);
    sym3_node.add_opaque_dep(&mut asg, sym1_node);

    sym1_node.root(&mut asg);

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

    let sym_node = declare(
        &mut asg,
        sym,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let dep_node = declare(
        &mut asg,
        dep,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    sym_node.add_opaque_dep(&mut asg, dep_node);
    dep_node.add_opaque_dep(&mut asg, sym_node);

    sym_node.root(&mut asg);

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

    let sym1_node = declare(
        &mut asg,
        sym1,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let sym2_node = declare(
        &mut asg,
        sym2,
        IdentKind::Func(Dim::Scalar, Dtype::Empty),
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let sym3_node = declare(
        &mut asg,
        sym3,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    sym1_node.add_opaque_dep(&mut asg, sym2_node);
    sym2_node.add_opaque_dep(&mut asg, sym3_node);
    sym3_node.add_opaque_dep(&mut asg, sym1_node);

    sym1_node.root(&mut asg);

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

    let sym_node = declare(
        &mut asg,
        sym,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("foo"))
    .unwrap();

    let dep_node = declare(
        &mut asg,
        dep,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("bar"))
    .unwrap();

    let ignored_node = declare(
        &mut asg,
        ignored,
        IdentKind::Tpl,
        Source {
            virtual_: true,
            ..Default::default()
        },
    )
    .unwrap()
    .set_fragment(&mut asg, FragmentText::from("baz"))
    .unwrap();

    sym_node.add_opaque_dep(&mut asg, dep_node);
    ignored_node.add_opaque_dep(&mut asg, sym_node);

    sym_node.root(&mut asg);

    let result = sort(&asg, Sections::new());

    match result {
        Ok(_) => (),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}
