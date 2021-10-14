// ASG lowering into xmle sections
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

//! Lowering of the [ASG](crate::ir::asg) into `xmle` [`XmleSections`].
//!
//! See the [parent module](super) for more information.

use super::section::{SectionsError, XmleSections};
use crate::{
    ir::asg::{Asg, BaseAsg, IdentKind, IdentObject, IndexType, ObjectRef},
    sym::{st, GlobalSymbolResolve, SymbolId},
};
use petgraph::visit::DfsPostOrder;

// Result of [`sort`].
pub type SortResult<T, Ix> = Result<T, SortError<Ix>>;

/// Lower ASG into [`XmleSections`] by ordering relocatable text fragments.
///
/// This performs the equivalent of a topological sort,
///   although function cycles are permitted.
/// The actual operation performed is a post-order depth-first traversal.
pub fn sort<'a, Ix, S: XmleSections<'a>>(
    asg: &'a BaseAsg<IdentObject, Ix>,
    roots: &[ObjectRef<Ix>],
    mut dest: S,
) -> SortResult<S, Ix>
where
    Ix: IndexType,
    S: XmleSections<'a>,
{
    // TODO: we should check for cycles as we sort (as the POC did).
    check_cycles(asg)?;

    // This is technically a topological sort, but functions have cycles.
    let mut dfs = DfsPostOrder::empty(&asg.graph);

    for index in roots {
        dfs.stack.push((*index).into());
    }

    // These are always generated by the map compiler,
    //   but do not have edges that would allow them to be properly ordered
    //   (adding an edge to every map object would be wasteful).
    dest.push(get_ident(asg, st::L_MAP_UUUHEAD))?;
    dest.push(get_ident(asg, st::L_RETMAP_UUUHEAD))?;

    while let Some(index) = dfs.next(&asg.graph) {
        let ident = asg.get(index).expect("missing node");
        dest.push(ident)?;
    }

    dest.push(get_ident(asg, st::L_MAP_UUUTAIL))?;
    dest.push(get_ident(asg, st::L_RETMAP_UUUTAIL))?;

    Ok(dest)
}

fn get_ident<'a, Ix, S>(
    depgraph: &'a BaseAsg<IdentObject, Ix>,
    name: S,
) -> &'a IdentObject
where
    Ix: IndexType,
    S: Into<SymbolId>,
{
    let sym = name.into();

    depgraph
        .lookup(sym)
        .and_then(|id| depgraph.get(id))
        .expect(&format!(
            "missing internal identifier: {}",
            sym.lookup_str()
        ))
}

/// Check graph for cycles
///
/// We want to catch any cycles before we start using the graph.
///   Unfortunately, we need to allow cycles for our [`IdentKind::Func`]
///   so we cannot use the typical algorithms in a straightforward manner.
///
/// We loop through all SCCs and check that they are not all functions. If
///   they are, we ignore the cycle, otherwise we will return an error.
fn check_cycles<Ix>(asg: &BaseAsg<IdentObject, Ix>) -> SortResult<(), Ix>
where
    Ix: IndexType,
{
    // While `tarjan_scc` does do a topological sort, it does not suit our
    // needs because we need to filter out some allowed cycles. It would
    // still be possible to use this, but we also need to only check nodes
    // that are attached to our "roots". We are doing our own sort and as of
    // the initial writing, this does not have a significant performance
    // impact.
    let sccs = petgraph::algo::tarjan_scc(&asg.graph);

    let cycles: Vec<_> = sccs
        .into_iter()
        .filter_map(|scc| {
            // For single-node SCCs, we just need to make sure they are
            // not neighbors with themselves.
            if scc.len() == 1
                && !asg.graph.neighbors(scc[0]).any(|nx| nx == scc[0])
            {
                return None;
            }

            let is_all_funcs = scc.iter().all(|nx| {
                let ident = Asg::get(asg, *nx).expect("missing node");
                matches!(ident.kind(), Some(IdentKind::Func(_, _)))
            });

            if is_all_funcs {
                None
            } else {
                let cycles =
                    scc.iter().map(|nx| ObjectRef::from(*nx)).collect();
                Some(cycles)
            }
        })
        .collect();

    if cycles.is_empty() {
        Ok(())
    } else {
        Err(SortError::Cycles(cycles))
    }
}

/// Error during graph sorting.
///
/// These errors reflect barriers to meaningfully understanding the
///   properties of the data in the graph with respect to sorting.
/// It does not represent bad underlying data that does not affect the
///   sorting process.
#[derive(Debug, PartialEq)]
pub enum SortError<Ix: IndexType> {
    /// Error while lowering into [`XmleSections`].
    SectionsError(SectionsError),

    /// The graph has a cyclic dependency.
    Cycles(Vec<Vec<ObjectRef<Ix>>>),
}

impl<Ix: IndexType> From<SectionsError> for SortError<Ix> {
    fn from(err: SectionsError) -> Self {
        Self::SectionsError(err)
    }
}

impl<Ix: IndexType> std::fmt::Display for SortError<Ix> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SectionsError(err) => err.fmt(fmt),
            Self::Cycles(_) => write!(fmt, "cyclic dependencies"),
        }
    }
}

impl<Ix: IndexType> std::error::Error for SortError<Ix> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ir::{
            asg::{Dim, FragmentText, IdentObject, Source},
            legacyir::SymDtype,
        },
        ld::xmle::{section::PushResult, Sections},
        sym::GlobalSymbolIntern,
    };

    type TestAsg = BaseAsg<IdentObject, u16>;

    /// Create a graph with the expected {ret,}map head/tail identifiers.
    fn make_asg() -> TestAsg {
        let mut asg = TestAsg::new();

        let text = "dummy fragment".intern();

        asg.declare(
            st::L_MAP_UUUHEAD.into(),
            IdentKind::MapHead,
            Default::default(),
        )
        .and_then(|id| asg.set_fragment(id, text))
        .unwrap();

        asg.declare(
            st::L_MAP_UUUTAIL.into(),
            IdentKind::MapTail,
            Default::default(),
        )
        .and_then(|id| asg.set_fragment(id, text))
        .unwrap();

        asg.declare(
            st::L_RETMAP_UUUHEAD.into(),
            IdentKind::RetMapHead,
            Default::default(),
        )
        .and_then(|id| asg.set_fragment(id, text))
        .unwrap();

        asg.declare(
            st::L_RETMAP_UUUTAIL.into(),
            IdentKind::RetMapTail,
            Default::default(),
        )
        .and_then(|id| asg.set_fragment(id, text))
        .unwrap();

        asg
    }

    #[test]
    fn graph_sort() -> SortResult<(), u16> {
        // We care only about the order of pushes, not the sections they end
        // up in.
        struct StubSections<'a> {
            pushed: Vec<&'a IdentObject>,
        }

        impl<'a> XmleSections<'a> for StubSections<'a> {
            fn push(&mut self, ident: &'a IdentObject) -> PushResult {
                self.pushed.push(ident);
                Ok(())
            }

            fn take_deps(&mut self) -> Vec<&'a IdentObject> {
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
            .declare("adep".into(), IdentKind::Meta, Default::default())
            .unwrap();
        let a = asg
            .declare("a".into(), IdentKind::Meta, Default::default())
            .unwrap();
        let adepdep = asg
            .declare("adepdep".into(), IdentKind::Meta, Default::default())
            .unwrap();

        asg.add_dep(a, adep);
        asg.add_dep(adep, adepdep);

        let sections =
            sort(&asg, &vec![a], StubSections { pushed: Vec::new() })?;

        assert_eq!(
            sections.pushed,
            vec![
                // Static head
                asg.lookup(st::L_MAP_UUUHEAD.into())
                    .and_then(|id| asg.get(id)),
                asg.lookup(st::L_RETMAP_UUUHEAD.into())
                    .and_then(|id| asg.get(id)),
                // Post-order
                asg.get(adepdep),
                asg.get(adep),
                asg.get(a),
                // Static tail
                asg.lookup(st::L_MAP_UUUTAIL.into())
                    .and_then(|id| asg.get(id)),
                asg.lookup(st::L_RETMAP_UUUTAIL.into())
                    .and_then(|id| asg.get(id)),
            ]
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .unwrap()
        );

        Ok(())
    }

    #[test]
    fn graph_sort_missing_node() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();

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

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);

        match sort(&asg, &vec![sym_node], Sections::new()) {
            Ok(_) => panic!("Unexpected success - dependency is not in graph"),
            Err(SortError::SectionsError(SectionsError::UnresolvedObject(
                _,
            ))) => (),
            bad => {
                panic!("Incorrect error result when dependency is not in graph: {:?}", bad)
            }
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_roots_same_as_empty_graph() -> SortResult<(), u16> {
        let mut asg_nonempty_no_roots = make_asg();

        // "empty" (it has the head/tail {ret,}map objects)
        let asg_empty = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();

        asg_nonempty_no_roots.add_dep_lookup(sym, dep);

        assert_eq!(
            sort(&asg_nonempty_no_roots, &vec![], Sections::new()),
            sort(&asg_empty, &vec![], Sections::new())
        );

        Ok(())
    }

    #[test]
    fn graph_sort_simple_cycle() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();

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

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);

        let result = sort(&asg, &vec![sym_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> =
            vec![vec![dep_node.into(), sym_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_two_simple_cycles() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let sym2 = "sym2".intern();
        let dep = "dep".intern();
        let dep2 = "dep2".intern();

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

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(dep_node, FragmentText::from("baz"))
            .unwrap();
        asg.set_fragment(dep2_node, FragmentText::from("huh"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);
        let (_, _) = asg.add_dep_lookup(sym2, dep2);
        let (_, _) = asg.add_dep_lookup(dep2, sym2);

        let result = sort(&asg, &vec![sym_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> = vec![
            vec![dep_node.into(), sym_node.into()],
            vec![dep2_node.into(), sym2_node.into()],
        ];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_cycle_with_edge_to_same_node() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();

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

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(sym, dep);

        let result = sort(&asg, &vec![sym_node], Sections::new());

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cycle_with_a_few_steps() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

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

        asg.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        let result = sort(&asg, &vec![sym1_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_with_non_function_with_a_few_steps(
    ) -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

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
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        asg.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        let result = sort(&asg, &vec![sym1_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_bookended_by_functions() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

        let sym1_node = asg
            .declare(
                sym1,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
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
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        asg.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        let result = sort(&asg, &vec![sym1_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_ignored() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let sym_node = asg
            .declare(
                sym,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = asg
            .declare(
                dep,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);

        let result = sort(&asg, &vec![sym_node], Sections::new());

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_is_bookended() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

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
                IdentKind::Func(Dim::default(), SymDtype::Empty),
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

        asg.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        let result = sort(&asg, &vec![sym1_node], Sections::new());

        let expected: Vec<Vec<ObjectRef<u16>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_ignore_non_linked() -> SortResult<(), u16> {
        let mut asg = make_asg();

        let sym = "sym".intern();
        let dep = "dep".intern();
        let ignored = "ignored".intern();

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

        let ignored_node = asg
            .declare(
                ignored,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        asg.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        asg.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();
        asg.set_fragment(ignored_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(ignored, sym);

        let result = sort(&asg, &vec![sym_node], Sections::new());

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }
}
