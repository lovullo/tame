// ASG lowering into xmle sections
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

//! Lowering of the [ASG](crate::asg) into `xmle` [`XmleSections`].
//!
//! See the [parent module](super) for more information.

use super::section::{SectionsError, XmleSections};
use crate::{
    asg::{Asg, Ident, IdentKind, Object, ObjectRef},
    diagnose::Diagnostic,
    sym::{st, GlobalSymbolResolve, SymbolId},
};
use petgraph::visit::DfsPostOrder;

// Result of [`sort`].
pub type SortResult<T> = Result<T, SortError>;

/// Lower ASG into [`XmleSections`] by ordering relocatable text fragments.
///
/// This performs the equivalent of a topological sort,
///   although function cycles are permitted.
/// The actual operation performed is a post-order depth-first traversal.
pub fn sort<'a, S: XmleSections<'a>>(asg: &'a Asg, mut dest: S) -> SortResult<S>
where
    S: XmleSections<'a>,
{
    // TODO: we should check for cycles as we sort (as the POC did).
    check_cycles(asg)?;

    // TODO: Encapsulate petgraph.
    // This is technically a topological sort, but functions have cycles.
    let mut dfs = DfsPostOrder::new(&asg.graph, asg.root());

    // These are always generated by the map compiler,
    //   but do not have edges that would allow them to be properly ordered
    //   (adding an edge to every map object would be wasteful).
    dest.push(get_ident(asg, st::L_MAP_UUUHEAD))?;
    dest.push(get_ident(asg, st::L_RETMAP_UUUHEAD))?;

    while let Some(index) = dfs.next(&asg.graph) {
        match asg.get(index).expect("missing object") {
            Object::Root => (),
            Object::Ident(ident) => dest.push(ident)?,
        }
    }

    dest.push(get_ident(asg, st::L_MAP_UUUTAIL))?;
    dest.push(get_ident(asg, st::L_RETMAP_UUUTAIL))?;

    Ok(dest)
}

fn get_ident<'a, S>(depgraph: &'a Asg, name: S) -> &'a Ident
where
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
        .unwrap_ident_ref()
}

/// Check graph for cycles
///
/// We want to catch any cycles before we start using the graph.
///   Unfortunately, we need to allow cycles for our [`IdentKind::Func`]
///   so we cannot use the typical algorithms in a straightforward manner.
///
/// We loop through all SCCs and check that they are not all functions. If
///   they are, we ignore the cycle, otherwise we will return an error.
fn check_cycles(asg: &Asg) -> SortResult<()> {
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
                let ident = asg.get(*nx).expect("missing node");
                matches!(
                    ident.as_ident_ref().and_then(Ident::kind),
                    Some(IdentKind::Func(..))
                )
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
pub enum SortError {
    /// Error while lowering into [`XmleSections`].
    SectionsError(SectionsError),

    /// The graph has a cyclic dependency.
    Cycles(Vec<Vec<ObjectRef>>),
}

impl From<SectionsError> for SortError {
    fn from(err: SectionsError) -> Self {
        Self::SectionsError(err)
    }
}

impl std::fmt::Display for SortError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::SectionsError(err) => err.fmt(fmt),
            Self::Cycles(_) => write!(fmt, "cyclic dependencies"),
        }
    }
}

impl std::error::Error for SortError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Diagnostic for SortError {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use SortError::*;

        match self {
            SectionsError(e) => e.describe(),
            // TODO
            Cycles(_) => vec![],
        }
    }
}

#[cfg(test)]
mod test {
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
                asg.lookup(st::L_MAP_UUUHEAD.into())
                    .and_then(|id| asg.get_ident(id)),
                asg.lookup(st::L_RETMAP_UUUHEAD.into())
                    .and_then(|id| asg.get_ident(id)),
                // Post-order
                asg.get_ident(adepdep),
                asg.get_ident(adep),
                asg.get_ident(a),
                // Static tail
                asg.lookup(st::L_MAP_UUUTAIL.into())
                    .and_then(|id| asg.get_ident(id)),
                asg.lookup(st::L_RETMAP_UUUTAIL.into())
                    .and_then(|id| asg.get_ident(id)),
            ]
            .into_iter()
            .collect::<Option<Vec<_>>>()
            .unwrap()
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

        let (_, _) = asg.add_dep_lookup(sym, dep);

        asg.add_root(sym_node);

        match sort(&asg, Sections::new()) {
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
    fn graph_sort_no_roots_same_as_empty_graph() -> SortResult<()> {
        let mut asg_nonempty_no_roots = make_asg();

        // "empty" (it has the head/tail {ret,}map objects)
        let asg_empty = make_asg();

        let sym = SPair("sym".into(), S1);
        let dep = SPair("dep".into(), S2);

        asg_nonempty_no_roots.add_dep_lookup(sym, dep);

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

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);

        asg.add_root(sym_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> =
            vec![vec![dep_node.into(), sym_node.into()]];
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

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);
        let (_, _) = asg.add_dep_lookup(sym2, dep2);
        let (_, _) = asg.add_dep_lookup(dep2, sym2);

        asg.add_root(sym_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> = vec![
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

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(sym, dep);

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

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        asg.add_root(sym1_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> =
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

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        asg.add_root(sym1_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
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

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        asg.add_root(sym1_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
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

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(dep, sym);

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

        let (_, _) = asg.add_dep_lookup(sym1, sym2);
        let (_, _) = asg.add_dep_lookup(sym2, sym3);
        let (_, _) = asg.add_dep_lookup(sym3, sym1);

        asg.add_root(sym1_node);

        let result = sort(&asg, Sections::new());

        let expected: Vec<Vec<ObjectRef>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
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

        let (_, _) = asg.add_dep_lookup(sym, dep);
        let (_, _) = asg.add_dep_lookup(ignored, sym);

        asg.add_root(sym_node);

        let result = sort(&asg, Sections::new());

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }
}
