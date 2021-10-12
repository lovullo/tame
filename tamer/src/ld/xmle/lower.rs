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

//! Lowering of the [ASG](crate::ir::asg) into `xmle` [`Sections`].
//!
//! See the [parent module](super) for more information.

use super::Sections;
use crate::{
    ir::asg::{
        Asg, BaseAsg, IdentKind, IdentObject, IdentObjectData,
        IdentObjectState, IndexType, ObjectRef, UnresolvedError,
    },
    sym::GlobalSymbolResolve,
};
use petgraph::visit::DfsPostOrder;

// Result of [`sort`].
pub type SortResult<T, Ix> = Result<T, SortError<Ix>>;

/// Lower ASG into [`Sections`] by sorting relocatable text fragments.
///
/// This performs the equivalent of a topological sort,
///   although function cycles are permitted.
/// The actual operation performed is a post-order depth-first traversal.
pub fn sort<'a, Ix>(
    asg: &'a BaseAsg<IdentObject, Ix>,
    roots: &[ObjectRef<Ix>],
) -> SortResult<Sections<'a>, Ix>
where
    Ix: IndexType,
{
    let mut deps = Sections::new();

    // TODO: we should check for cycles as we sort (as the POC did).
    check_cycles(asg)?;

    // This is technically a topological sort, but functions have cycles.
    let mut dfs = DfsPostOrder::empty(&asg.graph);

    for index in roots {
        dfs.stack.push((*index).into());
    }

    while let Some(index) = dfs.next(&asg.graph) {
        let ident = asg.get(index).expect("missing node").resolved()?;

        match ident.kind() {
            Some(kind) => match kind {
                IdentKind::Meta
                | IdentKind::Worksheet
                | IdentKind::Param(_, _)
                | IdentKind::Type(_)
                | IdentKind::Func(_, _)
                | IdentKind::Const(_, _) => deps.st.push_body(ident),
                IdentKind::MapHead | IdentKind::Map | IdentKind::MapTail => {
                    deps.map.push_body(ident)
                }
                IdentKind::RetMapHead
                | IdentKind::RetMap
                | IdentKind::RetMapTail => deps.retmap.push_body(ident),
                _ => deps.rater.push_body(ident),
            },
            None => {
                return Err(SortError::MissingObjectKind(
                    ident
                        .name()
                        .map(|name| format!("{}", name.lookup_str()))
                        .unwrap_or("<unknown>".into())
                        .into(),
                ))
            }
        }
    }

    Ok(deps)
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
    /// An unresolved object was encountered during sorting.
    ///
    /// An unresolved object means that the graph has an incomplete picture
    ///   of the program,
    ///     and so sorting cannot be reliably performed.
    /// Since all objects are supposed to be resolved prior to sorting,
    ///   this represents either a problem with the program being compiled
    ///   or a bug in the compiler itself.
    UnresolvedObject(UnresolvedError),

    /// The kind of an object encountered during sorting could not be
    ///   determined.
    ///
    /// Sorting uses the object kind to place objects into their appropriate
    ///   sections.
    /// It should never be the case that a resolved object has no kind,
    ///   so this likely represents a compiler bug.
    MissingObjectKind(String),

    /// The graph has a cyclic dependency.
    Cycles(Vec<Vec<ObjectRef<Ix>>>),
}

impl<Ix: IndexType> From<UnresolvedError> for SortError<Ix> {
    fn from(err: UnresolvedError) -> Self {
        Self::UnresolvedObject(err)
    }
}

impl<Ix: IndexType> std::fmt::Display for SortError<Ix> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnresolvedObject(err) => std::fmt::Display::fmt(err, fmt),
            Self::MissingObjectKind(name) => write!(
                fmt,
                "internal error: missing object kind for object `{}` (this may be a compiler bug!)",
                name,
            ),
            Self::Cycles(_) => write!(fmt, "cyclic dependencies"),
        }
    }
}

impl<Ix: IndexType> std::error::Error for SortError<Ix> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::UnresolvedObject(err) => Some(err),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ir::{
            asg::{DataType, Dim, FragmentText, IdentObject, Source},
            legacyir::SymDtype,
        },
        sym::GlobalSymbolIntern,
    };

    type Sut = BaseAsg<IdentObject, u16>;

    macro_rules! assert_section_sym {
        ( $iterable:expr, $s:ident ) => {{
            let mut pos = 0;

            for obj in $iterable.iter() {
                let sym = obj.name().expect("missing object");
                assert_eq!($s.get(pos).map(|s| *s), Some(sym));

                pos = pos + 1;
            }

            assert_eq!(pos, $s.len());
        };};
    }

    macro_rules! add_syms {
        ($sut:ident, $base:expr, {$($dest:ident <- $name:ident: $kind:expr,)*}) => {
            $(
                let sym = stringify!($name).intern();

                $sut.declare(sym, $kind, Source::default()).unwrap();
                let (_, _) = $sut.add_dep_lookup($base, sym);

                $dest.push(sym);
            )*
        };
    }

    #[test]
    fn graph_sort() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let mut st = vec![];
        let mut map = vec![];
        let mut retmap = vec![];

        let base = "sym1".intern();
        let base_node = sut
            .declare(base, IdentKind::Map, Source::default())
            .unwrap();

        add_syms!(sut, base, {
            st <- meta1: IdentKind::Meta,
            st <- work1: IdentKind::Worksheet,
            st <- const1: IdentKind::Const(Dim::from_u8(0), DataType::Float),
            map <- map1: IdentKind::MapHead,
            map <- map2: IdentKind::Map,
            map <- map3: IdentKind::MapTail,
            retmap <- retmap1: IdentKind::RetMapHead,
            retmap <- retmap2: IdentKind::RetMap,
            retmap <- retmap3: IdentKind::RetMapTail,
            retmap <- retmap4: IdentKind::RetMapTail,
            retmap <- retmap5: IdentKind::RetMap,
            map <- map4: IdentKind::MapHead,
            map <- map5: IdentKind::Map,
            st <- meta2: IdentKind::Meta,
            st <- work2: IdentKind::Worksheet,
            map <- map6: IdentKind::MapTail,
            retmap <- retmap6: IdentKind::RetMapHead,
            st <- const2: IdentKind::Const(Dim::from_u8(2), DataType::Float),
        });

        map.push(base);

        let sections = sort(&sut, &vec![base_node])?;

        assert_section_sym!(sections.st, st);
        assert_section_sym!(sections.map, map);
        assert_section_sym!(sections.retmap, retmap);

        Ok(())
    }

    #[test]
    fn graph_sort_missing_node() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);

        match sort(&sut, &vec![sym_node]) {
            Ok(_) => panic!("Unexpected success - dependency is not in graph"),
            Err(SortError::UnresolvedObject(_)) => (),
            bad => {
                panic!("Incorrect error result when dependency is not in graph: {:?}", bad)
            }
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_roots() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let (_, _) = sut.add_dep_lookup(sym, dep);

        let sections = sort(&sut, &vec![])?;

        assert_eq!(Sections::new(), sections);

        Ok(())
    }

    #[test]
    fn graph_sort_simple_cycle() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);
        let (_, _) = sut.add_dep_lookup(dep, sym);

        let result = sort(&sut, &vec![sym_node]);

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
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let sym2 = "sym2".intern();
        let dep = "dep".intern();
        let dep2 = "dep2".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep2_node = sut
            .declare(
                dep2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(dep_node, FragmentText::from("baz"))
            .unwrap();
        sut.set_fragment(dep2_node, FragmentText::from("huh"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);
        let (_, _) = sut.add_dep_lookup(dep, sym);
        let (_, _) = sut.add_dep_lookup(sym2, dep2);
        let (_, _) = sut.add_dep_lookup(dep2, sym2);

        let result = sort(&sut, &vec![sym_node]);

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
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);
        let (_, _) = sut.add_dep_lookup(sym, dep);

        let result = sort(&sut, &vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cycle_with_a_few_steps() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

        let sym1_node = sut
            .declare(
                sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                sym3,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym1, sym2);
        let (_, _) = sut.add_dep_lookup(sym2, sym3);
        let (_, _) = sut.add_dep_lookup(sym3, sym1);

        let result = sort(&sut, &vec![sym1_node]);

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
        let mut sut = Sut::new();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

        let sym1_node = sut
            .declare(
                sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                sym3,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym1, sym2);
        let (_, _) = sut.add_dep_lookup(sym2, sym3);
        let (_, _) = sut.add_dep_lookup(sym3, sym1);

        let result = sort(&sut, &vec![sym1_node]);

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
        let mut sut = Sut::new();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

        let sym1_node = sut
            .declare(
                sym1,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                sym3,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym1, sym2);
        let (_, _) = sut.add_dep_lookup(sym2, sym3);
        let (_, _) = sut.add_dep_lookup(sym3, sym1);

        let result = sort(&sut, &vec![sym1_node]);

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
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                dep,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);
        let (_, _) = sut.add_dep_lookup(dep, sym);

        let result = sort(&sut, &vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_is_bookended() -> SortResult<(), u16> {
        let mut sut = Sut::new();

        let sym1 = "sym1".intern();
        let sym2 = "sym2".intern();
        let sym3 = "sym3".intern();

        let sym1_node = sut
            .declare(
                sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                sym2,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                sym3,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym1_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(sym2_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(sym3_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym1, sym2);
        let (_, _) = sut.add_dep_lookup(sym2, sym3);
        let (_, _) = sut.add_dep_lookup(sym3, sym1);

        let result = sort(&sut, &vec![sym1_node]);

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
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();
        let ignored = "ignored".intern();

        let sym_node = sut
            .declare(
                sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let ignored_node = sut
            .declare(
                ignored,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();
        sut.set_fragment(dep_node, FragmentText::from("bar"))
            .unwrap();
        sut.set_fragment(ignored_node, FragmentText::from("baz"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(sym, dep);
        let (_, _) = sut.add_dep_lookup(ignored, sym);

        let result = sort(&sut, &vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }
}
