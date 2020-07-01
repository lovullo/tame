// Concrete ASG
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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

//! Base concrete [`Asg`] implementation.

use super::graph::{
    Asg, AsgEdge, AsgResult, IndexType, Node, ObjectRef, SortableAsg,
    SortableAsgError, SortableAsgResult,
};
use super::ident::IdentKind;
use super::object::{
    FragmentText, IdentObjectData, IdentObjectState, Source, TransitionResult,
};
use super::Sections;
use crate::sym::Symbol;
use petgraph::graph::{DiGraph, Graph, NodeIndex};
use petgraph::visit::DfsPostOrder;

/// Concrete ASG.
///
/// This implementation is currently based on [`petgraph`].
///
/// Identifiers are cached by name for `O(1)` lookup.
/// Since [`SymbolIndex`][crate::sym::SymbolIndex] is used for this purpose,
///   the index may contain more entries than nodes and may contain gaps.
///
/// For more information,
///   see [`Asg`].
pub struct BaseAsg<O, Ix>
where
    Ix: IndexType,
{
    /// Directed graph on which objects are stored.
    graph: DiGraph<Node<O>, AsgEdge, Ix>,

    /// Map of [`SymbolIndex`][crate::sym::SymbolIndex] to node indexes.
    ///
    /// This allows for `O(1)` lookup of identifiers in the graph.
    /// Note that,
    ///   while we store [`NodeIndex`] internally,
    ///   the public API encapsulates it within an [`ObjectRef`].
    index: Vec<NodeIndex<Ix>>,

    /// Empty node indicating that no object exists for a given index.
    empty_node: NodeIndex<Ix>,
}

impl<'i, O, Ix> BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: IdentObjectState<'i, O> + IdentObjectData<'i>,
{
    /// Create a new ASG.
    ///
    /// See also [`with_capacity`](BaseAsg::with_capacity).
    pub fn new() -> Self {
        Self::with_capacity(0, 0)
    }

    /// Create an ASG with the provided initial capacity.
    ///
    /// The value for `objects` will be used as the capacity for the nodes
    ///   in the graph,
    ///     as well as the initial index capacity.
    /// The value for `edges` may be more difficult to consider,
    ///   since edges are used to represent various relationships between
    ///   different types of objects,
    ///     but it's safe to say that each object will have at least one
    ///     edge to another object.
    pub fn with_capacity(objects: usize, edges: usize) -> Self {
        let mut graph = Graph::with_capacity(objects, edges);
        let mut index = Vec::with_capacity(objects);

        // Exhaust the first index to be used as a placeholder.
        let empty_node = graph.add_node(None);
        index.push(empty_node);

        Self {
            graph,
            index,
            empty_node,
        }
    }

    /// Get the underlying Graph
    pub fn into_inner(self) -> DiGraph<Node<O>, AsgEdge, Ix> {
        self.graph
    }

    /// Index the provided symbol `name` as representing the identifier `node`.
    ///
    /// This index permits `O(1)` identifier lookups.
    ///
    /// After an identifier is indexed it is not expected to be reassigned
    ///   to another node.
    /// Debug builds contain an assertion that will panic in this instance.
    ///
    /// Panics
    /// ======
    /// Will panic if unable to allocate more space for the index.
    fn index_identifier(&mut self, name: &'i Symbol<'i>, node: NodeIndex<Ix>) {
        let i: usize = name.index().into();

        if i >= self.index.len() {
            // If this is ever a problem we can fall back to usize max and
            // re-compare before panicing
            let new_size = (i + 1)
                .checked_next_power_of_two()
                .expect("internal error: cannot allocate space for ASG index");

            self.index.resize(new_size, self.empty_node);
        }

        // We should never overwrite indexes
        debug_assert!(self.index[i] == self.empty_node);

        self.index[i] = node;
    }

    /// Lookup `ident` or add a missing identifier to the graph and return a
    ///   reference to it.
    ///
    /// See [`IdentObjectState::declare`] for more information.
    fn lookup_or_missing(&mut self, ident: &'i Symbol<'i>) -> ObjectRef<Ix> {
        self.lookup(ident).unwrap_or_else(|| {
            let index = self.graph.add_node(Some(O::declare(ident)));

            self.index_identifier(ident, index);
            ObjectRef::new(index)
        })
    }

    /// Perform a state transition on an identifier by name.
    ///
    /// Look up `ident` or add a missing identifier if it does not yet exist
    ///   (see `lookup_or_missing`).
    /// Then invoke `f` with the located identifier and replace the
    ///   identifier on the graph with the result.
    ///
    /// This will safely restore graph state to the original identifier
    ///   value on transition failure.
    fn with_ident_lookup<F>(
        &mut self,
        name: &'i Symbol<'i>,
        f: F,
    ) -> AsgResult<ObjectRef<Ix>>
    where
        F: FnOnce(O) -> TransitionResult<O>,
    {
        let identi = self.lookup_or_missing(name);
        self.with_ident(identi, f)
    }

    /// Perform a state transition on an identifier by [`ObjectRef`].
    ///
    /// Invoke `f` with the located identifier and replace the identifier on
    ///   the graph with the result.
    ///
    /// This will safely restore graph state to the original identifier
    ///   value on transition failure.
    fn with_ident<F>(
        &mut self,
        identi: ObjectRef<Ix>,
        f: F,
    ) -> AsgResult<ObjectRef<Ix>>
    where
        F: FnOnce(O) -> TransitionResult<O>,
    {
        let node = self.graph.node_weight_mut(identi.into()).unwrap();

        let obj = node
            .take()
            .expect(&format!("internal error: missing object"));

        f(obj)
            .and_then(|obj| {
                node.replace(obj);
                Ok(identi)
            })
            .or_else(|(orig, err)| {
                node.replace(orig);
                Err(err.into())
            })
    }
}

impl<'i, O, Ix> Asg<'i, O, Ix> for BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: IdentObjectState<'i, O> + IdentObjectData<'i>,
{
    fn declare(
        &mut self,
        name: &'i Symbol<'i>,
        kind: IdentKind,
        src: Source<'i>,
    ) -> AsgResult<ObjectRef<Ix>> {
        self.with_ident_lookup(name, |obj| obj.resolve(kind, src))
    }

    fn declare_extern(
        &mut self,
        name: &'i Symbol<'i>,
        kind: IdentKind,
        src: Source<'i>,
    ) -> AsgResult<ObjectRef<Ix>> {
        self.with_ident_lookup(name, |obj| obj.extern_(kind, src))
    }

    fn set_fragment(
        &mut self,
        identi: ObjectRef<Ix>,
        text: FragmentText,
    ) -> AsgResult<ObjectRef<Ix>> {
        self.with_ident(identi, |obj| obj.set_fragment(text))
    }

    #[inline]
    fn get<I: Into<ObjectRef<Ix>>>(&self, index: I) -> Option<&O> {
        self.graph.node_weight(index.into().into()).map(|node| {
            node.as_ref()
                .expect("internal error: BaseAsg::get missing Node data")
        })
    }

    #[inline]
    fn lookup(&self, name: &'i Symbol<'i>) -> Option<ObjectRef<Ix>> {
        let i: usize = name.index().into();

        self.index
            .get(i)
            .filter(|ni| ni.index() > 0)
            .map(|ni| ObjectRef::new(*ni))
    }

    fn add_dep(&mut self, identi: ObjectRef<Ix>, depi: ObjectRef<Ix>) {
        self.graph
            .update_edge(identi.into(), depi.into(), Default::default());
    }

    #[inline]
    fn has_dep(&self, ident: ObjectRef<Ix>, dep: ObjectRef<Ix>) -> bool {
        self.graph.contains_edge(ident.into(), dep.into())
    }

    fn add_dep_lookup(
        &mut self,
        ident: &'i Symbol<'i>,
        dep: &'i Symbol<'i>,
    ) -> (ObjectRef<Ix>, ObjectRef<Ix>) {
        let identi = self.lookup_or_missing(ident);
        let depi = self.lookup_or_missing(dep);

        self.graph
            .update_edge(identi.into(), depi.into(), Default::default());

        (identi, depi)
    }
}

impl<'i, O, Ix> SortableAsg<'i, O, Ix> for BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: IdentObjectData<'i> + IdentObjectState<'i, O>,
{
    fn sort(
        &'i self,
        roots: &[ObjectRef<Ix>],
    ) -> SortableAsgResult<Sections<'i, O>, Ix> {
        let mut deps = Sections::new();

        check_cycles(self)?;

        // This is technically a topological sort, but functions have cycles.
        let mut dfs = DfsPostOrder::empty(&self.graph);

        for index in roots {
            dfs.stack.push((*index).into());
        }

        while let Some(index) = dfs.next(&self.graph) {
            let ident = self.get(index).expect("missing node");

            match ident.kind() {
                Some(kind) => match kind {
                    IdentKind::Meta => deps.meta.push_body(ident),
                    IdentKind::Worksheet => deps.worksheet.push_body(ident),
                    IdentKind::Param(_, _) => deps.params.push_body(ident),
                    IdentKind::Type(_) => deps.types.push_body(ident),
                    IdentKind::Func(_, _) => deps.funcs.push_body(ident),
                    IdentKind::MapHead
                    | IdentKind::Map
                    | IdentKind::MapTail => deps.map.push_body(ident),
                    IdentKind::RetMapHead
                    | IdentKind::RetMap
                    | IdentKind::RetMapTail => deps.retmap.push_body(ident),
                    _ => deps.rater.push_body(ident),
                },
                None => {
                    return Err(SortableAsgError::MissingObjectKind(
                        ident
                            .name()
                            .map(|name| name.as_ref())
                            .unwrap_or("<unknown>")
                            .into(),
                    ))
                }
            }
        }

        Ok(deps)
    }
}

/// Check graph for cycles
///
/// We want to catch any cycles before we start using the graph.
///   Unfortunately, we need to allow cycles for our [`IdentKind::Func`]
///   so we cannot use the typical algorithms in a straightforward manner.
///
/// We loop through all SCCs and check that they are not all functions. If
///   they are, we ignore the cycle, otherwise we will return an error.
fn check_cycles<'i, O, Ix>(asg: &BaseAsg<O, Ix>) -> SortableAsgResult<(), Ix>
where
    Ix: IndexType,
    O: IdentObjectData<'i> + IdentObjectState<'i, O>,
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
        Err(SortableAsgError::Cycles(cycles))
    }
}

#[cfg(test)]
mod test {
    use super::super::graph::AsgError;
    use super::*;
    use crate::ir::asg::{Dim, IdentObject, TransitionError, TransitionResult};
    use crate::ir::legacyir::SymDtype;
    use crate::sym::SymbolIndex;
    use std::cell::RefCell;

    #[derive(Debug, Default, PartialEq)]
    struct StubIdentObject<'i> {
        given_declare: Option<&'i Symbol<'i>>,
        given_extern: Option<(IdentKind, Source<'i>)>,
        given_resolve: Option<(IdentKind, Source<'i>)>,
        given_set_fragment: Option<FragmentText>,
        fail_redeclare: RefCell<Option<TransitionError>>,
        fail_extern: RefCell<Option<TransitionError>>,
        fail_set_fragment: RefCell<Option<TransitionError>>,
    }

    impl<'i> IdentObjectData<'i> for StubIdentObject<'i> {
        fn name(&self) -> Option<&'i Symbol<'i>> {
            self.given_declare
        }

        fn kind(&self) -> Option<&IdentKind> {
            self.given_resolve.as_ref().map(|args| &args.0)
        }

        fn src(&self) -> Option<&Source<'i>> {
            None
        }

        fn fragment(&self) -> Option<&FragmentText> {
            None
        }

        fn as_ident(&self) -> Option<&IdentObject<'i>> {
            None
        }
    }

    impl<'i> IdentObjectState<'i, StubIdentObject<'i>> for StubIdentObject<'i> {
        fn declare(ident: &'i Symbol<'i>) -> Self {
            Self {
                given_declare: Some(ident),
                ..Default::default()
            }
        }

        fn resolve(
            mut self,
            kind: IdentKind,
            src: Source<'i>,
        ) -> TransitionResult<StubIdentObject<'i>> {
            if self.fail_redeclare.borrow().is_some() {
                let err = self.fail_redeclare.replace(None).unwrap();
                return Err((self, err));
            }

            self.given_resolve = Some((kind, src));
            Ok(self)
        }

        fn extern_(
            mut self,
            kind: IdentKind,
            src: Source<'i>,
        ) -> TransitionResult<StubIdentObject<'i>> {
            if self.fail_extern.borrow().is_some() {
                let err = self.fail_extern.replace(None).unwrap();
                return Err((self, err));
            }

            self.given_extern = Some((kind, src));
            Ok(self)
        }

        fn set_fragment(
            mut self,
            text: FragmentText,
        ) -> TransitionResult<StubIdentObject<'i>> {
            if self.fail_set_fragment.borrow().is_some() {
                let err = self.fail_set_fragment.replace(None).unwrap();
                return Err((self, err));
            }

            self.given_set_fragment.replace(text);
            Ok(self)
        }
    }

    type Sut<'i> = BaseAsg<StubIdentObject<'i>, u8>;

    #[test]
    fn create_with_capacity() {
        let node_capacity = 100;
        let edge_capacity = 300;
        let sut = Sut::with_capacity(node_capacity, edge_capacity);

        // breaks encapsulation to introspect; the behavior is
        // transparent to callers (aside from performance
        // characteristics)
        let (nc, ec) = sut.graph.capacity();
        assert!(nc >= node_capacity);
        assert!(ec >= edge_capacity);
        assert!(sut.index.capacity() >= node_capacity);
    }

    #[test]
    fn declare_new_unique_idents() -> AsgResult<()> {
        let mut sut = Sut::new();

        // NB: The index ordering is important!  We first use a larger
        // index to create a gap, and then use an index within that gap
        // to ensure that it's not considered an already-defined
        // identifier.
        let syma = symbol_dummy!(5, "syma");
        let symb = symbol_dummy!(1, "symab");

        let nodea = sut.declare(
            &syma,
            IdentKind::Meta,
            Source {
                desc: Some("a".to_string()),
                ..Default::default()
            },
        )?;

        let nodeb = sut.declare(
            &symb,
            IdentKind::Worksheet,
            Source {
                desc: Some("b".to_string()),
                ..Default::default()
            },
        )?;

        assert_ne!(nodea, nodeb);

        assert_eq!(Some(&syma), sut.get(nodea).unwrap().given_declare);
        assert_eq!(
            Some((
                IdentKind::Meta,
                Source {
                    desc: Some("a".to_string()),
                    ..Default::default()
                },
            )),
            sut.get(nodea).unwrap().given_resolve
        );

        assert_eq!(Some(&symb), sut.get(nodeb).unwrap().given_declare);
        assert_eq!(
            Some((
                IdentKind::Worksheet,
                Source {
                    desc: Some("b".to_string()),
                    ..Default::default()
                },
            )),
            sut.get(nodeb).unwrap().given_resolve
        );

        Ok(())
    }

    #[test]
    fn lookup_by_symbol() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "lookup");
        let node = sut.declare(
            &sym,
            IdentKind::Meta,
            Source {
                generated: true,
                ..Default::default()
            },
        )?;

        assert_eq!(Some(node), sut.lookup(&sym));

        Ok(())
    }

    #[test]
    fn declare_returns_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "symdup");
        let src = Source::default();
        let node = sut.declare(&sym, IdentKind::Meta, src.clone())?;

        // Remember that our stub does not care about compatibility.
        let rekind = IdentKind::Class(Dim::from_u8(3));
        let resrc = Source {
            desc: Some("redeclare".into()),
            ..Default::default()
        };
        let redeclare = sut.declare(&sym, rekind.clone(), resrc.clone())?;

        // We don't care what the objects are for this test, just that the
        // same node is referenced.
        assert_eq!(node, redeclare);

        assert_eq!(Some((rekind, resrc)), sut.get(node).unwrap().given_resolve,);

        Ok(())
    }

    // Builds upon declare_returns_existing.
    #[test]
    fn declare_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "symdup");
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        // Set up an object to fail redeclaration.
        let node = sut.declare(&sym, IdentKind::Meta, src.clone())?;
        let obj = sut.get(node).unwrap();
        let terr = TransitionError::ExternResolution {
            name: String::from("test fail"),
            expected: IdentKind::Meta,
            given: IdentKind::Meta,
        };
        obj.fail_redeclare.replace(Some(terr.clone()));

        // Should invoke StubIdentObject::redeclare on the above `obj`.
        let result = sut.declare(&sym, IdentKind::Meta, Source::default());

        if let Err(err) = result {
            // The node should have been restored.
            let obj = sut.get(node).unwrap();
            assert_eq!(src, obj.given_resolve.as_ref().unwrap().1);

            assert_eq!(AsgError::ObjectTransition(terr), err);

            Ok(())
        } else {
            panic!("failure expected: {:?}", result);
        }
    }

    #[test]
    fn declare_extern_returns_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "symext");
        let src = Source::default();
        let node = sut.declare_extern(&sym, IdentKind::Meta, src.clone())?;

        // Remember that our stub does not care about compatibility.
        let rekind = IdentKind::Class(Dim::from_u8(3));
        let resrc = Source {
            desc: Some("redeclare".into()),
            ..Default::default()
        };
        let redeclare =
            sut.declare_extern(&sym, rekind.clone(), resrc.clone())?;

        // We don't care what the objects are for this test, just that the
        // same node is referenced.
        assert_eq!(node, redeclare);
        assert_eq!(Some((rekind, resrc)), sut.get(node).unwrap().given_extern);

        Ok(())
    }

    // Builds upon declare_returns_existing.
    #[test]
    fn declare_extern_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "symdup");
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        // Set up an object to fail redeclaration.
        let node = sut.declare_extern(&sym, IdentKind::Meta, src.clone())?;
        let obj = sut.get(node).unwrap();

        // It doesn't matter that this isn't the error that'll actually be
        // returned, as long as it's some sort of TransitionError.
        let terr = TransitionError::ExternResolution {
            name: String::from("test fail"),
            expected: IdentKind::Meta,
            given: IdentKind::Meta,
        };
        obj.fail_extern.replace(Some(terr.clone()));

        // Should invoke StubIdentObject::extern_ on the above `obj`.
        let result =
            sut.declare_extern(&sym, IdentKind::Meta, Source::default());

        if let Err(err) = result {
            // The node should have been restored.
            let obj = sut.get(node).unwrap();

            assert_eq!(src, obj.given_extern.as_ref().unwrap().1);
            assert_eq!(AsgError::ObjectTransition(terr), err);

            Ok(())
        } else {
            panic!("failure expected: {:?}", result);
        }
    }

    #[test]
    fn add_fragment_to_ident() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "tofrag");
        let src = Source {
            generated: true,
            ..Default::default()
        };
        let node = sut.declare(&sym, IdentKind::Meta, src.clone())?;

        let fragment = "a fragment".to_string();
        let node_with_frag = sut.set_fragment(node, fragment.clone())?;

        // Attaching a fragment should _replace_ the node, not create a
        // new one
        assert_eq!(
            node, node_with_frag,
            "fragment node does not match original node"
        );

        let obj = sut.get(node).unwrap();

        assert_eq!(Some(&sym), obj.given_declare);
        assert_eq!(Some((IdentKind::Meta, src)), obj.given_resolve);
        assert_eq!(Some(fragment), obj.given_set_fragment);

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "failfrag");
        let src = Source {
            generated: true,
            ..Default::default()
        };

        // The failure will come from terr below, not this.
        let node = sut.declare(&sym, IdentKind::Meta, src.clone())?;
        let obj = sut.get(node).unwrap();

        // It doesn't matter that this isn't the error that'll actually be
        // returned, as long as it's some sort of TransitionError.
        let terr = TransitionError::BadFragmentDest {
            name: String::from("test fail"),
        };
        obj.fail_set_fragment.replace(Some(terr.clone()));

        let result = sut
            .set_fragment(node, "".into())
            .expect_err("error expected");

        // The node should have been restored.
        let obj = sut.get(node).unwrap();

        assert_eq!(&sym, *obj.given_declare.as_ref().unwrap());
        assert_eq!(AsgError::ObjectTransition(terr), result);

        Ok(())
    }

    #[test]
    fn add_ident_dep_to_ident() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        let symnode = sut.declare(&sym, IdentKind::Meta, Source::default())?;
        let depnode = sut.declare(&dep, IdentKind::Meta, Source::default())?;

        sut.add_dep(symnode, depnode);
        assert!(sut.has_dep(symnode, depnode));

        // sanity check if we re-add a dep
        sut.add_dep(symnode, depnode);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    // same as above test
    #[test]
    fn add_dep_lookup_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        let _ = sut.declare(&sym, IdentKind::Meta, Source::default())?;
        let _ = sut.declare(&dep, IdentKind::Meta, Source::default())?;

        let (symnode, depnode) = sut.add_dep_lookup(&sym, &dep);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    #[test]
    fn add_dep_lookup_missing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        // both of these are missing
        let (symnode, depnode) = sut.add_dep_lookup(&sym, &dep);
        assert!(sut.has_dep(symnode, depnode));

        assert_eq!(Some(&sym), sut.get(symnode).unwrap().given_declare);
        assert_eq!(Some(&dep), sut.get(depnode).unwrap().given_declare);

        Ok(())
    }

    #[test]
    fn declare_return_missing_symbol() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        // both of these are missing, see add_dep_lookup_missing
        let (symnode, _) = sut.add_dep_lookup(&sym, &dep);

        let src = Source {
            desc: Some("redeclare missing".into()),
            ..Default::default()
        };

        // Check with a declared value
        let declared = sut.declare(&sym, IdentKind::Meta, src.clone())?;

        assert_eq!(symnode, declared);

        let obj = sut.get(declared).unwrap();

        assert_eq!(Some(&sym), obj.given_declare);
        assert_eq!(Some((IdentKind::Meta, src)), obj.given_resolve);

        Ok(())
    }

    macro_rules! assert_section_sym {
        ( $iter:expr, $s:ident ) => {{
            let mut pos = 0;
            for obj in $iter {
                let sym = obj.name().expect("missing object");
                assert_eq!($s.get(pos), Some(sym));

                pos = pos + 1;
            }
        };};
    }

    macro_rules! add_syms {
        ($sut:ident, $base:expr, {$($dest:ident <- $name:ident: $kind:path,)*}) => {
            let mut i = 1;

            $(
                i += 1;

                let sym = symbol_dummy!(i, stringify!($name));

                $sut.declare(&sym, $kind, Source::default()).unwrap();
                let (_, _) = $sut.add_dep_lookup($base, &sym);

                $dest.push(sym);
            )*
        };
    }

    #[test]
    fn graph_sort() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let mut meta = vec![];
        let mut worksheet = vec![];
        let mut map = vec![];
        let mut retmap = vec![];

        let base = symbol_dummy!(1, "sym1");
        let base_node = sut
            .declare(&base, IdentKind::Map, Source::default())
            .unwrap();

        add_syms!(sut, &base, {
            meta <- meta1: IdentKind::Meta,
            worksheet <- work1: IdentKind::Worksheet,
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
            meta <- meta2: IdentKind::Meta,
            worksheet <- work2: IdentKind::Worksheet,
            map <- map6: IdentKind::MapTail,
            retmap <- retmap6: IdentKind::RetMapHead,
        });

        map.push(base);

        let sections = sut.sort(&vec![base_node])?;

        assert_section_sym!(sections.meta.iter(), meta);
        assert_section_sym!(sections.worksheet.iter(), worksheet);
        assert_section_sym!(sections.map.iter(), map);
        assert_section_sym!(sections.retmap.iter(), retmap);

        Ok(())
    }

    #[test]
    fn graph_sort_missing_node() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        sut.set_fragment(sym_node, FragmentText::from("foo"))
            .unwrap();

        let (_, _) = sut.add_dep_lookup(&sym, &dep);

        match sut.sort(&vec![sym_node]) {
            Ok(_) => panic!("Unexpected success - dependency is not in graph"),
            Err(SortableAsgError::MissingObjectKind(_)) => (),
            _ => {
                panic!("Incorrect error result when dependency is not in graph")
            }
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_roots() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(1, "sym");
        let dep = symbol_dummy!(2, "dep");

        let (_, _) = sut.add_dep_lookup(&sym, &dep);

        let sections = sut.sort(&vec![])?;

        assert_eq!(Sections::new(), sections);

        Ok(())
    }

    #[test]
    fn graph_sort_simple_cycle() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(2, "sym");
        let dep = symbol_dummy!(3, "dep");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                &dep,
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

        let (_, _) = sut.add_dep_lookup(&sym, &dep);
        let (_, _) = sut.add_dep_lookup(&dep, &sym);

        let result = sut.sort(&vec![sym_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> =
            vec![vec![dep_node.into(), sym_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_two_simple_cycles() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(2, "sym");
        let sym2 = symbol_dummy!(3, "sym2");
        let dep = symbol_dummy!(4, "dep");
        let dep2 = symbol_dummy!(5, "dep2");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                &sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                &dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep2_node = sut
            .declare(
                &dep2,
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

        let (_, _) = sut.add_dep_lookup(&sym, &dep);
        let (_, _) = sut.add_dep_lookup(&dep, &sym);
        let (_, _) = sut.add_dep_lookup(&sym2, &dep2);
        let (_, _) = sut.add_dep_lookup(&dep2, &sym2);

        let result = sut.sort(&vec![sym_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> = vec![
            vec![dep_node.into(), sym_node.into()],
            vec![dep2_node.into(), sym2_node.into()],
        ];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_cycle_with_edge_to_same_node() -> SortableAsgResult<(), u8>
    {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(2, "sym");
        let dep = symbol_dummy!(3, "dep");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                &dep,
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

        let (_, _) = sut.add_dep_lookup(&sym, &dep);
        let (_, _) = sut.add_dep_lookup(&sym, &dep);

        let result = sut.sort(&vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cycle_with_a_few_steps() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym1 = symbol_dummy!(1, "sym1");
        let sym2 = symbol_dummy!(2, "sym2");
        let sym3 = symbol_dummy!(3, "sym3");

        let sym1_node = sut
            .declare(
                &sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                &sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                &sym3,
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

        let (_, _) = sut.add_dep_lookup(&sym1, &sym2);
        let (_, _) = sut.add_dep_lookup(&sym2, &sym3);
        let (_, _) = sut.add_dep_lookup(&sym3, &sym1);

        let result = sut.sort(&vec![sym1_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_with_non_function_with_a_few_steps(
    ) -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym1 = symbol_dummy!(1, "sym1");
        let sym2 = symbol_dummy!(2, "sym2");
        let sym3 = symbol_dummy!(3, "sym3");

        let sym1_node = sut
            .declare(
                &sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                &sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                &sym3,
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

        let (_, _) = sut.add_dep_lookup(&sym1, &sym2);
        let (_, _) = sut.add_dep_lookup(&sym2, &sym3);
        let (_, _) = sut.add_dep_lookup(&sym3, &sym1);

        let result = sut.sort(&vec![sym1_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_bookended_by_functions() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym1 = symbol_dummy!(1, "sym1");
        let sym2 = symbol_dummy!(2, "sym2");
        let sym3 = symbol_dummy!(3, "sym3");

        let sym1_node = sut
            .declare(
                &sym1,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                &sym2,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                &sym3,
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

        let (_, _) = sut.add_dep_lookup(&sym1, &sym2);
        let (_, _) = sut.add_dep_lookup(&sym2, &sym3);
        let (_, _) = sut.add_dep_lookup(&sym3, &sym1);

        let result = sut.sort(&vec![sym1_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_ignored() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(2, "sym");
        let dep = symbol_dummy!(3, "dep");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                &dep,
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

        let (_, _) = sut.add_dep_lookup(&sym, &dep);
        let (_, _) = sut.add_dep_lookup(&dep, &sym);

        let result = sut.sort(&vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_cyclic_function_is_bookended() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym1 = symbol_dummy!(1, "sym1");
        let sym2 = symbol_dummy!(2, "sym2");
        let sym3 = symbol_dummy!(3, "sym3");

        let sym1_node = sut
            .declare(
                &sym1,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym2_node = sut
            .declare(
                &sym2,
                IdentKind::Func(Dim::default(), SymDtype::Empty),
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let sym3_node = sut
            .declare(
                &sym3,
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

        let (_, _) = sut.add_dep_lookup(&sym1, &sym2);
        let (_, _) = sut.add_dep_lookup(&sym2, &sym3);
        let (_, _) = sut.add_dep_lookup(&sym3, &sym1);

        let result = sut.sort(&vec![sym1_node]);

        let expected: Vec<Vec<ObjectRef<u8>>> =
            vec![vec![sym3_node.into(), sym2_node.into(), sym1_node.into()]];
        match result {
            Ok(_) => panic!("sort did not detect cycle"),
            Err(SortableAsgError::Cycles(scc)) => assert_eq!(expected, scc),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }

    #[test]
    fn graph_sort_ignore_non_linked() -> SortableAsgResult<(), u8> {
        let mut sut = Sut::new();

        let sym = symbol_dummy!(2, "sym");
        let dep = symbol_dummy!(3, "dep");
        let ignored = symbol_dummy!(4, "ignored");

        let sym_node = sut
            .declare(
                &sym,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let dep_node = sut
            .declare(
                &dep,
                IdentKind::Tpl,
                Source {
                    virtual_: true,
                    ..Default::default()
                },
            )
            .unwrap();

        let ignored_node = sut
            .declare(
                &ignored,
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

        let (_, _) = sut.add_dep_lookup(&sym, &dep);
        let (_, _) = sut.add_dep_lookup(&ignored, &sym);

        let result = sut.sort(&vec![sym_node]);

        match result {
            Ok(_) => (),
            Err(e) => panic!("unexpected error: {}", e),
        }

        Ok(())
    }
}
