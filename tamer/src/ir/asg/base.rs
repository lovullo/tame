// Concrete ASG
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

//! Base concrete [`Asg`] implementation.

use super::graph::{Asg, AsgEdge, AsgResult, IndexType, Node, ObjectRef};
use super::ident::IdentKind;
use super::object::{
    FragmentText, IdentObjectData, IdentObjectState, Source, TransitionResult,
};
use crate::sym::SymbolId;
use petgraph::graph::{DiGraph, Graph, NodeIndex};

/// Concrete ASG.
///
/// This implementation is currently based on [`petgraph`].
///
/// Identifiers are cached by name for `O(1)` lookup.
/// Since [`SymbolId`][crate::sym::SymbolId] is used for this purpose,
///   the index may contain more entries than nodes and may contain gaps.
///
/// For more information,
///   see [`Asg`].
pub struct BaseAsg<O, Ix>
where
    Ix: IndexType,
{
    // TODO: private; see `ld::xmle::lower`.
    /// Directed graph on which objects are stored.
    pub graph: DiGraph<Node<O>, AsgEdge, Ix>,

    /// Map of [`SymbolId`][crate::sym::SymbolId] to node indexes.
    ///
    /// This allows for `O(1)` lookup of identifiers in the graph.
    /// Note that,
    ///   while we store [`NodeIndex`] internally,
    ///   the public API encapsulates it within an [`ObjectRef`].
    index: Vec<NodeIndex<Ix>>,

    /// Empty node indicating that no object exists for a given index.
    empty_node: NodeIndex<Ix>,
}

impl<O, Ix> BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: IdentObjectState<O> + IdentObjectData,
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
    fn index_identifier(&mut self, name: SymbolId, node: NodeIndex<Ix>) {
        let i = name.as_usize();

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
    fn lookup_or_missing(&mut self, ident: SymbolId) -> ObjectRef<Ix> {
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
        name: SymbolId,
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

impl<O, Ix> Asg<O, Ix> for BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: IdentObjectState<O> + IdentObjectData,
{
    fn declare(
        &mut self,
        name: SymbolId,
        kind: IdentKind,
        src: Source,
    ) -> AsgResult<ObjectRef<Ix>> {
        self.with_ident_lookup(name, |obj| obj.resolve(kind, src))
    }

    fn declare_extern(
        &mut self,
        name: SymbolId,
        kind: IdentKind,
        src: Source,
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
    fn lookup(&self, name: SymbolId) -> Option<ObjectRef<Ix>> {
        let i = name.as_usize();

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
        ident: SymbolId,
        dep: SymbolId,
    ) -> (ObjectRef<Ix>, ObjectRef<Ix>) {
        let identi = self.lookup_or_missing(ident);
        let depi = self.lookup_or_missing(dep);

        self.graph
            .update_edge(identi.into(), depi.into(), Default::default());

        (identi, depi)
    }
}

#[cfg(test)]
mod test {
    use super::super::graph::AsgError;
    use super::*;
    use crate::ir::asg::{
        Dim, IdentObject, TransitionError, TransitionResult, UnresolvedError,
    };
    use crate::sym::{GlobalSymbolIntern, SymbolId};
    use std::cell::RefCell;

    #[derive(Debug, Default, PartialEq)]
    struct StubIdentObject {
        given_declare: Option<SymbolId>,
        given_extern: Option<(IdentKind, Source)>,
        given_resolve: Option<(IdentKind, Source)>,
        given_set_fragment: Option<FragmentText>,
        fail_redeclare: RefCell<Option<TransitionError>>,
        fail_extern: RefCell<Option<TransitionError>>,
        fail_set_fragment: RefCell<Option<TransitionError>>,
        fail_resolved: RefCell<Option<UnresolvedError>>,
    }

    impl<'i> IdentObjectData for StubIdentObject {
        fn name(&self) -> Option<SymbolId> {
            self.given_declare
        }

        fn kind(&self) -> Option<&IdentKind> {
            self.given_resolve.as_ref().map(|args| &args.0)
        }

        fn src(&self) -> Option<&Source> {
            None
        }

        fn fragment(&self) -> Option<FragmentText> {
            None
        }

        fn as_ident(&self) -> Option<&IdentObject> {
            None
        }
    }

    impl<'i> IdentObjectState<StubIdentObject> for StubIdentObject {
        fn declare(ident: SymbolId) -> Self {
            Self {
                given_declare: Some(ident),
                ..Default::default()
            }
        }

        fn resolve(
            mut self,
            kind: IdentKind,
            src: Source,
        ) -> TransitionResult<StubIdentObject> {
            if self.fail_redeclare.borrow().is_some() {
                let err = self.fail_redeclare.replace(None).unwrap();
                return Err((self, err));
            }

            self.given_resolve = Some((kind, src));
            Ok(self)
        }

        fn resolved(&self) -> Result<&StubIdentObject, UnresolvedError> {
            if self.fail_resolved.borrow().is_some() {
                return Err(self.fail_resolved.replace(None).unwrap());
            }

            Ok(self)
        }

        fn extern_(
            mut self,
            kind: IdentKind,
            src: Source,
        ) -> TransitionResult<StubIdentObject> {
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
        ) -> TransitionResult<StubIdentObject> {
            if self.fail_set_fragment.borrow().is_some() {
                let err = self.fail_set_fragment.replace(None).unwrap();
                return Err((self, err));
            }

            self.given_set_fragment.replace(text);
            Ok(self)
        }
    }

    type Sut = BaseAsg<StubIdentObject, u16>;

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
        let syma = "syma".intern();
        let symb = "symab".intern();

        let nodea = sut.declare(
            syma,
            IdentKind::Meta,
            Source {
                desc: Some("a".into()),
                ..Default::default()
            },
        )?;

        let nodeb = sut.declare(
            symb,
            IdentKind::Worksheet,
            Source {
                desc: Some("b".into()),
                ..Default::default()
            },
        )?;

        assert_ne!(nodea, nodeb);

        assert_eq!(Some(syma), sut.get(nodea).unwrap().given_declare);
        assert_eq!(
            Some((
                IdentKind::Meta,
                Source {
                    desc: Some("a".into()),
                    ..Default::default()
                },
            )),
            sut.get(nodea).unwrap().given_resolve
        );

        assert_eq!(Some(symb), sut.get(nodeb).unwrap().given_declare);
        assert_eq!(
            Some((
                IdentKind::Worksheet,
                Source {
                    desc: Some("b".into()),
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

        let sym = "lookup".into();
        let node = sut.declare(
            sym,
            IdentKind::Meta,
            Source {
                generated: true,
                ..Default::default()
            },
        )?;

        assert_eq!(Some(node), sut.lookup(sym));

        Ok(())
    }

    #[test]
    fn declare_returns_existing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "symdup".into();
        let src = Source::default();
        let node = sut.declare(sym, IdentKind::Meta, src.clone())?;

        // Remember that our stub does not care about compatibility.
        let rekind = IdentKind::Class(Dim::from_u8(3));
        let resrc = Source {
            desc: Some("redeclare".into()),
            ..Default::default()
        };
        let redeclare = sut.declare(sym, rekind.clone(), resrc.clone())?;

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

        let sym = "symdup".intern();
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        // Set up an object to fail redeclaration.
        let node = sut.declare(sym, IdentKind::Meta, src.clone())?;
        let obj = sut.get(node).unwrap();
        let terr = TransitionError::ExternResolution {
            name: "test fail".into(),
            expected: IdentKind::Meta,
            given: IdentKind::Meta,
        };
        obj.fail_redeclare.replace(Some(terr.clone()));

        // Should invoke StubIdentObject::redeclare on the above `obj`.
        let result = sut.declare(sym, IdentKind::Meta, Source::default());

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

        let sym = "symext".intern();
        let src = Source::default();
        let node = sut.declare_extern(sym, IdentKind::Meta, src.clone())?;

        // Remember that our stub does not care about compatibility.
        let rekind = IdentKind::Class(Dim::from_u8(3));
        let resrc = Source {
            desc: Some("redeclare".into()),
            ..Default::default()
        };
        let redeclare =
            sut.declare_extern(sym, rekind.clone(), resrc.clone())?;

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

        let sym = "symdup".intern();
        let src = Source {
            desc: Some("orig".into()),
            ..Default::default()
        };

        // Set up an object to fail redeclaration.
        let node = sut.declare_extern(sym, IdentKind::Meta, src.clone())?;
        let obj = sut.get(node).unwrap();

        // It doesn't matter that this isn't the error that'll actually be
        // returned, as long as it's some sort of TransitionError.
        let terr = TransitionError::ExternResolution {
            name: "test fail".into(),
            expected: IdentKind::Meta,
            given: IdentKind::Meta,
        };
        obj.fail_extern.replace(Some(terr.clone()));

        // Should invoke StubIdentObject::extern_ on the above `obj`.
        let result =
            sut.declare_extern(sym, IdentKind::Meta, Source::default());

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

        let sym = "tofrag".intern();
        let src = Source {
            generated: true,
            ..Default::default()
        };
        let node = sut.declare(sym, IdentKind::Meta, src.clone())?;

        let fragment = "a fragment".intern();
        let node_with_frag = sut.set_fragment(node, fragment)?;

        // Attaching a fragment should _replace_ the node, not create a
        // new one
        assert_eq!(
            node, node_with_frag,
            "fragment node does not match original node"
        );

        let obj = sut.get(node).unwrap();

        assert_eq!(Some(sym), obj.given_declare);
        assert_eq!(Some((IdentKind::Meta, src)), obj.given_resolve);
        assert_eq!(Some(fragment), obj.given_set_fragment);

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident_fails_if_transition_fails() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "failfrag".intern();
        let src = Source {
            generated: true,
            ..Default::default()
        };

        // The failure will come from terr below, not this.
        let node = sut.declare(sym, IdentKind::Meta, src.clone())?;
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

        assert_eq!(sym, *obj.given_declare.as_ref().unwrap());
        assert_eq!(AsgError::ObjectTransition(terr), result);

        Ok(())
    }

    #[test]
    fn add_ident_dep_to_ident() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        let symnode = sut.declare(sym, IdentKind::Meta, Source::default())?;
        let depnode = sut.declare(dep, IdentKind::Meta, Source::default())?;

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

        let sym = "sym".intern();
        let dep = "dep".intern();

        let _ = sut.declare(sym, IdentKind::Meta, Source::default())?;
        let _ = sut.declare(dep, IdentKind::Meta, Source::default())?;

        let (symnode, depnode) = sut.add_dep_lookup(sym, dep);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    #[test]
    fn add_dep_lookup_missing() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        // both of these are missing
        let (symnode, depnode) = sut.add_dep_lookup(sym, dep);
        assert!(sut.has_dep(symnode, depnode));

        assert_eq!(Some(sym), sut.get(symnode).unwrap().given_declare);
        assert_eq!(Some(dep), sut.get(depnode).unwrap().given_declare);

        Ok(())
    }

    #[test]
    fn declare_return_missing_symbol() -> AsgResult<()> {
        let mut sut = Sut::new();

        let sym = "sym".intern();
        let dep = "dep".intern();

        // both of these are missing, see add_dep_lookup_missing
        let (symnode, _) = sut.add_dep_lookup(sym, dep);

        let src = Source {
            desc: Some("redeclare missing".into()),
            ..Default::default()
        };

        // Check with a declared value
        let declared = sut.declare(sym, IdentKind::Meta, src.clone())?;

        assert_eq!(symnode, declared);

        let obj = sut.get(declared).unwrap();

        assert_eq!(Some(sym), obj.given_declare);
        assert_eq!(Some((IdentKind::Meta, src)), obj.given_resolve);

        Ok(())
    }
}
