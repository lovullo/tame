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

use super::graph::{Asg, AsgEdge, AsgError, AsgResult, Node, ObjectRef};
use super::ident::IdentKind;
use super::object::{FragmentText, Object, Source};
use crate::sym::Symbol;
use fixedbitset::FixedBitSet;
use petgraph::graph::{
    DiGraph, EdgeIndex, Graph, IndexType, Neighbors, NodeIndex,
};
use petgraph::visit::{GraphBase, IntoNeighbors, Visitable};

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
pub struct BaseAsg<'i, Ix: IndexType> {
    /// Directed graph on which objects are stored.
    graph: DiGraph<Node<'i>, AsgEdge, Ix>,

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

impl<'i, Ix> BaseAsg<'i, Ix>
where
    Ix: IndexType,
{
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
    ///
    /// A basic `new` method is not provided to ensure that callers consider
    ///   capacity during construction,
    ///     since graphs can get quite large.
    pub fn with_capacity(objects: usize, edges: usize) -> Self {
        let mut graph = Graph::with_capacity(objects, edges);
        let mut index = Vec::with_capacity(objects);

        // Exhaust the first index to be used as a placeholder.
        let empty_node = graph.add_node(Some(Object::Empty));
        index.push(empty_node);

        Self {
            graph,
            index,
            empty_node,
        }
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

    /// Lookup `ident` or add an [`Object::Missing`] to the graph and
    ///   return a reference to it.
    #[inline]
    fn lookup_or_missing(&mut self, ident: &'i Symbol<'i>) -> ObjectRef<Ix> {
        self.lookup(ident).unwrap_or_else(|| {
            let index = self.graph.add_node(Some(Object::Missing(ident)));

            self.index_identifier(ident, index);
            ObjectRef(index)
        })
    }
}

impl<'i, Ix> Asg<'i, Ix> for BaseAsg<'i, Ix>
where
    Ix: IndexType,
{
    fn declare(
        &mut self,
        name: &'i Symbol<'i>,
        kind: IdentKind,
        src: Source<'i>,
    ) -> AsgResult<ObjectRef<Ix>> {
        // TODO: src check
        if let Some(existing) = self.lookup(name) {
            let node = self.graph.node_weight_mut(existing.0).unwrap();

            match node {
                Some(Object::Missing(_)) => {
                    node.replace(Object::Ident(name, kind, src));
                    return Ok(existing);
                }
                // TODO: no override-override
                Some(Object::Ident(_, _, orig_src))
                    if orig_src.virtual_ && src.override_ =>
                {
                    *orig_src = src;
                    return Ok(existing);
                }
                // TODO: no override-override
                Some(Object::IdentFragment(_, _, orig_src, _))
                    if orig_src.virtual_ && src.override_ =>
                {
                    // clears fragment, which is no longer applicable
                    node.replace(Object::Ident(name, kind, src));
                    return Ok(existing);
                }
                _ => return Ok(existing),
            }
        }

        let node = self.graph.add_node(Some(Object::Ident(name, kind, src)));

        self.index_identifier(name, node);

        Ok(ObjectRef(node))
    }

    fn declare_extern(
        &mut self,
        name: &'i Symbol<'i>,
        expected_kind: IdentKind,
    ) -> AsgResult<ObjectRef<Ix>> {
        // TODO: resolution!
        let node = self
            .graph
            .add_node(Some(Object::Extern(name, expected_kind)));

        self.index_identifier(name, node);

        Ok(ObjectRef(node))
    }

    fn set_fragment(
        &mut self,
        identi: ObjectRef<Ix>,
        text: FragmentText,
    ) -> AsgResult<ObjectRef<Ix>> {
        // This should _never_ happen as long as you're only using ObjectRef
        // values produced by these methods.
        let node = self
            .graph
            .node_weight_mut(identi.0)
            .expect("internal error: BaseAsg::set_fragment bogus identi");

        // This should also never happen, since we immediately repopulate
        // the node below.
        let ty = node
            .take()
            .expect("internal error: BaseAsg::set_fragment missing Node data");

        let result = match ty {
            Object::Ident(sym, kind, src) => {
                Ok(Object::IdentFragment(sym, kind, src, text))
            }
            Object::IdentFragment(_, IdentKind::MapHead, _, _) => Ok(ty),
            Object::IdentFragment(_, IdentKind::MapTail, _, _) => Ok(ty),
            Object::IdentFragment(_, IdentKind::RetMapHead, _, _) => Ok(ty),
            Object::IdentFragment(_, IdentKind::RetMapTail, _, _) => Ok(ty),
            // TODO remove these ignores when fixed
            Object::IdentFragment(
                sym,
                IdentKind::Map,
                Source {
                    virtual_: true,
                    override_: true,
                    ..
                },
                _,
            ) => {
                eprintln!(
                    "ignoring virtual and overridden map object: {}",
                    sym
                );
                Ok(ty)
            }
            Object::IdentFragment(
                sym,
                IdentKind::RetMap,
                Source {
                    virtual_: true,
                    override_: true,
                    ..
                },
                _,
            ) => {
                eprintln!(
                    "ignoring virtual and overridden retmap object: {}",
                    sym
                );
                Ok(ty)
            }
            _ => {
                let err = Err(AsgError::BadFragmentDest(format!(
                    "identifier is not a Object::Ident): {:?}",
                    ty,
                )));

                node.replace(ty);
                err
            }
        }?;

        node.replace(result);

        Ok(identi)
    }

    #[inline]
    fn get<I: Into<ObjectRef<Ix>>>(&self, index: I) -> Option<&Object<'i>> {
        self.graph.node_weight(index.into().0).map(|node| {
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
            .map(|ni| ObjectRef(*ni))
    }

    fn add_dep(&mut self, identi: ObjectRef<Ix>, depi: ObjectRef<Ix>) {
        self.graph.update_edge(identi.0, depi.0, Default::default());
    }

    #[inline]
    fn has_dep(&self, ident: ObjectRef<Ix>, dep: ObjectRef<Ix>) -> bool {
        self.graph.contains_edge(ident.0, dep.0)
    }

    fn add_dep_lookup(
        &mut self,
        ident: &'i Symbol<'i>,
        dep: &'i Symbol<'i>,
    ) -> (ObjectRef<Ix>, ObjectRef<Ix>) {
        let identi = self.lookup_or_missing(ident);
        let depi = self.lookup_or_missing(dep);

        self.graph.update_edge(identi.0, depi.0, Default::default());

        (identi, depi)
    }
}

// TODO: encapsulate Petgraph API (N.B. this is untested!)
impl<'i, Ix> Visitable for BaseAsg<'i, Ix>
where
    Ix: IndexType,
{
    type Map = FixedBitSet;

    fn visit_map(&self) -> Self::Map {
        self.graph.visit_map()
    }

    fn reset_map(&self, map: &mut Self::Map) {
        self.graph.reset_map(map)
    }
}

impl<'i, Ix> GraphBase for BaseAsg<'i, Ix>
where
    Ix: IndexType,
{
    type NodeId = NodeIndex<Ix>;
    type EdgeId = EdgeIndex<Ix>;
}

impl<'a, 'i, Ix> IntoNeighbors for &'a BaseAsg<'i, Ix>
where
    Ix: IndexType,
{
    type Neighbors = Neighbors<'a, AsgEdge, Ix>;

    fn neighbors(self, n: Self::NodeId) -> Self::Neighbors {
        self.graph.neighbors(n)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::SymbolIndex;

    type Sut<'i> = BaseAsg<'i, u8>;

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
        let mut sut = Sut::with_capacity(0, 0);

        // NB: The index ordering is important!  We first use a larger
        // index to create a gap, and then use an index within that gap
        // to ensure that it's not considered an already-defined
        // identifier.
        let syma = Symbol::new_dummy(SymbolIndex::from_u32(5), "syma");
        let symb = Symbol::new_dummy(SymbolIndex::from_u32(1), "symab");

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

        assert_eq!(
            Some(&Object::Ident(
                &syma,
                IdentKind::Meta,
                Source {
                    desc: Some("a".to_string()),
                    ..Default::default()
                },
            )),
            sut.get(nodea),
        );

        assert_eq!(
            Some(&Object::Ident(
                &symb,
                IdentKind::Worksheet,
                Source {
                    desc: Some("b".to_string()),
                    ..Default::default()
                },
            )),
            sut.get(nodeb),
        );

        Ok(())
    }

    #[test]
    fn lookup_by_symbol() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "lookup");
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
    fn declare_extern() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "extern");
        let node = sut.declare_extern(&sym, IdentKind::Meta)?;

        assert_eq!(Some(&Object::Extern(&sym, IdentKind::Meta)), sut.get(node),);

        Ok(())
    }

    // TODO: incompatible
    #[test]
    fn declare_returns_existing_compatible() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "symdup");
        let node = sut.declare(&sym, IdentKind::Meta, Source::default())?;

        // Same declaration a second time
        let redeclare =
            sut.declare(&sym, IdentKind::Meta, Source::default())?;

        assert_eq!(node, redeclare);
        Ok(())
    }

    // TODO: incompatible
    #[test]
    fn declare_override_virtual_ident() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "virtual");
        let over_src = Symbol::new_dummy(SymbolIndex::from_u32(2), "src");

        let virt_node = sut.declare(
            &sym,
            IdentKind::Meta,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )?;

        let over_src = Source {
            override_: true,
            src: Some(&over_src),
            ..Default::default()
        };

        let over_node = sut.declare(&sym, IdentKind::Meta, over_src.clone())?;

        assert_eq!(virt_node, over_node);

        assert_eq!(
            sut.get(over_node),
            Some(&Object::Ident(&sym, IdentKind::Meta, over_src,))
        );

        Ok(())
    }

    // TODO: incompatible
    #[test]
    fn declare_override_virtual_ident_fragment() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "virtual");
        let over_src = Symbol::new_dummy(SymbolIndex::from_u32(2), "src");

        let virt_node = sut.declare(
            &sym,
            IdentKind::Meta,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )?;

        sut.set_fragment(virt_node, FragmentText::from("remove me"))?;

        let over_src = Source {
            override_: true,
            src: Some(&over_src),
            ..Default::default()
        };

        let over_node = sut.declare(&sym, IdentKind::Meta, over_src.clone())?;

        assert_eq!(virt_node, over_node);

        // The act of overriding the node should have cleared any existing
        // fragment, making way for a new fragment to take its place as soon
        // as it is discovered.  (So, back to an Object::Ident.)
        assert_eq!(
            sut.get(over_node),
            Some(&Object::Ident(&sym, IdentKind::Meta, over_src,))
        );

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "tofrag");
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

        assert_eq!(
            Some(&Object::IdentFragment(&sym, IdentKind::Meta, src, fragment)),
            sut.get(node)
        );

        Ok(())
    }

    fn add_ident_kind_ignores(
        given: IdentKind,
        expected: IdentKind,
    ) -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "tofrag");
        let src = Source {
            generated: true,
            ..Default::default()
        };

        let node = sut.declare(&sym, given, src.clone())?;

        let fragment = "a fragment".to_string();
        let node_with_frag = sut.set_fragment(node, fragment.clone())?;

        // Attaching a fragment should _replace_ the node, not create a
        // new one
        assert_eq!(
            node, node_with_frag,
            "fragment node does not match original node"
        );

        assert_eq!(
            Some(&Object::IdentFragment(&sym, expected, src, fragment)),
            sut.get(node)
        );

        Ok(())
    }

    #[test]
    fn add_fragment_to_ident_map_head() -> AsgResult<()> {
        add_ident_kind_ignores(IdentKind::MapHead, IdentKind::MapHead)
    }

    #[test]
    fn add_fragment_to_ident_map_tail() -> AsgResult<()> {
        add_ident_kind_ignores(IdentKind::MapTail, IdentKind::MapTail)
    }

    #[test]
    fn add_fragment_to_ident_retmap_head() -> AsgResult<()> {
        add_ident_kind_ignores(IdentKind::RetMapHead, IdentKind::RetMapHead)
    }

    #[test]
    fn add_fragment_to_ident_retmap_tail() -> AsgResult<()> {
        add_ident_kind_ignores(IdentKind::RetMapTail, IdentKind::RetMapTail)
    }

    #[test]
    fn add_fragment_to_fragment_fails() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let node = sut.declare(&sym, IdentKind::Meta, Source::default())?;
        let fragment = "orig fragment".to_string();

        sut.set_fragment(node, fragment.clone())?;

        // Since it's already a fragment, this should fail.
        let err = sut
            .set_fragment(node, "replacement".to_string())
            .expect_err("Expected failure");

        match err {
            AsgError::BadFragmentDest(str) if str.contains("sym") => (),
            _ => panic!("expected AsgError::BadFragmentDest: {:?}", err),
        }

        // Make sure we didn't leave the node in an inconsistent state
        assert_eq!(
            Some(&Object::IdentFragment(
                &sym,
                IdentKind::Meta,
                Default::default(),
                fragment
            )),
            sut.get(node)
        );

        Ok(())
    }

    #[test]
    fn add_ident_dep_to_ident() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(1), "dep");

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
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

        let _ = sut.declare(&sym, IdentKind::Meta, Source::default())?;
        let _ = sut.declare(&dep, IdentKind::Meta, Source::default())?;

        let (symnode, depnode) = sut.add_dep_lookup(&sym, &dep);
        assert!(sut.has_dep(symnode, depnode));

        Ok(())
    }

    #[test]
    fn add_dep_lookup_missing() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

        // both of these are missing
        let (symnode, depnode) = sut.add_dep_lookup(&sym, &dep);
        assert!(sut.has_dep(symnode, depnode));

        assert_eq!(Some(&Object::Missing(&sym)), sut.get(symnode));
        assert_eq!(Some(&Object::Missing(&dep)), sut.get(depnode));

        Ok(())
    }

    #[test]
    fn declare_return_missing_symbol() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

        // both of these are missing, see add_dep_lookup_missing
        let (symnode, _) = sut.add_dep_lookup(&sym, &dep);

        let src = Source {
            desc: Some("Tamer is NOT lamer.".to_string()),
            ..Default::default()
        };

        // Check with a declared value
        let declared = sut.declare(&sym, IdentKind::Meta, src.clone())?;

        assert_eq!(symnode, declared);

        assert_eq!(
            Some(&Object::Ident(&sym, IdentKind::Meta, src)),
            sut.get(declared),
        );

        Ok(())
    }
}
