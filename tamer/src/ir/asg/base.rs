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
    Asg, AsgEdge, AsgError, AsgResult, Node, ObjectRef, SortableAsg,
};
use super::ident::IdentKind;
use super::object::{FragmentText, ObjectData, ObjectState, Source};
use super::Sections;
use crate::sym::Symbol;
use fixedbitset::FixedBitSet;
use petgraph::graph::{
    DiGraph, EdgeIndex, Graph, IndexType, Neighbors, NodeIndex,
};
use petgraph::visit::{DfsPostOrder, GraphBase, IntoNeighbors, Visitable};

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
    O: ObjectState<'i, O>,
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
        let empty_node = graph.add_node(None);
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

    /// Lookup `ident` or add a missing identifier to the graph and return a
    ///   reference to it.
    ///
    /// See [`ObjectState::missing`] for more information.
    #[inline]
    fn lookup_or_missing(&mut self, ident: &'i Symbol<'i>) -> ObjectRef<Ix> {
        self.lookup(ident).unwrap_or_else(|| {
            let index = self.graph.add_node(Some(O::missing(ident)));

            self.index_identifier(ident, index);
            ObjectRef(index)
        })
    }
}

impl<'i, O, Ix> Asg<'i, O, Ix> for BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: ObjectState<'i, O>,
{
    fn declare(
        &mut self,
        name: &'i Symbol<'i>,
        kind: IdentKind,
        src: Source<'i>,
    ) -> AsgResult<ObjectRef<Ix>> {
        if let Some(existing) = self.lookup(name) {
            let node = self.graph.node_weight_mut(existing.0).unwrap();

            let obj = node.take().expect(&format!(
                "internal error: missing object for {}",
                name
            ));

            // TODO: test inconsistent state (fixed)
            return obj
                .redeclare(kind, src)
                .and_then(|obj| {
                    node.replace(obj);
                    Ok(existing)
                })
                .or_else(|(orig, err)| {
                    node.replace(orig);
                    Err(err.into())
                });
        }

        let node = self.graph.add_node(Some(O::ident(name, kind, src)));

        self.index_identifier(name, node);

        Ok(ObjectRef(node))
    }

    fn declare_extern(
        &mut self,
        name: &'i Symbol<'i>,
        expected_kind: IdentKind,
    ) -> AsgResult<ObjectRef<Ix>> {
        // TODO: resolution!
        let node = self.graph.add_node(Some(O::extern_(name, expected_kind)));

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

        // Be sure to restore the previous node if the transition fails,
        // otherwise we'll be left in an inconsistent internal state.
        ty.set_fragment(text)
            .and_then(|obj| {
                node.replace(obj);
                Ok(identi)
            })
            .or_else(|(orig, err)| {
                node.replace(orig);
                Err(err.into())
            })
    }

    #[inline]
    fn get<I: Into<ObjectRef<Ix>>>(&self, index: I) -> Option<&O> {
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

impl<'i, O, Ix> SortableAsg<'i, O, Ix> for BaseAsg<O, Ix>
where
    Ix: IndexType,
    O: ObjectData<'i> + ObjectState<'i, O>,
{
    fn sort(&'i self, roots: &[ObjectRef<Ix>]) -> AsgResult<Sections<'i, O>> {
        let mut deps = Sections::new();

        // This is technically a topological sort, but functions have
        // cycles.  Once we have more symbol metadata, we can filter them out
        // and actually invoke toposort.
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
                    return Err(AsgError::UnexpectedNode(format!(
                        "{:?}",
                        ident.ident()
                    )))
                }
            }
        }

        Ok(deps)
    }
}

// TODO: encapsulate Petgraph API (N.B. this is untested!)
impl<'i, O, Ix> Visitable for BaseAsg<O, Ix>
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

impl<'i, O, Ix> GraphBase for BaseAsg<O, Ix>
where
    Ix: IndexType,
{
    type NodeId = NodeIndex<Ix>;
    type EdgeId = EdgeIndex<Ix>;
}

impl<'a, 'i, O, Ix> IntoNeighbors for &'a BaseAsg<O, Ix>
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
    use super::super::graph::AsgError;
    use super::*;
    use crate::ir::asg::Object;
    use crate::sym::SymbolIndex;

    // TODO: mock Object
    type Sut<'i> = BaseAsg<Object<'i>, u8>;

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

        Ok(())
    }

    #[test]
    fn add_ident_dep_to_ident() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

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

    macro_rules! assert_section_sym {
        ( $iter:expr, $s:ident ) => {{
            let mut pos = 0;
            for obj in $iter {
                match obj {
                    Object::Ident(sym, _, _)
                    | Object::IdentFragment(sym, _, _, _) => {
                        assert_eq!($s.get(pos), Some(*sym));
                    }
                    _ => panic!("unexpected object"),
                }

                pos = pos + 1;
            }
        };};
    }

    macro_rules! add_syms {
        ($sut:ident, $base:expr, {$($dest:ident <- $name:ident: $kind:path,)*}) => {
            let mut i = 1;

            $(
                i += 1;

                let sym = Symbol::new_dummy(
                    SymbolIndex::from_u32(i),
                    stringify!($name)
                );

                $sut.declare(&sym, $kind, Source::default())?;
                let (_, _) = $sut.add_dep_lookup($base, &sym);

                $dest.push(sym);
            )*
        };
    }

    #[test]
    fn graph_sort() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let mut meta = vec![];
        let mut worksheet = vec![];
        let mut map = vec![];
        let mut retmap = vec![];

        let base = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym1");
        let base_node =
            sut.declare(&base, IdentKind::Map, Source::default())?;

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
    fn graph_sort_missing_node() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

        let sym_node = sut.declare(
            &sym,
            IdentKind::Tpl,
            Source {
                virtual_: true,
                ..Default::default()
            },
        )?;

        sut.set_fragment(sym_node, FragmentText::from("foo"))?;

        let (_, _) = sut.add_dep_lookup(&sym, &dep);

        match sut.sort(&vec![sym_node]) {
            Ok(_) => panic!("Unexpected success - dependency is not in graph"),
            Err(AsgError::UnexpectedNode(_)) => (),
            _ => {
                panic!("Incorrect error result when dependency is not in graph")
            }
        }

        Ok(())
    }

    #[test]
    fn graph_sort_no_roots() -> AsgResult<()> {
        let mut sut = Sut::with_capacity(0, 0);

        let sym = Symbol::new_dummy(SymbolIndex::from_u32(1), "sym");
        let dep = Symbol::new_dummy(SymbolIndex::from_u32(2), "dep");

        let (_, _) = sut.add_dep_lookup(&sym, &dep);

        let sections = sut.sort(&vec![])?;

        assert_eq!(Sections::new(), sections);

        Ok(())
    }
}
