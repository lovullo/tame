// Proof-of-concept TAME linker
//
//  Copyright (C) 2014-2019 Ryan Specialty Group, LLC.
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

//! **This is a poorly-written proof of concept; do not use!**  It has been
//! banished to its own file to try to make that more clear.

use fixedbitset::FixedBitSet;
use petgraph::graph::{DiGraph, EdgeIndex, Neighbors, NodeIndex};
use petgraph::visit::{DfsPostOrder, GraphBase, IntoNeighbors, Visitable};
use quick_xml::events::Event;
use quick_xml::Reader;
use std::collections::hash_map::{Entry, Iter};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::io::BufRead;
use std::ops::{Deref, Index};
use std::rc::Rc;

// The term "sym" is used throughout because it's easier to search for that
// in source code than "symbol", which is a generic term with many different
// meanings.

// if mutability is needed:
//#[derive(Debug)]
//struct SymRecord {
//    data: SymData,
//
//    // the idea is to keep the index encapsulated so that nothing else can
//    // ever hold a reference to it, ensuring that it's freed when the node
//    // is removed
//    index: Rc<RefCell<Option<NodeIndex>>>,
//}

#[derive(Debug)]
struct SymData {
    name: Rc<str>,
}

type DepGraphNode = SymEntry;
type DepGraphEdge = ();

struct DepGraph {
    graph: DiGraph<DepGraphNode, DepGraphEdge>,

    // serves as both a string internment system and graph indexer
    index: HashMap<Rc<str>, SymRef>,
    // if removals are permitted:
    //index: HashMap<Rc<str>, Weak<RefCell<Option<NodeIndex>>>>,
}

// This encapsulates the underlying Graph to enforce certain
// assumptions.  For example, we do not permit removing nodes because that
// would invalidate the NodeIndex reference in the index, which would then
// require workarounds like the commented-out code above and below.
//
// While Petgraph's use of indexes to represent graph and edge references
// makes it easy to bypass the borrow checker, it does just that---it's no
// different than a pointer reference (albeit guaranteed to safely reference
// a node rather than an arbitrary memory location) that can change out from
// under you at any moment.  As such, much of the planning that went into
// this was determining how to best mitigate that.
//
// The linker has certain needs that may differ as the compiler evolves, so
// it may be desirable to permit deletions in the future.  In the meantime,
// if a node needs to be deleted, we can simply remove all edges from it and
// possibly mark it in a way that states it was removed.
//
// This graph uses a separate map to serve a dual role: a string internment
// system and an indexer by symbol name.  This will have to evolve in the
// future as the graph ends up containing more stuff.
//
// This is currently called a dependency graph, since that's what we're
// using it for, but in the future the compiler will also use it as an IR,
// so this will likely be renamed.
impl DepGraph {
    fn new() -> Self {
        Self {
            // TODO: with_capacity
            graph: DiGraph::new(),
            index: HashMap::new(),
        }
    }

    fn declare(&mut self, name: &str) -> SymRef {
        match self.index.entry(name.into()) {
            Entry::Occupied(o) => *o.get(),
            Entry::Vacant(v) => {
                let entry = SymEntry::MissingSym {
                    name: Rc::clone(v.key()),
                };

                let index = SymRef(self.graph.add_node(entry));
                v.insert(index);

                index
            }
        }
    }

    // will not duplicate dependencies if they already exist
    fn declare_dep(&mut self, symbol: SymRef, dep: SymRef) -> () {
        self.graph.update_edge(*symbol, *dep, ());
    }

    fn lookup(&self, name: &str) -> Option<SymRef> {
        self.index.get(name.into()).map(|index| *index)
    }

    fn index_iter(&self) -> Iter<Rc<str>, SymRef> {
        self.index.iter()
    }

    // POC when removals were permitted:
    //fn add_symbol(&mut self, sym: SymData) -> NodeIndex {
    //    let name = Rc::clone(&sym.name);
    //    let record = SymRecord { data: sym, index: Rc::new(RefCell::new(None)) };
    //    let index = self.graph.add_node(record);

    //    let index = Rc::downgrade(&self.graph[index].index);
    //    self.graph[index].index.replace(Some(index));

    //    self.index.insert(name, index);
    //    index
    //}
}

impl GraphBase for DepGraph {
    type NodeId = NodeIndex;
    type EdgeId = EdgeIndex;
}

impl Visitable for DepGraph {
    type Map = FixedBitSet;

    fn visit_map(&self) -> Self::Map {
        self.graph.visit_map()
    }

    fn reset_map(&self, map: &mut Self::Map) {
        self.graph.reset_map(map)
    }
}

impl<'a> IntoNeighbors for &'a DepGraph {
    type Neighbors = Neighbors<'a, DepGraphEdge>;

    fn neighbors(self, n: Self::NodeId) -> Self::Neighbors {
        self.graph.neighbors(n)
    }
}

impl Index<SymRef> for DepGraph {
    type Output = DepGraphNode;

    fn index(&self, index: SymRef) -> &Self::Output {
        &self.graph[*index]
    }
}

// TODO: we may not to allow this; using SymRef could be a means to
// guarantee that a lookup has occurred and that it actually exists.  We
// don't need this if we set NodeId = SymRef in GraphBase, but that requires
// implementing other traits as well.
impl Index<NodeIndex> for DepGraph {
    type Output = DepGraphNode;

    fn index(&self, index: NodeIndex) -> &Self::Output {
        &self.graph[index]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
struct SymRef(NodeIndex);

impl From<SymRef> for NodeIndex {
    fn from(symref: SymRef) -> Self {
        *symref
    }
}

impl From<NodeIndex> for SymRef {
    fn from(index: NodeIndex) -> Self {
        Self(index)
    }
}

impl Deref for SymRef {
    type Target = NodeIndex;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq)]
enum SymEntry {
    MissingSym { name: Rc<str> },
}

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut pkgs_seen = HashSet::<String>::new();
    let mut fragments = HashMap::<String, String>::new();
    let mut depgraph = DepGraph::new();

    let package_path = std::env::args().nth(1).expect("Missing argument");
    let abs_path = fs::canonicalize(package_path).unwrap();

    println!("WARNING: This is proof-of-concept; do not use!");

    load_xmlo(
        &abs_path.to_str().unwrap().to_string(),
        &mut pkgs_seen,
        &mut fragments,
        &mut depgraph,
    )?;

    //    println!(
    //        "Graph {:?}",
    //        depgraph
    //            .graph
    //            .raw_nodes()
    //            .iter()
    //            .map(|node| &node.weight)
    //            .collect::<Vec<_>>()
    //    );

    let sorted = sort_deps(&depgraph);

    println!("Sorted ({}): {:?}", sorted.len(), sorted);

    Ok(())
}

fn load_xmlo<'a>(
    path_str: &'a str,
    pkgs_seen: &mut HashSet<String>,
    fragments: &mut HashMap<String, String>,
    depgraph: &mut DepGraph,
) -> Result<(), Box<dyn Error>> {
    let path = fs::canonicalize(path_str)?;
    let path_str = path.to_str().unwrap();

    if !pkgs_seen.insert(path_str.to_string()) {
        return Ok(());
    }

    println!("processing {}", path_str);

    let mut found = HashSet::<String>::new();

    match Reader::from_file(&path) {
        Ok(mut reader) => loop {
            let mut buf = Vec::new();

            // we know that the XML produced by Saxon is valid
            reader.check_end_names(false);

            match reader.read_event(&mut buf) {
                Ok(Event::Start(ele)) | Ok(Event::Empty(ele)) => {
                    let mut attrs = ele.attributes();
                    let mut filtered =
                        attrs.with_checks(false).filter_map(Result::ok);

                    match ele.name() {
                        b"preproc:sym-dep" => filtered
                            .find(|attr| attr.key == b"name")
                            .map(|attr| attr.value)
                            .and_then(|mut name| {
                                read_deps(&mut reader, depgraph, name.to_mut())
                            })
                            .ok_or("Missing name"),

                        b"preproc:sym" => {
                            filtered
                                .find(|attr| attr.key == b"src")
                                .map(|attr| attr.value.to_owned())
                                .and_then(|src| {
                                    let path_str =
                                        std::str::from_utf8(&src).unwrap();

                                    found.insert(path_str.to_string());
                                    Some(())
                                });
                            Ok(())
                        }

                        b"preproc:fragment" => filtered
                            .find(|attr| attr.key == b"id")
                            .map(|attr| String::from_utf8(attr.value.to_vec()))
                            .and_then(|id| {
                                let fragment = reader
                                    .read_text(ele.name(), &mut Vec::new())
                                    .unwrap_or("".to_string());

                                fragments.insert(id.unwrap(), fragment);
                                Some(())
                            })
                            .ok_or("Missing fragment id"),
                        _ => Ok(()),
                    }
                }
                Ok(Event::End(ele)) => {
                    match ele.name() {
                        // We don't need to read any further than the end of
                        // the fragments (symtable, sym-deps, fragments)
                        b"preproc:fragments" => break (),
                        _ => Ok(()),
                    }
                }
                Ok(Event::Eof) => break (),
                Err(e) => {
                    panic!("Error at {}: {:?}", reader.buffer_position(), e);
                }
                _ => Ok(()),
            }
            .unwrap_or_else(|r| panic!("Parse error: {:?}", r));

            buf.clear();
        },
        Err(e) => panic!("Error {:?}", e),
    }

    let mut dir = path.clone();
    dir.pop();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        path_buf.push(relpath);
        path_buf.set_extension("xmlo");

        //println!("Trying {:?}", path_buf);
        let path_abs = path_buf.canonicalize().unwrap();
        let path = path_abs.to_str().unwrap();

        load_xmlo(path, pkgs_seen, fragments, depgraph)?;
    }

    Ok(())
}

fn read_deps<B>(
    reader: &mut Reader<B>,
    depgraph: &mut DepGraph,
    name: &[u8],
) -> Option<()>
where
    B: BufRead,
{
    // TODO: API needs to expose whether a symbol is already known so that
    // we can warn on them
    // note: using from_utf8_unchecked here did _not_ improve performance
    let sym_node = depgraph.declare(std::str::from_utf8(name).unwrap());

    //println!("processing deps for {}", sym_name);

    loop {
        match reader.read_event(&mut Vec::new()) {
            Ok(Event::Start(ele)) | Ok(Event::Empty(ele)) => {
                let mut attrs = ele.attributes();
                let mut filtered =
                    attrs.with_checks(false).filter_map(Result::ok);

                filtered.find(|attr| attr.key == b"name").and_then(
                    |mut attr| {
                        let name = attr.value.to_mut();
                        let str = std::str::from_utf8(name).unwrap();

                        let dep_node = depgraph.declare(&str);
                        depgraph.declare_dep(sym_node, dep_node);

                        Some(())
                    },
                );

                //println!("{:?}", ele.attributes().collect::<Vec<_>>());
            }

            Ok(Event::Eof) | Ok(Event::End(_)) => break Some(()),

            Err(e) => {
                panic!("Error at {}: {:?}", reader.buffer_position(), e);
            }

            _ => (),
        }
    }
}

fn sort_deps(depgraph: &DepGraph) -> Vec<&SymEntry> {
    // @type=meta, @preproc:elig-class-yields
    // @type={ret}map{,:head,:tail}

    let roots = discover_roots(depgraph);

    // This is technically a topological sort, but functions have
    // cycles.  Once we have more symbol metadata, we can filter them out
    // and actually invoke toposort.
    let mut dfs = DfsPostOrder::empty(&depgraph);
    let mut sorted = Vec::new();

    // TODO: we'll be processing various roots separately
    for index in roots {
        dfs.stack.push(*index);
    }

    while let Some(index) = dfs.next(&depgraph) {
        sorted.push(&depgraph[index]);
    }

    sorted
}

fn discover_roots(depgraph: &DepGraph) -> Vec<SymRef> {
    // TODO: filter_map
    let mut map_syms = depgraph
        .index_iter()
        .filter(|(key, _)| {
            key.starts_with(":map:") || key.starts_with(":retmap:")
        })
        .map(|(_, value)| *value)
        .collect::<Vec<_>>();

    let mut roots = vec!["___yield", "___worksheet"]
        .iter()
        .filter_map(|sym| depgraph.lookup(sym))
        .collect::<Vec<_>>();

    roots.append(&mut map_syms);

    //println!(
    //    "found roots: {:?}",
    //    roots
    //        .iter()
    //        .map(|index| &depgraph.graph[*index])
    //        .collect::<Vec<_>>()
    //);

    roots
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
