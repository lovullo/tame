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

use crate::global;
use crate::ir::asg::IdentKind;
use crate::ir::asg::{Asg, DefaultAsg, Object, ObjectRef};
use crate::obj::xmlo::reader::{XmloError, XmloEvent, XmloReader};
use crate::sym::{DefaultInterner, Interner};
use petgraph::visit::DfsPostOrder;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::error::Error;
use std::fs;
use std::io::BufReader;

type LinkerAsg<'i> = DefaultAsg<'i, global::ProgIdentSize>;
type LinkerObjectRef = ObjectRef<global::ProgIdentSize>;

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut pkgs_seen = HashSet::<String>::new();
    let mut fragments = HashMap::<&str, String>::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let mut roots = Vec::new();
    let interner = DefaultInterner::new();

    let package_path = std::env::args().nth(1).expect("Missing argument");
    let abs_path = fs::canonicalize(package_path).unwrap();

    println!("WARNING: This is proof-of-concept; do not use!");

    load_xmlo(
        &abs_path.to_str().unwrap().to_string(),
        &mut pkgs_seen,
        &mut fragments,
        &mut depgraph,
        &interner,
        &mut roots,
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

    roots.extend(
        vec!["___yield", "___worksheet"]
            .iter()
            .map(|name| interner.intern(name))
            .filter_map(|sym| depgraph.lookup(sym)),
    );

    let sorted = sort_deps(&depgraph, &roots);

    println!("Sorted ({}): {:?}", sorted.len(), sorted);

    Ok(())
}

fn load_xmlo<'a, 'i, I: Interner<'i>>(
    path_str: &'a str,
    pkgs_seen: &mut HashSet<String>,
    fragments: &mut HashMap<&'i str, String>,
    depgraph: &mut LinkerAsg<'i>,
    interner: &'i I,
    roots: &mut Vec<LinkerObjectRef>,
) -> Result<(), Box<dyn Error>> {
    let path = fs::canonicalize(path_str)?;
    let path_str = path.to_str().unwrap();

    if !pkgs_seen.insert(path_str.to_string()) {
        return Ok(());
    }

    //println!("processing {}", path_str);

    let mut found = HashSet::<&str>::new();

    let file = fs::File::open(&path)?;
    let reader = BufReader::new(file);
    let mut xmlo = XmloReader::new(reader, interner);
    let mut elig = None;

    loop {
        match xmlo.read_event() {
            Ok(XmloEvent::Package(attrs)) => {
                elig = attrs.elig;
            }

            Ok(XmloEvent::SymDeps(sym, deps)) => {
                // TODO: API needs to expose whether a symbol is already
                // known so that we can warn on them
                //
                // note: using from_utf8_unchecked here did _not_ improve
                // performance
                let sym_node = depgraph
                    .lookup(sym)
                    .expect(&format!("missing sym for deps: `{}`", sym));

                for dep_sym in deps {
                    let dep_node = depgraph.lookup(dep_sym).expect(&format!(
                        "missing dep sym for deps: `{}` -> `{}`",
                        sym, dep_sym
                    ));

                    depgraph.add_dep(sym_node, dep_node);
                }
            }

            Ok(XmloEvent::SymDecl(sym, attrs)) => {
                if let Some(sym_src) = attrs.src {
                    found.insert(sym_src);
                }

                let owned = attrs.src.is_none();

                let kind = attrs.try_into().map_err(|err| {
                    format!("sym `{}` attrs error: {}", sym, err)
                });

                // TODO: should probably track these down in the XSLT linker...
                match kind {
                    Ok(kindval) => {
                        // TODO: inefficient
                        let link_root = owned
                            && (kindval == IdentKind::Meta
                                || sym.starts_with(":map:")
                                || sym.starts_with(":retmap:"));

                        let node = depgraph.declare(sym, kindval)?;

                        if link_root {
                            roots.push(node);
                        }
                    }
                    Err(e) => println!("{:?}; skipping...", e),
                };
            }

            Ok(XmloEvent::Fragment(sym, text)) => {
                let result = depgraph.set_fragment(
                    depgraph.lookup(sym).expect(&format!(
                        "missing symbol for fragment: {}",
                        sym
                    )),
                    text,
                );

                match result {
                    Ok(_) => (),
                    Err(e) => println!("{:?}; skipping...", e),
                };
            }

            // We don't need to read any further than the end of the
            // header (symtable, sym-deps, fragments)
            Ok(XmloEvent::Eoh) => break,

            Err(err @ XmloError::UnassociatedFragment) => {
                println!("{:?}; skipping...", err);
            }

            err @ Err(_) => err.map(|_| ())?,
        }
    }

    if let Some(elig_sym) = elig {
        roots.push(depgraph.lookup(elig_sym).expect(
            "internal error: package elig references nonexistant symbol",
        ));
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

        load_xmlo(path, pkgs_seen, fragments, depgraph, interner, roots)?;
    }

    Ok(())
}

fn sort_deps<'a, 'i>(
    depgraph: &'a LinkerAsg<'i>,
    roots: &Vec<LinkerObjectRef>,
) -> Vec<&'a Object<'i>> {
    // @type=meta, @preproc:elig-class-yields
    // @type={ret}map{,:head,:tail}

    // This is technically a topological sort, but functions have
    // cycles.  Once we have more symbol metadata, we can filter them out
    // and actually invoke toposort.
    let mut dfs = DfsPostOrder::empty(&depgraph);
    let mut sorted = Vec::new();

    //println!("discovered roots: {:?}", roots);

    // TODO: we'll be processing various roots separately
    for index in roots {
        dfs.stack.push((*index).into());
    }

    // TODO: can we encapsulate NodeIndex?
    while let Some(index) = dfs.next(&depgraph) {
        sorted.push(depgraph.get(index).unwrap());
    }

    sorted
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
