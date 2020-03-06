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
use crate::ir::asg::{Asg, DefaultAsg, Object, ObjectRef, Source};
use crate::obj::xmle::writer::{Sections, XmleWriter};
use crate::obj::xmlo::reader::{XmloError, XmloEvent, XmloReader};
use crate::sym::{DefaultInterner, Interner, Symbol};
use fxhash::{FxHashMap, FxHashSet};
use petgraph::visit::DfsPostOrder;
use std::convert::TryInto;
use std::error::Error;
use std::fs;
use std::io::BufReader;

type LinkerAsg<'i> = DefaultAsg<'i, global::ProgIdentSize>;
type LinkerObjectRef = ObjectRef<global::ProgIdentSize>;

pub fn main(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut pkgs_seen: FxHashSet<String> = Default::default();
    let mut fragments: FxHashMap<&str, String> = Default::default();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let mut roots = Vec::new();
    let interner = DefaultInterner::new();

    let abs_path = fs::canonicalize(package_path).unwrap();

    println!("WARNING: This is proof-of-concept; do not use!");

    let (name, relroot) = load_xmlo(
        &abs_path.to_str().unwrap().to_string(),
        &mut pkgs_seen,
        &mut fragments,
        &mut depgraph,
        &interner,
        &mut roots,
    )?
    .expect("missing root package information");

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

    let mut sorted = sort_deps(&depgraph, &roots);

    //println!("Sorted ({}): {:?}", sorted.len(), sorted);

    output_xmle(
        &depgraph,
        &interner,
        &mut sorted,
        name.expect("missing root package name"),
        relroot.expect("missing root package relroot"),
        output,
    )?;

    Ok(())
}

fn load_xmlo<'a, 'i, I: Interner<'i>>(
    path_str: &'a str,
    pkgs_seen: &mut FxHashSet<String>,
    fragments: &mut FxHashMap<&'i str, String>,
    depgraph: &mut LinkerAsg<'i>,
    interner: &'i I,
    roots: &mut Vec<LinkerObjectRef>,
) -> Result<Option<(Option<&'i Symbol<'i>>, Option<String>)>, Box<dyn Error>> {
    let path = fs::canonicalize(path_str)?;
    let path_str = path.to_str().unwrap();

    let first = pkgs_seen.len() == 0;

    if !pkgs_seen.insert(path_str.to_string()) {
        return Ok(None);
    }

    //println!("processing {}", path_str);

    let mut found: FxHashSet<&str> = Default::default();

    let file = fs::File::open(&path)?;
    let reader = BufReader::new(file);
    let mut xmlo = XmloReader::new(reader, interner);
    let mut elig = None;

    let mut name: Option<&'i Symbol<'i>> = None;
    let mut relroot: Option<String> = None;

    loop {
        match xmlo.read_event() {
            Ok(XmloEvent::Package(attrs)) => {
                if first {
                    name = attrs.name;
                    relroot = attrs.relroot;
                }
                elig = attrs.elig;
            }

            Ok(XmloEvent::SymDeps(sym, deps)) => {
                // TODO: API needs to expose whether a symbol is already
                // known so that we can warn on them

                // Maps should not pull in symbols since we may end up
                // mapping to params that are never actually used
                if !sym.starts_with(":map:") {
                    for dep_sym in deps {
                        depgraph.add_dep_lookup(sym, dep_sym);
                    }
                }
            }

            Ok(XmloEvent::SymDecl(sym, attrs)) => {
                if let Some(sym_src) = attrs.src {
                    found.insert(sym_src);
                } else if attrs.extern_ {
                    // TODO: externs (they're implicitly handled, without
                    // checks, by Missing)
                    // depgraph.declare_extern(sym, kind);
                } else {
                    let owned = attrs.src.is_none();

                    let kind = (&attrs).try_into().map_err(|err| {
                        format!("sym `{}` attrs error: {}", sym, err)
                    });

                    let mut src: Source = attrs.into();

                    // Existing convention is to omit @src of local package
                    // (in this case, the program being linked)
                    if first {
                        src.pkg_name = None;
                    }

                    // TODO: should probably track these down in the XSLT linker...
                    match kind {
                        Ok(kindval) => {
                            // TODO: inefficient
                            let link_root = owned
                                && (kindval == IdentKind::Meta
                                    || kindval == IdentKind::Map
                                    || kindval == IdentKind::RetMap);

                            let node = depgraph.declare(sym, kindval, src)?;

                            if link_root {
                                roots.push(node);
                            }
                        }
                        Err(e) => println!("{:?}; skipping...", e),
                    };
                }
            }

            Ok(XmloEvent::Fragment(sym, text)) => {
                let result = depgraph.set_fragment(
                    depgraph.lookup(sym).unwrap_or_else(|| {
                        panic!("missing symbol for fragment: {}", sym)
                    }),
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

        // println!("Trying {:?}", path_buf);
        let path_abs = path_buf.canonicalize().unwrap();
        let path = path_abs.to_str().unwrap();

        load_xmlo(path, pkgs_seen, fragments, depgraph, interner, roots)?;
    }

    if first {
        Ok(Some((name, relroot)))
    } else {
        Ok(None)
    }
}

fn sort_deps<'a, 'i>(
    depgraph: &'a LinkerAsg<'i>,
    roots: &Vec<LinkerObjectRef>,
) -> Sections<'a, 'i> {
    // @type=meta, @preproc:elig-class-yields
    // @type={ret}map{,:head,:tail}

    let mut deps: Sections = Sections::new();

    // This is technically a topological sort, but functions have
    // cycles.  Once we have more symbol metadata, we can filter them out
    // and actually invoke toposort.
    let mut dfs = DfsPostOrder::empty(&depgraph);

    //println!("discovered roots: {:?}", roots);

    // TODO: we'll be processing various roots separately
    for index in roots {
        dfs.stack.push((*index).into());
    }

    // TODO: can we encapsulate NodeIndex?
    while let Some(index) = dfs.next(&depgraph) {
        let ident = depgraph.get(index).unwrap();

        match ident {
            Object::Ident(_, kind, _)
            | Object::IdentFragment(_, kind, _, _) => match kind {
                IdentKind::Meta => deps.meta.push_body(ident),
                IdentKind::Worksheet => deps.worksheet.push_body(ident),
                IdentKind::Param(_, _) => deps.params.push_body(ident),
                IdentKind::Type(_) => deps.types.push_body(ident),
                IdentKind::Func(_, _) => deps.funcs.push_body(ident),
                IdentKind::MapHead | IdentKind::Map | IdentKind::MapTail => {
                    deps.map.push_body(ident)
                }
                IdentKind::RetMapHead
                | IdentKind::RetMap
                | IdentKind::RetMapTail => deps.retmap.push_body(ident),
                _ => deps.rater.push_body(ident),
            },
            _ => panic!("unexpected node: {:?}", ident),
        }
    }

    deps
}

fn get_interner_value<'a, 'i, I: Interner<'i>>(
    depgraph: &'a LinkerAsg<'i>,
    interner: &'i I,
    name: &str,
) -> &'a Object<'i> {
    depgraph
        .get(
            depgraph
                .lookup(interner.intern(name))
                .unwrap_or_else(|| panic!("missing identifier: {}", name)),
        )
        .expect("Could not get interner value")
}

fn output_xmle<'a, 'i, I: Interner<'i>>(
    depgraph: &'a LinkerAsg<'i>,
    interner: &'i I,
    sorted: &mut Sections<'a, 'i>,
    name: &'i Symbol<'i>,
    relroot: String,
    output: &str,
) -> Result<(), Box<dyn Error>> {
    if !sorted.map.is_empty() {
        sorted.map.push_head(get_interner_value(
            depgraph,
            interner,
            &String::from(":map:___head"),
        ));
        sorted.map.push_tail(get_interner_value(
            depgraph,
            interner,
            &String::from(":map:___tail"),
        ));
    }

    if !sorted.retmap.is_empty() {
        sorted.retmap.push_head(get_interner_value(
            depgraph,
            interner,
            &String::from(":retmap:___head"),
        ));
        sorted.retmap.push_tail(get_interner_value(
            depgraph,
            interner,
            &String::from(":retmap:___tail"),
        ));
    }

    let file = fs::File::create(output)?;
    let mut xmle_writer = XmleWriter::new(file);
    xmle_writer
        .write(&sorted, name, &relroot)
        .expect("Could not write xmle output");

    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}