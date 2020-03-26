// Proof-of-concept TAME linker
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

//! **This is a poorly-written proof of concept; do not use!**  It has been
//! banished to its own file to try to make that more clear.

use crate::global;
use crate::ir::asg::{
    Asg, AsgError, DefaultAsg, IdentKind, IdentObject, IdentObjectData,
    ObjectRef, Sections, SortableAsg, Source,
};
use crate::obj::xmle::writer::XmleWriter;
use crate::obj::xmlo::reader::{XmloError, XmloEvent, XmloReader};
use crate::sym::{DefaultInterner, Interner, Symbol};
use fxhash::{FxHashMap, FxHashSet};
use std::convert::TryInto;
use std::error::Error;
use std::fs;
use std::io::BufReader;

type LinkerAsg<'i> = DefaultAsg<'i, IdentObject<'i>, global::ProgIdentSize>;
type LinkerObjectRef = ObjectRef<global::ProgIdentSize>;

type LoadResult<'i> =
    Result<Option<(Option<&'i Symbol<'i>>, Option<String>)>, Box<dyn Error>>;

pub fn main(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut pkgs_seen: FxHashSet<String> = Default::default();
    let mut fragments: FxHashMap<&str, String> = Default::default();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let mut roots = Vec::new();
    let interner = DefaultInterner::new();

    let abs_path = fs::canonicalize(package_path)?;

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

    let mut sorted = match depgraph.sort(&roots) {
        Ok(sections) => sections,
        Err(AsgError::Cycles(cycles)) => {
            let msg: Vec<String> = cycles
                .into_iter()
                .map(|cycle| {
                    let mut path: Vec<String> = cycle
                        .into_iter()
                        .map(|obj| {
                            format!(
                                "{}",
                                depgraph.get(obj).unwrap().name().unwrap()
                            )
                        })
                        .collect();

                    path.reverse();
                    path.push(path[0].clone());
                    format!("cycle: {}", path.join(" -> "))
                })
                .collect();

            return Err(msg.join("\n").into());
        }
        Err(e) => return Err(e.into()),
    };

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
) -> LoadResult<'i> {
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
                } else {
                    let owned = attrs.src.is_none();
                    let extern_ = attrs.extern_;

                    let kind = (&attrs).try_into().map_err(|err| {
                        format!("sym `{}` attrs error: {}", sym, err)
                    });

                    let mut src: Source = attrs.into();

                    // Existing convention is to omit @src of local package
                    // (in this case, the program being linked)
                    if first {
                        src.pkg_name = None;
                    }

                    match kind {
                        Ok(kindval) => {
                            // TODO: inefficient
                            let link_root = owned
                                && (kindval == IdentKind::Meta
                                    || kindval == IdentKind::Map
                                    || kindval == IdentKind::RetMap);

                            if extern_ {
                                depgraph.declare_extern(sym, kindval, src)?;
                            } else {
                                let node = depgraph.declare(
                                    sym,
                                    kindval,
                                    Some(src),
                                )?;

                                if link_root {
                                    roots.push(node);
                                }
                            }
                        }
                        Err(e) => return Err(e.into()),
                    };
                }
            }

            Ok(XmloEvent::Fragment(sym, text)) => {
                match depgraph.lookup(sym) {
                    Some(frag) => match depgraph.set_fragment(frag, text) {
                        Ok(_) => (),
                        Err(e) => return Err(e.into()),
                    },
                    None => {
                        return Err(XmloError::MissingFragment(String::from(
                            "missing fragment",
                        ))
                        .into());
                    }
                };
            }

            // We don't need to read any further than the end of the
            // header (symtable, sym-deps, fragments)
            Ok(XmloEvent::Eoh) => break,

            Err(e) => return Err(e.into()),
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
        let path_abs = path_buf.canonicalize()?;
        let path = path_abs.to_str().unwrap();

        load_xmlo(path, pkgs_seen, fragments, depgraph, interner, roots)?;
    }

    if first {
        Ok(Some((name, relroot)))
    } else {
        Ok(None)
    }
}

fn get_ident<'a, 'i>(
    depgraph: &'a LinkerAsg<'i>,
    name: &'i Symbol<'i>,
) -> Result<&'a IdentObject<'i>, XmloError> {
    depgraph
        .lookup(name)
        .and_then(|id| depgraph.get(id))
        .ok_or(XmloError::MissingFragment(String::from(name as &str)))
}

fn output_xmle<'a, 'i, I: Interner<'i>>(
    depgraph: &'a LinkerAsg<'i>,
    interner: &'i I,
    sorted: &mut Sections<'a, IdentObject<'i>>,
    name: &'i Symbol<'i>,
    relroot: String,
    output: &str,
) -> Result<(), Box<dyn Error>> {
    if !sorted.map.is_empty() {
        sorted
            .map
            .push_head(get_ident(depgraph, interner.intern(":map:___head"))?);
        sorted
            .map
            .push_tail(get_ident(depgraph, interner.intern(":map:___tail"))?);
    }

    if !sorted.retmap.is_empty() {
        sorted.retmap.push_head(get_ident(
            depgraph,
            interner.intern(":retmap:___head"),
        )?);
        sorted.retmap.push_tail(get_ident(
            depgraph,
            interner.intern(":retmap:___tail"),
        )?);
    }

    let file = fs::File::create(output)?;
    let mut xmle_writer = XmleWriter::new(file);
    xmle_writer.write(&sorted, name, &relroot)?;

    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
