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
use quick_xml::events::{BytesEnd, BytesStart, BytesText, Event};
use quick_xml::Writer;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};

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

    //println!("Sorted ({}): {:?}", sorted.len(), sorted);

    output_xmle(&depgraph, &interner, sorted)?;

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

                // Maps should not pull in symbols since we may end up
                // mapping to params that are never actually used
                if !sym.starts_with(":map:") {
                    for dep_sym in deps {
                        let dep_node =
                            depgraph.lookup(dep_sym).expect(&format!(
                                "missing dep sym for deps: `{}` -> `{}`",
                                sym, dep_sym
                            ));

                        depgraph.add_dep(sym_node, dep_node);
                    }
                }
            }

            Ok(XmloEvent::SymDecl(sym, attrs)) => {
                if let Some(sym_src) = attrs.src {
                    found.insert(sym_src);
                }

                let owned = attrs.src.is_none();

                let kind = (&attrs).try_into().map_err(|err| {
                    format!("sym `{}` attrs error: {}", sym, err)
                });

                let src = attrs.into();

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

type ObjectVec<'a, 'i> = Vec<&'a Object<'i>>;

// Note that the classifier has nothing in it anymore; it's only there for
// API compability, so we don't include it here.
#[derive(Default)]
struct SortedDeps<'a, 'i> {
    map: ObjectVec<'a, 'i>,
    retmap: ObjectVec<'a, 'i>,
    meta: ObjectVec<'a, 'i>,
    worksheet: ObjectVec<'a, 'i>,
    params: ObjectVec<'a, 'i>,
    types: ObjectVec<'a, 'i>,
    funcs: ObjectVec<'a, 'i>,
    rater: ObjectVec<'a, 'i>,
}

fn sort_deps<'a, 'i>(
    depgraph: &'a LinkerAsg<'i>,
    roots: &Vec<LinkerObjectRef>,
) -> SortedDeps<'a, 'i> {
    // @type=meta, @preproc:elig-class-yields
    // @type={ret}map{,:head,:tail}

    let mut deps: SortedDeps = Default::default();

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
                IdentKind::Meta => deps.meta.push(ident),
                IdentKind::Worksheet => deps.worksheet.push(ident),
                IdentKind::Param(_, _) => deps.params.push(ident),
                IdentKind::Type(_) => deps.types.push(ident),
                IdentKind::Func(_, _) => deps.funcs.push(ident),
                IdentKind::MapHead | IdentKind::Map | IdentKind::MapTail => {
                    deps.map.push(ident)
                }
                IdentKind::RetMapHead
                | IdentKind::RetMap
                | IdentKind::RetMapTail => deps.retmap.push(ident),
                _ => deps.rater.push(ident),
            },
            _ => panic!("unexpected node: {:?}", ident),
        }
    }

    deps
}

fn output_xmle<'a, 'i, I: Interner<'i>>(
    depgraph: &'a LinkerAsg<'i>,
    interner: &'i I,
    sorted: SortedDeps<'a, 'i>,
) -> Result<(), Box<dyn Error>> {
    use std::io::Cursor;

    let mut writer =
        Writer::new_with_indent(Cursor::new(Vec::new()), ' ' as u8, 2);

    let root =
        BytesStart::owned_name(b"package".to_vec()).with_attributes(vec![
            ("xmlns", "http://www.lovullo.com/rater"),
            ("xmlns:preproc", "http://www.lovullo.com/rater/preproc"),
            ("xmlns:l", "http://www.lovullo.com/rater/linker"),
            ("title", "Title TODO"), // TODO
            ("program", "true"),
            ("name", "name/todo"), // TODO
        ]);

    writer.write_event(Event::Start(root))?;

    // All of the other namespaces output in the existing xmle files are
    // unneeded.
    writer.write_event(Event::Start(BytesStart::borrowed_name(b"l:dep")))?;

    let all = sorted
        .meta
        .iter()
        .chain(sorted.map.iter())
        .chain(sorted.retmap.iter())
        .chain(sorted.worksheet.iter())
        .chain(sorted.params.iter())
        .chain(sorted.types.iter())
        .chain(sorted.funcs.iter())
        .chain(sorted.rater.iter());

    for ident in all {
        // TODO: we're doing this in two places!
        match ident {
            Object::Ident(sym, kind, src)
            | Object::IdentFragment(sym, kind, src, _) => {
                let name: &str = sym;

                // this'll be formalized more sanely
                let mut attrs = match kind {
                    IdentKind::Cgen(dim) => {
                        vec![("type", "cgen"), ("dim", dim.as_ref())]
                    }
                    IdentKind::Class(dim) => {
                        vec![("type", "class"), ("dim", dim.as_ref())]
                    }
                    IdentKind::Const(dim, dtype) => vec![
                        ("type", "const"),
                        ("dim", dim.as_ref()),
                        ("dtype", dtype.as_ref()),
                    ],
                    IdentKind::Func(dim, dtype) => vec![
                        ("type", "func"),
                        ("dim", dim.as_ref()),
                        ("dtype", dtype.as_ref()),
                    ],
                    IdentKind::Gen(dim, dtype) => vec![
                        ("type", "gen"),
                        ("dim", dim.as_ref()),
                        ("dtype", dtype.as_ref()),
                    ],
                    IdentKind::Lparam(dim, dtype) => vec![
                        ("type", "lparam"),
                        ("dim", dim.as_ref()),
                        ("dtype", dtype.as_ref()),
                    ],
                    IdentKind::Param(dim, dtype) => vec![
                        ("type", "param"),
                        ("dim", dim.as_ref()),
                        ("dtype", dtype.as_ref()),
                    ],
                    IdentKind::Rate(dtype) => {
                        vec![("type", "rate"), ("dtype", dtype.as_ref())]
                    }
                    IdentKind::Tpl => vec![("type", "tpl")],
                    IdentKind::Type(dtype) => {
                        vec![("type", "type"), ("dtype", dtype.as_ref())]
                    }
                    IdentKind::MapHead => vec![("type", "map:head")],
                    IdentKind::Map => vec![("type", "map")],
                    IdentKind::MapTail => vec![("type", "map:tail")],
                    IdentKind::RetMapHead => vec![("type", "retmap:head")],
                    IdentKind::RetMap => vec![("type", "retmap")],
                    IdentKind::RetMapTail => vec![("type", "retmap:tail")],
                    IdentKind::Meta => vec![("type", "meta")],
                    IdentKind::Worksheet => vec![("type", "worksheet")],
                };

                attrs.push(("name", name));

                if src.generated {
                    attrs.push(("preproc:generated", "true"));
                }

                if let Some(parent) = src.parent {
                    attrs.push(("parent", parent));
                }
                if let Some(yields) = src.yields {
                    attrs.push(("yields", yields));
                }
                if let Some(desc) = &src.desc {
                    attrs.push(("desc", &desc));
                }

                let sym = BytesStart::owned_name(b"preproc:sym".to_vec())
                    .with_attributes(attrs);

                writer.write_event(Event::Empty(sym))?;
            }
            _ => unreachable!("filtered out during sorting"),
        }
    }

    writer.write_event(Event::End(BytesEnd::borrowed(b"l:dep")))?;

    // This was not in the original linker, but we need to be able to convey
    // this information for `standalones` (which has received some logic
    // from the old linker for the time being).
    writer
        .write_event(Event::Start(BytesStart::borrowed_name(b"l:map-from")))?;

    let mut map_froms = HashSet::<&str>::new();

    for map_ident in &sorted.map {
        match map_ident {
            Object::Ident(_, _, src) | Object::IdentFragment(_, _, src, _) => {
                if let Some(froms) = &src.from {
                    for from in froms {
                        map_froms.insert(from);
                    }
                }
            }

            _ => unreachable!("filtered out during sorting"),
        }
    }

    for from in map_froms {
        let name: &str = from;

        writer.write_event(Event::Empty(
            BytesStart::borrowed_name(b"l:from")
                .with_attributes(vec![("name", name)]),
        ))?;
    }

    writer.write_event(Event::End(BytesEnd::borrowed(b"l:map-from")))?;
    writer
        .write_event(Event::Start(BytesStart::borrowed_name(b"l:map-exec")))?;

    if sorted.map.len() > 0 {
        write_fragments(
            &mut writer,
            &vec![depgraph
                .get(depgraph.lookup(interner.intern(":map:___head")).unwrap())
                .unwrap()],
        )?;
        write_fragments(&mut writer, &sorted.map)?;
        write_fragments(
            &mut writer,
            &vec![depgraph
                .get(depgraph.lookup(interner.intern(":map:___tail")).unwrap())
                .unwrap()],
        )?;
    }

    writer.write_event(Event::End(BytesEnd::borrowed(b"l:map-exec")))?;
    writer.write_event(Event::Start(BytesStart::borrowed_name(
        b"l:retmap-exec",
    )))?;

    if sorted.retmap.len() > 0 {
        write_fragments(
            &mut writer,
            &vec![depgraph
                .get(
                    depgraph
                        .lookup(interner.intern(":retmap:___head"))
                        .unwrap(),
                )
                .unwrap()],
        )?;
        write_fragments(&mut writer, &sorted.retmap)?;
        write_fragments(
            &mut writer,
            &vec![depgraph
                .get(
                    depgraph
                        .lookup(interner.intern(":retmap:___tail"))
                        .unwrap(),
                )
                .unwrap()],
        )?;
    }

    writer.write_event(Event::End(BytesEnd::borrowed(b"l:retmap-exec")))?;
    writer.write_event(Event::Start(BytesStart::borrowed_name(b"l:exec")))?;

    write_fragments(&mut writer, &sorted.meta)?;
    write_fragments(&mut writer, &sorted.worksheet)?;
    write_fragments(&mut writer, &sorted.params)?;
    write_fragments(&mut writer, &sorted.types)?;
    write_fragments(&mut writer, &sorted.funcs)?;
    write_fragments(&mut writer, &sorted.rater)?;

    writer.write_event(Event::End(BytesEnd::borrowed(b"l:exec")))?;
    writer.write_event(Event::End(BytesEnd::borrowed(b"package")))?;

    print!("{}", String::from_utf8(writer.into_inner().into_inner())?);

    Ok(())
}

fn write_fragments<W: Write>(
    writer: &mut Writer<W>,
    idents: &ObjectVec,
) -> Result<(), Box<dyn Error>> {
    for ident in idents {
        match ident {
            Object::IdentFragment(_, _, _, frag) => {
                writer.write_event(Event::Text(BytesText::from_plain_str(
                    frag,
                )))?;
            }
            _ => (),
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
