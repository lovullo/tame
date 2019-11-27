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

use quick_xml::events::Event;
use quick_xml::Reader;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::io::BufRead;

type SymRef = String;
type DepMap<T = SymRef> = HashMap<T, Vec<T>>;

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut pkgs_seen = HashSet::<String>::new();
    let mut deps: DepMap = HashMap::new();
    let mut fragments = HashMap::<String, String>::new();

    let package_path = std::env::args().nth(1).expect("Missing argument");
    let abs_path = fs::canonicalize(package_path).unwrap();

    println!("WARNING: This is proof-of-concept; do not use!");

    load_xmlo(
        &abs_path.to_str().unwrap().to_string(),
        &mut pkgs_seen,
        &mut deps,
        &mut fragments,
    )?;

    let sorted = sort_deps(deps)?;

    println!("Sorted ({}): {:?}", sorted.len(), sorted);

    Ok(())
}

fn load_xmlo<'a>(
    path_str: &'a str,
    pkgs_seen: &mut HashSet<String>,
    deps: &mut DepMap,
    fragments: &mut HashMap<String, String>,
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
                                read_deps(&mut reader, deps, name.to_mut())
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
                            .map(|attr| {
                                String::from_utf8(
                                    attr.value.to_owned().to_vec(),
                                )
                            })
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

        load_xmlo(path, pkgs_seen, deps, fragments)?;
    }

    Ok(())
}

fn read_deps<B>(
    reader: &mut Reader<B>,
    deps: &mut HashMap<String, Vec<String>>,
    name: &[u8],
) -> Option<()>
where
    B: BufRead,
{
    let sym_name = String::from_utf8(name.to_vec()).unwrap();
    let mut sym_deps = Vec::<String>::new();

    //println!("processing deps for {}", sym_name);

    loop {
        match reader.read_event(&mut Vec::new()) {
            Ok(Event::Start(ele)) | Ok(Event::Empty(ele)) => {
                let mut attrs = ele.attributes();
                let mut filtered =
                    attrs.with_checks(false).filter_map(Result::ok);

                filtered
                    .find(|attr| attr.key == b"name")
                    .map(|attr| attr.value.to_owned())
                    .and_then(|name| {
                        let str = String::from_utf8(name.to_vec()).unwrap();
                        sym_deps.push(str);
                        Some(())
                    });

                //println!("{:?}", ele.attributes().collect::<Vec<_>>());
            }

            Ok(Event::Eof) | Ok(Event::End(_)) => break Some(()),

            Err(e) => {
                panic!("Error at {}: {:?}", reader.buffer_position(), e);
            }

            _ => (),
        }
    }
    .and_then(|_| {
        //println!("{}: {:?}", sym_name, sym_deps);

        let prev_value = deps.insert(sym_name.clone(), sym_deps);

        if prev_value.is_some() {
            println!(
                "WARNING: {} previously had deps: {:?}",
                sym_name,
                prev_value.unwrap()
            );
        };

        Some(())
    })
}

// TODO: use something like linked_hash_set (a crate), or a set in
// combination with a stack, to be able to provide debugging information
// for cycles
//
// symbol moves from deps -> processing -> sorted
struct SortState<T = SymRef> {
    deps: DepMap<T>,
    processing: HashSet<T>,
    visited: HashSet<T>,
    sorted: Vec<T>,
}

fn sort_deps(deps: DepMap) -> Result<Vec<SymRef>, Box<dyn Error>> {
    // @type=meta, @preproc:elig-class-yields
    // @type={ret}map{,:head,:tail}

    let roots = discover_roots(&deps);

    let mut state = SortState {
        deps: deps,
        processing: HashSet::new(),
        visited: HashSet::new(),
        sorted: Vec::new(),
    };

    // unfortunately these roots are hardcoded (we can address that in the
    // future; we must maintain BC for now)
    roots
        .iter()
        .for_each(|root_sym| process_dep(&mut state, root_sym.to_string()));

    Ok(state.sorted)
}

fn discover_roots(deps: &DepMap) -> Vec<SymRef> {
    let mut map_syms = deps
        .keys()
        .filter(|key| key.starts_with(":map:") || key.starts_with(":retmap:"))
        .map(|key| key.to_string())
        .collect::<Vec<_>>();

    let mut roots = vec!["___yield", "___worksheet"]
        .iter()
        .map(|sym| sym.to_string())
        .collect::<Vec<_>>();

    roots.append(&mut map_syms);

    //println!("found roots: {:?}", roots);

    roots
}

fn process_dep(state: &mut SortState, current: SymRef) {
    // TODO: since we're bailing out early, it's possible we _would have_
    // encountered a cycle if we kept going.  Do we care about this?
    // Possibly not, since it's still possible to perform our sort, but then
    // cycles should be caught by the compiler.
    //
    // TODO: Profile: Is it more performant to perform a check on the
    // intersection of the visited set and a set of all dependencies?  That
    // requires creating a new set, so possibly not.
    if !state.visited.insert(current.to_string()) {
        return;
    }

    if !state.processing.insert(current.to_string()) {
        panic!("Cycle: {}", current);
    }

    let deps = state.deps.remove(&current).unwrap_or_else(|| {
        println!("warning: Missing dependencies for {}", current);
        vec![]
    });

    deps.iter()
        .for_each(|dep| process_dep(state, dep.to_string()));

    state.processing.remove(&current);
    state.sorted.push(current);
}

#[cfg(test)]
mod tests {
    #[test]
    fn placeholder() {}
}
