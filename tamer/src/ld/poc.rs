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

use crate::fs::{
    Filesystem, FsCanonicalizer, PathFile, VisitOnceFile, VisitOnceFilesystem,
};
use crate::global;
use crate::ir::asg::{
    Asg, DefaultAsg, IdentObject, IdentObjectData, Sections, SortableAsg,
    SortableAsgError,
};
use crate::obj::xmle::writer::XmleWriter;
use crate::obj::xmlo::{AsgBuilder, AsgBuilderState, XmloReader};
use crate::sym::{DefaultInterner, Interner, Symbol};
use fxhash::FxBuildHasher;
use petgraph_graphml::GraphMl;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::path::{Path, PathBuf};

type LinkerAsg<'i> = DefaultAsg<'i, IdentObject<'i>, global::ProgIdentSize>;

type LinkerAsgBuilderState<'i> =
    AsgBuilderState<'i, FxBuildHasher, global::ProgIdentSize>;

pub fn xmle(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let interner = DefaultInterner::new();

    let state = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
        &interner,
        AsgBuilderState::new(),
    )?;

    let AsgBuilderState {
        mut roots,
        name,
        relroot,
        found: _,
    } = state;

    roots.extend(
        vec!["___yield", "___worksheet"]
            .iter()
            .map(|name| interner.intern(name))
            .filter_map(|sym| depgraph.lookup(sym)),
    );

    let mut sorted = match depgraph.sort(&roots) {
        Ok(sections) => sections,
        Err(SortableAsgError::Cycles(cycles)) => {
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

pub fn graphml(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);
    let interner = DefaultInterner::new();

    let _ = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
        &interner,
        AsgBuilderState::new(),
    )?;

    // if we move away from petgraph, we will need to abstract this away
    let g = depgraph.into_inner();
    let graphml =
        GraphMl::new(&g)
            .pretty_print(true)
            .export_node_weights(Box::new(|node| {
                // eprintln!("{:?}", node);

                let (name, kind, generated) = match node {
                    Some(n) => {
                        let generated = match n.src() {
                            Some(src) => src.generated,
                            None => false,
                        };

                        (
                            format!("{}", n),
                            n.kind().unwrap().as_ref(),
                            format!("{}", generated),
                        )
                    }
                    None => (
                        String::from("missing"),
                        "missing",
                        format!("{}", false),
                    ),
                };

                vec![
                    ("label".into(), name.into()),
                    ("kind".into(), kind.into()),
                    ("generated".into(), generated.into()),
                ]
            }));

    fs::write(output, graphml.to_string())?;

    Ok(())
}

fn load_xmlo<'a, 'i, I: Interner<'i>, P: AsRef<Path>>(
    path_str: P,
    fs: &mut VisitOnceFilesystem<FsCanonicalizer, FxBuildHasher>,
    depgraph: &mut LinkerAsg<'i>,
    interner: &'i I,
    state: LinkerAsgBuilderState<'i>,
) -> Result<LinkerAsgBuilderState<'i>, Box<dyn Error>> {
    let cfile: PathFile<BufReader<fs::File>> = match fs.open(path_str)? {
        VisitOnceFile::FirstVisit(file) => file,
        VisitOnceFile::Visited => return Ok(state),
    };

    let (path, file) = cfile.into();

    let xmlo: XmloReader<'_, _, _> = (file, interner).into();

    let mut state = depgraph.import_xmlo(xmlo, state)?;

    let mut dir: PathBuf = path.clone();
    dir.pop();

    let found = state.found.take().unwrap_or_default();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        path_buf.push(relpath);
        path_buf.set_extension("xmlo");

        state = load_xmlo(path_buf, fs, depgraph, interner, state)?;
    }

    Ok(state)
}

fn get_ident<'a, 'i>(
    depgraph: &'a LinkerAsg<'i>,
    name: &'i Symbol<'i>,
) -> Result<&'a IdentObject<'i>, String> {
    depgraph
        .lookup(name)
        .and_then(|id| depgraph.get(id))
        .ok_or(format!("missing identifier: {}", name))
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
