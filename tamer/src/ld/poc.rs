// Proof-of-concept TAME linker
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

//! **This contains the remaining portions of the proof-of-concept linker.**
//! It is feature-complete and just needs final refactoring.

use super::xmle::{
    lower::{sort, SortError},
    xir::lower_iter,
    XmleSections,
};
use crate::asg::{Asg, DefaultAsg, IdentObject};
use crate::global;
use crate::obj::xmlo::{AsgBuilder, AsgBuilderState, XmloReader};
use crate::sym::SymbolId;
use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve};
use crate::xir::writer::XmlWriter;
use crate::{
    fs::{
        Filesystem, FsCanonicalizer, PathFile, VisitOnceFile,
        VisitOnceFilesystem,
    },
    ld::xmle::Sections,
};
use fxhash::FxBuildHasher;
use petgraph_graphml::GraphMl;
use std::error::Error;
use std::fs;
use std::io::Write;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

type LinkerAsg = DefaultAsg<IdentObject, global::ProgIdentSize>;

type LinkerAsgBuilderState =
    AsgBuilderState<FxBuildHasher, global::ProgIdentSize>;

pub fn xmle(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);

    let state = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
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
            .map(|name| name.intern())
            .filter_map(|sym| depgraph.lookup(sym)),
    );

    let sorted = match sort(&depgraph, &roots, Sections::new()) {
        Ok(sections) => sections,
        Err(SortError::Cycles(cycles)) => {
            let msg: Vec<String> = cycles
                .into_iter()
                .map(|cycle| {
                    let mut path = cycle
                        .into_iter()
                        .map(|obj| {
                            depgraph.get(obj).unwrap().name().lookup_str()
                        })
                        .collect::<Vec<&str>>();

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
        sorted,
        name.expect("missing root package name"),
        relroot.expect("missing root package relroot"),
        output,
    )?;

    Ok(())
}

pub fn graphml(package_path: &str, output: &str) -> Result<(), Box<dyn Error>> {
    let mut fs = VisitOnceFilesystem::new();
    let mut depgraph = LinkerAsg::with_capacity(65536, 65536);

    let _ = load_xmlo(
        package_path,
        &mut fs,
        &mut depgraph,
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
                            n.name().lookup_str().into(),
                            n.kind().unwrap().as_sym(),
                            format!("{}", generated),
                        )
                    }
                    None => (
                        String::from("missing"),
                        "missing".into(),
                        format!("{}", false),
                    ),
                };

                vec![
                    ("label".into(), name.into()),
                    ("kind".into(), kind.lookup_str().into()),
                    ("generated".into(), generated.into()),
                ]
            }));

    fs::write(output, graphml.to_string())?;

    Ok(())
}

fn load_xmlo<'a, P: AsRef<Path>>(
    path_str: P,
    fs: &mut VisitOnceFilesystem<FsCanonicalizer, FxBuildHasher>,
    depgraph: &mut LinkerAsg,
    state: LinkerAsgBuilderState,
) -> Result<LinkerAsgBuilderState, Box<dyn Error>> {
    let cfile: PathFile<BufReader<fs::File>> = match fs.open(path_str)? {
        VisitOnceFile::FirstVisit(file) => file,
        VisitOnceFile::Visited => return Ok(state),
    };

    let (path, file) = cfile.into();

    let mut state = {
        #[cfg(not(feature = "wip-xmlo-xir-reader"))]
        {
            let xmlo: XmloReader<_> = file.into();
            depgraph.import_xmlo(xmlo, state)?
        }

        #[cfg(feature = "wip-xmlo-xir-reader")]
        {
            use crate::iter::into_iter_while_ok;
            use crate::xir::reader::XmlXirReader;

            into_iter_while_ok(XmlXirReader::from(file), |toks| {
                let xmlo: XmloReader<_> = toks.into();
                depgraph.import_xmlo(xmlo, state)
            })??
        }
    };

    let mut dir: PathBuf = path.clone();
    dir.pop();

    let found = state.found.take().unwrap_or_default();

    for relpath in found.iter() {
        let mut path_buf = dir.clone();
        let str: &str = &relpath.lookup_str();
        path_buf.push(str);
        path_buf.set_extension("xmlo");

        state = load_xmlo(path_buf, fs, depgraph, state)?;
    }

    Ok(state)
}

fn output_xmle<'a, S: XmleSections<'a>>(
    sorted: S,
    name: SymbolId,
    relroot: SymbolId,
    output: &str,
) -> Result<(), Box<dyn Error>> {
    let file = fs::File::create(output)?;
    let mut buf = BufWriter::new(file);

    lower_iter(sorted, name, relroot).write(&mut buf, Default::default())?;
    buf.flush()?;

    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
