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
use crate::obj::xmlo::{AsgBuilder, AsgBuilderState, XmloReader};
use crate::sym::SymbolId;
use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve};
use fxhash::FxBuildHasher;
use petgraph_graphml::GraphMl;
use std::error::Error;
use std::fs;
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
                                depgraph
                                    .get(obj)
                                    .unwrap()
                                    .name()
                                    .unwrap()
                                    .lookup_str(),
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
                            format!("{}", n.name().unwrap().lookup_str()),
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
                    ("kind".into(), kind.lookup_str().as_str().into()),
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

    let xmlo: XmloReader<_> = file.into();

    let mut state = depgraph.import_xmlo(xmlo, state)?;

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

fn get_ident<'a>(
    depgraph: &'a LinkerAsg,
    name: SymbolId<global::ProgSymSize>,
) -> Result<&'a IdentObject, String> {
    depgraph
        .lookup(name)
        .and_then(|id| depgraph.get(id))
        .ok_or(format!("missing identifier: {}", name.lookup_str()))
}

fn output_xmle<'a>(
    depgraph: &'a LinkerAsg,
    sorted: &mut Sections<'a, IdentObject>,
    name: SymbolId,
    relroot: String,
    output: &str,
) -> Result<(), Box<dyn Error>> {
    if !sorted.map.is_empty() {
        sorted
            .map
            .push_head(get_ident(depgraph, ":map:___head".intern())?);
        sorted
            .map
            .push_tail(get_ident(depgraph, ":map:___tail".intern())?);
    }

    if !sorted.retmap.is_empty() {
        sorted
            .retmap
            .push_head(get_ident(depgraph, ":retmap:___head".intern())?);
        sorted
            .retmap
            .push_tail(get_ident(depgraph, ":retmap:___tail".intern())?);
    }

    let file = fs::File::create(output)?;

    #[cfg(not(feature = "wip-xir-xmle-writer"))]
    {
        use crate::obj::xmle::writer::XmleWriter;

        let mut xmle_writer = XmleWriter::new(BufWriter::new(file));
        xmle_writer.write(&sorted, name, &relroot)?;
    }
    #[cfg(feature = "wip-xir-xmle-writer")]
    {
        use std::io::Write;
        use crate::ir::xir::writer::XmlWriter;
        use crate::obj::xmle::xir::lower_iter;

        eprintln!("warning: using wip-xir-xmle-writer");

        let mut buf = BufWriter::new(file);

        // TODO: check writer final state to make sure it actually finished
        lower_iter(&sorted, name, relroot.intern())
            .write(&mut buf, Default::default())?;

        buf.flush()?;
    }

    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn placeholder() {}
}
