// Read from xmlo and immediately lower to ASG IR
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

use super::reader::{XmloError, XmloEvent, XmloResult};
use crate::ir::asg::{
    Asg, IdentKind, IdentObjectState, IndexType, ObjectRef, Source,
};
use crate::sym::Symbol;
use std::collections::HashSet;
use std::convert::TryInto;
use std::error::Error;
use std::hash::BuildHasher;

// TODO error type
pub type Result<'i, S, Ix> =
    std::result::Result<AsgBuilderState<'i, S, Ix>, Box<dyn Error>>;

#[derive(Debug, Default)]
pub struct AsgBuilderState<'i, S, Ix>
where
    S: BuildHasher,
    Ix: IndexType,
{
    pub roots: Vec<ObjectRef<Ix>>,
    pub found: Option<HashSet<&'i str, S>>,
    pub name: Option<&'i Symbol<'i>>,
    pub relroot: Option<String>,
}

pub trait AsgBuilder<'i, O, S, Ix>
where
    O: IdentObjectState<'i, O>,
    S: BuildHasher,
    Ix: IndexType,
{
    fn import_xmlo(
        &mut self,
        xmlo: impl Iterator<Item = XmloResult<XmloEvent<'i>>>,
        state: AsgBuilderState<'i, S, Ix>,
    ) -> Result<'i, S, Ix>;
}

impl<'i, O, S, Ix, G> AsgBuilder<'i, O, S, Ix> for G
where
    O: IdentObjectState<'i, O>,
    S: BuildHasher + Default,
    Ix: IndexType,
    G: Asg<'i, O, Ix>,
{
    fn import_xmlo(
        &mut self,
        mut xmlo: impl Iterator<Item = XmloResult<XmloEvent<'i>>>,
        mut state: AsgBuilderState<'i, S, Ix>,
    ) -> Result<'i, S, Ix> {
        let mut elig = None;
        let first = state.name.is_none();
        let found = state.found.get_or_insert(Default::default());

        loop {
            match xmlo.next().unwrap()? {
                XmloEvent::Package(attrs) => {
                    if first {
                        state.name = attrs.name;
                        state.relroot = attrs.relroot;
                    }

                    elig = attrs.elig;
                }

                XmloEvent::SymDeps(sym, deps) => {
                    // Maps should not pull in symbols since we may end up
                    // mapping to params that are never actually used
                    if !sym.starts_with(":map:") {
                        for dep_sym in deps {
                            self.add_dep_lookup(sym, dep_sym);
                        }
                    }
                }

                XmloEvent::SymDecl(sym, attrs) => {
                    if let Some(sym_src) = attrs.src {
                        found.insert(sym_src);
                    } else {
                        let owned = attrs.src.is_none();
                        let extern_ = attrs.extern_;

                        let kindval = (&attrs).try_into().map_err(|err| {
                            format!("sym `{}` attrs error: {}", sym, err)
                        })?;

                        let mut src: Source = attrs.into();

                        // Existing convention is to omit @src of local package
                        // (in this case, the program being linked)
                        if first {
                            src.pkg_name = None;
                        }

                        let link_root = owned
                            && matches!(
                                kindval,
                                IdentKind::Meta
                                    | IdentKind::Map
                                    | IdentKind::RetMap
                            );

                        if extern_ {
                            self.declare_extern(sym, kindval, src)?;
                        } else {
                            let node = self.declare(sym, kindval, src)?;

                            if link_root {
                                state.roots.push(node);
                            }
                        }
                    }
                }

                XmloEvent::Fragment(sym, text) => {
                    let frag =
                        self.lookup(sym).ok_or(XmloError::MissingFragment(
                            String::from("missing fragment"),
                        ))?;

                    self.set_fragment(frag, text)?;
                }

                // We don't need to read any further than the end of the
                // header (symtable, sym-deps, fragments)
                XmloEvent::Eoh => break,
            }
        }

        if let Some(elig_sym) = elig {
            state.roots.push(self.lookup(elig_sym).expect(
                "internal error: package elig references nonexistant symbol",
            ));
        }

        Ok(state)
    }
}
