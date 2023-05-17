// Test ASG lowering into xmle sections
//
//  Copyright (C) 2014-2023 Ryan Specialty, LLC.
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

use super::*;
use crate::{
    asg::{
        air::{test::asg_from_toks, Air},
        AsgError, FragmentText, Ident, IdentKind, Source,
    },
    ld::xmle::{section::PushResult, Sections},
    parse::util::SPair,
    span::dummy::*,
    sym::SymbolId,
};

use Air::*;

#[test]
fn graph_sort() -> SortResult<()> {
    // We care only about the order of pushes, not the sections they end
    // up in.
    struct StubSections<'a> {
        pushed: Vec<&'a Ident>,
    }

    impl<'a> XmleSections<'a> for StubSections<'a> {
        fn push(&mut self, ident: &'a Ident) -> PushResult {
            self.pushed.push(ident);
            Ok(())
        }

        fn take_deps(&mut self) -> Vec<&'a Ident> {
            unimplemented!()
        }

        fn take_static(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_map(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_map_froms(&mut self) -> fxhash::FxHashSet<SymbolId> {
            unimplemented!()
        }

        fn take_retmap(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }

        fn take_exec(&mut self) -> Vec<SymbolId> {
            unimplemented!()
        }
    }

    let name_a_dep = SPair("adep".into(), S4);
    let name_a = SPair("a".into(), S3);
    let name_a_dep_dep = SPair("adepdep".into(), S2);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, SPair("/pkg".into(), S1)),
          // Intentionally out-of-order.
          IdentDecl(
              name_a_dep,
              IdentKind::Meta,
              Default::default(),
          ),

          IdentDecl(
              name_a,
              IdentKind::Meta,
              Default::default(),
          ),

          IdentDecl(
              name_a_dep_dep,
              IdentKind::Meta,
              Default::default(),
          ),

          IdentDep(name_a, name_a_dep),
          IdentDep(name_a_dep, name_a_dep_dep),
          IdentRoot(name_a),
        PkgEnd(S5),
    ];

    let asg = asg_from_toks(toks);
    let sections = sort(&asg, StubSections { pushed: Vec::new() })?;

    let expected = vec![
        // Post-order
        name_a_dep_dep,
        name_a_dep,
        name_a,
    ]
    .into_iter()
    .collect::<Vec<_>>();

    assert_eq!(
        expected,
        sections
            .pushed
            .iter()
            .map(|ident| ident.name())
            .collect::<Vec<_>>(),
    );

    Ok(())
}

#[test]
fn graph_sort_missing_node() -> SortResult<()> {
    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, SPair("/pkg".into(), S1)),
          IdentDecl(
              sym,
              IdentKind::Tpl,
              Source {
                  virtual_: true,
                  ..Default::default()
              },
          ),
          IdentFragment(sym, FragmentText::from("foo")),

          // dep will be added as Missing
          IdentDep(sym, dep),

          IdentRoot(sym),
        PkgEnd(S5),
    ];

    let asg = asg_from_toks(toks);

    match sort(&asg, Sections::new()) {
        Ok(_) => panic!("Unexpected success - dependency is not in graph"),
        Err(SortError::SectionsError(SectionsError::UnresolvedObject(_))) => (),
        bad => {
            panic!(
                "Incorrect error result when dependency is not in graph: {:?}",
                bad
            )
        }
    }

    Ok(())
}

#[test]
fn graph_sort_simple_cycle() -> SortResult<()> {
    let sym = SPair("sym".into(), S1);
    let dep = SPair("dep".into(), S2);

    #[rustfmt::skip]
    let toks = [
        PkgStart(S1, SPair("/pkg".into(), S1)),
          IdentDecl(
              sym,
              IdentKind::Tpl,
              Source {
                  virtual_: true,
                  ..Default::default()
              },
          ),
          IdentFragment(sym, FragmentText::from("foo")),

          IdentDecl(
              dep,
              IdentKind::Tpl,
              Source {
                  virtual_: true,
                  ..Default::default()
              },
          ),
          IdentFragment(dep, FragmentText::from("bar")),

          // Produce a cycle (which will be an error)
          IdentDep(sym, dep),
          IdentDep(dep, sym),

          IdentRoot(sym),
        PkgEnd(S5),
    ];

    let asg = asg_from_toks(toks);
    let result = sort(&asg, Sections::new());

    match result {
        Ok(_) => panic!("sort did not detect cycle"),
        Err(SortError::AsgError(AsgError::UnsupportedCycle(_))) => (),
        Err(e) => panic!("unexpected error: {}", e),
    }

    Ok(())
}
