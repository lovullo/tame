// ASG IR
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
        air::{Air, AirAggregate},
        graph::object::ObjectRelTy,
        ExprOp,
    },
    f::Functor,
    parse::{util::SPair, ParseState},
    span::{dummy::*, Span},
};
use std::fmt::Debug;

use Air::*;

// More concise values for tables below.
use ObjectRelTy::*;
const SU: Span = UNKNOWN_SPAN;

fn asg_from_toks<I: IntoIterator<Item = Air>>(toks: I) -> Asg
where
    I::IntoIter: Debug,
{
    let mut parser = AirAggregate::parse(toks.into_iter());
    assert!(parser.all(|x| x.is_ok()));
    parser.finalize().unwrap().into_context()
}

// Note that this is an integration test beginning at AIR.
// It is therefore fragile,
//   and will break if any of these complex systems fail in subtle ways.
// If you arrived at this test without having modified the visitor,
//   then it's quite possible that you should be looking elsewhere.
//
// We will construct a test ASG using the same subsystem as the user;
//   we want to be sure that the traversal works as we expect it to in
//   practice,
//     since the system is fairly complex and failures are more likely
//     to occur at integration points.
#[test]
fn traverses_ontological_tree() {
    let id_a = SPair("expr_a".into(), S3);
    let id_b = SPair("expr_b".into(), S9);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_a),

            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),

            RefIdent(SPair(id_b.symbol(), S6)),
          ExprEnd(S7),

          ExprStart(ExprOp::Sum, S8),
            BindIdent(id_b),
          ExprEnd(S10),
        PkgEnd(S11),
    ];

    let asg = asg_from_toks(toks);

    // From the above graph,
    //   we're going to traverse in such a way as to reconstruct the source
    //   tree.
    let sut = tree_reconstruction(&asg);

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    // Note that the `Depth` beings at 1 because the actual root of the
    //   graph is not emitted.
    // Further note that the depth is the depth of the _path_,
    //   and so identifiers contribute to the depth even though the source
    //   language doesn't have such nesting.
    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|-> B span  | espan  |  depth
        vec![//-----|-------|-----------|-----------|--------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S11), None    ), Depth(1)),
            (d(Pkg,   Ident, m(S1, S11), S3,         None    ),   Depth(2)),
            (d(Ident, Expr,  S3,         m(S2, S7),  None    ),     Depth(3)),
            (d(Expr,  Expr,  m(S2, S7),  m(S4, S5),  None    ),       Depth(4)),
            (d(Expr,  Ident, m(S2, S7),  S9,         Some(S6)),       Depth(4)),
            (d(Pkg,   Ident, m(S1, S11), S9,         None    ),   Depth(2)),
            (d(Ident, Expr,  S9,         m(S8, S10), None    ),     Depth(3)),
        ],
        sut.map(|TreeWalkRel(rel, depth)| (
            rel.map(|(soi, toi)| (
                soi.resolve(&asg).span(),
                toi.resolve(&asg).span()
            )),
            depth
        )).collect::<Vec<_>>(),
    );
}

// This is a variation of the above test,
//   focusing on the fact that templates may contain odd constructions that
//   wouldn't necessarily be valid in other contexts.
// This merely establishes a concrete example to re-enforce intuition and
//   serve as an example of the system's behavior in a laboratory setting,
//     as opposed to having to scan through real-life traces and all the
//     complexity and noise therein.
//
// This also serves as an integration test to ensure that templates produce
//   the expected result on the graph.
// Just as was mentioned above,
//   that makes this test very fragile,
//   and you should look at other failing tests before assuming that this
//     one is broken;
//       let this help to guide your reasoning of the system rather than
//       your suspicion.
#[test]
fn traverses_ontological_tree_tpl_with_sibling_at_increasing_depth() {
    let id_tpl = SPair("_tpl_".into(), S3);
    let id_expr = SPair("expr".into(), S7);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1),
          TplStart(S2),
            BindIdent(id_tpl),

            // Dangling
            ExprStart(ExprOp::Sum, S4),
            ExprEnd(S5),

            // Reachable
            ExprStart(ExprOp::Sum, S6),
              BindIdent(id_expr),
            ExprEnd(S8),
          TplEnd(S9),
        PkgEnd(S10),
    ];

    let asg = asg_from_toks(toks);
    let sut = tree_reconstruction(&asg);

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    // Writing this example helped to highlight how the system is
    //   functioning and immediately obviated a bug downstream in the
    //   lowering pipeline (xmli derivation) at the time of writing.
    // The `Tpl->Ident` was ignored along with its `Depth` because it
    //   produced no output,
    //     and therefore the final expression was interpreted as being a
    //     child of its sibling.
    // This traversal was always correct;
    //   the problem manifested in the integration of these systems and
    //   was caught by system tests.
    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|-> B span  | espan|  depth
        vec![//-----|-------|-----------|-----------|------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S10), None), Depth(1)),
            (d(Pkg,   Ident, m(S1, S10), S3,         None),   Depth(2)),
            (d(Ident, Tpl,   S3,         m(S2, S9),  None),     Depth(3)),
            (d(Tpl,   Expr,  m(S2, S9),  m(S4, S5),  None),       Depth(4)),  // --,
            (d(Tpl,   Ident, m(S2, S9),  S7,         None),       Depth(4)),  //   |
            (d(Ident, Expr,  S7,         m(S6, S8),  None),         Depth(5)), // <'
        ],
        sut.map(|TreeWalkRel(rel, depth)| (
            rel.map(|(soi, toi)| (
                soi.resolve(&asg).span(),
                toi.resolve(&asg).span()
            )),
            depth
        )).collect::<Vec<_>>(),
    );
}
