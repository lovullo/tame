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

fn asg_from_toks<I: IntoIterator<Item = Air>>(toks: I) -> Asg
where
    I::IntoIter: Debug,
{
    let mut parser = AirAggregate::parse(toks.into_iter());
    assert!(parser.all(|x| x.is_ok()));
    parser.finalize().unwrap().into_context()
}

// Note that this is an integration test beginning at AIR.
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
        PkgOpen(S1),
          ExprOpen(ExprOp::Sum, S2),
            ExprIdent(id_a),

            ExprOpen(ExprOp::Sum, S4),
            ExprClose(S5),

            ExprRef(SPair(id_b.symbol(), S6)),
          ExprClose(S7),

          ExprOpen(ExprOp::Sum, S8),
            ExprIdent(id_b),
          ExprClose(S10),
        PkgClose(S11),
    ];

    let asg = asg_from_toks(toks);

    // From the above graph,
    //   we're going to traverse in such a way as to reconstruct the source
    //   tree.
    let sut = tree_reconstruction(&asg);

    // We need more concise expressions for the below table of values.
    use ObjectRelTy as Ty;
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    // Note that the `Depth` beings at 1 because the actual root of the
    //   graph is not emitted.
    // Further note that the depth is the depth of the _path_,
    //   and so identifiers contribute to the depth even though the source
    //   language doesn't have such nesting.
    #[rustfmt::skip]
    assert_eq!(
        vec![
            (d(Ty::Root,  Ty::Pkg,   m(S1, S11), None    ), Depth(1)),
            (d(Ty::Pkg,   Ty::Ident, S3,         None    ),   Depth(2)),
            (d(Ty::Ident, Ty::Expr,  m(S2, S7),  None    ),     Depth(3)),
            (d(Ty::Expr,  Ty::Expr,  m(S4, S5),  None    ),       Depth(4)),
            (d(Ty::Expr,  Ty::Ident, S9,         Some(S6)),       Depth(4)),
            (d(Ty::Pkg,   Ty::Ident, S9,         None    ),   Depth(2)),
            (d(Ty::Ident, Ty::Expr,  m(S8, S10), None    ),     Depth(3)),
        ],
        sut.map(|TreeWalkRel(rel, depth)| {
            (rel.map(|oi| oi.resolve(&asg).span()), depth)
        }).collect::<Vec<_>>(),
    );
}
