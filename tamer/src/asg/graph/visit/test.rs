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
        ExprOp,
    },
    parse::{util::SPair, ParseState},
    span::dummy::*,
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

    let toks = vec![
        // <package>
        PkgOpen(S1),
        //   <expr>
        ExprOpen(ExprOp::Sum, S2),
        ExprIdent(id_a),
        //     <expr>
        ExprOpen(ExprOp::Sum, S4),
        ExprClose(S5),
        //     </expr>
        ExprRef(SPair(id_b.symbol(), S6)),
        ExprClose(S7),
        //   </expr>
        //   <expr>
        ExprOpen(ExprOp::Sum, S8),
        ExprIdent(id_b),
        ExprClose(S10),
        //   </expr>
        // </package>
        PkgClose(S11),
    ];

    let asg = asg_from_toks(toks);

    // From the above graph,
    //   we're going to traverse in such a way as to reconstruct the source
    //   tree.
    let sut = tree_reconstruction(&asg);

    assert_eq!(
        vec![
            S1.merge(S11).unwrap(), // Pkg
            S3,                     //   Ident (id_a)
            S2.merge(S7).unwrap(),  //   Expr
            S4.merge(S5).unwrap(),  //     Expr
            S9,                     //     Ident (ExpRef)¹
            S9,                     //   Ident (id_b)
            S8.merge(S10).unwrap(), //   Expr
        ],
        sut.map(ObjectIndex::cresolve(&asg))
            .map(Object::span)
            .collect::<Vec<_>>()
    );
    // ¹ We have lost the reference context (S6),
    //     which is probably the more appropriate one to be output here,
    //     given that this is a source reconstruction and ought to be mapped
    //     back to what the user entered at the equivalent point in the tree.
    //   TODO: Figure out how to best expose this,
    //     which probably also involves the introduction of edge spans.
}
