// Test topological sort ASG traversal
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
        graph::object::{
            self, ObjectKind, ObjectRelFrom, ObjectRelatable, ObjectTy, Root,
        },
        ExprOp, IdentKind,
    },
    num::{Dim, Dtype},
    parse::{util::SPair, ParseState},
    span::{dummy::*, Span, UNKNOWN_SPAN},
};
use std::fmt::Debug;

use Air::*;

fn topo_report_only(
    asg: &Asg,
    edges: impl Iterator<Item = ObjectIndex<Object>>,
) -> Vec<Result<(ObjectTy, Span), Vec<(ObjectTy, Span)>>> {
    topo_sort(asg, edges)
        .map(|result| {
            result
                .map(|oi| oi.resolve(asg))
                .map(|obj| (obj.ty(), obj.span()))
                .map_err(|cycle| {
                    cycle
                        .path_rev()
                        .iter()
                        // Retain the resolved span from Cycle so that our
                        //   assertions verify that it is being resolved
                        //   correctly.
                        .map(|oirs| (oirs.oi().resolve(asg).ty(), oirs.span()))
                        .collect()
                })
        })
        .collect()
}

fn topo_report<O: ObjectKind + ObjectRelatable, I: IntoIterator<Item = Air>>(
    toks: I,
) -> Vec<Result<(ObjectTy, Span), Vec<(ObjectTy, Span)>>>
where
    I::IntoIter: Debug,
    O: ObjectRelFrom<Root>,
{
    let mut parser = AirAggregate::parse(toks.into_iter());
    assert!(parser.all(|x| x.is_ok()));

    let asg = &parser.finalize().unwrap().into_context().finish();
    let oi_root = asg.root(UNKNOWN_SPAN);

    topo_report_only(
        asg,
        oi_root.edges_filtered::<O>(asg).map(ObjectIndex::widen),
    )
}

#[test]
fn sorts_objects_given_single_root() {
    let id_a = SPair("expr_a".into(), S3);
    let id_b = SPair("expr_b".into(), S9);
    let id_c = SPair("expr_c".into(), S12);

    #[rustfmt::skip]
    let toks = vec![
        // Packages are auto-rooted as part of the graph's ontology.
        // There is only one for this test.
        PkgStart(S1, SPair("/pkg".into(), S1)),
          // Before this can be computed,
          //   its dependencies must be.
          ExprStart(ExprOp::Sum, S2),           // -.
            BindIdent(id_a),                    //  |
                                                //  |
            // This is a dependency,            //  |
            //   but it is owned by this Expr   //  |
            //   and so would have been emitted //  |
            //   first anyway.                  //  |
            ExprStart(ExprOp::Sum, S4),         //  |
            ExprEnd(S5),                        //  |
                                                //  v
            // But this is a reference to another
            //   Expr that appears later.
            RefIdent(SPair(id_b.symbol(), S6)),  // --. 
          ExprEnd(S7),                           //    | 
                                                 //    |
          // This will have to be emitted        //    |
          //   _before_ the above Expr that      //    |
          //   depends on its value having been  //    |
          //   computed.                         //   /  
          ExprStart(ExprOp::Sum, S8),            // <`
            BindIdent(id_b),
          ExprEnd(S10),

          // A sibling expression with no dependency on
          //   other expressions.
          ExprStart(ExprOp::Sum, S11),
            BindIdent(id_c),
          ExprEnd(S13),
        PkgEnd(S14),
    ];

    use ObjectTy::*;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            // The first leaf is this anonymous child expression,
            //   which has no dependencies.
            (Expr,  m(S4, S5)  ),  // child of id_a

            // The sibling of the above expression is a reference to the
            //   value of `id_b`.
            // `id_a` cannot be computed before it.
            (Expr,  m(S8, S10) ),  // id_b
            (Ident, S9,        ),  // id_b

            // With `id_b` emitted,
            //   `id_a` has no more dependencies,
            //   and so itself can be emitted.
            (Expr,  m(S2, S7)  ),  // id_a
            (Ident, S3,        ),  // id_a

            // `id_a` has a sibling `id_c`.
            // Its ordering is undefined relative to `id_a`
            //   (it could also be ordered before it),
            //   but the implementation of the traversal causes it to be
            //     output in the same order as it appeared in the source
            //     token stream.
            (Expr,  m(S11, S13)),  // id_c
            (Ident, S12        ),  // id_c

            // We end with the root that was explicitly provided to
            //   `topo_sort` via `topo_report`.
            (Pkg,   m(S1, S14) ),
        ]),
        topo_report::<object::Pkg, _>(toks).into_iter().collect(),
    );
}

// Like the above test,
//   but the path is deeper to emphasize that the topological sort applies
//   recursively to dependencies.
// Multiple expressions depending on the same dependency have an arbitrary
//   order that is deterministic between runs.
#[test]
fn sorts_objects_given_single_root_more_complex() {
    let id_a = SPair("expr_a".into(), S3);
    let id_b = SPair("expr_b".into(), S7);
    let id_c = SPair("expr_c".into(), S11);
    let id_d = SPair("expr_d".into(), S15);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg".into(), S1)),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_a),
            RefIdent(SPair(id_b.symbol(), S4)),  // ---.
          ExprEnd(S5),                           //     )
                                                 //    /
          ExprStart(ExprOp::Sum, S6),            //   /
            BindIdent(id_b),                     // <'
            RefIdent(SPair(id_d.symbol(), S8)),  // -------.
          ExprEnd(S9),                           // <.      |
                                                 //   \     |
          ExprStart(ExprOp::Sum, S10),           //    \    |
            BindIdent(id_c),                     //     )   |
            RefIdent(SPair(id_b.symbol(), S12)), // ---'   /
          ExprEnd(S13),                          //       /
                                                 //      /
          ExprStart(ExprOp::Sum, S14),           //     /
            BindIdent(id_d),                     // <--'
          ExprEnd(S16),
        PkgEnd(S17),
    ];

    use ObjectTy::*;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            (Expr,  m(S14, S16)),  // id_d
            (Ident, S15        ),  // id_d

            (Expr,  m(S6, S9) ),   // id_b
            (Ident, S7,        ),  // id_b

            (Expr,  m(S2, S5)  ),  // id_a
            (Ident, S3,        ),  // id_a

            (Expr,  m(S10, S13)),  // id_c
            (Ident, S11        ),  // id_c

            (Pkg,   m(S1, S17) ),
        ]),
        topo_report::<object::Pkg, _>(toks).into_iter().collect(),
    );
}

// This tests what the linker (tameld) does:
//   topologically sorts explicitly rooted objects and ignores everything
//   else.
// This also gives us dead code elimination.
#[test]
fn omits_unreachable() {
    let id_a = SPair("expr_a".into(), S3);
    let id_b = SPair("expr_b".into(), S7);
    let id_c = SPair("expr_c".into(), S11);
    let id_d = SPair("expr_d".into(), S15);

    // We will only use a portion of this graph.
    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg".into(), S1)),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_a),
            RefIdent(SPair(id_b.symbol(), S4)),  // ---.
          ExprEnd(S5),                           //     )
                                                 //    /
          ExprStart(ExprOp::Sum, S6),            //   /
            BindIdent(id_b),                     // <'
            RefIdent(SPair(id_d.symbol(), S8)),  // -------.
          ExprEnd(S9),                           // <.      |
                                                 //   \     |
          ExprStart(ExprOp::Sum, S10),           //    \    |
            BindIdent(id_c),                     //     )   |
            RefIdent(SPair(id_b.symbol(), S12)), // ---'   /
          ExprEnd(S13),                          //       /
                                                 //      /
          ExprStart(ExprOp::Sum, S14),           //     /
            BindIdent(id_d),                     // <--'
          ExprEnd(S16),
        PkgEnd(S17),
    ];

    use ObjectTy::*;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    let mut parser = AirAggregate::parse(toks.into_iter());
    assert!(parser.all(|x| x.is_ok()));

    let asg = &parser.finalize().unwrap().into_context().finish();

    let oi_pkg = asg
        .root(UNKNOWN_SPAN)
        .edges_filtered::<object::Pkg>(&asg)
        .next()
        .expect("cannot find Pkg on graph");

    let oi_b = oi_pkg.lookup_local_linear(asg, id_b).expect("missing oi_b");

    // We'll use only `oi_b` as the root,
    //   which will include it and its (only) dependency.
    // The rest of the graph must be ignored.
    let report = topo_report_only(&asg, [oi_b.widen()].into_iter());

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            (Expr,  m(S14, S16)),  // id_d
            (Ident, S15        ),  // id_d

            (Expr,  m(S6, S9) ),   // id_b
            (Ident, S7,        ),  // id_b
        ]),
        report.into_iter().collect(),
    );
}

// If multiple roots are given,
//   and they have entirely independent subgraphs,
//   then their ordering is deterministic between runs of the same graph,
//     but undefined.
//
// This is no different than the ordering of siblings above;
//   this simply provides an explicit example for the behavior of provided
//   roots since that is the entry point for this API.
#[test]
fn sorts_objects_given_multiple_roots() {
    let pkg_a_name = SPair("/pkg/a".into(), S2);
    let pkg_b_name = SPair("/pkg/b".into(), S8);

    let id_a = SPair("expr_a".into(), S4);
    let id_b = SPair("expr_b".into(), S10);

    #[rustfmt::skip]
    let toks = vec![
        // First root
        PkgStart(S1, pkg_a_name),
          ExprStart(ExprOp::Sum, S3),
            BindIdent(id_a),
          ExprEnd(S5),
        PkgEnd(S6),

        // Second root,
        //   independent of the first.
        PkgStart(S7, pkg_b_name),
          ExprStart(ExprOp::Sum, S9),
            BindIdent(id_b),
          ExprEnd(S11),
        PkgEnd(S12),
    ];

    use ObjectTy::*;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        Ok(vec![
            // First root.
            (Expr,  m(S3, S5) ),
            (Ident, S4),
            (Pkg,   m(S1, S6) ),

            // Second root,
            //   but the fact that it is emitted after the first is not
            //   behavior that should be relied upon.
            (Expr,  m(S9, S11) ),
            (Ident, S10),
            (Pkg,   m(S7, S12)),
        ]),
        topo_report::<object::Pkg, _>(toks).into_iter().collect(),
    );
}

// Most cycles are unsupported by TAME.
// Recovery allows compilation/linking to continue so that additional errors
//   can be discovered and reported.
#[test]
fn unsupported_cycles_with_recovery() {
    let id_a = SPair("expr_a".into(), S3);
    let id_b = SPair("expr_b".into(), S8);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg-a".into(), S1)),
          ExprStart(ExprOp::Sum, S2),
            BindIdent(id_a),                     // <----.  self-cycle
            RefIdent(SPair(id_a.symbol(), S4)),  // ____/ \
            RefIdent(SPair(id_b.symbol(), S5)),  // ---.   \  a->b->a
          ExprEnd(S6),                           //     )   )  cycle
                                                 //    /   /
          ExprStart(ExprOp::Sum, S7),            //   /   /
            BindIdent(id_b),                     // <'   /
            RefIdent(SPair(id_a.symbol(), S9)),  // ----'
          ExprEnd(S10),
        PkgEnd(S11),
    ];

    use ObjectTy::*;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    assert_eq!(
        #[rustfmt::skip]
        vec![
            // Pkg -> Ident (id_a) -> Ref (id_a) gives us a self-cycle.
            Err(vec![
                (Expr,  m(S2, S6)),  // -.
                (Ident, S3       ),  // <'   id_a
            ]),

            // RECOVERY: We do not traverse into the cycle and continue as
            //   if the edge causing the cycle was not taken.

            // ...which unfortunately lands us on another cycle caused by
            //   a->b->a before we can emit the parent Expr.
            // TODO: In the future we ought to represent the reference here
            //   as well.
            Err(vec![
                (Expr, m(S7, S10)),  // -.
                (Ident, S8       ),  //  |   id_b
                (Expr,  m(S2, S6)),  //  |
                (Ident, S3       ),  // <'   id_a
            ]),

            // RECOVERY: We ignore the edge leading to the cycle,
            //   which means that id_b Expr has no more dependencies.
            Ok((Expr,  m(S7, S10))),
            Ok((Ident, S8        )),

            // And id_a is now also complete,
            //   since the cycle was the last dependency.
            Ok((Expr,  m(S2, S6 ))),
            Ok((Ident, S3        )),

            Ok((Pkg,   m(S1, S11))),
        ],
        topo_report::<object::Pkg, _>(toks)
            .into_iter()
            .collect::<Vec<_>>(),
    );
}

// TAME supports cycles in certain contexts,
//   as a component of the graph's ontology.
// A topological sort of a graph containing permitted cycles should be
//   viewed as sorting a graph that first "cuts" those cycles,
//     filtering out the edge that would have caused the cycle to occur.
// It is the responsibility of the ontology to ensure that all such cuts
//   will result in a topological sort.
#[test]
fn supported_cycles() {
    let id_a = SPair("func_a".into(), S3);
    let id_b = SPair("func_b".into(), S8);
    let kind = IdentKind::Func(Dim::Scalar, Dtype::Integer);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg-a".into(), S1)),
          // Two mutually recursive functions.
          IdentDecl(id_a, kind.clone(), Default::default()),  // <--.
          IdentDep(id_a, id_b),                               // -. |
                                                              //  | |
          IdentDecl(id_b, kind.clone(), Default::default()),  // <' |
          IdentDep(id_b, id_a),                               // ---'

          // Root so that `topo_report` will find them.
          IdentRoot(id_a),
          IdentRoot(id_b),
        PkgEnd(S11),
    ];

    // TODO: Template recursion was not part of the ontology at the time of
    //   writing.

    use ObjectTy::*;

    assert_eq!(
        #[rustfmt::skip]
        Ok(vec![
            // The order in which the above functions will be visited is
            //   undefined;
            //     this is the ordering that happens to be taken by the
            //     implementation based on the definition and stack
            //     ordering.
            (Ident, S8),
            (Ident, S3),
        ]),
        topo_report::<object::Ident, _>(toks)
            .into_iter()
            .collect::<Result<Vec<_>, _>>(),
    );
}
