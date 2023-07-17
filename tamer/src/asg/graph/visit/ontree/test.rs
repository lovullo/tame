// Test ontological tree preorder ASG traversal
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
    parse::{
        util::{spair, SPair},
        ParseState,
    },
    span::{dummy::*, Span},
};
use std::fmt::Debug;

use Air::*;

// More concise values for tables below.
use ObjectRelTy::*;
const SU: Span = UNKNOWN_SPAN;

fn tree_reconstruction_report<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Vec<(DynObjectRel<Span, Span>, Depth)>
where
    I::IntoIter: Debug,
{
    let mut parser = AirAggregate::parse(toks.into_iter());
    assert!(parser.all(|x| x.is_ok()));

    let asg = &parser.finalize().unwrap().into_context().finish();

    tree_reconstruction(asg)
        .map(|TreeWalkRel(rel, depth)| {
            (
                rel.map(|(soi, toi)| {
                    (soi.resolve(asg).span(), toi.resolve(asg).span())
                }),
                depth,
            )
        })
        .collect()
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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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
        tree_reconstruction_report(toks),
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
        PkgStart(S1, SPair("/pkg".into(), S1)),
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
        tree_reconstruction_report(toks),
    );
}

// The way template applications are handled on the ASG differs from NIR.
// This test ensure that the representation on the ASG is precise;
//   it's far easier to catch those problems here than it is to catch them
//   in an implementation utilizing these data that's failing in some
//   unexpected way.
#[test]
fn traverses_ontological_tree_tpl_apply() {
    let name_tpl = "_tpl-to-apply_".into();
    let id_tpl = SPair(name_tpl, S3);
    let ref_tpl = SPair(name_tpl, S6);
    let id_param = SPair("@param@".into(), S8);
    let value_param = SPair("value".into(), S9);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg".into(), S1)),
          // The template that will be applied.
          TplStart(S2),
            BindIdent(id_tpl),                          // <-,
                                                        //   |
            // This test is light for now,              //   |
            //   until we develop the ASG further.      //   |
          TplEnd(S4),                                   //   |
                                                        //   |
          // Apply the above template.                  //   |
          TplStart(S5),                                 //   |
            RefIdent(ref_tpl),                          //   |
                                                        //   |
            MetaStart(S7),                              //   |
              BindIdent(id_param),                      //   |
              MetaLexeme(value_param),                  //   |
            MetaEnd(S10),                               //   |
          TplEndRef(S11),  // notice the `Ref` at the end  --'
        PkgEnd(S12),
    ];

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|-> B span  | espan  |  depth
        vec![//-----|-------|-----------|-----------|--------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S12), None    ), Depth(1)),
            (d(Pkg,   Ident, m(S1, S12), S3,         None    ),   Depth(2)),
            (d(Ident, Tpl,   S3,         m(S2, S4),  None    ),     Depth(3)),
            (d(Pkg,   Tpl,   m(S1, S12), m(S5, S11), None    ),   Depth(2)),
            (d(Tpl,   Ident, m(S5, S11), S8,         None    ),     Depth(3)),
            (d(Ident, Meta,  S8,         m(S7, S10), None    ),       Depth(4)),
  /*cross*/ (d(Tpl,   Ident, m(S5, S11), S3,         Some(S6)),     Depth(3)),
         //             ^
         //              `- Note that the cross edge was moved to the bottom
         //                 because all template params are moved into the
         //                 template header for `SourceCompatibleTreeEdgeOrder`.
        ],
        tree_reconstruction_report(toks),
    );
}

// A template acts as a container for anything defined therein,
//   to be expanded into an application site.
// This means that identifiers that might otherwise be bound to the package
//   need to be contained by the template,
//     and further that identifier _resolution_ must be able to occur within
//     the template,
//       e.g. to apply templates defined therein.
#[test]
fn traverses_ontological_tree_tpl_within_template() {
    let name_outer = "_tpl-outer_".into();
    let id_tpl_outer = SPair(name_outer, S3);
    let name_inner = "_tpl-inner_".into();
    let id_tpl_inner = SPair(name_inner, S10);
    let ref_inner_before = SPair(name_inner, S7);
    let ref_inner_after = SPair(name_inner, S13);

    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, SPair("/pkg".into(), S1)),
          TplStart(S2),
            BindIdent(id_tpl_outer),

            // Anonymous inner template application.
            TplStart(S4),
            TplEndRef(S5),  // notice the `Ref` at the end

            // Apply above inner template,
            //   _before_ definition,
            //   which will begin as Missing and must be later resolved when
            //     the template is defined.
            TplStart(S6),
              RefIdent(ref_inner_before),   // --.
            TplEndRef(S8),                  //    |
                                            //    |
            // Named inner template.        //    |
            TplStart(S9),                   //   /
              BindIdent(id_tpl_inner),      //<-:
            TplEnd(S11),                    //   \
                                            //    |
            // Apply above inner template,  //    |
            //   _after_ definition.        //    |
            TplStart(S12),                  //    |
              RefIdent(ref_inner_after),    // __/
            TplEndRef(S14),
          TplEnd(S15),
        PkgEnd(S16),
    ];

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|-> B span  |  espan  |  depth
        vec![//-----|-------|-----------|-----------|---------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S16), None     ), Depth(1)),
            (d(Pkg,   Ident, m(S1, S16), S3,         None     ),   Depth(2)),
            (d(Ident, Tpl,   S3,         m(S2, S15), None     ),     Depth(3)),
            (d(Tpl,   Tpl,   m(S2, S15), m(S4, S5),  None     ),       Depth(4)),
            (d(Tpl,   Tpl,   m(S2, S15), m(S6, S8),  None     ),       Depth(4)),
  /*cross*/ (d(Tpl,   Ident, m(S6, S8),  S10,        Some(S7) ),         Depth(5)),
    //  ,--------------------------------^^^
    /* | */ (d(Tpl,   Ident, m(S2, S15), S10,        None     ),       Depth(4)),
    /* | */ (d(Ident, Tpl,   S10,        m(S9, S11), None     ),         Depth(5)),
    /* | */ (d(Tpl,   Tpl,   m(S2, S15), m(S12,S14), None     ),       Depth(4)),
  /*cross*/ (d(Tpl,   Ident, m(S12,S14), S10,        Some(S13)),         Depth(5)),
    // |    //                           ^^^
    // |    // Note that successfully    /
    // |    //   resolving this span  --`
    // |    //   as S10 (Ident::Transparent) instead of S13 (which would mean
    // |    //   a new Ident::Missing was created) requires that we resolve
    // |    //   a local identifier that is rooted in the _template_ rather
    // |    //   than the global scope.
    //  `----> Similarly,
            //   resolving the former as S10 instead of S7 means that a
            //   local identifier that was originally Missing is properly
            //   resolved to Transparent once it was defined;
            //     which asserts consistency in identifier scope regardless
            //     of reference/definition order.
            // This lexical analysis is something that the XSLT-based TAME
            //   was not capable of doing.
        ],
        tree_reconstruction_report(toks),
    );
}

// Metavariables are used to represent template parameters,
//   and are used to perform various lexical manipulations.
// The most fundamental of them is concatenation,
//   and in the special case of concatenating a single value,
//   assignment.
//
// This asserts that concatenation results in the expected graph and that
//   the traversal respects concatenation order.
#[test]
fn traverses_ontological_tree_complex_tpl_meta() {
    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, spair("/pkg", S1)),
          TplStart(S2),
            BindIdent(spair("_tpl_", S3)),

            // -- Above this line was setup -- //

            MetaStart(S4),
              BindIdent(spair("@param@", S5)),

              // It will be important to observe that ordering
              //   is respected during traversal,
              //     otherwise concatenation order will be wrong.
              MetaLexeme(spair("foo", S6)),
              RefIdent(spair("@other@", S7)),    // --.
              MetaLexeme(spair("bar", S8)),      //   |
            MetaEnd(S9),                         //   |
                                                 //   |
            MetaStart(S10),                      //   |
              BindIdent(spair("@other@", S11)),  // <-'
            MetaEnd(S12),
          TplEnd(S13),
       PkgEnd(S14),
    ];

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|->  B span  | espan  |  depth
        vec![//-----|-------|-----------|------------|--------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S14),  None    ), Depth(1)),
            (d(Pkg,   Ident, m(S1, S14), S3,          None    ),   Depth(2)),
            (d(Ident, Tpl,   S3,         m(S2, S13),  None    ),     Depth(3)),
            (d(Tpl,   Ident, m(S2, S13), S5,          None    ),       Depth(4)),
            (d(Ident, Meta,  S5,         m(S4, S9),   None    ),         Depth(5)),
            (d(Meta,  Meta,  m(S4, S9),  S6,          None    ),           Depth(6)),
  /*cross*/ (d(Meta,  Ident, m(S4, S9),  S11,         Some(S7)),           Depth(6)),
            (d(Meta,  Meta,  m(S4, S9),  S8,          None,   ),           Depth(6)),
            (d(Tpl,   Ident, m(S2, S13), S11,         None    ),       Depth(4)),
            (d(Ident, Meta,  S11,        m(S10, S12), None    ),         Depth(5)),
        ],
        tree_reconstruction_report(toks),
    );
}

// TAME's grammar expects that template parameters be defined in a header,
//   before the template body.
// This is important for disambiguating `<param`> in the sources,
//   since they could otherwise refer to other types of parameters.
//
// TAMER generates metavariables during interpolation,
//   causing params to be mixed with the body of the template;
//     this is reflected in the natural ordering.
// But this would result in a semantically invalid source reconstruction,
//   and so we must reorder edges during traversal such that the
//   metavariables representing template parameters are visited _before_
//   everything else.
#[test]
fn tpl_header_source_order() {
    #[rustfmt::skip]
    let toks = vec![
        PkgStart(S1, spair("/pkg", S1)),
          TplStart(S2),
            BindIdent(spair("_tpl_", S3)),

            // -- Above this line was setup -- //

            MetaStart(S4),
              BindIdent(spair("@param_before@", S5)),
            MetaEnd(S6),

            // Dangling (no Ident)
            ExprStart(ExprOp::Sum, S7),
            ExprEnd(S8),

            MetaStart(S9),
              BindIdent(spair("@param_after_a@", S10)),
            MetaEnd(S11),

            MetaStart(S12),
              BindIdent(spair("@param_after_b@", S13)),
            MetaEnd(S14),

            // Reachable (with an Ident)
            //   (We want to be sure that we're not just hoisting all Idents
            //      without checking that they're actually Meta
            //      definitions).
            ExprStart(ExprOp::Sum, S15),
              BindIdent(spair("sum", S16)),
            ExprEnd(S17),
          TplEnd(S18),
       PkgEnd(S19),
    ];

    // We need more concise expressions for the below table of values.
    let d = DynObjectRel::new;
    let m = |a: Span, b: Span| a.merge(b).unwrap();

    #[rustfmt::skip]
    assert_eq!(
        //      A  -|-> B   |  A span  -|->  B span  | espan  |  depth
        vec![//-----|-------|-----------|------------|--------|-----------------
            (d(Root,  Pkg,   SU,         m(S1, S19),  None    ), Depth(1)),
            (d(Pkg,   Ident, m(S1, S19), S3,          None    ),   Depth(2)),
            (d(Ident, Tpl,   S3,         m(S2, S18),  None    ),     Depth(3)),
            (d(Tpl,   Ident, m(S2, S18), S5,          None    ),       Depth(4)),
            (d(Ident, Meta,  S5,         m(S4, S6),   None    ),         Depth(5)),
        // ,-----------------------------------------------------------------------,
            (d(Tpl,   Ident, m(S2, S18), S10,         None    ),       Depth(4)),
            (d(Ident, Meta,  S10,        m(S9, S11),  None    ),         Depth(5)),
            (d(Tpl,   Ident, m(S2, S18), S13,         None    ),       Depth(4)),
            (d(Ident, Meta,  S13,        m(S12, S14), None    ),         Depth(5)),
        // '-----------------------------------------------------------------------'
            (d(Tpl,   Expr,  m(S2, S18), m(S7, S8),   None    ),       Depth(4)),
            (d(Tpl,   Ident, m(S2, S18), S16,         None    ),       Depth(4)),
            (d(Ident, Expr,  S16,        m(S15, S17), None    ),         Depth(5)),
        ],
        // ^ The enclosed Ident->Meta pairs above have been hoisted out of
        //     the body and into the header of `Tpl`.
        //   This is a stable, partial ordering;
        //     elements do not change poisitions relative to one-another
        //     with the exception of hoisting.
        //   This means that all hoisted params retain their order relative
        //     to other params,
        //       and all objects in the body retain their positions relative
        //       to other objects in the body.
        tree_reconstruction_report(toks),
    );
}
