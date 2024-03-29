// Scope tests for ASG IR
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

//! Scoping tests.
//!
//! These tests verify that identifiers are scoped to the proper
//!   environments.
//! These may duplicate portions of other tests,
//!   but having concrete examples all in one place helps to develop, debug,
//!   and understand a system that can be quite confusing in the abstract.
//!
//! These tests _do not_ assert that identifiers are properly assigned to
//!   their corresponding definitions;
//!     those tests exist elsewhere.
//!
//! The core abstraction for these tests is [`assert_scope`],
//!   which allows for declarative assertions of identifier scope against
//!   the graph;
//!     it is key to creating tests that can be both easily created and
//!     easily understood.
//!
//! If You Are Here Due To A Test Failure
//! =====================================
//! If there are failing tests in parent or sibling modules,
//!   check those first;
//!     these tests are potentially fragile given that they test only a
//!     subset of behavior without first asserting against other system
//!     invariants,
//!       as described above.

use super::*;
use crate::{
    asg::{
        graph::object::{self, ObjectTy},
        visit::{tree_reconstruction, TreeWalkRel},
        ExprOp,
    },
    parse::util::spair,
    span::UNKNOWN_SPAN,
};
use std::iter::once;

use EnvScopeKind::*;

const S0: Span = UNKNOWN_SPAN;

fn m(a: Span, b: Span) -> Span {
    a.merge(b).unwrap()
}

/// Assert that the scope of the identifier named `name` is that of the
///   provided environment list `expected`.
///
/// The inner value of `$kind` is the span of the identifier that is
///   expected to have been indexed at that environment.
/// This distinction comes into play for local identifiers that may have
///   overlapping shadows.
macro_rules! test_scopes {
    (
        setup { $($setup:tt)* }
        air $toks:block

        #[test]
        $name:ident == [
            $( ($obj:ident, $span:expr, $kind:expr), )*
        ];

        $( $rest:tt )*
    ) => {
        #[test]
        fn $name() {
            $($setup)*
            let ctx = air_ctx_from_toks($toks);

            let given = derive_scopes_from_asg(&ctx, $name);
            let expected = [
                $( (ObjectTy::$obj, $span, $kind), )*
            ];

            // Collection allows us to see the entire expected and given
            //   lists on assertion failure.
            // Asserting within the macro itself ensures that panics
            //   will reference the test function rather than a utility
            //   function.
            assert_eq!(
                given.collect::<Vec<_>>(),
                expected.into_iter().collect::<Vec<_>>(),
                "left: given, right: expected",
            );
        }

        test_scopes! {
            setup { $($setup)* }
            air $toks

            $($rest)*
        }
    };

    (
        setup { $($setup:tt)* }
        air $toks:block
    ) => {}
}

test_scopes! {
    setup {
        let pkg_name = spair("/pkg", S1);
        let outer = spair("outer", S3);
        let inner = spair("inner", S5);
    }

    air {
        [
            // ENV: 0 global          lexical scoping boundaries (envs)
            PkgStart(S1, pkg_name),            //- -.
              // ENV: 1 pkg                    //   :
              ExprStart(ExprOp::Sum, S2),      //   :
                // ENV: 1 pkg                  //   :
                BindIdent(outer),              // v :v
                                               //   :
                ExprStart(ExprOp::Sum, S4),    //  1: 0
                  // ENV: 1 pkg                //   :
                  BindIdent(inner),            // v :v
                ExprEnd(S6),                   //   :
              ExprEnd(S7),                     //   :
            PkgEnd(S8),                        //- -'
        ]
    }

    #[test]
    outer == [
        // The identifier is not local,
        //   and so its scope should extend into the global environment.
        (Root, S0,       Visible(S3)),

        // Expr does not introduce a new environment,
        //   and so the innermost environment in which we should be able to
        //   find the identifier is the Pkg.
        (Pkg, m(S1, S8), Visible(S3)),
    ];

    #[test]
    inner == [
        // Same as above since the environment is the same;
        //   `Expr` does not introduce a new environment.
        (Root, S0,       Visible(S5)),
        (Pkg, m(S1, S8), Visible(S5)),
    ];
}

test_scopes! {
    setup {
        let pkg_name = spair("/pkg", S1);

        let tpl_outer = spair("_tpl-outer_", S3);
        let meta_outer = spair("@param_outer@", S5);
        let expr_outer = spair("exprOuter", S8);

        let tpl_inner = spair("_tpl-inner_", S11);
        let meta_inner = spair("@param_inner@", S13);
        let expr_inner = spair("exprInner", S16);
    }

    air {
        [
            // ENV: 0 global          lexical scoping boundaries (envs)
            PkgStart(S1, pkg_name),            //- - - - -.
              // ENV: 1 pkg                    //         :
              TplStart(S2),                    //–-----.  :
                // ENV: 2 tpl                  //      |  :
                BindIdent(tpl_outer),          //      |v :v
                                               //      |  :
                MetaStart(S4),                 //      |  :
                  BindIdent(meta_outer),       //    vl|s :
                MetaEnd(S6),                   //      |  :
                                               //      |  :
                ExprStart(ExprOp::Sum, S7),    //      |  :
                  BindIdent(expr_outer),       //    vd|s :
                ExprEnd(S9),                   //      |  :
                                               //      |  :
                TplStart(S10),                 //---.  |  :
                  // ENV: 3 tpl                //   |  |  :
                  BindIdent(tpl_inner),        //   |v |s :
                                               //   |  |  :
                  MetaStart(S12),              //   |  |  :
                    BindIdent(meta_inner),     // vl|s |s :
                  MetaEnd(S14),                //   |  |  :
                                               //  3| 2| 1: 0
                  ExprStart(ExprOp::Sum, S15), //   |  |  :
                    BindIdent(expr_inner),     // vd|s |s :
                  ExprEnd(S17),                //   |  |  :
                TplEnd(S18),                   //---'  |  :   v,s = EnvScopeKind
              TplEnd(S19),                     //–-----'  :   |
            PkgEnd(S20),                       //- - - - -'   |`- l = local
        ]  //                                      ^           `- d = defer
           //      observe: - (l)ocal shadows until root
           //               - (d)efer shadows until root
           //               - visual >|> shadow
           //               - visual >:> visual (pool)
           //               - shadow >|> shadow
           //               - shadow >:> (no visual/pool)
    }

    #[test]
    tpl_outer == [
        // The template is defined at the package level,
        //   and so is incorporated into the global environment.
        (Root, S0,        Visible(S3)),

        // Definition environment.
        (Pkg, m(S1, S20), Visible(S3)),
    ];

    #[test]
    meta_outer == [
        // The metavariable is local to the template,
        //   and so is not scoped outside of it.
        // It does not contribute to the global scope,
        //   however we must introduce shadow records so that we're able to
        //   provide an error if shadowing would occur due to another
        //   identifier of the same name,
        //     such as a template within another template.
        // Root never contains shadow records since it is not part of a
        //   hierarchy,
        //     so it is omitted from the metavariable's scope.
        (Pkg, m(S1, S20), Shadow (S5)),
        (Tpl, m(S2, S19), Visible(S5)),
    ];

    #[test]
    expr_outer == [
        // Expressions defined within templates will eventually be scoped to
        //   their _expansion site_.
        // Since the future scope cannot possibly be known until the point
        //   of expansion,
        //     we don't know what its parent environment will be.
        //
        // Why, then, does it shadow?
        //
        // Templates in TAMER
        //   (unlike in the original XSLT-based TAME)
        //   are designed to _close_ over their definition environment.
        // If a template references a value defined within the scope of its
        //   definition
        //     (e.g. an identifier imported into the package into which the
        //       template itself was defined),
        //     the intent is to be able to utilize that identifier at the
        //     expansion site without having to break encapsulation by
        //     having to know implementation details of the template;
        //       this awkward problem is the reason for `import/@export`,
        //         so that packages templates could re-export their symbols
        //         to avoid this trap,
        //           which is far too heavy-handed of an approach and is
        //           easily forgotten.
        // In that sense,
        //   templates act more like how one would expect functions to
        //   operate.
        //
        // Because of that lexical capture,
        //   it is important that identifiers shadow to ensure that we do
        //   not rebind an identifier without the user realizing it.
        // The intent is that the system should just do the right thing
        //   unless there happens to be a problem.
        // If a user references an identifier from the outer scope,
        //   the intent is almost certainly to have it be lexically captured
        //   and available at the expansion site.
        // If an identifier is unknown,
        //   perhaps the intent is to have it defined by another template,
        //   or to be defined at the expansion site.
        // And if the situation changes from the second to the first because
        //   of the introduction of an import or a duplicate identifier,
        //     we want to help the user at the earliest possible moment.
        (Pkg, m(S1, S20), Shadow (S8)),
        (Tpl, m(S2, S19), Visible(S8)),
    ];

    #[test]
    tpl_inner == [
        // This is similar to `expr_outer` above.
        // Even though the template is entirely scoped within the parent
        //   `tpl_outer` such that it isn't even defined until it is expanded,
        //     at which point it is defined within its expansion context,
        //     we still want shadow records so that any _references_ to this
        //       template can be resolved unambiguously in ways that are
        //       helpful to the user
        //         (see `expr_outer` above for more information).
        (Pkg, m(S1, S20), Shadow (S11)),
        (Tpl, m(S2, S19), Visible(S11)),
    ];

    #[test]
    meta_inner == [
        // Just as the previous metavariable,
        //   we need to cast a shadow all the way up to the package level to
        //   ensure that we do not permit identifier shadowing.
        // See `meta_outer` above for more information.
        (Pkg, m(S1,  S20), Shadow (S13)),
        (Tpl, m(S2,  S19), Shadow (S13)),
        (Tpl, m(S10, S18), Visible(S13)),
    ];

    #[test]
    expr_inner == [
        // Just the same as the previous expression.
        // Note the intended consequence of this:
        //   if `tpl_outer` contains an identifier,
        //     it cannot be shadowed by `tpl_inner`.
        (Pkg, m(S1,  S20), Shadow (S16)),
        (Tpl, m(S2,  S19), Shadow (S16)),
        (Tpl, m(S10, S18), Visible(S16)),
    ];
}

test_scopes! {
    setup {
        let pkg_name = spair("/pkg", S1);

        let tpl_outer = spair("_tpl-outer_", S3);
        let tpl_inner = spair("_tpl-inner_", S9);

        // Note how these have the _same name_.
        let meta_name = "@param@";
        let meta_same_a = spair(meta_name, S5);
        let meta_same_b = spair(meta_name, S11);

        // This one will be used for asserting.
        let meta_same = spair(meta_name, S11);
    }

    air {
        // Note that,
        //   unlike the above set of tests,
        //   these templates are _siblings_.
        [
            // ENV: 0 global          lexical scoping boundaries (envs)
            PkgStart(S1, pkg_name),            //- - - -.
              // ENV: 1 pkg                    //       :
              TplStart(S2),                    //----.  :
                // ENV: 2 tpl                  //    |  :
                BindIdent(tpl_outer),          //    |~ :
                                               //    |  :
                MetaStart(S4),                 //    |  :
                  BindIdent(meta_same_a),      //  vl|s :   <--.
                MetaEnd(S6),                   //    |  :      |
              TplEnd(S7),                      //----'  :      |
                                               //       :      |s
              TplStart(S8),                    //----.  :      |a
                // ENV: 3 tpl                  //    |  :      |m
                BindIdent(tpl_inner),          //    |~ :      |e
                                               //    |  :      |
                MetaStart(S10),                //    |  :      |
                  BindIdent(meta_same_b),      //  vl|s :   <--'
                MetaEnd(S12),                  //    |  :
              TplEnd(S13),                     //----'  :    ~ = ignored for
            PkgEnd(S14),                       //- - - -'        these tests
        ]
    }

    // Detailed information on metavariables is present in previous tests.
    // We focus here only on the fact that these definitions were permitted
    //   to occur since identifiers of the same name have overlapping
    //   shadows.

    // Keep in mind that this test is a filtering of an ontological tree,
    //   so this is ordered as such and does not contain duplicate objects.
    #[test]
    meta_same == [
        // A shadow is cast by both `meta_same_a` and `meta_same_b`.
        // When they intersect,
        //   we must make a choice:
        //
        //   (a) Index both of them; or
        //   (b) Keep only one of them.
        //
        // At the time of writing,
        //   the choice was (b).
        // But by keeping only one identifier indexed,
        //   we do lose information and therefore will only be able to
        //     present one diagnostic error in the event that we later
        //     discover that the metavariables shadow another identifier
        //     that is visible in scope.
        // This would present only one error at a time to the user,
        //   depending on how they chose to resolve it,
        //   and so is not ideal;
        //     the solution may eventually move to (a) to retain this
        //     important information.
        //
        // Since (b) was chosen,
        //   which do we keep?
        // This choice is not important,
        //   since in the event of a future error we'll still be providing
        //     correct
        //       (albeit incomplete)
        //       information to the user,
        //   but it is defined by implementation details,
        //     and so is subject to change.
        // At the time of writing,
        //   because of how indexing is carried out,
        //   we retain the shadow of the _first_ encountered identifier.
        (Pkg, m(S1, S14), Shadow (S5 )),

        // The first identifier of this name is found in the first
        //   template.
        // Its shadow is discussed above.
        (Tpl, m(S2, S7),  Visible(S5 )),

        // And the second identifier in the second template,
        //   with its shadow also discussed above.
        // As noted atop this test,
        //   we do not have duplicate objects in this test data,
        //   and so the `Pkg` object above is not duplicated despite two
        //   shadows being cast.
        (Tpl, m(S8, S13), Visible(S11)),
    ];
}

// From the perspective of the linker (tameld):
test_scopes! {
    setup {
        let pkg_a = spair("/pkg/a", S1);
        let opaque_a = spair("opaque_a", S2);

        let pkg_b = spair("/pkg/b", S4);
        let opaque_b = spair("opaque_b", S5);
    }

    air {
        [
            // ENV: 0 global          lexical scoping boundaries (envs)
            PkgStart(S1, pkg_a),               //- -.
              // ENV: 1 pkg                    //   :
              IdentDecl(                       // v :v
                  opaque_a,                    //   :
                  IdentKind::Meta,             //   :
                  Default::default(),          //   :
              ),                               //  1:
            PkgEnd(S3),                        //- -'
                                               //     0
            PkgStart(S4, pkg_b),               //- -.
              // ENV: 1 pkg                    //  1:
              IdentDecl(                       // v :v
                  opaque_b,                    //   :
                  IdentKind::Meta,             //   :
                  Default::default(),          //   :
              ),                               //   :
            PkgEnd(S6),                        //- -'
        ]
    }

    #[test]
    opaque_a == [
        (Root, S0,        Visible(S2)),
        (Pkg,  m(S1, S3), Visible(S2)),
    ];

    #[test]
    opaque_b == [
        (Root, S0,        Visible(S5)),
        (Pkg,  m(S4, S6), Visible(S5)),
    ];
}

///// Tests end above this line, plumbing below /////

/// Independently derive identifier scopes from the graph.
///
/// This will search the graph for all environments in which `name` has been
///   indexed,
///     gather information about those environments,
///     and compare them against `expected`.
/// The environment listing is expected to be in ontological order,
///   as by [`tree_reconstruction`].
///
/// This function is essential to providing easily understood,
///   declarative scope test definitions,
///   which make it easy to form and prove hypotheses about the behavior of
///   TAMER's scoping system.
fn derive_scopes_from_asg<'a>(
    ctx: &'a <AirAggregate as ParseState>::Context,
    name: SPair,
) -> impl Iterator<Item = (ObjectTy, Span, EnvScopeKind<Span>)> + 'a {
    // We use what was most convenient at the time of writing to gather
    //   environments representing the scope of `name`.
    // This is not the most efficient,
    //   but our test graphs are quite small,
    //   and so that won't matter.
    //
    // The reason that this works is because the traversal will visit
    //   objects following the graph's ontology,
    //     which will produce a tree in the expected order.
    // We filter on index lookup,
    //   which discards the portions of the tree
    //     (the graph)
    //     that we are not interested in.
    //
    // This also means that a failure to extend the scope of `name` to a
    //   particular environment will cause it to be omitted from this
    //   iterator,
    //     but that is okay;
    //       it'll result in a test failure that should be easy enough to
    //       understand.
    let given_without_root = tree_reconstruction(ctx.asg_ref()).filter_map(
        move |TreeWalkRel(dynrel, _)| {
            dynrel.target_oi_rel_to_dyn::<object::Ident>().map(|oi_to| {
                (
                    dynrel.target_ty(),
                    dynrel.target().resolve(ctx.asg_ref()).span(),
                    ctx.env_scope_lookup_raw(oi_to, name),
                )
            })
        },
    );

    // `tree_reconstruction` omits root,
    //   so we'll have to add it ourselves.
    let oi_root = ctx.asg_ref().root(name);

    once((ObjectTy::Root, S0, ctx.env_scope_lookup_raw(oi_root, name)))
        .chain(given_without_root)
        .filter_map(|(ty, span, oeoi)| {
            oeoi.map(|eoi| {
                (ty, span, eoi.map(ObjectIndex::cresolve(ctx.asg_ref())))
            })
        })
        // We discard the inner ObjectIndex since it is not relevant for the
        //   test assertion.
        .map(|(ty, span, eid)| (ty, span, eid.map(|id| id.span())))
}
