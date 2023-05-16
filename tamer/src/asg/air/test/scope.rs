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
    fmt::{DisplayWrapper, TtQuote},
    span::UNKNOWN_SPAN,
};
use std::iter::once;

use EnvScopeKind::*;
use ObjectTy::*;

const S0: Span = UNKNOWN_SPAN;

fn m(a: Span, b: Span) -> Span {
    a.merge(b).unwrap()
}

/// Apply [`assert_scope()`] without concern for the inner type or value of
///   the expected [`EnvScopeKind`].
macro_rules! assert_scope {
    (
        $asg:ident, $name:ident, [
            $( ($obj:ident, $span:expr, $kind:ident), )*
        ]
    ) => {
        assert_scope(&$asg, $name, [
            $( ($obj, $span, $kind(())), )*
        ])
    }
}

#[test]
fn pkg_nested_expr_definition() {
    let pkg_name = SPair("/pkg".into(), S1);
    let outer = SPair("outer".into(), S3);
    let inner = SPair("inner".into(), S5);

    #[rustfmt::skip]
    let toks = vec![
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
    ];

    let asg = asg_from_toks_raw(toks);

    #[rustfmt::skip]
    assert_scope!(asg, outer, [
        // The identifier is not local,
        //   and so its scope should extend into the global environment.
        // TODO: (Root, S0, Visible),

        // Expr does not introduce a new environment,
        //   and so the innermost environment in which we should be able to
        //   find the identifier is the Pkg.
        (Pkg, m(S1, S8), Visible),
    ]);

    #[rustfmt::skip]
    assert_scope!(asg, inner, [
        // The identifier is not local,
        //   and so its scope should extend into the global environment.
        // TODO: (Root, S0, Visible),

        // Expr does not introduce a new environment,
        //   and so just as the outer expression,
        //   the inner is scoped to a package environment.
        (Pkg, m(S1, S8), Visible),
    ]);
}

#[test]
fn pkg_tpl_definition() {
    let pkg_name = SPair("/pkg".into(), S1);

    let tpl_outer = SPair("_tpl-outer_".into(), S3);
    let meta_outer = SPair("@param_outer@".into(), S5);
    let expr_outer = SPair("exprOuter".into(), S8);

    let tpl_inner = SPair("_tpl-inner_".into(), S11);
    let meta_inner = SPair("@param_inner@".into(), S13);
    let expr_inner = SPair("exprInner".into(), S16);

    #[rustfmt::skip]
    let toks = vec![
        // ENV: 0 global          lexical scoping boundaries (envs)
        PkgStart(S1, pkg_name),            //- - - - -.
          // ENV: 1 pkg                    //         :
          TplStart(S2),                    //–-----.  :
            // ENV: 2 tpl                  //      |  :
            BindIdent(tpl_outer),          //      |v :v
                                           //      |  :
            TplMetaStart(S4),              //      |  :
              BindIdent(meta_outer),       //    vl|s :
            TplMetaEnd(S6),                //      |  :
                                           //      |  :
            ExprStart(ExprOp::Sum, S7),    //      |  :
              BindIdent(expr_outer),       //    vd|s :
            ExprEnd(S9),                   //      |  :
                                           //      |  :
            TplStart(S10),                 //---.  |  :
              // ENV: 3 tpl                //   |  |  :
              BindIdent(tpl_inner),        //   |v |s :
                                           //   |  |  :
              TplMetaStart(S12),           //   |  |  :
                BindIdent(meta_inner),     // vl|s |s :
              TplMetaEnd(S14),             //   |  |  :
                                           //  3| 2| 1: 0
              ExprStart(ExprOp::Sum, S15), //   |  |  :
                BindIdent(expr_inner),     // vd|s |s :
              ExprEnd(S17),                //   |  |  :
            TplEnd(S18),                   //---'  |  :   v,s = EnvScopeKind
          TplEnd(S19),                     //–-----'  :   |
        PkgEnd(S20),                       //- - - - -'   |`- l = local
    ]; //                                      ^           `- d = defer
       //      observe: - (l)ocal shadows until root
       //               - (d)efer shadows until root
       //               - visual >|> shadow
       //               - visual >:> visual (pool)
       //               - shadow >|> shadow
       //               - shadow >:> (no visual/pool)

    let asg = asg_from_toks_raw(toks);

    #[rustfmt::skip]
    assert_scope!(asg, tpl_outer, [
        // The template is defined at the package level,
        //   and so is incorporated into the global environment.
        // TODO: (Root, S0, Visible),

        // Definition environment.
        (Pkg, m(S1, S20), Visible),
    ]);
    #[rustfmt::skip]
    assert_scope!(asg, meta_outer, [
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
        // TODO: (Pkg, m(S1, S20), Shadow),
        // TODO: (Tpl, m(S2, S19), Visible),
    ]);
    #[rustfmt::skip]
    assert_scope!(asg, expr_outer, [
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
        (Pkg, m(S1, S20), Shadow),
        (Tpl, m(S2, S19), Visible),
    ]);

    #[rustfmt::skip]
    assert_scope!(asg, tpl_inner, [
        // This is similar to `expr_outer` above.
        // Even though the template is entirely scoped within the parent
        //   `tpl_outer` such that it isn't even defined until it is expanded,
        //     at which point it is defined within its expansion context,
        //     we still want shadow records so that any _references_ to this
        //       template can be resolved unambiguously in ways that are
        //       helpful to the user
        //         (see `expr_outer` above for more information).
        (Pkg, m(S1, S20), Shadow),
        (Tpl, m(S2, S19), Visible),
    ]);
    #[rustfmt::skip]
    assert_scope!(asg, meta_inner, [
        // Just as the previous metavariable,
        //   we need to cast a shadow all the way up to the package level to
        //   ensure that we do not permit identifier shadowing.
        // See `meta_outer` above for more information.
        // TODO: (Pkg, m(S1, S20), Shadow),
        // TODO: (Tpl, m(S2, S19), Shadow),
        // TODO: (Tpl, m(S10, S18), Visible),
    ]);
    #[rustfmt::skip]
    assert_scope!(asg, expr_inner, [
        // Just the same as the previous expression.
        // Note the intended consequence of this:
        //   if `tpl_outer` contains an identifier,
        //     it cannot be shadowed by `tpl_inner`.
        (Pkg, m(S1, S20), Shadow),
        (Tpl, m(S2, S19), Shadow),
        (Tpl, m(S10, S18), Visible),
    ]);
}

///// Tests end above this line, plumbing begins below /////

/// Assert that the scope of the identifier named `name` is that of the
///   provided environment list `expected`.
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
fn assert_scope(
    asg: &Asg,
    name: SPair,
    expected: impl IntoIterator<Item = (ObjectTy, Span, EnvScopeKind<()>)>,
) {
    // We are interested only in identifiers for scoping,
    //   not the objects that they point to.
    // We will use the span of the identifier that we locate via index
    //   lookups to determine whether we've found the right one.
    // This relies on the tests using unique spans for each object on the
    //   graph,
    //     which is standard convention for TAMER's tests.
    let expected_span = name.span();

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
    let given_without_root =
        tree_reconstruction(asg).filter_map(|TreeWalkRel(dynrel, _)| {
            dynrel.target_oi_rel_to_dyn::<object::Ident>().map(|oi_to| {
                (
                    dynrel.target_ty(),
                    dynrel.target().resolve(asg).span(),
                    asg.lookup_raw(oi_to, name),
                )
            })
        });

    // `tree_reconstruction` omits root,
    //   so we'll have to add it ourselves.
    let oi_root = asg.root(name);
    let given = once((Root, S0, asg.lookup_raw(oi_root, name)))
        .chain(given_without_root)
        .filter_map(|(ty, span, oeoi)| {
            oeoi.map(|eoi| (ty, span, eoi.map(ObjectIndex::cresolve(asg))))
        })
        .inspect(|(ty, span, eid)| assert_eq!(
            expected_span,
            eid.as_ref().span(),
            "expected {wname} span {expected_span} at {ty}:{span}, but found {given}",
            wname = TtQuote::wrap(name),
            given = eid.as_ref().span(),
        ))
        // We discard the inner ObjectIndex since it is not relevant for the
        //   test assertion.
        .map(|(ty, span, eid)| (ty, span, eid.map(|_| ())));

    // Collection allows us to see the entire expected and given lists on
    //   assertion failure.
    assert_eq!(
        expected.into_iter().collect::<Vec<_>>(),
        given.collect::<Vec<_>>(),
    )
}
