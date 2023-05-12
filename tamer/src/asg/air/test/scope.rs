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

#[test]
fn pkg_child_definition() {
    let pkg_name = SPair("/pkg".into(), S1);
    let name = SPair("foo".into(), S3);

    #[rustfmt::skip]
    let toks = vec![
        // ENV: 0 global
        PkgStart(S1, pkg_name),
          // ENV: 1 pkg
          ExprStart(ExprOp::Sum, S2),
            // ENV: 1 pkg
            BindIdent(name),
          ExprEnd(S4),
        PkgEnd(S5),
    ];

    let asg = asg_from_toks_raw(toks);

    #[rustfmt::skip]
    assert_scope(&asg, name, [
        // The identifier is not local,
        //   and so its scope should extend into the global environment.
        // TODO: (Root, S0, Pool),

        // Expr does not introduce a new environment,
        //   and so the innermost environment in which we should be able to
        //   find the identifier is the Pkg.
        (Pkg, m(S1, S5), Visible)
    ]);
}

#[test]
fn pkg_nested_expr_definition() {
    let pkg_name = SPair("/pkg".into(), S1);
    let outer = SPair("outer".into(), S3);
    let inner = SPair("inner".into(), S5);

    #[rustfmt::skip]
    let toks = vec![
        // ENV: 0 global
        PkgStart(S1, pkg_name),
          // ENV: 1 pkg
          ExprStart(ExprOp::Sum, S2),
            // ENV: 1 pkg
            BindIdent(outer),

            ExprStart(ExprOp::Sum, S4),
              // ENV: 1 pkg
              BindIdent(inner),
            ExprEnd(S6),
          ExprEnd(S7),
        PkgEnd(S8),
    ];

    let asg = asg_from_toks_raw(toks);

    #[rustfmt::skip]
    assert_scope(&asg, inner, [
        // The identifier is not local,
        //   and so its scope should extend into the global environment.
        // TODO: (Root, S0, Pool),

        // Expr does not introduce a new environment,
        //   and so just as the outer expression,
        //   the inner is scoped to a package environment.
        (Pkg, m(S1, S8), Visible)
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
        // ENV: 0 global
        PkgStart(S1, pkg_name),
          // ENV: 1 pkg
          TplStart(S2),
            // ENV: 2 tpl
            BindIdent(tpl_outer),

            TplMetaStart(S4),
              BindIdent(meta_outer),
            TplMetaEnd(S6),

            ExprStart(ExprOp::Sum, S7),
              BindIdent(expr_outer),
            ExprEnd(S9),

            TplStart(S10),
              // ENV: 3 tpl
              BindIdent(tpl_inner),

              TplMetaStart(S12),
                BindIdent(meta_inner),
              TplMetaEnd(S14),

              ExprStart(ExprOp::Sum, S15),
                BindIdent(expr_inner),
              ExprEnd(S17),
            TplEnd(S18),
          TplEnd(S19),
        PkgEnd(S20),
    ];

    let asg = asg_from_toks_raw(toks);

    #[rustfmt::skip]
    assert_scope(&asg, tpl_outer, [
        // TODO: (Root, S0, Pool),
        (Pkg, m(S1, S20), Visible)
    ]);
    #[rustfmt::skip]
    assert_scope(&asg, meta_outer, [
        // TODO: (Tpl, m(S2, S19), Visible)
    ]);
    #[rustfmt::skip]
    assert_scope(&asg, expr_outer, [
        (Tpl, m(S2, S19), Visible)
    ]);

    #[rustfmt::skip]
    assert_scope(&asg, tpl_inner, [
        (Tpl, m(S2, S19), Visible)
    ]);
    #[rustfmt::skip]
    assert_scope(&asg, meta_inner, [
        // TODO: (Tpl, m(S10, S18), Visible)
    ]);
    #[rustfmt::skip]
    assert_scope(&asg, expr_inner, [
        (Tpl, m(S10, S18), Visible)
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
    expected: impl IntoIterator<Item = (ObjectTy, Span, EnvScopeKind)>,
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
                    asg.lookup(oi_to, name),
                )
            })
        });

    // `tree_reconstruction` omits root,
    //   so we'll have to add it ourselves.
    let oi_root = asg.root(name);
    let given = once((Root, S0, asg.lookup(oi_root, name)))
        .chain(given_without_root)
        .filter_map(|(ty, span, ooi)| ooi.map(|oi| (ty, span, oi.resolve(asg))))
        .inspect(|(ty, span, ident)| assert_eq!(
            expected_span,
            ident.span(),
            "expected {wname} span {expected_span} at {ty}:{span}, but found {given}",
            wname = TtQuote::wrap(name),
            given = ident.span(),
        ))
        // TODO
        .map(|(ty, span, _)| (ty, span, EnvScopeKind::Visible));

    // Collection allows us to see the entire expected and given lists on
    //   assertion failure.
    assert_eq!(
        expected.into_iter().collect::<Vec<_>>(),
        given.collect::<Vec<_>>(),
    )
}
