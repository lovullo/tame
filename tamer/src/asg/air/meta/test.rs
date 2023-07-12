// Tests for ASG IR metavariable parsing
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

/// Metavariable parsing tests.
///
/// Note that some metavariable-related tests are in
///   [`super::super::tpl::test`],
///     where the focus is on how they are integrated as template
///     parameters.
/// The tests here focus instead on the particulars of metavariables
///   themselves,
///     with templates being only incidental.
use super::*;
use crate::{
    asg::{
        air::{
            test::{parse_as_pkg_body, pkg_lookup},
            Air::{self, *},
        },
        graph::object::{meta::MetaRel, Meta, Tpl},
    },
    parse::{util::spair, Parser},
    span::{dummy::*, Span},
    sym::SymbolId,
};

type Sut = AirAggregate;

// Metavariables can reference the lexical value of other metavariables.
// This does not actually check that the target reference is a metavariable,
//   at least not at the time of writing.
// TODO: But the system ought to,
//   since if we defer that to expansion-time,
//   then we don't have confidence that the template definition is actually
//     valid as a part of this compilation unit.
#[test]
fn metavar_ref_only() {
    let name_meta = "@foo@";
    let name_other = "@other@";

    #[rustfmt::skip]
    assert_concat_list(
        [
            MetaStart(S1),
              BindIdent(spair(name_meta, S2)),

              // Reference to another metavariable,
              //   effectively creating an alias.
              RefIdent(spair(name_other, S3)),    // --.
            MetaEnd(S4),                          //   |
                                                  //   |
            MetaStart(S5),                        //   |
              BindIdent(spair(name_other, S6)),   // <-'
            MetaEnd(S7),
        ],

        name_meta,
        S1.merge(S4),

        [
            &Meta::Required(S5.merge(S7).unwrap()),
        ],
    );
}

// Similar to above,
//   but multiple references.
#[test]
fn metavar_ref_multiple() {
    let name_meta = "@foo@";
    let name_other_a = "@other-a@";
    let name_other_b = "@other-b@";

    #[rustfmt::skip]
    assert_concat_list(
        // Def/ref is commutative,
        //   so this defines with both orderings to demonstrate that.
        [
            MetaStart(S1),
              BindIdent(spair(name_other_a, S2)),  // <-.
            MetaEnd(S3),                           //   |
                                                   //   |
            MetaStart(S3),                         //   |
              BindIdent(spair(name_meta, S4)),     //   |
                                                   //   |
              RefIdent(spair(name_other_a, S5)),   // --'
              RefIdent(spair(name_other_b, S6)),   // --.
            MetaEnd(S7),                           //   |
                                                   //   |
            MetaStart(S8),                         //   |
              BindIdent(spair(name_other_b, S9)),  // <-'
            MetaEnd(S10),
        ],

        name_meta,
        S3.merge(S7),

        [
            &Meta::Required(S1.merge(S3).unwrap()),
            &Meta::Required(S8.merge(S10).unwrap()),
        ],
    );
}

// If a metavariable already has a concrete lexical value,
//   then appending a reference to it should convert it into a list.
#[test]
fn metavar_ref_after_lexeme() {
    let name_meta = "@foo@";
    let value = "foo value";
    let name_other = "@other@";

    #[rustfmt::skip]
    assert_concat_list(
        [
            MetaStart(S1),
              BindIdent(spair(name_meta, S2)),

              // At this point,
              //   we have only a concrete lexeme,
              //   and so we are not a list.
              MetaLexeme(spair(value, S3)),

              // Upon encountering this token,
              //   we should convert into a list.
              RefIdent(spair(name_other, S4)),    // --.
            MetaEnd(S5),                          //   |
                                                  //   |
            MetaStart(S6),                        //   |
              BindIdent(spair(name_other, S7)),   // <-'
            MetaEnd(S8),
        ],

        name_meta,
        S1.merge(S5),

        // Having been converted into a list,
        //   we should have a reference to _two_ metavariables:
        //     one holding the original lexeme and the reference.
        [
            &Meta::Lexeme(S3, spair(value, S3)),
            &Meta::Required(S6.merge(S8).unwrap()),
        ],
    );
}

// If we have a reference,
//   then that means we already have a `ConcatList`,
//   and therefore should just have to append to it.
// This is the same as the above test,
//   with operations reversed.
// It is not commutative,
//   though,
//   since concatenation is ordered.
#[test]
fn lexeme_after_metavar_ref() {
    let name_meta = "@foo@";
    let value = "foo value";
    let name_other = "@other@";

    #[rustfmt::skip]
    assert_concat_list(
        [
            MetaStart(S1),
              BindIdent(spair(name_meta, S2)),

              // We produce a list here...
              RefIdent(spair(name_other, S3)),   // --.
                                                 //   |
              // ...and should just append a     //   |
              //   `Lexeme` here.                //   |
              MetaLexeme(spair(value, S4)),      //   |
            MetaEnd(S5),                         //   |
                                                 //   |
            MetaStart(S6),                       //   |
              BindIdent(spair(name_other, S7)),  // <-'
            MetaEnd(S8),
        ],

        name_meta,
        S1.merge(S5),

        // Same as the previous test,
        //   but concatenation order is reversed.
        [
            &Meta::Required(S6.merge(S8).unwrap()),

            // Note the first span here is not that of the parent
            //   metavariable,
            //    since we do not want diagnostics to suggest that this
            //    object represents the entirety of the parent.
            &Meta::Lexeme(S4, spair(value, S4)),
        ],
    );
}

// Multiple lexemes should _also_ produce a list.
// While this could have been replaced by a single lexeme,
//   there are still uses:
//     maybe this was the result of template expansion,
//       or simply a multi-line formatting choice in the provided sources.
#[test]
fn lexeme_after_lexeme() {
    let name_meta = "@foo@";
    let value_a = "foo value a";
    let value_b = "foo value b";

    #[rustfmt::skip]
    assert_concat_list(
        [
            MetaStart(S1),
              BindIdent(spair(name_meta, S2)),

              MetaLexeme(spair(value_a, S3)),
              MetaLexeme(spair(value_b, S4)),
            MetaEnd(S5),
        ],

        name_meta,
        S1.merge(S5),

        [
            // Since these are Meta objects derived from the original,
            //   our spans
            //     (the first value in the tuple)
            //     are not that of the containing metavariable;
            //       we don't want diagnostics implying that this represents
            //       the entirety of that the parent metavariable.
            &Meta::Lexeme(S3, spair(value_a, S3)),
            &Meta::Lexeme(S4, spair(value_b, S4)),
        ],
    );
}

///////                                     ///////
/////// Tests above; plumbing begins below  ///////
///////                                     ///////

fn assert_concat_list<'a, IT, IE: 'a>(
    toks: IT,
    meta_name: impl Into<SymbolId>,
    expect_span: Option<Span>,
    expect: IE,
) where
    IT: IntoIterator<Item = Air>,
    IT::IntoIter: Debug,
    IE: IntoIterator<Item = &'a Meta>,
    IE::IntoIter: Debug + DoubleEndedIterator,
{
    let (ctx, oi_tpl) = air_ctx_from_tpl_body_toks(toks);
    let asg = ctx.asg_ref();

    let oi_meta = ctx
        .env_scope_lookup_ident_dfn::<Meta>(
            oi_tpl,
            spair(meta_name.into(), DUMMY_SPAN),
        )
        .unwrap();

    // Meta references are only supported through lexical concatenation.
    assert_eq!(
        &Meta::ConcatList(expect_span.expect("expected metavar span is None")),
        oi_meta.resolve(asg),
        "expected metavariable to be a ConcatList with the provided span",
    );

    // We `collect()` rather than using `Iterator::eq` so that the failure
    //   message includes the data.
    // If we do this too often,
    //   we can consider a crate like `itertools` or write our own
    //   comparison,
    //     but it's not worth doing for these tests where the cost of
    //     collection is so low and insignificant.
    assert_eq!(
        // TODO: We need to modify edge methods to return in the proper
        //   order (not reversed) without a performance hit,
        //     which will involve investigating Petgraph further.
        expect.into_iter().rev().collect::<Vec<_>>(),
        // Each reference is to an Ident whose definition is the other
        //   metavariable.
        oi_meta
            .edges(asg)
            .filter_map(|rel| match rel {
                MetaRel::Meta(oi) => Some(oi),
                MetaRel::Ident(oi) => oi.definition::<Meta>(asg),
                MetaRel::Doc(_) => None,
            })
            .map(ObjectIndex::cresolve(asg))
            .collect::<Vec<_>>(),
        "note: expected tokens are in reverse order in this error message",
    );
}

const STUB_TPL_NAME: &str = "__incidental-tpl__";

/// Parse the provided tokens within the context of a template body.
///
/// This allows tests to focus on testing of metavariables instead of
///   mudding tests with setup.
///
/// This creates a package and template that are purely incidental and serve
///   only as scaffolding to put the parser into the necessary state and
///   context.
/// This is an alternative to providing methods to force the parse into a
///   certain context,
///     since we're acting as users of the SUT's public API and are
///     therefore testing real-world situations.
fn parse_as_tpl_body<I: IntoIterator<Item = Air>>(
    toks: I,
) -> Parser<Sut, impl Iterator<Item = Air> + Debug>
where
    <I as IntoIterator>::IntoIter: Debug,
{
    #[rustfmt::skip]
    let head = [
        TplStart(S1),
          BindIdent(spair(STUB_TPL_NAME, S1)),
    ];

    #[rustfmt::skip]
    let tail = [
        TplEnd(S1),
    ];

    #[rustfmt::skip]
    parse_as_pkg_body(
        head.into_iter()
        .chain(toks.into_iter())
        .chain(tail.into_iter())
    )
}

fn air_ctx_from_tpl_body_toks<I: IntoIterator<Item = Air>>(
    toks: I,
) -> (<Sut as ParseState>::Context, ObjectIndex<Tpl>)
where
    I::IntoIter: Debug,
{
    let mut sut = parse_as_tpl_body(toks);
    assert!(sut.all(|x| x.is_ok()));

    let ctx = sut.finalize().unwrap().into_private_context();

    let oi_tpl = pkg_lookup(&ctx, spair(STUB_TPL_NAME, S1))
        .expect(
            "could not locate stub template (did you call \
            air_ctx_from_tpl_body_toks without parse_as_tpl_body?)",
        )
        .definition(ctx.asg_ref())
        .expect("missing stub template definition (test setup bug?)");

    (ctx, oi_tpl)
}
