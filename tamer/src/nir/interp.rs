// Interpolation parser for desugaring NIR
//
//  Copyright (C) 2014-2022 Ryan Specialty Group, LLC.
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

//! Interpolation parser for desugaring NIR.
//!
//! String interpolation occurs for attributes containing curly braces
//!   (`{` and `}`)
//!   during TAME's parsing phase,
//!     before template expansion.
//! An attribute containing curly braces is referred to in TAME as an
//!   _interpolation specification_.
//!
//! Interpolation is used as a form of short-hand syntactic sugar for
//!   concatenation of string literals and template metavariables,
//!     whose result is then processed by the template system.
//! For example,
//!   consider the following code:
//!
//! ```xml
//!   <c:value-of name="foo{@bar@}baz" />
//! ```
//!
//! The string `foo{@bar@}baz` is the interpolation specification.
//! This ends up desugaring into the [`Nir`] equivalent of this:
//!
//! ```xml
//!   <param name="@___dsgr_01@"
//!          desc="Generated from interpolated string `foo{@bar@}baz`">
//!     <text>foo</text>
//!     <param-value name="@bar@" />
//!     <text>baz</text>
//!   </param>
//!
//!   <c:value-of name="@___dsgr_01@" />
//!   <!--              ^^^^^^^^^^^^
//!                     replacement -->
//! ```
//!
//! Since interpolation currently supports only string literals and template
//!   metavariables within specifications,
//!     they are only semantically valid within the context of a template
//!     definition.
//! This desugaring process does not check for this context;
//!   errors would occur later on in the lowering pipeline.
//!
//! Since interpolation desugars into [`Nir`],
//!   and not source XML,
//!   generated `param`s will be automatically be interpreted downstream in
//!     the lowering pipeline as if they were hoisted to the template
//!     definition header.
//!
//! If a string does not require interpolation,
//!   then it is interpreted as a literal within the context of the template
//!   system and is echoed back unchanged.
//!
//! Desugared Spans
//! ---------------
//! [`Span`]s for the generated tokens are derived from the specification
//!   string.
//! In the above example,
//!   we have:
//!
//! ```xml
//!   <!--
//!     foo{@bar@}baz
//!     [-] [---] [-]
//!      A    B    C
//!   -->
//!
//!   <text>foo</text>
//!   <!--   A     -->
//!
//!   <param-value name="@bar@">
//!   <!--       B           -->
//!
//!   <text>baz</text>
//!   <!--   C     -->
//! ```
//!
//! This means that any errors that subsequently occur due to contextual
//!   issues will be mapped back to a source location that makes sense to
//!   the user with a high level of granularity.

use memchr::memchr2;

use super::{Nir, NirEntity};
use crate::{
    diagnose::{panic::DiagnosticPanic, Annotate, AnnotatedSpan, Diagnostic},
    f::Functor,
    fmt::{DisplayWrapper, TtQuote},
    parse::{prelude::*, util::SPair, NoContext},
    span::Span,
    sym::{
        st::quick_contains_byte, GlobalSymbolIntern, GlobalSymbolResolve,
        SymbolId,
    },
};
use std::{error::Error, fmt::Display};

// Expose variants for enums defined in this module to reduce verbosity.
use InterpState::*;

/// A generated identifier.
#[derive(Debug, PartialEq, Eq)]
pub struct GenIdentSymbolId(SymbolId);

/// A dereferenced [`SymbolId`] representing an interpolation specification.
///
/// This saves us from having to continuously dereference the symbol for
///   each state change.
type SpecSlice = &'static str;

/// Offset within a [`SpecSlice`] to begin parsing at for the current
///   [`InterpState`].
type SpecOffset = usize;

/// Interpolation desugaring operation.
///
/// This parser continuously yields the provided interpolation specification
///   token as lookahead until it has completed its parsing,
///     allowing it to stream without buffering expansion tokens.
///
/// The parser has two primary contexts:
///
///   1. The outer literal context represented by [`ParseLiteralAt`]; and
///   2. The inner interpolation context
///        (conceptually between curly braces)
///        represented by [`ParseInterpAt`].
///
/// For more information,
///   see the [parent module](super).
#[derive(Debug, PartialEq, Eq, Default)]
pub enum InterpState {
    /// The next token will be inspected to determine whether it requires
    ///   interpolation.
    #[default]
    Ready,

    /// Genearate an identifier for the expansion template parameter to be
    ///   generated from the provided interpolation string.
    GenIdent(SymbolId),

    /// Generate a description for the expansion template parameter that is
    ///   intended as a human-readable debugging string.
    GenDesc(SymbolId, GenIdentSymbolId),

    /// Interpolation will continue in a literal context at the provided
    ///   offset relative to the start of the specification string.
    ParseLiteralAt(SpecSlice, GenIdentSymbolId, SpecOffset),

    /// Like [`ParseLiteralAt`],
    ///   except in the context of an interpolated value
    ///     (after having encountered a curly brace).
    ParseInterpAt(SpecSlice, GenIdentSymbolId, SpecOffset),

    /// Expansion has completed;
    ///   the final step is to replace the provided specification string
    ///   with a reference to the generated template param.
    FinishSym(SpecSlice, GenIdentSymbolId),
}

impl Display for InterpState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InterpState::*;

        match self {
            Ready => write!(
                f,
                "expecting a new symbol to determine whether \
                   interpolation is necessary"
            ),

            GenIdent(sym) => {
                write!(
                    f,
                    "ready to generate template param identifier \
                        for specification {}",
                    TtQuote::wrap(sym),
                )
            }

            GenDesc(sym, GenIdentSymbolId(gen_sym)) => write!(
                f,
                "ready to generate debug description for generated \
                    template param {param} from specification {spec}",
                param = TtQuote::wrap(gen_sym),
                spec = TtQuote::wrap(sym),
            ),

            ParseLiteralAt(spec, _, x) => write!(
                f,
                "parsing specification {fmt_spec} at offset {x} \
                   in a literal context",
                fmt_spec = TtQuote::wrap(spec),
            ),

            ParseInterpAt(spec, _, x) => write!(
                f,
                "parsing specification {fmt_spec} at offset {x} \
                   in an interpolated value context",
                fmt_spec = TtQuote::wrap(spec),
            ),

            FinishSym(spec, GenIdentSymbolId(gen)) => write!(
                f,
                "ready to replace specification {fmt_spec} \
                    with expanded metavariable reference {fmt_gen}",
                fmt_spec = TtQuote::wrap(spec),
                fmt_gen = TtQuote::wrap(gen),
            ),
        }
    }
}

impl ParseState for InterpState {
    type Token = Nir;
    type Object = Nir;
    type Error = InterpError;

    // TODO: Span slicing should be coupled with `SPair` so that it's not
    //   possible for them to get out of sync with specification slices
    //     (slices are `char`s, spans are bytes).
    //   When doing so,
    //     also introduce diagnostic panics for string slicing.
    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        // Every expansion token that we emit must be derived from the span
        //   of the source token,
        //     ensuring that diagnostics reference the source code that can
        //     actually be acted upon by the user.
        let span = tok.span();

        match self {
            // When receiving a new symbol,
            //   we must make a quick determination as to whether it
            //   requires desugaring.
            // Since the vast majority of symbols we encounter will require
            //   no interpolation,
            //     we first perform a separate check that is designed to
            //     filter out non-interpolated strings quickly,
            //       before we start to parse.
            // Symbols that require no interpoolation are simply echoed back.
            Ready => match tok.symbol() {
                Some(sym) if needs_interpolation(sym) => {
                    Transition(GenIdent(sym))
                        .ok(Nir::Open(NirEntity::TplParam, span))
                        .with_lookahead(tok)
                }

                // No desugaring is needed.
                _ => Transition(Ready).ok(tok),
            },

            GenIdent(sym) => {
                let gen_ident = gen_tpl_param_ident_at_offset(span);
                let GenIdentSymbolId(ident_sym) = gen_ident;

                Transition(GenDesc(sym, gen_ident))
                    .ok(Nir::BindIdent(SPair(ident_sym, span)))
                    .with_lookahead(tok)
            }

            GenDesc(sym, gen_ident) => {
                let s = sym.lookup_str();

                // Description is not interned since there's no use in
                //   wasting time hashing something that will not be
                //   referenced
                //     (it's just informative for a human).
                // Note that this means that tests cannot compare SymbolId.
                let gen_desc = format!(
                    "Generated from interpolated string {}",
                    TtQuote::wrap(s)
                )
                .clone_uninterned();

                // Begin parsing in a _literal_ context,
                //   since interpolation is most commonly utilized with literal
                //   prefixes.
                Transition(ParseLiteralAt(s, gen_ident, 0))
                    .ok(Nir::Desc(SPair(gen_desc, span)))
                    .with_lookahead(tok)
            }

            // The outermost parsing context is that of the literal,
            //   where a sequence of characters up to `{` stand for
            //   themselves.
            ParseLiteralAt(s, gen_param, offset) => {
                if offset == s.len() {
                    // We've reached the end of the specification string.
                    // Since we're in the outermost (literal) context,
                    //   we're safe to complete.
                    return {
                        // We have one last thing to do before we're complete,
                        //   which is to perform the final replacement of the original
                        //   symbol that we've been fed
                        //     (the specification string).
                        Transition(FinishSym(s, gen_param))
                            .ok(Nir::Close(span))
                            .with_lookahead(tok)
                    };
                }

                match next_delim(s, span, offset) {
                    // Close before open
                    Some((bad_pos, '}')) => {
                        let espan = span.slice(offset + bad_pos, 1);

                        Transition(ParseLiteralAt(s, gen_param, s.len()))
                            .err(InterpError::Unopened(espan))
                            .with_lookahead(tok)
                    }

                    // The literal is the empty string,
                    //   which is useless to output,
                    //   so ignore it and proceed with parsing.
                    Some((0, _)) => {
                        Transition(ParseInterpAt(s, gen_param, offset + 1))
                            .incomplete()
                            .with_lookahead(tok)
                    }

                    // Everything from the offset until the curly brace is a
                    //   literal.
                    Some((rel_pos, _)) => {
                        let end = offset + rel_pos;

                        let literal = s[offset..end].intern();
                        let span_text = span.slice(offset, rel_pos);

                        let text = Nir::Text(SPair(literal, span_text));

                        Transition(ParseInterpAt(s, gen_param, end + 1))
                            .ok(text)
                            .with_lookahead(tok)
                    }

                    // The remainder of the specification is a literal.
                    None => {
                        let literal = s[offset..].intern();
                        let span_text = span.slice(offset, s.len() - offset);

                        let text = Nir::Text(SPair(literal, span_text));

                        // Keep in the current state but update the offset;
                        //   we'll complete parsing next pass.
                        Transition(ParseLiteralAt(s, gen_param, s.len()))
                            .ok(text)
                            .with_lookahead(tok)
                    }
                }
            }

            // Parsing is continuing after having encountered an
            //   interpolation delimiter `{`.
            // This is an inner context that cannot complete without being
            //   explicitly closed,
            //     and cannot not be nested.
            ParseInterpAt(s, gen_param, offset) => {
                let ospan = span.slice(offset - 1, 1);

                // Note that this is the position _relative to the offset_,
                //   not the beginning of the string.
                match next_delim(s, span, offset) {
                    Some((nested_pos, '{')) => {
                        let nspan = span.slice(offset + nested_pos, 1);

                        // Recovery:
                        //   Since we do not know the user's intent,
                        //     just bail out to the next reasonable
                        //     synchronization point
                        //       (end of the specification).
                        Transition(ParseLiteralAt(s, gen_param, s.len()))
                            .err(InterpError::NestedDelim(ospan, nspan))
                            .with_lookahead(tok)
                    }

                    // Empty param `{}`
                    Some((0, '}')) => {
                        // Recovery:
                        //   Skip the empty param and continue parsing,
                        //     since it does nothing anyway.
                        Transition(ParseLiteralAt(s, gen_param, offset + 1))
                            .err(InterpError::EmptyParam(
                                span.slice(offset - 1, 2),
                            ))
                            .with_lookahead(tok)
                    }

                    Some((rel_pos, _)) => {
                        let end = offset + rel_pos;

                        // The value `@foo` in `{@foo@}`.
                        let value = s[offset..end].intern();

                        // Since rel_pos is 0-indexed,
                        //   it is also the length of the value string.
                        let span_value = span.slice(offset, rel_pos);

                        let param_value = Nir::Ref(SPair(value, span_value));

                        // Continue parsing one character past the '}',
                        //   back in a literal context.
                        Transition(ParseLiteralAt(s, gen_param, end + 1))
                            .ok(param_value)
                            .with_lookahead(tok)
                    }

                    // End of specification string before finding closing `}`.
                    // Since we were unable to complete parsing of the parameter,
                    //   we cannot output it;
                    //     to recover we will omit the token entirely and
                    //     proceed to close.
                    None => {
                        let espan = span.slice(s.len() - 1, 1);

                        // Recovery:
                        //   We cannot emit a param that has not yet been parsed,
                        //     and we have reached the end of the string,
                        //     so we can prepare to conclude parsing.
                        Transition(ParseLiteralAt(s, gen_param, s.len()))
                            .err(InterpError::Unclosed(ospan, espan))
                            .with_lookahead(tok)
                    }
                }
            }

            // Interpolation has completed,
            //   and we're ready to replace the provided symbol
            //     (the interpolation specification)
            //   with a metavariable referencing the parameter that we just
            //     generated.
            // We finally release the lookahead symbol.
            FinishSym(_, GenIdentSymbolId(gen_param)) => {
                Transition(Ready).ok(tok.map(|_| gen_param))
            }
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        self == &Self::Ready
    }
}

/// Locate the next opening or closing delimiter beginning at the given
///   `offset` for the specification string `s`,
///     if any.
fn next_delim(s: &str, span: Span, offset: usize) -> Option<(usize, char)> {
    s.get(offset..)
        .diagnostic_expect(
            span.internal_error("while parsing this specification")
                .into(),
            &format!("specification byte offset {offset} is out of bounds"),
        )
        .char_indices()
        .find(|(_, ch)| matches!(*ch, '{' | '}'))
}

/// Whether a value represented by the provided [`SymbolId`] requires
///   interpolation.
///
/// _NB: This dereferences the provided [`SymbolId`] if it is dynamically
///   allocated._
///
/// The provided value requires interpolation if it contains,
///   anywhere in the string,
///   the characters `{` or `}`.
/// This uses [`memchr2()`] on the raw byte representation of the symbol to
///   quickly determine whether a string is only a literal and does not
///   require any interpolation,
///     which will be the case the vast majority of the time.
///
/// Since this operates on raw bytes,
///   but we later operate on the symbol as a [`str`],
///   it is not useful to return the located byte offset if an opening brace
///     is found;
///       that can be re-located quickly enough.
#[inline]
fn needs_interpolation(val: SymbolId) -> bool {
    // We can skip pre-interned symbols that we know cannot include the
    //   interpolation characters.
    // TODO: Abstract into `sym::symbol` module.
    quick_contains_byte(val, b'{')
        .or_else(|| quick_contains_byte(val, b'}'))
        .or_else(|| {
            memchr2(b'{', b'}', val.lookup_str().as_bytes()).map(|_| true)
        })
        .unwrap_or(false)
}

/// Generate a deterministic template param identifier name that is unique
///   relative to the offset in the source context (file) of the given
///   [`Span`].
///
/// Since template params are local to the containing template,
///   this is always safe.
/// We are able to simply use the offset of the provided span since we will
///   never generate more than one unique identifier at the exact same offset.
///
/// The identifier will include `"___dsgr"`,
///   meaning "desugar",
///   and serves as a unique string that can be used to track down this code
///     that generates it.
///
/// Hygiene is not a concern since identifiers cannot be redeclared,
///   so conflicts with manually-created identifiers will result in a
///   compilation error
///     (albeit a cryptic one);
///       the hope is that the informally-compiler-reserved `___` convention
///       mitigates that unlikely occurrence.
/// Consequently,
///   we _must_ intern to ensure that error can occur
///     (we cannot use [`GlobalSymbolIntern::clone_uninterned`]).
#[inline]
fn gen_tpl_param_ident_at_offset(span: Span) -> GenIdentSymbolId {
    GenIdentSymbolId(format!("@___dsgr_{:x}@", span.offset()).intern())
}

/// Error while desugaring an interpolation specification.
#[derive(Debug, PartialEq)]
pub enum InterpError {
    /// End of interpolation string was reached before an expected closing
    ///   delimiter `{`.
    ///
    /// The two spans represent,
    ///   respectively,
    ///   the position of the opening delimiter and the final character of
    ///   the interpolation string.
    /// The latter span will be rendered in such a way as to indicate that
    ///   parsing ended at that point without having located the closing
    ///   delimiter;
    ///     it is _not_ appropriate to suggest that the user add the closing
    ///     delimiter at that location,
    ///       since that may not be correct.
    /// We could be more intelligent about this in the future,
    ///   but it's probably not worth the effort given that the solution
    ///   will hopefully be obvious to the user.
    Unclosed(Span, Span),

    /// A closing delimiter was found in a literal context.
    ///
    /// A corresponding opening delimiter `{` was not present for the
    ///   closing delimiter `}` found at this [`Span`].
    Unopened(Span),

    /// An opening delimiter was found while already parsing a parameter in
    ///   the specification string.
    ///
    /// The first span indicates the opening delimiter,
    ///   and the second span the extra opening delimiter.
    NestedDelim(Span, Span),

    /// An interpolation parameter is the empty string.
    ///
    /// This is useless at best,
    ///   but was possibly accidental.
    ///
    /// Note that this does not throw for whitespace-only parameters;
    ///   that'll be handled by later parsers.
    /// An empty parameter would _also_ be able to be handled downstream,
    ///   but this provides a more friendly diagnostic message.
    EmptyParam(Span),
}

impl Display for InterpError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InterpError::*;

        match self {
            Unclosed(_, _) => write!(
                f,
                "unexpected end of interpolation specification \
                    (expected closing delimiter `}}`)",
            ),

            Unopened(_) => write!(
                f,
                "unexpected closing delimiter `}}` in \
                    interpolation specification \
                    (missing corresponding opening delimiter `}}`)",
            ),

            NestedDelim(_, _) => {
                write!(f, "cannot nest interpolation parameter delimiter `{{`")
            }

            EmptyParam(_) => write!(f, "empty interpolation parameter"),
        }
    }
}

impl Error for InterpError {}

impl Diagnostic for InterpError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        use InterpError::*;

        match self {
            Unclosed(sopen, send) => vec![
                sopen.note("opening delimiter for interpolation parameter"),
                send.error(
                    "specification ended here while expecting closing `}`",
                ),
            ],

            Unopened(span) => span
                .error("this has no corresponding opening delimiter `{`")
                .into(),

            // It is important to convey that nesting is not supported while
            //   not jumping to conclusions as to why this error may have
            //   occurred;
            //     if we want to provide suggestions,
            //       then we need to evaluate the erroneous specification
            //       further and establish some heuristics to guess what the
            //       user might have meant.
            NestedDelim(sopen, send) => vec![
                sopen.note("opening delimiter for interpolation parameter"),
                send.error("cannot nest opening delimiter"),
            ],

            EmptyParam(span) => vec![
                span.error(
                    "interpolation parameter must contain an identifier",
                ),
                span.help(
                    "an empty interpolation parameter (`{}`) would have no \
                        effect on the",
                ),
                // TODO: auto-wrap instead of having to output multiple help
                span.help("interpolated string, and so it is disallowed."),
            ],
        }
    }
}

#[cfg(test)]
mod test;
