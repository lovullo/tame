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
//! This ends up desugaring into the [`PlainNir`] equivalent of this:
//!
//! ```xm
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
//! Since interpolation desugars into [`PlainNir`],
//!   and not source XML,
//!   generated `param`s will be automatically be interpreted downstream in
//!     the lowering pipeline as if they were hoisted to the template
//!     definition header.
//!
//! If a string does not require interpolation,
//!   then it is interpreted as a literal within the context of the template
//!   system and is echoed back unchanged.
//!
//! NB: All attributes are reasoned about as string literals until they
//!   contain no metavariables,
//!     which may require expansion via the template system;
//!       the [`NirSymbolTy`] represents the type that the literal will
//!       _ultimately_ be parsed as once that time comes.
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

use memchr::memchr;

use super::super::{NirSymbolTy, PlainNir, PlainNirSymbol, SugaredNirSymbol};
use crate::{
    diagnose::{AnnotatedSpan, Diagnostic},
    fmt::{DisplayWrapper, TtQuote},
    parse::{prelude::*, NoContext},
    span::Span,
    sym::{
        st::quick_contains_byte, GlobalSymbolIntern, GlobalSymbolResolve,
        SymbolId,
    },
};
use std::{error::Error, fmt::Display};

// Expose variants for enums defined in this module to reduce verbosity.
use InterpObject::*;
use InterpState::*;

/// Object resulting from interpolation.
///
/// The provided [`SugaredNirSymbol`] is interpreted as a specification for
///   interpolation.
/// This specification is expanded into a sequence of [`PlainNir`] tokens
///   via the [`Expanded`](Self::Expanded) variant,
///     representing the definition of a template parameter whose default
///     value will yield the equivalent of the specification.
///
/// After expansion,
///   the original [`SugaredNirSymbol`] is expected to be replaced with a
///   [`PlainNirSymbol`] via the [`ReplaceSym`](Self::ReplaceSym) variant,
///     containing the name of the newly-generated metavariable.
#[derive(Debug, PartialEq, Eq)]
pub enum InterpObject<const TY: NirSymbolTy> {
    /// A token generated as part of interpolation which is to be merged
    ///   into the NIR token stream.
    Expanded(PlainNir),

    /// Interpolation has resulted in the creation of a new metavariable
    ///   which should take place of the original NIR symbol containing the
    ///   interpolation specification.
    ReplaceSym(PlainNirSymbol<TY>),
}

impl<const TY: NirSymbolTy> Token for InterpObject<TY> {
    fn ir_name() -> &'static str {
        "Interpolation"
    }

    fn span(&self) -> Span {
        match self {
            Self::Expanded(nir) => nir.span(),
            Self::ReplaceSym(nir_sym) => nir_sym.span(),
        }
    }
}

impl<const TY: NirSymbolTy> Object for InterpObject<TY> {}

impl<const TY: NirSymbolTy> Display for InterpObject<TY> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            InterpObject::Expanded(nir) => write!(f, "interpolated {nir}"),
            InterpObject::ReplaceSym(nir_sym) => {
                write!(f, "interpolation specification replacement {nir_sym}")
            }
        }
    }
}

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
pub enum InterpState<const TY: NirSymbolTy> {
    /// The next token will be inspected to determine whether it requires
    ///   interpolation.
    #[default]
    Ready,

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

impl<const TY: NirSymbolTy> Display for InterpState<TY> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use InterpState::*;

        match self {
            Ready => write!(
                f,
                "expecting a new symbol to determine whether \
                   interpolation is necessary"
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

impl<const TY: NirSymbolTy> ParseState for InterpState<TY> {
    type Token = SugaredNirSymbol<TY>;
    type Object = InterpObject<TY>;
    type Error = InterpError;

    fn parse_token(
        self,
        tok: Self::Token,
        _: NoContext,
    ) -> TransitionResult<Self> {
        match (self, tok) {
            // When receiving a new symbol,
            //   we must make a quick determination as to whether it
            //   requires desugaring.
            // Since the vast majority of symbols we encounter will require
            //   no interpolation,
            //     we first perform a separate check that is designed to
            //     filter out non-interpolated strings quickly,
            //       before we start to parse.
            // Symbols that require no interpoolation are simply echoed back.
            (Ready, SugaredNirSymbol(sym, span)) => {
                if needs_interpolation(sym) {
                    Self::begin_expansion(sym, span)
                } else {
                    // No desugaring is needed.
                    Self::yield_symbol(sym, span)
                }
            }

            // The outermost parsing context is that of the literal,
            //   where a sequence of characters up to `{` stand for
            //   themselves.
            (
                ParseLiteralAt(s, gen_param, offset),
                SugaredNirSymbol(sym, span),
            ) => {
                if offset == s.len() {
                    // We've reached the end of the specification string.
                    // Since we're in the outermost (literal) context,
                    //   we're safe to complete.
                    return Self::end_expansion(s, gen_param, sym, span);
                }

                // Note that this is the position _relative to the offset_,
                //   not the beginning of the string.
                match s[offset..].chars().position(|ch| ch == '{') {
                    // The literal is the empty string,
                    //   which is useless to output,
                    //   so ignore it and proceed with parsing.
                    Some(0) => {
                        Transition(ParseInterpAt(s, gen_param, offset + 1))
                            .incomplete()
                            .with_lookahead(SugaredNirSymbol(sym, span))
                    }

                    // Everything from the offset until the curly brace is a
                    //   literal.
                    Some(rel_pos) => {
                        let end = offset + rel_pos;

                        let literal = s[offset..end].intern();
                        let span_text = span.slice(offset, rel_pos);

                        let text = PlainNir::TplParamText(
                            PlainNirSymbol::Todo(literal, span_text),
                        );

                        Transition(ParseInterpAt(s, gen_param, end + 1))
                            .ok(Expanded(text))
                            .with_lookahead(SugaredNirSymbol(sym, span))
                    }

                    // The remainder of the specification is a literal.
                    None => {
                        let literal = s[offset..].intern();
                        let span_text = span.slice(offset, s.len() - offset);

                        let text = PlainNir::TplParamText(
                            PlainNirSymbol::Todo(literal, span_text),
                        );

                        // Keep in the current state but update the offset;
                        //   we'll complete parsing next pass.
                        Transition(ParseLiteralAt(s, gen_param, s.len()))
                            .ok(Expanded(text))
                            .with_lookahead(SugaredNirSymbol(sym, span))
                    }
                }
            }

            // Parsing is continuing after having encountered an
            //   interpolation delimiter `{`.
            // This is an inner context that cannot complete without being
            //   explicitly closed,
            //     and cannot not be nested.
            (
                ParseInterpAt(s, gen_param, offset),
                SugaredNirSymbol(sym, span),
            ) => {
                // TODO: Make sure offset exists, avoid panic
                // TODO: Prevent nested `{`.

                // Note that this is the position _relative to the offset_,
                //   not the beginning of the string.
                match s[offset..].chars().position(|ch| ch == '}') {
                    Some(0) => todo!("empty interp"),

                    Some(rel_pos) => {
                        let end = offset + rel_pos;

                        // The value `@foo` in `{@foo@}`.
                        let value = s[offset..end].intern();

                        // Since rel_pos is 0-indexed,
                        //   it is also the length of the value string.
                        let span_value = span.slice(offset, rel_pos);

                        let param_value = PlainNir::TplParamValue(
                            PlainNirSymbol::Todo(value, span_value),
                        );

                        // Continue parsing one character past the '}',
                        //   back in a literal context.
                        Transition(ParseLiteralAt(s, gen_param, end + 1))
                            .ok(Expanded(param_value))
                            .with_lookahead(SugaredNirSymbol(sym, span))
                    }

                    None => todo!("missing closing '}}'"),
                }
            }

            // Interpolation has completed,
            //   and we're ready to replace the provided symbol
            //     (the interpolation specification)
            //   with a metavariable referencing the parameter that we just
            //     generated.
            (
                FinishSym(_, GenIdentSymbolId(gen_param)),
                SugaredNirSymbol(_, span),
            ) => Self::yield_symbol(gen_param, span),
        }
    }

    fn is_accepting(&self, _: &Self::Context) -> bool {
        self == &Self::Ready
    }
}

impl<const TY: NirSymbolTy> InterpState<TY> {
    /// Yield the final result of this operation in place of the original
    ///   specification string,
    ///     which may or may not have required interpolation.
    ///
    /// If no interpolation was required,
    ///   `sym` will be the original string;
    ///   otherwise,
    ///     `sym` ought to be a metavariable referencing the generated
    ///     template param.
    ///
    /// This transitions back to [`Ready`] and finally releases the
    ///   lookahead symbol.
    fn yield_symbol(sym: SymbolId, span: Span) -> TransitionResult<Self> {
        Transition(Ready).ok(ReplaceSym(PlainNirSymbol::Todo(sym, span)))
    }

    /// Begin expansion of an interpolation specification by generating a
    ///   new template parameter that will hold the interpolated body.
    ///
    /// For more information on identifier generation,
    ///   see [`gen_tpl_param_ident_at_offset`].
    fn begin_expansion(sym: SymbolId, span: Span) -> TransitionResult<Self> {
        let gen_param = gen_tpl_param_ident_at_offset(span);

        // Description is not interned since there's no use in
        //   wasting time hashing something that will not be
        //   referenced
        //     (it's just informative for a human).
        // Note that this means that tests cannot compare SymbolId.
        let gen_desc = format!(
            "Generated from interpolated string {}",
            TtQuote::wrap(sym)
        )
        .clone_uninterned();

        let GenIdentSymbolId(gen_param_sym) = gen_param;

        let open = PlainNir::TplParamOpen(
            PlainNirSymbol::Todo(gen_param_sym, span),
            PlainNirSymbol::Todo(gen_desc, span),
        );

        // Begin parsing in a _literal_ context,
        //   since interpolation is most commonly utilized with literal
        //   prefixes.
        Transition(ParseLiteralAt(sym.lookup_str(), gen_param, 0))
            .ok(Expanded(open))
            .with_lookahead(SugaredNirSymbol(sym, span))
    }

    /// Complete expansion of an interpolation specification string.
    ///
    /// This closes the newly generated template param `gen_param`,
    ///   and then transitions to [`FinishSym`].
    fn end_expansion(
        s: SpecSlice,
        gen_param: GenIdentSymbolId,
        sym: SymbolId,
        span: Span,
    ) -> TransitionResult<Self> {
        let close = PlainNir::TplParamClose(span);

        // We have one last thing to do before we're complete,
        //   which is to perform the final replacement of the original
        //   symbol that we've been fed
        //     (the specification string).
        Transition(FinishSym(s, gen_param))
            .ok(Expanded(close))
            .with_lookahead(SugaredNirSymbol(sym, span))
    }
}

/// Whether a value represented by the provided [`SymbolId`] requires
///   interpolation.
///
/// _NB: This dereferences the provided [`SymbolId`] if it is dynamically
///   allocated._
///
/// The provided value requires interpolation if it contains,
///   anywhere in the string,
///   the character [`}`].
/// This uses [`memchr()`] on the raw byte representation of the symbol to
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
    let ch = b'{';

    // We can skip pre-interned symbols that we know cannot include the
    //   interpolation character.
    // TODO: Abstract into `sym::symbol` module.
    quick_contains_byte(val, ch)
        .or_else(|| memchr(ch, val.lookup_str().as_bytes()).map(|_| true))
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
pub enum InterpError {}

impl Display for InterpError {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // No errors yet.
        Ok(())
    }
}

impl Error for InterpError {}

impl Diagnostic for InterpError {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        // No errors yet.
        vec![]
    }
}

#[cfg(test)]
mod test;
