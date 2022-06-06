// Source spans
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

//! Mapping to source input byte intervals.
//!
//! A [`Span`] is a mapping to a byte interval within a source file,
//!   representing primarily where some IR entity originated.
//! This underpins the diagnostic system,
//!   intended to:
//!
//!   1. Give the user specific information for debugging errors in their
//!       programs; and
//!   2. Provide high-resolution information for source code inquiries,
//!        such as "where is this identifier?" and "what exists at my cursor
//!        position within this file"?
//!
//! A span contains a [`Context`] representing the source location.
//! A context's path is a [`PathSymbolId`],
//!   which represents an interned string slice,
//!     _not_ a [`PathBuf`](std::path::PathBuf) or
//!     [`OsStr`](std::ffi::OsStr).
//!
//! ```
//! use tamer::span::{Span, Context};
//! use tamer::sym::GlobalSymbolIntern;
//!
//! // From raw parts
//! let ctx: Context = "some/path/foo".intern().into();
//! let span = Span::new(2, 6, ctx);
//!
//! assert_eq!(2, span.offset());
//! assert_eq!(6, span.len());
//! assert_eq!(ctx, span.context());
//!
//! // From a closed byte interval
//! let spani = Span::from_byte_interval((10, 25), "some/path/bar".intern());
//! assert_eq!(10, spani.offset());
//! assert_eq!(15, spani.len());
//!
//! // Freely copyable
//! let cp = span;
//! assert_eq!(cp, span);
//! ```
//!
//! Span is expected to be able to fit within a general-purpose CPU register
//!   on a 64-bit system, and so does not exceed 8 bytes in length.
//!
//! Spans are one of the most common objects in TAMER,
//!   competing only with [symbols](crate::sym).
//! But unlike symbols,
//!   [`Span`]s are designed to be meaningfully identifiable and copyable
//!   without interning.
//!
//! A span is ordered as such:
//!
//!   1. Spans group by [`Context`],
//!        though the relative ordering of each [`Context`] isn't
//!        necessarily meaningful;
//!   2. Spans are then ordered relative to their offset; and
//!   3. Spans are finally ordered by their length.
//!
//! Note that this means that a span beginning after but ending before
//!   another span will still order higher,
//!     as shown in the example below.
//!
//! ```
//! # use tamer::span::{Span, Context};
//! # use tamer::sym::GlobalSymbolIntern;
//! #
//! # let ctx: Context = "some/path/foo".intern().into();
//! #
//! // Visualization of spans:
//! // [..... ..... ..... .....]
//! //    [A====]   [B==] |   
//! //        | |   [C=]  |
//! //        | |   [D====]
//! //        | |    [E]  |
//! //        [F=====]    |
//! //        [G=======]  |
//!
//! let A = Span::new(2,  6, ctx);
//! let B = Span::new(10, 5, ctx);
//! let C = Span::new(10, 4, ctx);
//! let D = Span::new(10, 6, ctx);
//! let E = Span::new(11, 3, ctx);
//! let F = Span::new(5,  7, ctx);
//! let G = Span::new(5,  8, ctx);
//!
//! let mut spans = vec![A, B, C, D, E, F, G];
//! spans.sort();
//!
//! assert_eq!(spans, vec![A, F, G, C, B, D, E]);
//! ```
//!
//! Design Rationale
//! ================
//! It is expected that spans will be created and copied frequently,
//!   as they are propagated to every IR in the system.
//! It is further expected that the data within a span will only be
//!   referenced for diagnostic purposes,
//!     or for utilities operating on original source code
//!       (such as code formatters).
//!
//! When a span is referenced,
//!   it will either be to determine the exact location of a particular
//!     entity,
//!   or it will be to attempt to locate a similar entity in a higher-level
//!     IR associated with the same region of code.
//! The latter requires that spans be comparable in a meaningful way,
//!   exhibiting at least partial ordering.
//!
//! Spans are therefore optimized for three primary use cases:
//!   - copying;
//!   - comparison; and
//!   - ordering.
//!
//! Span Structure
//! --------------
//! Spans are packed into 64-bit values that can be readily converted into a
//!   [`u64`] value that is totally ordered relative to a given [`Context`],
//!   byte offset, and byte length.
//! This means that sorting a collection of [`Span`]s will group spans by
//!   their [`Context`];
//!     will sort those spans relative to their starting offset within that
//!       context; and
//!     will sort again by the ending offset.
//!
//! This means that spans are [`Eq`] and [`Ord`],
//!   and efficiently so by simply comparing the byte values of the entire
//!   struct as a single [`u64`].
//! This allows spans to be sorted relative to their positions within a
//!   context;
//!     be placed into a binary tree for mapping back to higher-level IRs;
//!     gives spans a meaningful unique identifier;
//!     be freely copied without cost;
//!     and more,
//!       all very efficiently and without having to access individual
//!         struct members.
//!
//! To accomplish this,
//!   [`Span`] uses `repr(packed)` and orders the fields for little endian
//!   systems like `x86_64`,
//!     which is what our team uses.
//! The `packed` representation had to be used because the byte orderings
//!   are [`u16`], [`u32`], [`u16`],
//!     which makes the [`u32`] byte offset unaligned.
//! Note that,
//!   while this _is_ unaligned,
//!   this is _not_ unaligned _memory_ access,
//!   since the entire [`Span`] will be retrieved from (aligned) memory at
//!     once;
//!       the unaligned fields within the [`u64`] do not incur a measurable
//!       performance cost.
//!
//! Related Work
//! ============
//! This span is motivated by [rustc's compressed `Span`](rustc-span).
//! TAMER's span size relies on 16 bits being sufficient for holding
//!   interned paths,
//!     which _should_ be a very reasonable assumption unless the interner
//!     ends up being shared with too many different things.
//! If ever that assumption becomes violated,
//!   and it is deemed that packages containing so many symbols should be permitted,
//!   TAMER's [`Span`] can accommodate in a similar with to rustc's by
//!     interning the larger span data and tagging this span as such.
//!
//!
//! [rustc-span]: https://doc.rust-lang.org/stable/nightly-rustc/rustc_span/struct.Span.html

use crate::{
    global,
    sym::{st16, ContextStaticSymbolId, GlobalSymbolResolve, SymbolId},
};
use std::{convert::TryInto, fmt::Display, path::Path};

/// A symbol size sufficient for holding interned paths.
pub type PathSymbolId = SymbolId<u16>;

/// Size of a [`Span`]'s `offset` field.
pub type SpanOffsetSize = global::SourceFileSize;

/// Size of a [`Span`]'s `len` field.
pub type SpanLenSize = global::FrontendTokenLength;

/// Description of a source location and byte interval for some object.
///
/// Spans represent byte intervals within a given source context.
/// A span should map to useful positions for helping users debug error
///   messages.
/// If code is generated, desugared, or otherwise manipulated,
///   the span ought to reference the original location of the code that can
///   be referenced and modified to correct any problems.
///
/// See the [module-level documentation](self) for more information.
#[cfg(target_endian = "little")]
#[repr(packed)]
#[derive(Debug, Clone, Copy)]
pub struct Span {
    /// Token length (ending byte offset - `offset`).
    len: SpanLenSize,

    /// Starting 0-indexed byte position, inclusive.
    offset: SpanOffsetSize,

    /// Context onto which byte offsets are mapped,
    ///   such as a source file.
    ///
    /// N.B.: This is an unaligned field,
    ///   and accessing it frequently may have a negative impact on
    ///   performance.
    ctx: Context,
}

impl Span {
    /// Create a new span from its constituent parts.
    pub fn new<C: Into<Context>>(
        offset: SpanOffsetSize,
        len: SpanLenSize,
        ctx: C,
    ) -> Self {
        Self {
            ctx: ctx.into(),
            offset,
            len,
        }
    }

    /// Create a constant span from a static context.
    pub const fn st_ctx(sym: ContextStaticSymbolId) -> Self {
        Self {
            ctx: Context(sym.as_sym()),
            offset: 0,
            len: 0,
        }
    }

    /// Create a span from a byte interval and context.
    ///
    /// Panics
    /// ======
    /// This will panic in the unlikely case that the difference between the
    ///   start and end of the interval exceeds the maximum of
    ///   [`global::FrontendTokenLength`].
    ///
    /// If this error occurs,
    ///   the parser should consider splitting large tokens up into multiple
    ///   tokens;
    ///     increasing [`global::FrontendTokenLength`] should be a last
    ///     resort,
    ///       since it has wide-reaching implications on the size of
    ///       [`Span`].
    ///
    /// The user is not expected to know how to recover from this error
    ///   without debugging the compiler.
    /// It is not expected that this would occur on any valid inputs.
    pub fn from_byte_interval<B, C>(interval: B, ctx: C) -> Self
    where
        B: Into<ClosedByteInterval>,
        C: Into<Context>,
    {
        let binterval = interval.into();

        Self {
            offset: binterval.0,
            len: (binterval.1 - binterval.0)
                .try_into()
                .expect("span length exceeds global::FrontendTokenLength"),
            ctx: ctx.into(),
        }
    }

    // A span represented uniquely as a totally ordered [`u64`].
    //
    // For more information on this important properly,
    //   see the documentation for [`Span`] itself.
    pub fn as_u64(self) -> u64 {
        // We take a number of precautions to make this safe (in the sense
        //   of correctness),
        //     through struct packing and a `cfg` directive for endianness.
        // In any case,
        //   a `u64` isn't going to harm anyone.
        unsafe { std::mem::transmute(self) }
    }

    /// Byte offset of the beginning of the span relative to its context.
    pub fn offset(&self) -> SpanOffsetSize {
        self.offset
    }

    /// Length of the span in bytes.
    ///
    /// The interval of the span is `[offset, offset+len]`.
    pub fn len(&self) -> SpanLenSize {
        self.len
    }

    /// The context to which the span applies.
    ///
    /// The context is, for example, a file.
    pub fn context(&self) -> Context {
        self.ctx
    }

    /// Further offset a span.
    ///
    /// This attempts to offset a span relative to its current offset by the
    ///   provided value.
    /// If the resulting offset exceeds [`SpanOffsetSize`],
    ///   the result will be [`None`].
    pub const fn offset_add(self, value: SpanOffsetSize) -> Option<Self> {
        match self.offset.checked_add(value) {
            Some(offset) => Some(Self { offset, ..self }),
            None => None,
        }
    }

    /// Create two zero-length spans representing respectively the first and
    ///   last offsets in the span.
    ///
    /// The second endpoint will be [`None`] if the offset cannot be
    ///   represented by [`SpanOffsetSize`].
    ///
    /// ```
    /// # use tamer::span::{Span, Context};
    /// # use tamer::sym::GlobalSymbolIntern;
    /// #
    /// # let ctx: Context = "some/path/foo".intern().into();
    /// #
    /// // [..... .....]
    /// //    [A===]
    /// //    2    6
    /// let A = Span::new(2, 6, ctx);
    ///
    /// assert_eq!(
    ///   A.endpoints(),
    ///   (
    ///       Span::new(2, 0, ctx),
    ///       Some(Span::new(8, 0, ctx)),
    ///   ),
    /// );
    /// ```
    pub const fn endpoints(self) -> (Self, Option<Self>) {
        (
            // First endpoint.
            Self {
                offset: self.offset,
                len: 0,
                ..self
            },
            // Second endpoint.
            match self.offset.checked_add(self.len as u32) {
                Some(offset) => Some(Self {
                    offset,
                    len: 0,
                    ..self
                }),
                None => None,
            },
        )
    }

    /// Create two zero-length spans representing respectively the first and
    ///   last offsets in the span,
    ///     saturating the ending offset if it cannot be represented by
    ///     [`SpanOffsetSize`].
    ///
    /// Aside from the saturation,
    ///   this is identical to [`Span::endpoints`].
    pub fn endpoints_saturated(self) -> (Self, Self) {
        let endpoints = self.endpoints();

        (
            endpoints.0,
            endpoints.1.unwrap_or(Self {
                offset: SpanOffsetSize::MAX,
                ..endpoints.0
            }),
        )
    }

    /// Adjust span such that its offset is relative to the provided span.
    ///
    /// If the provide `rel_span` does not precede this span,
    ///   [`None`] will be returned.
    ///
    /// If the two spans do not share the same [`Context`],
    ///   no comparison can be made and [`None`] will be returned.
    pub fn relative_to(self, rel_span: Span) -> Option<Self> {
        // Note that this is unaligned.
        if self.context() != rel_span.context() {
            return None;
        }

        if self.offset() < rel_span.offset() {
            return None;
        }

        Some(Self {
            ctx: self.ctx,
            offset: self.offset.saturating_sub(rel_span.offset),
            len: self.len,
        })
    }
}

impl Into<u64> for Span {
    fn into(self) -> u64 {
        self.as_u64()
    }
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.as_u64() == other.as_u64()
    }
}

impl Eq for Span {}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_u64().cmp(&other.as_u64())
    }
}

// This assertion verifies our above expectations.
// If this fails,
//   then you have either modified [`global`] constants or you have modified
//   the fields of [`Span`] itself,
//     in which case you should read "Related Work" above to determine
//     whether this was a good idea or if interned spans should be
//     introduced.
// In any case,
//   hopefully this was planned for,
//   because otherwise your week has just been ruined.
assert_eq_size!(Span, u64);

impl From<Span> for (Span, Span) {
    /// Expand a [`Span`] into a two-span.
    ///
    /// A two-span `(A, B)` is equivalent to a span beginning at the start
    ///   of `A` and ending at the end of `B`.
    ///
    /// We gain no resolution from performing this operation,
    ///   but it does allow for using a single span in contexts where a
    ///   higher resolution is supported.
    fn from(span: Span) -> Self {
        (span, span)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Needed to avoid unaligned references since Span is packed.
        let ctx = self.ctx;
        let offset = self.offset as usize;

        let end = offset + self.len as usize;

        // Very primitive information to begin with; we'll have something
        // more useful in the future.
        write!(f, "[{} offset {}-{}]", ctx, offset, end)
    }
}

/// A placeholder span indicating that a span is expected but is not yet
///   known.
pub const UNKNOWN_SPAN: Span = Span::st_ctx(st16::CTX_UNKNOWN);

/// A dummy span that can be used in contexts where a span is expected but
///   is not important.
///
/// This is intended primarily for tests;
///   you should always use an appropriate span to permit sensible error
///   messages and source analysis.
///
/// Additional dummy spans can be derived from this one.
pub const DUMMY_SPAN: Span = Span::st_ctx(st16::CTX_DUMMY);

/// Context for byte offsets (e.g. a source file).
///
/// A context is lifetime-free and [`Copy`]-able,
///   with the assumption that an interned [`PathSymbolId`] will only need
///   to be resolved to its underlying value in a diagnostic context where
///   the internment system is readily available.
///
/// Since this is used within [`Span`],
///   it must be kept as small as possible.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Context(PathSymbolId);

impl Context {
    /// Produce a [`Span`] within the given context.
    #[inline]
    pub const fn span(self, offset: SpanOffsetSize, len: SpanLenSize) -> Span {
        Span {
            ctx: self,
            offset,
            len,
        }
    }

    /// Attempt to produce a [`Span`] of the given length at the given
    ///   offset,
    ///     otherwise fall back to a `(0,0)` (ZZ) span.
    ///
    /// If the offset cannot be stored,
    ///   then the length will always be `0` even if it could otherwise be
    ///   represented;
    ///     `(0,0)` indicates no span,
    ///       whereas `(0,N)` would indicate a span of length `N` at
    ///       offset `0`,
    ///         which would not be true.
    ///
    /// If the offset can be represented but not the length,
    ///   then a zero-length span at that offset will be produced,
    ///   which still provides useful information.
    /// This may be the case for very large objects,
    ///   like compiled text fragments.
    ///
    /// The rationale here is that spans are intended to be informative.
    /// If we are unable to provide that information due to exceptional
    ///   circumstances
    ///     (very large file or very large token),
    ///     then it's better to provide _some_ information than to bail out
    ///       with an error and interrupt the entire process,
    ///         potentially masking errors in the process.
    #[inline]
    pub fn span_or_zz(self, offset: usize, len: usize) -> Span {
        self.span(offset.try_into().unwrap_or(0), len.try_into().unwrap_or(0))
    }
}

/// A placeholder context indicating that a context is expected but is not
///   yet known.
pub const UNKNOWN_CONTEXT: Context = Context(st16::raw::CTX_UNKNOWN);

/// A dummy context that can be used where a span is expected but is not
///   important.
///
/// This is intended primarily for tests;
///   you should always use an appropriate span to permit sensible error
///   messages and source analysis.
///
/// See also [`UNKNOWN_CONTEXT`].
pub const DUMMY_CONTEXT: Context = Context(st16::raw::CTX_DUMMY);

impl<P: Into<PathSymbolId>> From<P> for Context {
    fn from(sym: P) -> Self {
        Self(sym.into())
    }
}

impl Display for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl AsRef<Path> for Context {
    fn as_ref(&self) -> &Path {
        &Path::new(self.0.lookup_str())
    }
}

/// A closed interval (range of values including its endpoints) representing
///   source bytes associated with a token.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct ClosedByteInterval<T = SpanOffsetSize>(pub T, pub T)
where
    T: Copy + PartialOrd;

impl<T: Copy + PartialOrd> From<(T, T)> for ClosedByteInterval<T> {
    /// Convert a tuple into a closed byte interval where the first index
    ///   represents the start of the interval and the second index the
    ///   end.
    ///
    /// Panics
    /// ======
    /// The second value must be ≥ the first.
    fn from(src: (T, T)) -> Self {
        assert!(src.1 >= src.0);

        Self(src.0, src.1)
    }
}

assert_eq_size!(ClosedByteInterval, u64);

#[cfg(test)]
mod test {
    use super::*;

    // Little endian check.
    //
    // This ensures that the byte ordering is as expected,
    //   otherwise the resulting integer will not have the properties we
    //   require for sorting and comparison.
    #[cfg(target_endian = "little")]
    #[test]
    fn span_pack_le() {
        let span =
            Span::new(0xA3A2A1A0, 0xB1B0, SymbolId::test_from_int(0xC1C0));

        assert_eq!(
            0xC1C0_A3A2A1A0_B1B0,
            // ^       ^     ^
            // ctx   offset  len
            span.as_u64(),
            "endianness check failed: {:X?}",
            span.as_u64()
        );
    }

    #[cfg(target_endian = "big")]
    #[test]
    fn span_pack_be_not_supported() {
        panic!("Big-endian systems are not currently supported");
    }

    // The tests that follow are corollaries of the above, but the below
    // tests do test that the implementations function as intended.

    #[test]
    fn span_at_later_offset_in_same_context_compares_greater() {
        let ctx = Context::from("imaginary");
        let first = ctx.span(10, 5);
        let second = ctx.span(20, 5);

        // These two assertions must be identical.
        assert!(second > first);
        assert!(second.as_u64() > first.as_u64());
    }

    #[test]
    fn spans_order_by_context_start_and_len() {
        let ctxa = Context::from("context a");
        let ctxb = Context::from("context b");

        // Sanity check, otherwise this test won't work as expected.
        assert!(ctxa.0 < ctxb.0);

        let sa1 = ctxa.span(10, 1);
        let sa2 = ctxa.span(22, 1);
        let sa3 = ctxa.span(35, 1);

        let sb1 = ctxb.span(11, 1);
        let sb2 = ctxb.span(20, 1);
        let sb3 = ctxb.span(33, 1);

        let mut spans = vec![sa3, sb2, sb1, sa2, sa1, sb3];
        spans.sort();

        assert_eq!(spans, vec![sa1, sa2, sa3, sb1, sb2, sb3]);
    }

    #[test]
    fn retrieve_span_components() {
        let ctx = Context::from("foo");
        let offset = 100;
        let len = 50;

        let span = ctx.span(offset, len);

        assert_eq!(
            (offset, len, ctx),
            (span.offset(), span.len(), span.context())
        );
    }

    #[test]
    fn span_offset_add() {
        let ctx = Context::from("addtest");
        let offset = 10;
        let len = 5;

        let span = ctx.span(offset, len);

        // Successful add.
        assert_eq!(
            span.offset_add(10),
            Some(Span {
                offset: offset + 10,
                len,
                ctx
            })
        );

        // Fail, do not wrap.
        assert_eq!(span.offset_add(SpanOffsetSize::MAX), None);
    }

    #[test]
    fn span_into_twospan() {
        let ctx = Context::from("foo");
        let span = ctx.span(10, 50);

        assert_eq!((span, span), span.into());
    }

    #[test]
    fn span_endpoints() {
        let ctx = Context::from("end");
        let span = ctx.span(10, 20);

        let (start, end) = span.endpoints();

        assert_eq!(start, Span::new(10, 0, ctx));
        assert_eq!(end, Some(Span::new(30, 0, ctx)));
    }

    #[test]
    fn span_endpoints_exceeding_max_offset() {
        let ctx = Context::from("end");
        let offset = SpanOffsetSize::MAX - 5;
        let span = ctx.span(offset, 10);

        let (start, end) = span.endpoints();

        assert_eq!(start, Span::new(offset, 0, ctx));
        assert_eq!(end, None);
    }

    #[test]
    fn span_endpoints_saturated() {
        let ctx = Context::from("end");
        let offset = SpanOffsetSize::MAX - 5;
        let span = ctx.span(offset, 10);

        let (start, end) = span.endpoints_saturated();

        assert_eq!(start, Span::new(offset, 0, ctx));
        assert_eq!(end, Span::new(SpanOffsetSize::MAX, 0, ctx));
    }
}
