// XML IR (XIR)
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

//! Intermediate representation (IR) of an XML document.
//!
//! XIR serves not only as a TAMER-specific IR,
//!   but also as an abstraction layer atop of whatever XML library is
//!   used (e.g. `quick_xml`).
//! XIR is _not_ intended to be comprehensive,
//!   or even general-purpose---it
//!     exists to solve concerns specific to TAMER's construction.
//!
//! To parse an entire XML document,
//!   see [`reader`].
//!
//! _Note:_ XIR refers to "opening" and "closing" tags,
//!   as opposed to "start" and "end" as used in the XML specification.
//! TAMER uses a uniform terminology for all delimited data.

use crate::span::{Span, SpanLenSize};
use crate::sym::{
    st_as_sym, GlobalSymbolIntern, GlobalSymbolInternBytes, SymbolId,
};
use memchr::memchr;
use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::ops::Deref;

mod error;
pub use error::Error;

mod escape;
pub use escape::{DefaultEscaper, Escaper};

use error::SpanlessError;
use st::qname::QNameCompatibleStaticSymbolId;

pub mod attr;
pub mod flat;
pub mod fmt;
pub mod iter;
pub mod pred;
pub mod reader;
pub mod st;
pub mod tree;
pub mod writer;

#[macro_use]
pub mod parse;

/// An infallible [`Token`] stream.
///
/// If the token stream originates from an operation that could potentially
///   fail and ought to be propagated,
///     use [`TokenResultStream`].
///
/// The name "stream" in place of "iterator" is intended to convey that this
///   type is expected to be processed in real-time as a stream,
///     not read into memory.
pub trait TokenStream = Iterator<Item = Token>;

/// A [`Token`] stream that may encounter errors during parsing.
///
/// If the stream cannot fail,
///   consider using [`TokenStream`].
pub trait TokenResultStream = Iterator<Item = Result<Token, Error>>;

/// XML Name minus `":"`.
///
/// The intent is to check a string for validity _before_ interning;
///   otherwise,
///     the string would have to be first retrieved from the intern pool
///     for comparison,
///       which is not an operation we want to do implicitly.
/// Those methods will be created as they are needed.
///
/// See <https://www.w3.org/TR/REC-xml-names/#NT-NCName>.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NCName(SymbolId);

impl NCName {
    /// Create a new NCName from a symbol without validating that the symbol
    ///   is a valid NCName.
    ///
    /// Safety
    /// ======
    /// This is not unsafe in the traditional sense;
    ///   it's unsafe in a sense similar to non-UTF-8 `str` slices,
    ///     in that it is expected that an `NCName` means that you do not
    ///     have to worry about whether it's syntatically valid as XML.
    pub unsafe fn new_unchecked(value: SymbolId) -> Self {
        Self(value)
    }
}

impl TryFrom<&[u8]> for NCName {
    type Error = SpanlessError;

    /// Attempt to parse a byte slice into an [`NCName`].
    ///
    /// If the slice contains `b':'`,
    ///   an error will be produced.
    /// No other checks are performed beyond checking that the byte sequence
    ///   represents a valid UTF-8 string.
    /// The string will be interned for you.
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        match value.contains(&b':') {
            true => Err(SpanlessError::NCColon(value.intern_utf8()?)),
            false => Ok(NCName(value.intern_utf8()?)),
        }
    }
}

impl Deref for NCName {
    type Target = SymbolId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<SymbolId> for NCName {
    fn eq(&self, other: &SymbolId) -> bool {
        self.0 == *other
    }
}

impl TryFrom<&str> for NCName {
    type Error = SpanlessError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.contains(':') {
            return Err(SpanlessError::NCColon(value.into()));
        }

        Ok(Self(value.intern()))
    }
}

/// Namespace prefix of a [`QName`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Prefix(NCName);

/// Local name portion of a [`QName`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocalPart(NCName);

impl Deref for Prefix {
    type Target = SymbolId;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl Deref for LocalPart {
    type Target = SymbolId;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl From<NCName> for Prefix {
    fn from(name: NCName) -> Self {
        Self(name)
    }
}

impl From<NCName> for LocalPart {
    fn from(name: NCName) -> Self {
        Self(name)
    }
}

impl TryFrom<&str> for Prefix {
    type Error = SpanlessError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

impl TryFrom<&str> for LocalPart {
    type Error = SpanlessError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Display for LocalPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// A qualified name (namespace prefix and local name).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct QName(Option<Prefix>, LocalPart);

// Since we implement Copy, ensure size matches our expectations:
const_assert!(std::mem::size_of::<QName>() <= std::mem::size_of::<usize>());

impl QName {
    /// Create a new fully-qualified name (including both a namespace URI
    ///   and local name).
    pub fn new(prefix: Option<Prefix>, local_name: LocalPart) -> Self {
        Self(prefix, local_name)
    }

    /// Create a new name from a local name only.
    ///
    /// This should only be used for attributes in TAMER,
    ///   since all elements should have an associated namespace.
    ///
    /// _(If this is ever not true (e.g. due to new targets),
    ///   please update this comment.)_
    pub fn new_local(local_name: LocalPart) -> Self {
        Self(None, local_name)
    }

    /// Fully qualified namespace associated with a name.
    pub fn prefix(&self) -> Option<Prefix> {
        self.0
    }

    /// Local part of a name (name without namespace).
    pub fn local_name(&self) -> LocalPart {
        self.1
    }

    /// Construct a constant QName from static C-style symbols.
    pub const fn st_cid<T, U>(prefix_sym: &T, local_sym: &U) -> Self
    where
        T: QNameCompatibleStaticSymbolId,
        U: QNameCompatibleStaticSymbolId,
    {
        Self(
            Some(Prefix(NCName(st_as_sym(prefix_sym)))),
            LocalPart(NCName(st_as_sym(local_sym))),
        )
    }

    /// Construct a constant QName with a local name only from a static
    ///   C-style symbol.
    pub const fn st_cid_local<T: QNameCompatibleStaticSymbolId>(
        local_sym: &T,
    ) -> Self {
        Self(None, LocalPart(NCName(st_as_sym(local_sym))))
    }
}

impl<P, L> TryFrom<(P, L)> for QName
where
    P: TryInto<Prefix>,
    L: TryInto<LocalPart, Error = P::Error>,
{
    type Error = P::Error;

    fn try_from(value: (P, L)) -> Result<Self, Self::Error> {
        Ok(Self(Some(value.0.try_into()?), value.1.try_into()?))
    }
}

impl<P, L> TryFrom<(Option<P>, L)> for QName
where
    P: TryInto<Prefix>,
    L: TryInto<LocalPart, Error = P::Error>,
{
    type Error = P::Error;

    fn try_from(value: (Option<P>, L)) -> Result<Self, Self::Error> {
        let ns = match value.0 {
            None => None,
            Some(ns) => Some(ns.try_into()?),
        };

        Ok(Self(ns, value.1.try_into()?))
    }
}

impl TryFrom<&str> for QName {
    type Error = SpanlessError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(QName(None, value.try_into()?))
    }
}

impl TryFrom<&[u8]> for QName {
    type Error = SpanlessError;

    /// Attempt to parse a byte slice into a [`QName`].
    ///
    /// The byte slice must represent a valid QName in UTF-8.
    /// If a colon is present,
    ///   it delimits the namespace [`Prefix`] and [`LocalPart`],
    ///   and therefore must not be in the first or last byte position.
    fn try_from(name: &[u8]) -> Result<Self, Self::Error> {
        match memchr(b':', name) {
            // Leading colon means we're missing a prefix, trailing means
            //   that we have no local part.
            Some(pos) if pos == 0 || pos == name.len() - 1 => {
                Err(SpanlessError::InvalidQName(name.intern_utf8()?))
            }

            // There is _at least_ one colon in the string.
            Some(pos) => {
                // The prefix is before the first colon,
                //   and so itself must not contain a colon and is therefore
                //   a valid NCName.
                let prefix = NCName(name[..pos].intern_utf8()?);

                // But there could be a _second_ colon,
                //   so the local part requires validation.
                let local = NCName::try_from(&name[(pos + 1)..])?;

                Ok(Self::new(Some(prefix.into()), local.into()))
            }

            // There are no colons in the string, so the entire string is
            //   both a local part and a valid NCName.
            None => Ok(Self::new(None, NCName(name.intern_utf8()?).into())),
        }
    }
}

impl Display for QName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QName(Some(local), suffix) => write!(f, "{}:{}", local, suffix),
            QName(None, suffix) => suffix.fmt(f),
        }
    }
}

/// A span representing an opening (starting) element tag.
///
/// See [`EleSpan`] for more information.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct OpenSpan(Span, EleNameLen);

impl OpenSpan {
    pub fn without_name_span(span: Span) -> Self {
        Self(span, 0)
    }
}

/// A span representing a closing (ending) element tag.
///
/// See [`EleSpan`] for more information.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CloseSpan(Span, EleNameLen);

impl CloseSpan {
    /// A [`CloseSpan`] representing the closing of an empty tag.
    ///
    /// This type of span has no element name.
    pub fn empty(span: Span) -> Self {
        Self::without_name_span(span)
    }

    pub fn without_name_span(span: Span) -> Self {
        Self(span, 0)
    }
}

/// Number of bytes representing the name of the element.
pub type EleNameLen = SpanLenSize;

/// Spans associated with an element opening or closing tag.
///
/// The diagram below illustrates the behavior of [`EleSpan`].
/// Spans are represented by `[---]` intervals,
///   with the byte offset at each end,
///   and the single-letter span name centered below the interval.
///
/// ```text
///   <open  >          <open ...>     </close  >          <empty ' />
///   |[--]  |          |[--]          | [---]  |          |[---] ' []
///   |1  4  |          |1  4          | 2   6  |          |1   5 ' 9`10
///   | N    |          | N |          |   N    |          |  N | ' T
///   |      |          |   |          |        |          |    | '
///   [------]          [---]          [--------]          [----] '
///   0      7          0   4          0        9          0    5 '
///      T                T                T                 T    '
/// ```
///
/// Above we have
///
///   - `T` = [`EleSpan::span`]; and
///   - `N` = [`EleSpan::name_span`].
///
/// The purpose of the `T` span is to represent the entire token that has
///   been emitted by XIR.
/// If an opening tag does not contain any attributes,
///   then `T` represents the entire opening tag with both the opening and
///   closing angle brackets.
/// If an opening tag is expected to contain attributes,
///   then only the opening angle bracket is included.
/// A closing tag is entirely contained by `T`.
///
/// The empty tag is separated into two tokens in XIR---a
///   [`Token::Open`] and a [`Token::Close`] with a [`None`] for the name.
/// Unlike a typical closing tag,
///   there is no `N` span available for the closing token,
///     and so requesting one via [`EleSpan::name_span`] will simply
///     return the `T` span,
///       rather than complicating the API with an [`Option`].
/// It is generally assumed that reporting on element names will occur
///   within the context of the _opening_ tag.
///
/// The tag may contain whitespace following the element name,
///   as permitted by `STag` and `ETag` in the
///     [XML specification][xmlspec-tag].
///
/// [xmlspec-tag]: https://www.w3.org/TR/xml/#dt-stag
pub trait EleSpan {
    /// A [`Span`] encompassing the entire opening element token.
    ///
    /// Note that what exactly this token represents varies.
    fn span(&self) -> Span;

    /// Span representing the relevant portion of the element tag.
    ///
    /// This is a more descriptive alias of [`EleSpan::span`] that may be
    ///   appropriate in certain contexts.
    fn tag_span(&self) -> Span {
        self.span()
    }

    /// A [`Span`] representing only the element name,
    ///   if available.
    ///
    /// An element name is _not_ available for empty tags.
    /// Rather than complicating the API with [`Option`],
    ///   [`EleSpan::span`] is returned instead.
    fn name_span(&self) -> Span;
}

impl EleSpan for OpenSpan {
    fn span(&self) -> Span {
        match self {
            Self(t, _) => *t,
        }
    }

    fn name_span(&self) -> Span {
        match self {
            // <open  ...>
            //  ^^^^ offset '<' and length of name
            //
            // If the length is 0,
            //   then this will result in a 0-length span at the location
            //   that the element name ought to be,
            //     and so the resulting span will still be useful.
            // This should not happen for tokens read using XIR,
            //   but may happen for system-generated tokens.
            Self(t, name_len) => {
                t.context().span(t.offset().saturating_add(1), *name_len)
            }
        }
    }
}

impl EleSpan for CloseSpan {
    fn span(&self) -> Span {
        match self {
            Self(t, _) => *t,
        }
    }

    fn name_span(&self) -> Span {
        match self {
            // If the length of the element name is 0,
            //   then this must be an empty tag,
            //     which contains no independent element name.
            //
            // <foo ' />
            //      ' ^^
            Self(_t, 0) => self.span(),

            // </close  >
            //   ^^^^^ offset '</' and length of name
            Self(t, name_len) => {
                t.context().span(t.offset().saturating_add(2), *name_len)
            }
        }
    }
}

/// Lightly-structured XML tokens with associated [`Span`]s.
///
/// This is a streamable IR for XML.
/// A writer requires knowledge only of a previous state,
///   such as whether a node is open,
///   and so this IR can be processed by a simple state machine
///     (see [`writer::WriterState`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// Opening tag of an element.
    Open(QName, OpenSpan),

    /// Closing tag of an element.
    ///
    /// If the name is [`None`],
    ///   then the tag is self-closing.
    /// This is intended primarily as a safety measure:
    ///   It allows writers to act as simple state machines without having
    ///     to ensure balancing by indicating that a node was intended to
    ///     self-close.
    ///   Otherwise,
    ///     we wouldn't know whether to self-close or to close and then
    ///     create a new closing tag;
    ///       if we blindly did the former,
    ///         we risk losing a closing tag when it wasn't intended.
    ///   Instead of losing tags,
    ///     writers can error,
    ///       indicating a bug in the stream.
    ///
    /// The reason for using an option here rather than a variant is to
    ///   simplify pattern matching,
    ///     given especially that bindings after `@` in patterns have not
    ///     yet been stabalized at the time of writing (but are very
    ///     close!).
    Close(Option<QName>, CloseSpan),

    /// Element attribute name.
    AttrName(QName, Span),

    /// Element attribute value.
    AttrValue(SymbolId, Span),

    /// A portion of an element attribute value.
    ///
    /// This allows for concatenating values into an attribute value without
    ///   having to copy values.
    /// The last fragment must be a [`Token::AttrValue`].
    ///
    /// Since each fragment contains a span,
    ///   this also potentially gives higher resolution for the origin of
    ///   components of generated attribute values.
    ///
    /// _This should be used only for writing._
    /// These will never be encountered during reading,
    ///   and so to keep the parsers and IRs simple,
    ///   there is no support for fragments beyond XIR.
    /// (There was in the past,
    ///    but it was removed.)
    AttrValueFragment(SymbolId, Span),

    /// Comment node.
    Comment(SymbolId, Span),

    /// Character data as part of an element.
    ///
    /// See also [`CData`](Token::CData) variant.
    Text(SymbolId, Span),

    /// CData node (`<![CDATA[...]]>`).
    ///
    /// _Warning: It is up to the caller to ensure that the string `]]>` is
    ///   not present in the text!_
    /// This is intended for reading existing XML data where CData is
    ///   already present,
    ///     not for producing new CData safely!
    CData(SymbolId, Span),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // _Do not_ render large amounts of text here;
        //   this is not only a risk depending on what is output,
        //     but the diagnostic system also quote source lines to provide
        //     the necessary context.
        match self {
            Self::Open(qname, _) => write!(f, "`<{}>`", qname),
            Self::Close(Some(qname), _) => write!(f, "`</{}>`", qname),
            // Its context is contained within the Open,
            //   and hopefully any user-visible errors will display that instead.
            Self::Close(None, _) => {
                write!(f, "`/>`")
            }
            Self::AttrName(qname, _) => {
                write!(f, "`@{}`", qname)
            }
            Self::AttrValue(attr_val, _) => {
                write!(f, "attribute value `{}`", attr_val)
            }
            Self::AttrValueFragment(attr_val, _) => {
                write!(f, "attribute value fragment `{}`", attr_val)
            }
            Self::Comment(..) => write!(f, "comment"),
            Self::Text(..) => write!(f, "text"),
            Self::CData(..) => write!(f, "CDATA"),
        }
    }
}

impl crate::parse::Token for Token {
    fn ir_name() -> &'static str {
        "XIR"
    }

    /// Retrieve the [`Span`] associated with a given [`Token`].
    ///
    /// Every token has an associated span.
    fn span(&self) -> Span {
        use Token::*;

        match self {
            Open(_, OpenSpan(span, _))
            | Close(_, CloseSpan(span, _))
            | AttrName(_, span)
            | AttrValue(_, span)
            | AttrValueFragment(_, span)
            | Comment(_, span)
            | Text(_, span)
            | CData(_, span) => *span,
        }
    }
}

impl crate::parse::Object for Token {}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::convert::ExpectInto;
    use crate::sym::GlobalSymbolIntern;
    use std::convert::TryInto;
    use std::fmt::Debug;

    type TestResult = Result<(), Box<dyn std::error::Error>>;

    // Prefer [`open`] below when possible.
    impl From<Span> for OpenSpan {
        fn from(span: Span) -> Self {
            Self::without_name_span(span)
        }
    }

    // Prefer [`close`] below when possible.
    impl From<Span> for CloseSpan {
        fn from(span: Span) -> Self {
            Self::without_name_span(span)
        }
    }

    /// Hastily and lazily produce a [`XirfToken::Open`].
    ///
    /// This function is not suitable for production use as it does not
    ///   produce a complete [`OpenSpan`].
    pub fn open<Q: TryInto<QName>, S: Into<OpenSpan>>(
        qname: Q,
        span: S,
    ) -> Token
    where
        <Q as TryInto<QName>>::Error: Debug,
    {
        Token::Open(qname.unwrap_into(), span.into())
    }

    /// Hastily and lazily produce a [`XirfToken::Close`] for an empty tag.
    ///
    /// This is [`close`] with the omission of the `qname` argument; the
    ///   type parameter `Q` cannot be inferred if the value is [`None`].
    ///
    /// This function is not suitable for production use as it does not
    ///   produce a complete [`OpenSpan`].
    pub fn close_empty<S: Into<CloseSpan>>(span: S) -> Token {
        Token::Close(None, span.into())
    }

    /// Hastily and lazily produce a [`XirfToken::Close`].
    ///
    /// See also [`close_empty`] if `Q` cannot be inferred.
    ///
    /// This function is not suitable for production use as it does not
    ///   produce a complete [`OpenSpan`].
    pub fn close<Q: TryInto<QName>, S: Into<CloseSpan>>(
        qname: Option<Q>,
        span: S,
    ) -> Token
    where
        <Q as TryInto<QName>>::Error: Debug,
    {
        Token::Close(qname.map(ExpectInto::unwrap_into), span.into())
    }

    mod name {
        use super::*;

        #[test]
        fn ncname_comparable_to_sym() {
            let foo = "foo".intern();
            assert_eq!(NCName(foo), foo);
        }

        #[test]
        fn ncname_try_into_from_str_no_colon() -> TestResult {
            let name: NCName = "no-colon".try_into()?;
            assert_eq!(name, "no-colon".intern());
            Ok(())
        }

        #[test]
        fn ncname_try_into_from_str_fails_with_colon() {
            assert_eq!(
                NCName::try_from("look:a-colon"),
                Err(SpanlessError::NCColon("look:a-colon".into()))
            );
        }

        #[test]
        fn ncname_from_byte_slice() -> TestResult {
            let name: NCName = (b"no-colon" as &[u8]).try_into()?;
            assert_eq!(name, "no-colon".intern());
            Ok(())
        }

        #[test]
        fn ncname_from_byte_slice_fails_with_colon() {
            assert_eq!(
                NCName::try_from(b"a:colon" as &[u8]),
                Err(SpanlessError::NCColon("a:colon".into()))
            );
        }

        #[test]
        fn local_name_from_local_part_only() -> TestResult {
            let name = QName::new_local("foo".try_into()?);

            assert_eq!(name.local_name(), "foo".try_into()?);
            assert_eq!(None, name.prefix());

            Ok(())
        }

        #[test]
        fn local_name_from_option_tuple() -> TestResult {
            let name: QName = (Option::<&str>::None, "foo").try_into()?;

            assert_eq!(name.local_name(), "foo".try_into()?);
            assert_eq!(None, name.prefix());

            Ok(())
        }

        #[test]
        fn fully_qualified_name() -> TestResult {
            let name: QName = ("foons", "foo").try_into()?;

            assert_eq!(name.prefix(), Some("foons".try_into()?));
            assert_eq!(name.local_name(), "foo".try_into()?);

            Ok(())
        }
    }

    mod ele_span {
        use super::*;
        use crate::span::DUMMY_CONTEXT as DC;

        #[test]
        fn open_without_attrs() {
            // See docblock for [`EleSpan`].
            const T: Span = DC.span(0, 8); // Relevant portion of tag
            const N: Span = DC.span(1, 4); // Element name

            let sut = OpenSpan(T, N.len());

            assert_eq!(sut.span(), T);
            assert_eq!(sut.name_span(), N);
        }

        #[test]
        fn open_with_attrs() {
            // See docblock for [`EleSpan`].
            const T: Span = DC.span(0, 5); // Relevant portion of tag
            const N: Span = DC.span(1, 4); // Element name

            let sut = OpenSpan(T, N.len());

            assert_eq!(sut.span(), T);
            assert_eq!(sut.name_span(), N);
        }

        #[test]
        fn close() {
            // See docblock for [`EleSpan`].
            const T: Span = DC.span(0, 10); // Relevant portion of tag
            const N: Span = DC.span(2, 5); //  Element name

            let sut = CloseSpan(T, N.len());

            assert_eq!(sut.span(), T);
            assert_eq!(sut.name_span(), N);
        }

        #[test]
        fn close_empty() {
            // See docblock for [`EleSpan`].
            const T: Span = DC.span(9, 2); // Relevant portion of tag

            let sut = CloseSpan(T, 0);

            assert_eq!(sut.span(), T);
            // There is no name,
            //   only Zuul.
            assert_eq!(sut.name_span(), T);
        }
    }
}
