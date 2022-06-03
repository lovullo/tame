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

use crate::parse;
use crate::span::Span;
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
pub mod iter;
pub mod pred;
pub mod reader;
pub mod st;
pub mod tree;
pub mod writer;

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

/// A sequence of one or more whitespace characters.
///
/// Whitespace here is expected to consist of `[ \n\t\r]`
///   (where the first character in that class is a space).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Whitespace(SymbolId);

impl Deref for Whitespace {
    type Target = SymbolId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<&str> for Whitespace {
    type Error = SpanlessError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // We do not expect this to ever be a large value based on how we
        //   use it.
        // If it is, well, someone's doing something they ought not to be
        //   and we're not going to optimize for it.
        if !value.as_bytes().iter().all(u8::is_ascii_whitespace) {
            return Err(SpanlessError::NotWhitespace(value.into()));
        }

        Ok(Self(value.intern()))
    }
}

impl From<Whitespace> for SymbolId {
    fn from(ws: Whitespace) -> Self {
        ws.0
    }
}

impl Display for Whitespace {
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
    Open(QName, Span),

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
    Close(Option<QName>, Span),

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

    /// Similar to `Text`,
    ///   but intended for use where only whitespace is allowed,
    ///     such as alignment of attributes.
    Whitespace(Whitespace, Span),
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
            Self::Whitespace(..) => write!(f, "whitespace"),
        }
    }
}

impl parse::Token for Token {
    /// Retrieve the [`Span`] associated with a given [`Token`].
    ///
    /// Every token has an associated span.
    fn span(&self) -> Span {
        use Token::*;

        match self {
            Open(_, span)
            | Close(_, span)
            | AttrName(_, span)
            | AttrValue(_, span)
            | AttrValueFragment(_, span)
            | Comment(_, span)
            | Text(_, span)
            | CData(_, span)
            | Whitespace(_, span) => *span,
        }
    }
}

impl parse::Object for Token {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::sym::GlobalSymbolIntern;
    use std::convert::TryInto;

    type TestResult = Result<(), Box<dyn std::error::Error>>;

    lazy_static! {
        static ref S: Span =
            Span::from_byte_interval((0, 0), "test case".intern());
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

    #[test]
    fn whitespace() -> TestResult {
        assert_eq!(Whitespace::try_from("  ")?, "  ".try_into()?);
        assert_eq!(Whitespace::try_from(" \t ")?, " \t ".try_into()?);

        assert_eq!(
            Whitespace::try_from("not ws!"),
            Err(SpanlessError::NotWhitespace("not ws!".into(),))
        );

        Ok(())
    }

    #[test]
    fn whitespace_as_text() -> TestResult {
        assert_eq!(" ".intern(), Whitespace::try_from(" ")?.into(),);

        Ok(())
    }
}
