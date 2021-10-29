// XML IR (XIR)
//
//  Copyright (C) 2014-2021 Ryan Specialty Group, LLC.
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
//! Parsing and Safety
//! ==================
//! Many XIR elements know how to safely parse into themselves,
//!   exposing [`TryFrom`] traits that will largely do the right thing for
//!   you.
//! For example,
//!   [`QName`] is able to construct itself from a byte slice and from a
//!     string tuple,
//!       among other things.
//!
//! ```
//! use tamer::ir::xir::QName;
//! use tamer::sym::GlobalSymbolIntern;
//!
//!# fn main() -> Result<(), tamer::ir::xir::Error> {
//! let src = "foo:bar".as_bytes();
//! let qname = QName::try_from(src)?;
//!
//! assert_eq!(qname, ("foo", "bar").try_into()?);
//!
//!# Ok(())
//!# }
//! ```
//!
//! However,
//!   certain elements cannot fully parse on their own because require
//!   important contextual information,
//!     such as [`AttrValue`],
//!       which requires knowing whether the provided value is escaped.
//! It is important that the caller is diligent in making the proper
//!   determination in these cases,
//!     otherwise it could result in situations ranging from invalid
//!     compiler output to security vulnerabilities
//!       (via XML injection).
//!
//! To parse an entire XML document,
//!   see [`reader`].

use crate::span::Span;
use crate::sym::{
    st_as_sym, CIdentStaticSymbolId, GlobalSymbolIntern,
    GlobalSymbolInternBytes, StaticSymbolId, SymbolId, TameIdentStaticSymbolId,
    UriStaticSymbolId,
};
use memchr::memchr;
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;

mod error;
pub use error::Error;

pub mod iter;
pub mod pred;
pub mod reader;
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

/// A static symbol that can be safely converted into a [`QName`] without
///   any checks.
///
/// This must only be implemented on static symbol types that are known to
///   be valid QNames.
pub trait QNameCompatibleStaticSymbolId: StaticSymbolId {}

impl QNameCompatibleStaticSymbolId for CIdentStaticSymbolId {}
impl QNameCompatibleStaticSymbolId for TameIdentStaticSymbolId {}

#[doc(hidden)]
macro_rules! qname_const_inner {
    ($name:ident = :$local:ident) => {
        const $name: crate::ir::xir::QName =
            crate::ir::xir::QName::st_cid_local(&$local);
    };

    ($name:ident = $prefix:ident:$local:ident) => {
        const $name: crate::ir::xir::QName =
            crate::ir::xir::QName::st_cid(&$prefix, &$local);
    };
}

/// Construct a series of [`QName`] constants.
///
/// The syntax for each constant is `NAME: [PREFIX]:LOCAL`,
///   where `PREFIX` is optional.
///
/// See [`crate::sym::st`] for usable symbol constants.
#[macro_export]
macro_rules! qname_const {
    ($($name:ident: $($prefix:ident)? : $local:ident,)*) => {
        $(
            qname_const_inner!($name = $($prefix)?:$local);
        )*
    }
}

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
    type Error = Error;

    /// Attempt to parse a byte slice into an [`NCName`].
    ///
    /// If the slice contains `b':'`,
    ///   an error will be produced.
    /// No other checks are performed beyond checking that the byte sequence
    ///   represents a valid UTF-8 string.
    /// The string will be interned for you.
    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        match value.contains(&b':') {
            true => Err(Error::NCColon(value.to_owned())),
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
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.contains(':') {
            return Err(Error::NCColon(value.into()));
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
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

impl TryFrom<&str> for LocalPart {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(Self(value.try_into()?))
    }
}

/// A sequence of one or more whitespace characters.
///
/// Whitespace here is expected to consist of `[ \n\t\r]`
///   (where the first character in that class is a space).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Whitespace(SymbolId);

impl Deref for Whitespace {
    type Target = SymbolId;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<&str> for Whitespace {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // We do not expect this to ever be a large value based on how we
        //   use it.
        // If it is, well, someone's doing something they ought not to be
        //   and we're not going to optimize for it.
        if !value.as_bytes().iter().all(u8::is_ascii_whitespace) {
            return Err(Error::NotWhitespace(value.into()));
        }

        Ok(Self(value.intern()))
    }
}

impl From<Whitespace> for Text {
    fn from(ws: Whitespace) -> Self {
        // Whitespace needs no escaping
        Self::Escaped(ws.0)
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
    type Error = Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(QName(None, value.try_into()?))
    }
}

impl TryFrom<&[u8]> for QName {
    type Error = Error;

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
                Err(Error::InvalidQName(name.to_owned()))
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

/// Represents text and its escaped state.
///
/// Being explicit about the state of escaping allows us to skip checks when
///   we know that the generated text could not possibly require escaping.
/// This does, however, put the onus on the caller to ensure that they got
///   the escaping status correct.
/// (TODO: More information on why this burden isn"t all that bad,
///    despite the risk.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Text {
    /// Text node that requires escaping.
    ///
    /// Unescaped text requires further processing before writing.
    ///
    /// Note that,
    ///   since the unescaped text is interned,
    ///   it may be wasteful to intern a large text node with the intent of
    ///     escaping and re-interning later.
    /// Instead,
    ///   if escaping is only needed for writing,
    ///   it is likely better to leave it to the writer to escape,
    ///     which does _not_ require interning of the resulting string.
    Unescaped(SymbolId),

    /// Text node that either has already been escaped or is known not to
    ///   require escaping.
    ///
    /// Escaped text can be written as-is without any further processing.
    Escaped(SymbolId),
}

/// Represents an attribute value and its escaped contents.
///
/// Being explicit about the state of escaping allows us to skip checks when
///   we know that the generated text could not possibly require escaping.
/// This does, however, put the onus on the caller to ensure that they got
///   the escaping status correct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AttrValue {
    /// Value that requires escaping.
    ///
    /// Unescaped values require further processing before writing.
    Unescaped(SymbolId),

    /// Value that either has already been escaped or is known not to
    ///   require escaping.
    ///
    /// Escaped values can be written as-is without any further processing.
    Escaped(SymbolId),
}

impl AttrValue {
    /// Construct a constant escaped attribute from a static C-style symbol.
    pub const fn st_cid(sym: CIdentStaticSymbolId) -> Self {
        Self::Escaped(sym.as_sym())
    }

    /// Construct a constant escaped attribute from a static URI symbol.
    ///
    /// URIs are expected _not_ to contain quotes.
    pub const fn st_uri(sym: UriStaticSymbolId) -> Self {
        Self::Escaped(sym.as_sym())
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
    AttrValue(AttrValue, Span),

    /// A portion of an element attribute value.
    ///
    /// This allows for concatenating values into an attribute value without
    ///   having to copy values.
    /// The last fragment must be a [`Token::AttrValue`].
    ///
    /// Since each fragment contains a span,
    ///   this also potentially gives higher resolution for the origin of
    ///   components of generated attribute values.
    AttrValueFragment(AttrValue, Span),

    /// A delimiter indicating that attribute processing has ended and the
    ///   next token will be either a child node or [`Token::Close`].
    ///
    /// This allows for streaming attribute collection without any
    ///   lookahead,
    ///     which would otherwise require an iterator supporting a `peek`
    ///     operation.
    ///
    /// This is mandatory for _readers_ to produce,
    ///   but _writers must ignore it and not require it to be present_,
    ///     allowing for the reduction of token counts for generated XIR in
    ///     situations where we know that it will not be further parsed.
    AttrEnd,

    /// Comment node.
    Comment(Text, Span),

    /// Character data as part of an element.
    ///
    /// See also [`CData`](Token::CData) variant.
    Text(Text, Span),

    /// CData node (`<![CDATA[...]]>`).
    ///
    /// See also [`Text`](Token::Text) variant.
    ///
    /// _Warning: It is up to the caller to ensure that the string `]]>` is
    ///   not present in the text!_
    /// This is intended for reading existing XML data where CData is
    ///   already present,
    ///     not for producing new CData safely!
    CData(Text, Span),

    /// Similar to `Text`,
    ///   but intended for use where only whitespace is allowed,
    ///     such as alignment of attributes.
    Whitespace(Whitespace, Span),
}

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
                Err(Error::NCColon("look:a-colon".into()))
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
                Err(Error::NCColon("a:colon".into()))
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
            Err(Error::NotWhitespace("not ws!".into()))
        );

        Ok(())
    }

    #[test]
    fn whitespace_as_text() -> TestResult {
        assert_eq!(
            Text::Escaped(" ".intern()),
            Whitespace::try_from(" ")?.into(),
        );

        Ok(())
    }
}
