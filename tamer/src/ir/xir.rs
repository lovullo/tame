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
//!
//! XIR is _not_ intended to be comprehensive,
//!   or even general-purpose---it
//!     exists to solve concerns specific to TAMER's construction.
//!
//! _This is a work in progress!_

use crate::span::Span;
use crate::sym::{
    CIdentStaticSymbolId, GlobalSymbolIntern, SymbolId, UriStaticSymbolId,
};
use std::convert::{TryFrom, TryInto};
use std::fmt::Display;
use std::ops::Deref;

pub mod pred;
pub mod tree;
pub mod writer;

macro_rules! qname_const_inner {
    ($name:ident = :$local:ident) => {
        const $name: QName = QName::st_cid_local($local);
    };

    ($name:ident = $prefix:ident:$local:ident) => {
        const $name: QName = QName::st_cid($prefix, $local);
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

// TODO: Move into crate::sym if this is staying around.
macro_rules! newtype_symbol {
	{$($(#[$meta:meta])* pub struct $name:ident;)*} => {
        $(
		    $(#[$meta])*
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            pub struct $name(SymbolId);

            impl Deref for $name {
                type Target = SymbolId;

                fn deref(&self) -> &Self::Target {
                    &self.0
                }
            }

            impl PartialEq<SymbolId> for $name {
                fn eq(&self, other: &SymbolId) -> bool {
                    self.0 == *other
                }
            }
        )*
	};
}

// TODO: Derive macro instead?
newtype_symbol! {
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
    pub struct NCName;
}

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

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    /// Provided name contains a `':'`.
    NCColon(String),

    /// Provided string contains non-ASCII-whitespace characters.
    NotWhitespace(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NCColon(name) => {
                write!(f, "NCName must not contain a colon: `{}`", name)
            }
            Self::NotWhitespace(s) => {
                write!(f, "String contains non-ASCII-whitespace: `{}`", s)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Prefix(NCName);
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

#[derive(Debug, PartialEq, Eq)]
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
    pub fn new(prefix: Prefix, local_name: LocalPart) -> Self {
        Self(Some(prefix), local_name)
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
    pub const fn st_cid(
        prefix_sym: CIdentStaticSymbolId,
        local_sym: CIdentStaticSymbolId,
    ) -> Self {
        Self(
            Some(Prefix(NCName(prefix_sym.as_sym()))),
            LocalPart(NCName(local_sym.as_sym())),
        )
    }

    /// Construct a constant QName with a local name only from a static
    ///   C-style symbol.
    pub const fn st_cid_local(local_sym: CIdentStaticSymbolId) -> Self {
        Self(None, LocalPart(NCName(local_sym.as_sym())))
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, PartialEq, Eq)]
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
