// ASG IR
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

use super::{Asg, AsgError, FragmentText, IdentKind, Source};
use crate::{
    parse::{self, ParseState, Token, Transition, Transitionable},
    span::UNKNOWN_SPAN,
    sym::SymbolId,
};
use std::fmt::{Debug, Display};

///! Intermediate representation for construction of the
///!   [abstract semantic graph (ASG)](super) (AIR).
///!
///! AIR serves as an abstraction layer between higher-level parsers and the
///!   aggregate ASG.
///! It allows parsers to operate as a raw stream of data without having to
///!   worry about ownership of or references to the ASG,
///!     and allows for multiple such parsers to be joined.
///!
///! AIR is _not_ intended to replace the API of the ASG---it
///!   is intended as a termination point for the parsing pipeline,
///!     and as such implements a subset of the ASG's API that is suitable
///!     for aggregating raw data from source and object files.
///! Given that it does so little and is so close to the [`Asg`] API,
///!   one might say that the abstraction is as light as air,
///!   but that would surely result in face-palming and so we're not going
///!     air such cringeworthy dad jokes here.

pub type IdentSym = SymbolId;
pub type DepSym = SymbolId;

/// AIR token.
///
/// These tokens mimic a public API for the ASG,
///   and allow parsers to be completely decoupled from the ASG object that
///   they will eventually aggregate data into.
///
/// This IR is not intended to perform sophisticated manipulation of the
///   ASG---it
///     is intended to perform initial aggregation as part of a parsing
///     phase,
///       populating the ASG with the raw data that that will be
///       subsequently analyzed and rewritten.
#[derive(Debug, PartialEq)]
pub enum AirToken {
    /// Declare a resolved identifier.
    IdentDecl(IdentSym, IdentKind, Source),
    /// Declare an external identifier that must be resolved before linking.
    IdentExternDecl(IdentSym, IdentKind, Source),
    /// Declare that an identifier depends on another for its definition.
    IdentDep(IdentSym, DepSym),
    /// Associate a code fragment with an identifier.
    IdentFragment(IdentSym, FragmentText),
    /// Root an identifier.
    IdentRoot(IdentSym),
}

impl Token for AirToken {
    fn span(&self) -> crate::span::Span {
        // TODO: This can be provided once the xmlo files store source
        //   locations for symbols.
        UNKNOWN_SPAN
    }
}

impl parse::Object for AirToken {}

impl Display for AirToken {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirToken::*;

        match self {
            IdentDecl(sym, ..) => {
                write!(f, "declaration of identifier `{sym}`")
            }
            IdentExternDecl(sym, ..) => {
                write!(f, "declaration of external identifier `{sym}`")
            }
            IdentDep(isym, dsym) => write!(
                f,
                "declaration of identifier dependency `{isym} -> {dsym}`"
            ),
            IdentFragment(sym, ..) => {
                write!(f, "identifier `{sym}` fragment text")
            }
            IdentRoot(sym) => write!(f, "rooting of identifier `{sym}`"),
        }
    }
}

/// AIR parser state.
///
/// This currently has no parsing state;
///   all state is stored on the ASG itself,
///     which is the parsing context.
#[derive(Debug, PartialEq, Eq, Default)]
pub enum AirState {
    #[default]
    Empty,
}

impl ParseState for AirState {
    type Token = AirToken;
    type Object = ();
    type Error = AsgError;

    /// Destination [`Asg`] that this parser lowers into.
    ///
    /// This ASG will be yielded by [`parse::Parser::finalize`].
    type Context = Asg;

    fn parse_token(
        self,
        tok: Self::Token,
        asg: &mut Self::Context,
    ) -> crate::parse::TransitionResult<Self> {
        use AirState::*;
        use AirToken::*;

        match (self, tok) {
            (Empty, IdentDecl(sym, kind, src)) => {
                asg.declare(sym, kind, src).map(|_| ()).transition(Empty)
            }

            (Empty, IdentExternDecl(sym, kind, src)) => asg
                .declare_extern(sym, kind, src)
                .map(|_| ())
                .transition(Empty),

            (Empty, IdentDep(sym, dep)) => {
                asg.add_dep_lookup(sym, dep);
                Transition(Empty).incomplete()
            }

            (Empty, IdentFragment(sym, text)) => {
                asg.set_fragment(sym, text).map(|_| ()).transition(Empty)
            }

            (Empty, IdentRoot(sym)) => {
                let obj = asg.lookup_or_missing(sym);
                asg.add_root(obj);

                Transition(Empty).incomplete()
            }
        }
    }

    fn is_accepting(&self) -> bool {
        *self == Self::Empty
    }
}

impl Display for AirState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use AirState::*;

        // This is not terribly useful beyond indicating which parser caused
        //   an error.
        match self {
            Empty => write!(f, "awaiting AIR input for ASG"),
        }
    }
}

// These are tested as if they are another API directly atop of the ASG,
//   since that is how they are used.
#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use crate::{
        asg::{Ident, Object},
        parse::{ParseError, Parsed},
    };

    use super::*;

    type Sut = AirState;

    #[test]
    fn ident_decl() {
        let sym = "foo".into();
        let kind = IdentKind::Tpl;
        let src = Source {
            src: Some("test/decl".into()),
            ..Default::default()
        };

        let toks = vec![AirToken::IdentDecl(sym, kind.clone(), src.clone())]
            .into_iter();
        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

        let asg = sut.finalize().unwrap();

        let ident_node =
            asg.lookup(sym).expect("identifier was not added to graph");
        let ident = asg.get(ident_node).unwrap();

        assert_eq!(
            Ok(ident),
            Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .map(Object::Ident)
                .as_ref(),
        );

        // Re-instantiate the parser and test an error by attempting to
        //   redeclare the same identifier.
        let bad_toks = vec![AirToken::IdentDecl(sym, kind, src)].into_iter();
        let mut sut = Sut::parse_with_context(bad_toks, asg);

        assert_matches!(
            sut.next(),
            Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
        );
    }

    #[test]
    fn ident_extern_decl() {
        let sym = "foo".into();
        let kind = IdentKind::Tpl;
        let src = Source {
            src: Some("test/decl-extern".into()),
            ..Default::default()
        };

        let toks =
            vec![AirToken::IdentExternDecl(sym, kind.clone(), src.clone())]
                .into_iter();
        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

        let asg = sut.finalize().unwrap();

        let ident_node =
            asg.lookup(sym).expect("identifier was not added to graph");
        let ident = asg.get(ident_node).unwrap();

        assert_eq!(
            Ok(ident),
            Ident::declare(sym)
                .extern_(kind, src.clone())
                .map(Object::Ident)
                .as_ref(),
        );

        // Re-instantiate the parser and test an error by attempting to
        //   redeclare with a different kind.
        let different_kind = IdentKind::Meta;
        let bad_toks =
            vec![AirToken::IdentExternDecl(sym, different_kind, src)]
                .into_iter();
        let mut sut = Sut::parse_with_context(bad_toks, asg);

        assert_matches!(
            sut.next(),
            Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
        );
    }

    #[test]
    fn ident_dep() {
        let ident = "foo".into();
        let dep = "dep".into();

        let toks = vec![AirToken::IdentDep(ident, dep)].into_iter();
        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

        let asg = sut.finalize().unwrap();

        let ident_node = asg
            .lookup(ident)
            .expect("identifier was not added to graph");
        let dep_node = asg.lookup(dep).expect("dep was not added to graph");

        assert!(asg.has_dep(ident_node, dep_node));
    }

    #[test]
    fn ident_fragment() {
        let sym = "frag".into();
        let kind = IdentKind::Tpl;
        let src = Source {
            src: Some("test/frag".into()),
            ..Default::default()
        };
        let frag = "fragment text".into();

        let toks = vec![
            // Identifier must be declared before it can be given a
            //   fragment.
            AirToken::IdentDecl(sym, kind.clone(), src.clone()),
            AirToken::IdentFragment(sym, frag),
        ]
        .into_iter();

        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentFragment

        let asg = sut.finalize().unwrap();

        let ident_node =
            asg.lookup(sym).expect("identifier was not added to graph");
        let ident = asg.get(ident_node).unwrap();

        assert_eq!(
            Ok(ident),
            Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .and_then(|resolved| resolved.set_fragment(frag))
                .map(Object::Ident)
                .as_ref(),
        );

        // Re-instantiate the parser and test an error by attempting to
        //   re-set the fragment.
        let bad_toks = vec![AirToken::IdentFragment(sym, frag)].into_iter();
        let mut sut = Sut::parse_with_context(bad_toks, asg);

        assert_matches!(
            sut.next(),
            Some(Err(ParseError::StateError(AsgError::IdentTransition(_)))),
        );
    }

    // Adding a root before the identifier exists should add a
    //   `Ident::Missing`.
    #[test]
    fn ident_root_missing() {
        let sym = "toroot".into();

        let toks = vec![AirToken::IdentRoot(sym)].into_iter();
        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next());

        let asg = sut.finalize().unwrap();

        let ident_node = asg
            .lookup(sym)
            .expect("identifier was not added to the graph");
        let ident = asg.get(ident_node).unwrap();

        // The identifier did not previously exist,
        //   and so a missing node is created as a placeholder.
        assert_eq!(&Object::Ident(Ident::Missing(sym)), ident);

        // And that missing identifier should be rooted.
        assert!(asg.is_rooted(ident_node));
    }

    #[test]
    fn ident_root_existing() {
        let sym = "toroot".into();
        let kind = IdentKind::Tpl;
        let src = Source {
            src: Some("test/root-existing".into()),
            ..Default::default()
        };

        // Ensure that it won't auto-root based on the kind,
        //   otherwise we won't be testing the right thing.
        assert!(!kind.is_auto_root());

        let toks = vec![
            AirToken::IdentDecl(sym, kind.clone(), src.clone()),
            AirToken::IdentRoot(sym),
        ]
        .into_iter();

        let mut sut = Sut::parse(toks);

        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentDecl
        assert_eq!(Some(Ok(Parsed::Incomplete)), sut.next()); // IdentRoot

        let asg = sut.finalize().unwrap();

        let ident_node = asg
            .lookup(sym)
            .expect("identifier was not added to the graph");
        let ident = asg.get(ident_node).unwrap();

        // The previously-declared identifier...
        assert_eq!(
            Ok(ident),
            Ident::declare(sym)
                .resolve(kind.clone(), src.clone())
                .map(Object::Ident)
                .as_ref()
        );

        // ...should have been subsequently rooted.
        assert!(asg.is_rooted(ident_node));
    }
}
