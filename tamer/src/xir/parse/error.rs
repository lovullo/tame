// XIR element parser generator errors
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

//! Parsing errors from the parser generator.

use core::fmt::Debug;
use std::{
    error::Error,
    fmt::{Display, Formatter},
    marker::PhantomData,
};

use crate::{
    diagnose::{Annotate, AnnotatedSpan, Diagnostic},
    fmt::{DisplayFn, DisplayWrapper, ListDisplayWrapper, TtQuote},
    span::Span,
    xir::{
        attr::Attr,
        flat::{RefinedText, XirfToken},
        fmt::XmlAttrList,
        EleSpan, OpenSpan, QName,
    },
};

use super::{AttrParseState, Nt, SumNt};

#[derive(Debug, PartialEq)]
pub enum NtError<NT: Nt, A: AttrParseState> {
    /// An element was expected,
    ///   but the name of the element was unexpected.
    UnexpectedEle(QName, Span),

    /// Unexpected input while expecting an end tag for this
    ///   element.
    ///
    /// The span corresponds to the opening tag.
    CloseExpected(QName, OpenSpan, XirfToken<RefinedText>),

    Attrs(AttrParseError<A>, PhantomData<NT>),
}

impl<NT: Nt, A: AttrParseState> From<AttrParseError<A>> for NtError<NT, A> {
    fn from(e: AttrParseError<A>) -> Self {
        Self::Attrs(e, PhantomData::default())
    }
}

impl<NT: Nt, A: AttrParseState> Error for NtError<NT, A> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        // TODO
        None
    }
}

impl<NT: Nt, A: AttrParseState> Display for NtError<NT, A> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use crate::xir::fmt::{TtCloseXmlEle, TtOpenXmlEle};

        match self {
            Self::UnexpectedEle(name, _) => write!(
                f,
                "unexpected {unexpected} (expecting {expected})",
                unexpected = TtOpenXmlEle::wrap(name),
                expected = TtOpenXmlEle::wrap(NT::matcher()),
            ),

            Self::CloseExpected(qname, _, tok) => write!(
                f,
                "expected {}, but found {}",
                TtCloseXmlEle::wrap(qname),
                TtQuote::wrap(tok)
            ),

            Self::Attrs(e, _) => Display::fmt(e, f),
        }
    }
}

impl<NT: Nt, A: AttrParseState> Diagnostic for NtError<NT, A> {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        use crate::{parse::Token, xir::fmt::TtCloseXmlEle};

        match self {
            Self::UnexpectedEle(_, ospan) => ospan
                .error(format!(
                    "expected {ele_name} here",
                    ele_name = TtQuote::wrap(NT::matcher())
                ))
                .into(),

            Self::CloseExpected(qname, ospan, tok) => vec![
                ospan.span().note("element starts here"),
                tok.span()
                    .error(format!("expected {}", TtCloseXmlEle::wrap(qname),)),
            ],

            Self::Attrs(e, _) => e.describe(),
        }
    }
}

/// Error during parsing of a sum nonterminal.
#[derive(Debug, PartialEq)]
pub enum SumNtError<NT: SumNt> {
    UnexpectedEle(QName, Span, PhantomData<NT>),
}

impl<NT: SumNt> Error for SumNtError<NT> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl<NT: SumNt> Display for SumNtError<NT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use crate::xir::fmt::TtOpenXmlEle;

        match self {
            Self::UnexpectedEle(qname, _, _) => {
                write!(f, "unexpected {}", TtOpenXmlEle::wrap(qname))
            }
        }
    }
}

impl<NT: SumNt> Diagnostic for SumNtError<NT> {
    fn describe(&self) -> Vec<crate::diagnose::AnnotatedSpan> {
        // Note that we should place expected values in the help
        //   footnote rather than the span label because it can
        //   get rather long.
        // Maybe in the future the diagnostic renderer can be
        //   smart about that based on the terminal width and
        //   automatically move into the footer.
        match self {
            Self::UnexpectedEle(qname, span, _) => span
                .error(format!(
                    "element {name} cannot appear here",
                    name = TtQuote::wrap(qname),
                ))
                .with_help(format!(
                    "expecting {}",
                    DisplayFn(NT::fmt_matches_top)
                ))
                .into(),
        }
    }
}

pub type ElementQName = QName;
pub type FirstSpan = Span;

/// Error while parsing element attributes.
#[derive(Debug, PartialEq)]
pub enum AttrParseError<S: AttrParseState> {
    /// One or more required attributes are missing.
    ///
    /// Since required attributes are not checked until parsing is complete,
    ///   and that determination requires a token of lookahead,
    ///   this error produces a lookahead token that must be handled by the
    ///   caller.
    ///
    /// This also provices the actual [`AttrParseState`],
    ///   which can be used to retrieve the missing required attributes
    ///     (using [`AttrParseState::required_missing`]),
    ///       can be used to retrieve information about the attributes that
    ///         _have_ been successfully parsed,
    ///       and can be used to resume parsing if desired.
    ///
    /// The caller must determine whether to proceed with parsing of the
    ///   element despite these problems;
    ///     such recovery is beyond the scope of this parser.
    MissingRequired(S),

    /// An attribute was encountered that was not expected by this parser.
    ///
    /// Parsing may recover by simply ignoring this attribute.
    UnexpectedAttr(Attr, ElementQName),

    /// An attribute with the same name as a previous attribute has been
    ///   encountered within the context of this element.
    ///
    /// The duplicate attribute is provided in its entirety.
    /// The key span of the first-encountered attribute of the same name is
    ///   included to provide more robust diagnostic information.
    /// The value of the previous attribute is not included because it is
    ///   expected that the diagnostic system will render the code
    ///   associated with the span;
    ///     displaying an attribute value in an error message is asking for
    ///     too much trouble given that it is arbitrary text.
    DuplicateAttr(Attr, FirstSpan, ElementQName),

    /// An error occurred while parsing an attribute value into the
    ///   declared type.
    InvalidValue(S::ValueError, ElementQName),
}

impl<S: AttrParseState> Display for AttrParseError<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::MissingRequired(st) => {
                let ele_name = st.element_name();
                write!(f, "element `{ele_name}` missing required ")?;

                XmlAttrList::fmt(&st.required_missing(), f)
            }

            Self::UnexpectedAttr(attr, ele_name) => {
                write!(
                    f,
                    "unexpected attribute `{attr}` for \
                       element `{ele_name}`"
                )
            }

            Self::DuplicateAttr(attr, _, ele_name) => {
                write!(
                    f,
                    "duplicate attribute `{attr}` for \
                       element element `{ele_name}`"
                )
            }

            Self::InvalidValue(ev, ele_name) => {
                Display::fmt(ev, f)?;
                write!(f, " for element {}", TtQuote::wrap(ele_name))
            }
        }
    }
}

impl<S: AttrParseState> Error for AttrParseError<S> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl<S: AttrParseState> Diagnostic for AttrParseError<S> {
    fn describe(&self) -> Vec<AnnotatedSpan> {
        match self {
            Self::MissingRequired(st) => st
                .element_span()
                .tag_span()
                .error(format!(
                    "missing required {}",
                    XmlAttrList::wrap(&st.required_missing()),
                ))
                .into(),

            // TODO: help stating attributes that can appear instead
            Self::UnexpectedAttr(attr @ Attr(.., aspan), ele_name) => aspan
                .key_span()
                .error(format!("element `{ele_name}` cannot contain `{attr}`"))
                .into(),

            Self::DuplicateAttr(Attr(name, _, aspan), first_span, _) => {
                vec![
                    first_span.note(format!(
                        "{} previously encountered here",
                        TtQuote::wrap(name)
                    )),
                    aspan.key_span().error(format!(
                        "{} here is a duplicate",
                        TtQuote::wrap(name)
                    )),
                ]
            }

            Self::InvalidValue(ev, _) => ev.describe(),
        }
    }
}
