// Lowering operations into XIR.
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

//! Lower [`XmleSections`] into a XIR [`Token`] stream for `xmle` output.
//!
//! This is the final step in the linker,
//!   producing the `xmle` file that can be used to produce the standalone
//!   `js` file
//!     (which is still part of the XSLT-based system at the time of
//!       writing).
//!
//! Use [`lower_iter`] to produce the lowering iterator,
//!   which can then use [`XmlWriter`](crate::xir::writer::XmlWriter)
//!   for writing.

use super::{super::LSPAN, section::XmleSections};
use crate::{
    asg::{Ident, IdentKind},
    sym::{st::raw, SymbolId},
    xir::{
        iter::{elem_wrap, ElemWrapIter},
        st::qname::*,
        CloseSpan, OpenSpan, QName, Token,
    },
};
use arrayvec::ArrayVec;
use std::{array, collections::hash_set, iter::Chain, vec};

const HEADER_SIZE: usize = 14;
type HeaderIter = array::IntoIter<Token, HEADER_SIZE>;

/// Beginning [`Token`]s representing the root node of an `xmle` document
///   and its immediate child.
#[inline]
fn header(pkg_name: SymbolId, relroot: SymbolId) -> HeaderIter {
    [
        Token::AttrName(QN_XMLNS, LSPAN),
        Token::AttrValue(raw::URI_LV_RATER, LSPAN),
        Token::AttrName(QN_XMLNS_PREPROC, LSPAN),
        Token::AttrValue(raw::URI_LV_PREPROC, LSPAN),
        Token::AttrName(QN_XMLNS_L, LSPAN),
        Token::AttrValue(raw::URI_LV_LINKER, LSPAN),
        Token::AttrName(QN_TITLE, LSPAN),
        Token::AttrValue(pkg_name, LSPAN),
        Token::AttrName(QN_PROGRAM, LSPAN),
        Token::AttrValue(raw::L_TRUE, LSPAN),
        Token::AttrName(QN_NAME, LSPAN),
        Token::AttrValue(pkg_name, LSPAN),
        Token::AttrName(QN_UUROOTPATH, LSPAN),
        Token::AttrValue(relroot, LSPAN),
    ]
    .into_iter()
}

const DEP_MAX_ATTRS: usize = 9;
const DEP_MAX_ATTRS_KEY_VAL: usize = DEP_MAX_ATTRS * 2;
const DEP_CLOSE: usize = 1; // open is never stored; see `refill_toks`

/// Size of [`DepListIter`] [`Token`] buffer.
const DEP_TOK_SIZE: usize = DEP_MAX_ATTRS_KEY_VAL + DEP_CLOSE;

/// Iterator that lowers [`XmleSections`] into `l:dep` as a XIR [`Token`]
///   stream.
///
/// This iterator functions by allocating a constant-sized
///   [`ArrayVec`]-based buffer that is populated with token data each time
///   an object is requested from the underlying iterator.
/// Once the buffer runs out,
///   another object is requested and the buffer populated with the
///   appropriate token stream.
/// This repeats until no more section object data is available.
struct DepListIter<'a> {
    /// Source data to lower into `l:deps`.
    iter: vec::IntoIter<&'a Ident>,
    /// Constant-size [`Token`] buffer used as a stack.
    toks: ArrayVec<Token, DEP_TOK_SIZE>,
    /// Relative path to project root.
    relroot: SymbolId,
}

impl<'a> DepListIter<'a> {
    #[inline]
    fn new(iter: vec::IntoIter<&'a Ident>, relroot: SymbolId) -> Self {
        Self {
            iter,
            toks: ArrayVec::new(),
            relroot,
        }
    }

    /// Re-fill buffer with a new list of [`Token]s representing the next
    ///   available object from the inner iterator.
    ///
    /// Each token is pushed onto the buffer _in reverse_,
    ///   since it is treated like a stack;
    ///     this allows us to cheaply `pop` with each [`Iterator::next`]
    ///     call.
    fn refill_toks(&mut self) -> Option<Token> {
        // Tokens will be popped, so push in reverse.
        // They are arranged in the same order as the original writer so
        //   that we can diff the two;
        //     TODO: re-order sensibly once we're done.
        self.iter.next().map(|obj| {
            match obj {
                Ident::Opaque(sym, kind, src)
                | Ident::IdentFragment(sym, kind, src, _) => (*sym, kind, src),
                _ => unreachable!(
                    "identifier should have been filtered out during sorting: {:?}",
                    obj,
                ),
            }
        }).map(|(name, kind, src)| {
            self.toks.push(Token::Close(None, CloseSpan::empty(LSPAN)));

            self.toks_push_attr(QN_DESC, src.desc);
            self.toks_push_attr(QN_YIELDS, src.yields);
            self.toks_push_attr(QN_PARENT, src.parent);

            if let Some(pkg_name) = src.pkg_name {
                // TODO: Introduce newtypes so that we do not have to make unsafe
                //   assumptions.
                self.toks.push(Token::AttrValue(pkg_name, LSPAN));
                self.toks.push(Token::AttrValueFragment(self.relroot, LSPAN));
                self.toks.push(Token::AttrName(QN_SRC, LSPAN));
            }

            self.toks_push_attr(QN_P_GENERATED, match src.generated {
                true => Some(raw::L_TRUE),
                false => None,
            });

            // TODO: Note that we're not yet writing any span information.
            self.toks_push_attr(QN_NAME, Some(name.symbol()));
            self.toks_push_obj_attrs(kind);

            Token::Open(QN_P_SYM, OpenSpan::without_name_span(LSPAN))
        })
    }

    /// Optionally push an attribute if it has a `value`.
    ///
    /// Like [`refill_toks`](DepListIter::refill_toks),
    ///   we push in reverse.
    #[inline]
    fn toks_push_attr(&mut self, name: QName, value: Option<SymbolId>) {
        if let Some(val) = value {
            self.toks.push(Token::AttrValue(val, LSPAN));
            self.toks.push(Token::AttrName(name, LSPAN));
        }
    }

    /// Generate object-specific attributes.
    ///
    /// All objects will produce a [`QN_TYPE`] attribute.
    fn toks_push_obj_attrs(&mut self, kind: &IdentKind) {
        match kind {
            IdentKind::Cgen(dim) | IdentKind::Class(dim) => {
                self.toks_push_attr(QN_DIM, Some((*dim).into()));
            }

            IdentKind::Const(dim, dtype)
            | IdentKind::Func(dim, dtype)
            | IdentKind::Gen(dim, dtype)
            | IdentKind::Lparam(dim, dtype)
            | IdentKind::Param(dim, dtype) => {
                self.toks_push_attr(QN_DTYPE, Some((*dtype).into()));
                self.toks_push_attr(QN_DIM, Some((*dim).into()));
            }

            IdentKind::Rate(dtype) | IdentKind::Type(dtype) => {
                self.toks_push_attr(QN_DTYPE, Some((*dtype).into()));
            }

            // No additional attributes (explicit match so that the
            // exhaustiveness check will warn us if new ones are added)
            IdentKind::Tpl
            | IdentKind::MapHead
            | IdentKind::Map
            | IdentKind::MapTail
            | IdentKind::RetMapHead
            | IdentKind::RetMap
            | IdentKind::RetMapTail
            | IdentKind::Meta
            | IdentKind::Worksheet => {}
        }

        self.toks_push_attr(QN_TYPE, Some(kind.as_sym()));
    }
}

impl<'a> Iterator for DepListIter<'a> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.toks.pop().or_else(|| self.refill_toks())
    }
}

// Maximum size of token buffer.
//
// See [`MapFromIter::refill_toks`].
const MAP_FROM_TOK_SIZE: usize = 3;

/// Generate `l:map-from` section.
struct MapFromsIter {
    /// Source data to lower into `l:deps`.
    iter: hash_set::IntoIter<SymbolId>,
    /// Token buffer.
    toks: ArrayVec<Token, MAP_FROM_TOK_SIZE>,
}

impl MapFromsIter {
    #[inline]
    fn new(iter: hash_set::IntoIter<SymbolId>) -> Self {
        Self {
            iter,
            // Most of the time we have a single `from` (4 tokens).
            toks: ArrayVec::new(),
        }
    }

    #[inline]
    fn refill_toks(&mut self) -> Option<Token> {
        self.iter.next().map(|from| {
            self.toks.push(Token::Close(None, CloseSpan::empty(LSPAN)));

            self.toks.push(Token::AttrValue(from, LSPAN));
            self.toks.push(Token::AttrName(QN_NAME, LSPAN));

            Token::Open(QN_L_FROM, OpenSpan::without_name_span(LSPAN))
        })
    }
}

impl Iterator for MapFromsIter {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.toks.pop().or_else(|| self.refill_toks())
    }
}

/// Produce text fragments associated with objects.
///
/// Here, "text" refers to the compiled program text.
struct FragmentIter {
    iter: vec::IntoIter<SymbolId>,
}

impl FragmentIter {
    #[inline]
    fn new(iter: vec::IntoIter<SymbolId>) -> Self {
        Self { iter }
    }
}

impl Iterator for FragmentIter {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .by_ref()
            .map(|frag| Token::Text(frag, LSPAN))
            .next()
    }
}

/// Iterator that lazily lowers `xmle` object files into XIR.
///
/// This serves primarily to encapsulate the nasty iterator type without
///   having to resort to dynamic dispatch,
///     since this iterator will receive over a million calls on larger
///     programs (and hundreds of thousands on smaller).
#[allow(clippy::type_complexity)] // more clear as one type
pub struct LowerIter<'a>(
    ElemWrapIter<
        Chain<
            Chain<
                Chain<
                    Chain<
                        Chain<
                            Chain<HeaderIter, ElemWrapIter<DepListIter<'a>>>,
                            ElemWrapIter<MapFromsIter>,
                        >,
                        ElemWrapIter<FragmentIter>,
                    >,
                    ElemWrapIter<FragmentIter>,
                >,
                ElemWrapIter<FragmentIter>,
            >,
            ElemWrapIter<FragmentIter>,
        >,
    >,
);

impl<'a> Iterator for LowerIter<'a> {
    type Item = Token;

    /// Produce the next XIR [`Token`] representing the lowering of
    ///   [`XmleSections`] from the [ASG](crate::asg).
    ///
    /// This produces a single token at a time,
    ///   but [`DepListIter`] buffers tokens before emitting them,
    ///     so certain `next` calls have a processing cost while others are
    ///     essentially free.
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

/// Lower [`XmleSections`] into a XIR [`Token`] stream for writing.
///
/// This produces the final representation for the `xmle` file,
///   which can be written using
///   [`XmlWriter`](crate::xir::writer::XmlWriter).
#[inline]
pub fn lower_iter<'a, S: XmleSections<'a>>(
    mut sections: S,
    pkg_name: SymbolId,
    relroot: SymbolId,
) -> LowerIter<'a> {
    LowerIter(elem_wrap(
        QN_PACKAGE,
        LSPAN,
        header(pkg_name, relroot)
            .chain(elem_wrap(
                QN_L_DEP,
                LSPAN,
                DepListIter::new(sections.take_deps().into_iter(), relroot),
            ))
            .chain(elem_wrap(
                QN_L_MAP_FROM,
                LSPAN,
                MapFromsIter::new(sections.take_map_froms().into_iter()),
            ))
            .chain(elem_wrap(
                QN_L_MAP_EXEC,
                LSPAN,
                FragmentIter::new(sections.take_map().into_iter()),
            ))
            .chain(elem_wrap(
                QN_L_RETMAP_EXEC,
                LSPAN,
                FragmentIter::new(sections.take_retmap().into_iter()),
            ))
            .chain(elem_wrap(
                QN_L_STATIC,
                LSPAN,
                FragmentIter::new(sections.take_static().into_iter()),
            ))
            .chain(elem_wrap(
                QN_L_EXEC,
                LSPAN,
                FragmentIter::new(sections.take_exec().into_iter()),
            )),
    ))
}

#[cfg(test)]
pub mod test;
