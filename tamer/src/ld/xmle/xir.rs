// Lowering operations into XIR.
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

//! Lower [`Sections`] into a XIR [`Token`] stream for `xmle` output.
//!
//! This is the final step in the linker,
//!   producing the `xmle` file that can be used to produce the standalone
//!   `js` file
//!     (which is still part of the XSLT-based system at the time of
//!       writing).
//!
//! Use [`lower_iter`] to produce the lowering iterator,
//!   which can then use [`XmlWriter`](crate::ir::xir::writer::XmlWriter)
//!   for writing.

use super::{super::LSPAN, Sections, SectionsIter};
use crate::{
    ir::{
        asg::{IdentKind, IdentObject},
        xir::{
            iter::{elem_wrap, ElemWrapIter},
            AttrValue, QName, Text, Token,
        },
    },
    sym::{st::*, SymbolId},
};
use arrayvec::ArrayVec;
use std::iter::Chain;
use std::{array, collections::hash_set};

qname_const! {
    QN_DESC: :L_DESC,
    QN_DIM: :L_DIM,
    QN_DTYPE: :L_DTYPE,
    QN_GENERATED: L_PREPROC:L_GENERATED,
    QN_L_DEP: L_L:L_DEP,
    QN_L_EXEC: L_L:L_EXEC,
    QN_L_FROM: L_L:L_FROM,
    QN_L_MAP_EXEC: L_L:L_MAP_EXEC,
    QN_L_MAP_FROM: L_L:L_MAP_FROM,
    QN_L_RETMAP_EXEC: L_L:L_RETMAP_EXEC,
    QN_L_STATIC: L_L:L_STATIC,
    QN_NAME: :L_NAME,
    QN_PACKAGE: :L_PACKAGE,
    QN_PARENT: :L_PARENT,
    QN_PROGRAM: :L_PROGRAM,
    QN_P_SYM: L_PREPROC:L_SYM,
    QN_SRC: :L_SRC,
    QN_TITLE: :L_TITLE,
    QN_TYPE: :L_TYPE,
    QN_UUROOTPATH: :L_UUROOTPATH,
    QN_XMLNS: :L_XMLNS,
    QN_XMLNS_L: L_XMLNS:L_L,
    QN_XMLNS_PREPROC: L_XMLNS:L_PREPROC,
    QN_YIELDS: :L_YIELDS,
}

const HEADER_SIZE: usize = 14;
type HeaderIter = array::IntoIter<Token, HEADER_SIZE>;

/// Beginning [`Token`]s representing the root node of an `xmle` document
///   and its immediate child.
#[inline]
fn header(pkg_name: SymbolId, relroot: SymbolId) -> HeaderIter {
    let pkg_name_val = AttrValue::Escaped(pkg_name);

    [
        Token::AttrName(QN_XMLNS, LSPAN),
        Token::AttrValue(AttrValue::st_uri(URI_LV_RATER), LSPAN),
        Token::AttrName(QN_XMLNS_PREPROC, LSPAN),
        Token::AttrValue(AttrValue::st_uri(URI_LV_PREPROC), LSPAN),
        Token::AttrName(QN_XMLNS_L, LSPAN),
        Token::AttrValue(AttrValue::st_uri(URI_LV_LINKER), LSPAN),
        Token::AttrName(QN_TITLE, LSPAN),
        Token::AttrValue(pkg_name_val, LSPAN),
        Token::AttrName(QN_PROGRAM, LSPAN),
        Token::AttrValue(AttrValue::st_cid(L_TRUE), LSPAN),
        Token::AttrName(QN_NAME, LSPAN),
        Token::AttrValue(pkg_name_val, LSPAN),
        Token::AttrName(QN_UUROOTPATH, LSPAN),
        Token::AttrValue(AttrValue::Escaped(relroot), LSPAN),
    ]
    .into_iter()
}

const DEP_MAX_ATTRS: usize = 9;
const DEP_MAX_ATTRS_KEY_VAL: usize = DEP_MAX_ATTRS * 2;
const DEP_CLOSE: usize = 1; // open is never stored; see `refill_toks`

/// Size of [`DepListIter`] [`Token`] buffer.
const DEP_TOK_SIZE: usize = DEP_MAX_ATTRS_KEY_VAL + DEP_CLOSE;

/// Iterator that lowers [`Sections`] into `l:dep` as a XIR [`Token`]
///   stream.
///
/// This iterator functions by allocating a constant-sized
///   [`ArrayVec`]-based buffer that is populated with token data each time
///   an object is requested from the underlying [`SectionsIter`].
/// Once the buffer runs out,
///   another object is requested and the buffer populated with the
///   appropriate token stream.
/// This repeats until no more section object data is available.
struct DepListIter<'a> {
    /// Source data to lower into `l:deps`.
    iter: SectionsIter<'a>,
    /// Constant-size [`Token`] buffer used as a stack.
    toks: ArrayVec<Token, DEP_TOK_SIZE>,
    /// Relative path to project root.
    relroot: AttrValue,
}

impl<'a> DepListIter<'a> {
    #[inline]
    fn new(sections: &'a Sections, relroot: SymbolId) -> Self {
        Self {
            iter: sections.iter_all(),
            toks: ArrayVec::new(),
            // TODO: we cannot trust that an arbitrary symbol is escaped; this
            // needs better typing, along with other things.
            relroot: AttrValue::Escaped(relroot),
        }
    }

    /// Re-fill buffer with a new list of [`Token]s representing the next
    ///   available object from the inner [`SectionsIter`].
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
                IdentObject::Ident(sym, kind, src)
                | IdentObject::IdentFragment(sym, kind, src, _) => (*sym, kind, src),
                _ => unreachable!(
                    "identifier should have been filtered out during sorting: {:?}",
                    obj,
                ),
            }
        }).and_then(|(sym, kind, src)| {
            self.toks.push(Token::Close(None, LSPAN));

            self.toks_push_attr(QN_DESC, src.desc);
            self.toks_push_attr(QN_YIELDS, src.yields);
            self.toks_push_attr(QN_PARENT, src.parent);

            if let Some(pkg_name) = src.pkg_name {
                self.toks.push(Token::AttrValue(AttrValue::Escaped(pkg_name), LSPAN));
                self.toks.push(Token::AttrValueFragment(self.relroot, LSPAN));
                self.toks.push(Token::AttrName(QN_SRC, LSPAN));
            }

            self.toks_push_attr(QN_GENERATED, match src.generated {
                true => Some(L_TRUE.as_sym()),
                false => None,
            });

            self.toks_push_attr(QN_NAME, Some(sym));
            self.toks_push_obj_attrs(kind);

            Some(Token::Open(QN_P_SYM, LSPAN))
        })
    }

    /// Optionally push an attribute if it has a `value`.
    ///
    /// _The provided `value` must be escaped;_
    ///   it is blindly wrapped in [`AttrValue::Escaped`]!
    ///
    /// Like [`refill_toks`](DepListIter::refill_toks),
    ///   we push in reverse.
    #[inline]
    fn toks_push_attr(&mut self, name: QName, value: Option<SymbolId>) {
        if let Some(val) = value {
            self.toks
                .push(Token::AttrValue(AttrValue::Escaped(val), LSPAN));
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
    fn new<'a>(sections: &'a Sections) -> Self {
        let iter = Self {
            iter: sections.iter_map_froms_uniq(),
            // Most of the time we have a single `from` (4 tokens).
            toks: ArrayVec::new(),
        };

        iter
    }

    #[inline]
    fn refill_toks(&mut self) -> Option<Token> {
        self.iter.next().and_then(|from| {
            self.toks.push(Token::Close(None, LSPAN));

            self.toks
                .push(Token::AttrValue(AttrValue::Escaped(from), LSPAN));
            self.toks.push(Token::AttrName(QN_NAME, LSPAN));

            Some(Token::Open(QN_L_FROM, LSPAN))
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
struct FragmentIter<'a> {
    iter: SectionsIter<'a>,
}

impl<'a> FragmentIter<'a> {
    #[inline]
    fn new(iter: SectionsIter<'a>) -> Self {
        Self { iter }
    }
}

impl<'a> Iterator for FragmentIter<'a> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .by_ref()
            .filter_map(|obj| {
                match obj {
                    IdentObject::IdentFragment(_, _, _, frag) => Some(*frag),

                    // These will never have fragments.
                    IdentObject::Ident(_, IdentKind::Cgen(_), _)
                    | IdentObject::Ident(_, IdentKind::Gen(_, _), _)
                    | IdentObject::Ident(_, IdentKind::Lparam(_, _), _) => None,

                    // Error, but we really should catch that during Section
                    // lowering.
                    _ => todo! {},
                }
            })
            .map(|frag| Token::Text(Text::Escaped(frag), LSPAN))
            .next()
    }
}

/// Iterator that lazily lowers `xmle` object files into XIR.
///
/// This serves primarily to encapsulate the nasty iterator type without
///   having to resort to dynamic dispatch,
///     since this iterator will receive over a million calls on larger
///     programs (and hundreds of thousands on smaller).
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
                        ElemWrapIter<FragmentIter<'a>>,
                    >,
                    ElemWrapIter<FragmentIter<'a>>,
                >,
                ElemWrapIter<FragmentIter<'a>>,
            >,
            ElemWrapIter<FragmentIter<'a>>,
        >,
    >,
);

impl<'a> Iterator for LowerIter<'a> {
    type Item = Token;

    /// Produce the next XIR [`Token`] representing the lowering of
    ///   [`Sections`] from the [ASG](crate::ir::asg).
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

/// Lower [`Sections`] into a XIR [`Token`] stream for writing.
///
/// This produces the final representation for the `xmle` file,
///   which can be written using
///   [`XmlWriter`](crate::ir::xir::writer::XmlWriter).
#[inline]
pub fn lower_iter<'a>(
    sections: &'a Sections,
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
                DepListIter::new(sections, relroot),
            ))
            .chain(elem_wrap(QN_L_MAP_FROM, LSPAN, MapFromsIter::new(sections)))
            .chain(elem_wrap(
                QN_L_MAP_EXEC,
                LSPAN,
                FragmentIter::new(sections.iter_map()),
            ))
            .chain(elem_wrap(
                QN_L_RETMAP_EXEC,
                LSPAN,
                FragmentIter::new(sections.iter_retmap()),
            ))
            .chain(elem_wrap(
                QN_L_STATIC,
                LSPAN,
                FragmentIter::new(sections.iter_static()),
            ))
            .chain(elem_wrap(
                QN_L_EXEC,
                LSPAN,
                FragmentIter::new(sections.iter_exec()),
            )),
    ))
}

#[cfg(test)]
pub mod test;
