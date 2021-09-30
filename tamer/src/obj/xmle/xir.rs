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

//! WIP lowering to XIR-based `xmle`.

use crate::{
    ir::{
        asg::{IdentObject, IdentObjectData, Sections, SectionsIter},
        xir::{AttrValue, QName, Token},
    },
    ld::LSPAN,
    sym::{st::*, SymbolId},
};
use arrayvec::ArrayVec;
use std::array;
use std::iter::Chain;

qname_const! {
    QN_DESC: :L_DESC,
    QN_GENERATED: L_PREPROC:L_GENERATED,
    QN_L_DEP: L_L:L_DEP,
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

const HEADER_SIZE: usize = 16;
type HeaderIter = array::IntoIter<Token, HEADER_SIZE>;

fn header(pkg_name: SymbolId, relroot: SymbolId) -> HeaderIter {
    let pkg_name_val = AttrValue::Escaped(pkg_name);

    // See [`array`] docs regarding differences between Rust 2018 and 2021
    // editions regarding arrays and [`IntoIter`].  This was written in
    // edition 2018; 2021 will be out in a few months at the time of
    // writing.
    array::IntoIter::new([
        Token::Open(QN_PACKAGE, LSPAN),
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
        Token::Open(QN_L_DEP, LSPAN),
    ])
}

const DEP_MAX_ATTRS: usize = 9;
const DEP_MAX_ATTRS_KEY_VAL: usize = DEP_MAX_ATTRS * 2;
const DEP_CLOSE: usize = 1; // open is never stored; see `refill_toks`
const DEP_TOK_SIZE: usize = DEP_MAX_ATTRS_KEY_VAL + DEP_CLOSE;

struct DepListIter<'a, T: IdentObjectData> {
    iter: SectionsIter<'a, T>,
    toks: ArrayVec<Token, DEP_TOK_SIZE>,
    relroot: AttrValue,
}

type DepIter<'a, T> = DepListIter<'a, T>;

impl<'a, T: IdentObjectData> DepListIter<'a, T> {
    fn refill_toks(&mut self) -> Option<Token> {
        // Tokens will be popped, so push in reverse.
        self.iter.next().map(|obj| {
            let ident = obj.as_ident().expect("unexpected non-identifier object");

            match ident {
                IdentObject::Ident(sym, kind, src)
                | IdentObject::IdentFragment(sym, kind, src, _) => (*sym, kind, src),
                _ => unreachable!(
                    "identifier should have been filtered out during sorting: {:?}",
                    ident,
                ),
            }
        }).and_then(|(sym, kind, src)| {
            self.toks.push(Token::Close(None, LSPAN));

            if let Some(pkg_name) = src.pkg_name {
                self.toks.push(Token::AttrValue(AttrValue::Escaped(pkg_name), LSPAN));
                self.toks.push(Token::AttrValueFragment(self.relroot, LSPAN));
                self.toks.push(Token::AttrName(QN_SRC, LSPAN));
            }

            self.toks_push_attr(QN_GENERATED, match src.generated {
                true => Some(L_TRUE.as_sym()),
                false => None,
            });

            self.toks_push_attr(QN_DESC, src.desc);
            self.toks_push_attr(QN_YIELDS, src.yields);
            self.toks_push_attr(QN_PARENT, src.parent);
            self.toks_push_attr(QN_NAME, Some(sym));
            self.toks_push_attr(QN_TYPE, Some(kind.as_sym()));

            Some(Token::Open(QN_P_SYM, LSPAN))
        })
    }

    #[inline]
    fn toks_push_attr(&mut self, name: QName, value: Option<SymbolId>) {
        if let Some(val) = value {
            self.toks
                .push(Token::AttrValue(AttrValue::Escaped(val), LSPAN));
            self.toks.push(Token::AttrName(name, LSPAN));
        }
    }
}

impl<'a, T: IdentObjectData> Iterator for DepListIter<'a, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.toks.pop().or_else(|| self.refill_toks())
    }
}

fn deps<'a, T: IdentObjectData>(
    sections: &'a Sections<T>,
    relroot: SymbolId,
) -> DepIter<'a, T> {
    DepListIter {
        iter: sections.iter_all(),
        toks: ArrayVec::new(),
        // TODO: we cannot trust that an arbitrary symbol is escaped; this
        // needs better typing, along with other things.
        relroot: AttrValue::Escaped(relroot),
    }
}

const FOOTER_SIZE: usize = 2;
type FooterIter = array::IntoIter<Token, FOOTER_SIZE>;

#[inline]
fn footer() -> FooterIter {
    array::IntoIter::new([
        Token::Close(Some(QN_L_DEP), LSPAN),
        Token::Close(Some(QN_PACKAGE), LSPAN),
    ])
}

/// Iterator that lazily lowers `xmle` object files into Xir.
///
/// This serves primarily to encapsulate the nasty iterator type without
///   having to resort to dynamic dispatch,
///     since this iterator will receive hundreds of thousands of calls for
///     large programs.
pub struct LowerIter<'a, T: IdentObjectData>(
    Chain<Chain<HeaderIter, DepIter<'a, T>>, FooterIter>,
);

impl<'a, T: IdentObjectData> Iterator for LowerIter<'a, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub fn lower_iter<'a, T: IdentObjectData>(
    sections: &'a Sections<T>,
    pkg_name: SymbolId,
    relroot: SymbolId,
) -> LowerIter<'a, T> {
    LowerIter(
        header(pkg_name, relroot)
            .chain(deps(sections, relroot))
            .chain(footer()),
    )
}

// Note: at the time of writing, the Xir tree did not exist, and so these
// assert verbosely on the stream itself.
#[cfg(test)]
pub mod test {
    use super::*;
    use crate::convert::ExpectInto;
    use crate::ir::{
        asg::{Dim, IdentKind, Source},
        xir::{
            pred::{not, open},
            tree::{parser_from, Attr},
        },
    };
    use crate::sym::{GlobalSymbolIntern, GlobalSymbolResolve};

    type TestResult = Result<(), Box<dyn std::error::Error>>;

    #[test]
    fn test_produces_header() -> TestResult {
        let empty = Sections::<IdentObject>::new();
        let name = "test-pkg".intern();
        let relroot = "rel/root/".intern();

        let result = lower_iter(&empty, name, relroot)
            .take_while(|tok| match tok {
                // TODO
                Token::Close(_, _) => false,
                _ => true,
            })
            .collect::<Vec<Token>>();

        assert_eq!(Token::Open(QN_PACKAGE, LSPAN), result[0]);

        Ok(())
    }

    #[test]
    fn test_closes_package() -> TestResult {
        let empty = Sections::<IdentObject>::new();

        let result =
            lower_iter(&empty, "foo".intern(), "relroot".intern()).last();

        assert_eq!(Some(Token::Close(Some(QN_PACKAGE), LSPAN)), result);
        Ok(())
    }

    #[test]
    fn test_writes_deps() -> TestResult {
        let mut sections = Sections::new();
        let relroot = "relroot-deps".intern();

        let objs = [
            IdentObject::Ident(
                "a".intern(),
                IdentKind::Meta,
                Source {
                    desc: Some("test desc".intern()),
                    ..Default::default()
                },
            ),
            IdentObject::Ident(
                "b".intern(),
                IdentKind::MapHead,
                Default::default(),
            ),
            IdentObject::Ident(
                "c".intern(),
                IdentKind::Cgen(Dim::from_u8(1)),
                Source {
                    yields: Some("yieldsValue".intern()),
                    parent: Some("cparent".intern()),
                    generated: true,
                    pkg_name: Some("pkg/name".intern()),
                    ..Default::default()
                },
            ),
        ];

        objs.iter().for_each(|x| sections.consts.push_body(x));

        let mut iter = parser_from(
            lower_iter(&sections, "pkg".intern(), relroot)
                .skip_while(not(open(QN_L_DEP))),
        );

        let given = iter
            .next()
            .expect("tree object expected")
            .unwrap() // Tree
            .into_element()
            .expect("element expected");

        // Sanity check to ensure we have the element we're expecting.
        assert_eq!(QN_L_DEP, given.name());

        let children = given.children();

        assert_eq!(
            children.len(),
            objs.len(),
            "expected child node for each dependency"
        );

        let p_syms = children.into_iter().map(|child| {
            let ele =
                child.as_element().expect("child expected to be an element");

            assert_eq!(QN_P_SYM, ele.name());

            ele
        });

        p_syms.enumerate().for_each(|(i, sym)| {
            let ident = objs[i].as_ident().unwrap();
            let attrs = sym.attrs();

            assert_eq!(
                attrs.find(QN_NAME).and_then(|a| a.value_atom()),
                Some(AttrValue::Escaped(ident.name().unwrap())),
            );

            assert_eq!(
                attrs.find(QN_TYPE).and_then(|a| a.value_atom()),
                Some(AttrValue::Escaped(ident.kind().unwrap().as_sym()))
            );

            let generated =
                attrs.find(QN_GENERATED).and_then(|a| a.value_atom());

            if let Some(Source {
                generated: true, ..
            }) = ident.src()
            {
                assert_eq!(
                    generated,
                    Some(AttrValue::Escaped("true".intern()))
                );
            } else {
                assert_eq!(generated, None);
            }

            if let Some(Source { parent, .. }) = ident.src() {
                assert_eq!(
                    attrs
                        .find("parent".unwrap_into())
                        .and_then(|a| a.value_atom()),
                    parent.map(|x| AttrValue::Escaped(x)),
                );
            }

            if let Some(Source { yields, .. }) = ident.src() {
                assert_eq!(
                    attrs
                        .find("yields".unwrap_into())
                        .and_then(|a| a.value_atom()),
                    yields.map(|x| AttrValue::Escaped(x)),
                );
            }

            if let Some(Source {
                desc: Some(desc), ..
            }) = ident.src()
            {
                // We must take extra effort to compare these, since desc is
                // uninterned and therefore cannot be compared as a
                // `SymbolId`.  Once the reader takes care of creating the
                // symbol, we'll have no such problem.
                match attrs
                    .find("desc".unwrap_into())
                    .and_then(|a| a.value_atom())
                {
                    Some(AttrValue::Escaped(given)) => {
                        assert_eq!(desc.lookup_str(), given.lookup_str());
                    }
                    invalid => panic!("unexpected desc: {:?}", invalid),
                }
            }

            if let Some(Source {
                pkg_name: Some(pkg_name),
                ..
            }) = ident.src()
            {
                match attrs.find("src".unwrap_into()) {
                    Some(Attr::Extensible(parts)) => {
                        assert_eq!(
                            parts.value_fragments(),
                            &vec![
                                (AttrValue::Escaped(relroot), LSPAN),
                                (AttrValue::Escaped(*pkg_name), LSPAN),
                            ]
                        );
                    }
                    invalid => panic!("unexpected desc: {:?}", invalid),
                }
            }
        });

        Ok(())
    }
}
