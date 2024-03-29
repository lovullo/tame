// Comparisons between Rust built-ins and memchr.
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

#![feature(test)]

//! Assessment of overhead of Xir compared to baselines.
//!
//! A lot of time in TAMER is spent parsing and writing XML files, so it's
//!   important that these operations be efficient.
//! Xir is intended to be a very lightweight IR,
//!   able to provide convenient abstractions and validations only when
//!   both necessary and desired.
//!
//! Rust touts "zero-cost abstractions",
//!   which is a generally true statement (with some exceptions) that allows
//!   us to create dense newtype abstractions that represent validated and
//!   structured data,
//!     at a compile-time but not runtime cost.
//! These tests serve to demonstrate that such a claim is true for Xir,
//!   and help to obviate any potential future regressions.

extern crate quick_xml;
extern crate tamer;
extern crate test;

use std::convert::{TryFrom, TryInto};
use tamer::sym::{GlobalSymbolIntern, GlobalSymbolResolve, SymbolId};
use tamer::xir::{NCName, QName, Token};
use test::Bencher;

fn gen_strs(n: usize, suffix: &str) -> Vec<String> {
    (0..n).map(|n| n.to_string() + suffix).collect()
}

mod name {
    use super::*;

    // Essentially duplicates sym::interner::global::with_all_new_1000, but
    // provides a local baseline that we can be sure will be available to
    // compare against, at a glance.
    #[bench]
    fn baseline_global_intern_str_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000, "foobar");

        bench.iter(|| {
            strs.iter()
                .map(|s| s.as_str().intern() as SymbolId)
                .for_each(drop);
        });
    }

    // This should be cost-free relative to the previous test.
    #[bench]
    fn ncname_new_unchecked_str_intern_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000, "foobar");

        bench.iter(|| {
            strs.iter()
                .map(|s| unsafe { NCName::new_unchecked(s.as_str().intern()) })
                .for_each(drop);
        });
    }

    // This duplicates a memchr test, but allows us to have a comparable
    // baseline at a glance.
    #[bench]
    fn baseline_str_contains_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000, "foobar");

        bench.iter(|| {
            strs.iter().map(|s| s.as_str().contains(':')).for_each(drop);
        });
    }

    // This should be approximately as expensive as the two baselines added
    // together.
    #[bench]
    fn ncname_try_from_str_1000(bench: &mut Bencher) {
        let strs = gen_strs(1000, "foobar");

        bench.iter(|| {
            strs.iter()
                .map(|s| NCName::try_from(s.as_str()))
                .for_each(drop);
        });
    }

    // Should be ~2x previous test, since it contains two `NCName`s.
    #[bench]
    fn qname_try_from_str_pair_1000(bench: &mut Bencher) {
        let prefixes = gen_strs(1000, "prefix");
        let names = gen_strs(1000, "name");

        bench.iter(|| {
            prefixes
                .iter()
                .zip(names.iter())
                .map(|(p, s)| QName::try_from((p.as_str(), s.as_str())))
                .for_each(drop);
        });
    }
}

mod writer {
    use super::*;
    use quick_xml::{
        events::{BytesStart, BytesText, Event as XmlEvent},
        Writer as QuickXmlWriter,
    };
    use std::borrow::Cow;
    use tamer::xir::{writer::XmlWriter, CloseSpan, Escaper, OpenSpan};
    use tamer::{span::Span, xir::DefaultEscaper};

    const FRAGMENT: &str = r#"<fragment>
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.
This is pretend fragment text.  We need a lot of it.</fragment>
"#;

    // TAME makes heavy use of attributes, which unfortunately requires
    // copies in quick-xml.  This will serve as our baseline---we want to
    // perform _at least_ as well (but we do end up performing much better,
    // despite the global symbol lookups).
    #[bench]
    fn baseline_quick_xml_empty_with_attrs_1000(bench: &mut Bencher) {
        let buf = Vec::<u8>::new();
        let mut writer = QuickXmlWriter::new(buf);

        bench.iter(|| {
            (0..1000).for_each(|_| {
                writer
                    .write_event(XmlEvent::Empty(
                        BytesStart::borrowed_name(b"test:foo").with_attributes(
                            vec![("first", "value"), ("second", "value2")],
                        ),
                    ))
                    .unwrap();
            });
        });
    }

    // Produces the same output as above.
    #[bench]
    fn xir_empty_with_attrs_preinterned_1000(bench: &mut Bencher) {
        let mut buf = Vec::<u8>::new();

        // Perform all interning beforehand, since in practice, values will
        // have been interned well before we get to the writer.  Further,
        // common values such as these (QNames) will be pre-defined and
        // reused.
        let span = Span::from_byte_interval((0, 0), "path".intern());
        let name = QName::try_from(("test", "foo")).unwrap();
        let attr1 = QName::new_local("first".try_into().unwrap());
        let attr2 = QName::new_local("second".try_into().unwrap());
        let val1 = "value".intern();
        let val2 = "value2".intern();

        // Prime the cache, since BytesStart is already assumed to be
        // escaped.  We will have cached on read in a real-world scenario.
        let escaper = DefaultEscaper::default();
        escaper.escape(val1);
        escaper.escape(val2);

        bench.iter(|| {
            (0..1000).for_each(|_| {
                vec![
                    Token::Open(name, OpenSpan::without_name_span(span)),
                    Token::AttrName(attr1, span),
                    Token::AttrValue(val1.into(), span),
                    Token::AttrName(attr2, span),
                    Token::AttrValue(val2.into(), span),
                    Token::Close(None, CloseSpan::empty(span)),
                ]
                .into_iter()
                .write(&mut buf, Default::default(), &escaper)
                .unwrap();
            });
        });
    }

    // The other major thing we do is output large amounts of text (the
    // linked fragments).
    #[bench]
    fn baseline_quick_xml_text_50(bench: &mut Bencher) {
        let buf = Vec::<u8>::with_capacity(FRAGMENT.len() * 50);
        let mut writer = QuickXmlWriter::new(buf);

        let frag: SymbolId = FRAGMENT.intern();

        bench.iter(|| {
            (0..50).for_each(|_| {
                writer
                    .write_event(XmlEvent::Text(BytesText::from_escaped_str(
                        Cow::Borrowed(&frag.lookup_str() as &str),
                    )))
                    .unwrap();
            });
        });
    }

    // This test and the above are expected to perform similarly, and can
    // vary wildy run-to-run.
    #[bench]
    fn xir_text_50(bench: &mut Bencher) {
        let mut buf = Vec::<u8>::with_capacity(FRAGMENT.len() * 50);
        let frag: SymbolId = FRAGMENT.intern();
        let span = Span::from_byte_interval((0, 0), "path".intern());

        // Prime the cache, since BytesStart is already assumed to be
        // escaped.
        let escaper = DefaultEscaper::default();
        escaper.escape(frag);

        bench.iter(|| {
            (0..50).for_each(|_| {
                Token::Text(frag, span)
                    .write(&mut buf, Default::default(), &escaper)
                    .unwrap();
            });
        });
    }
}
