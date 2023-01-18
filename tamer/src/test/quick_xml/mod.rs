// quick_xml mocks
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

use quick_xml::Result as XmlResult;
use std::borrow::Cow;
use std::cell::Cell;
use std::io::BufRead;

pub enum MockXmlEvent<'a> {
    Start(MockBytesStart<'a>),
    End(MockBytesEnd<'a>),
    Empty(MockBytesStart<'a>),
    #[allow(dead_code)]
    Text(MockBytesText<'a>),
}

pub struct MockBytesStart<'a> {
    name: &'a [u8],
    attrs: Cell<Option<MockAttributes<'a>>>,
}

impl<'a> MockBytesStart<'a> {
    pub fn new(name: &'a [u8], attrs: Option<MockAttributes<'a>>) -> Self {
        Self {
            name,
            attrs: Cell::new(attrs),
        }
    }

    pub fn name(&self) -> &[u8] {
        self.name
    }

    pub fn attributes(&self) -> MockAttributes {
        self.attrs.take().expect("missing mock attributes")
    }
}

pub struct MockBytesEnd<'a> {
    name: Cow<'a, [u8]>,
}

impl<'a> MockBytesEnd<'a> {
    pub fn new(name: &'a [u8]) -> Self {
        Self {
            name: Cow::Borrowed(name),
        }
    }

    pub fn name(&self) -> &[u8] {
        &*self.name
    }
}

pub struct MockBytesText<'a> {
    #[allow(dead_code)]
    content: Cow<'a, [u8]>,
}

impl<'a> MockBytesText<'a> {
    pub fn new(content: &'a [u8]) -> Self {
        Self {
            content: Cow::Borrowed(content),
        }
    }

    pub fn escaped(&self) -> &[u8] {
        self.content.as_ref()
    }
}

pub struct MockAttributes<'a> {
    attrs: Vec<MockAttribute<'a>>,
    with_checks: Option<bool>,
}

impl<'a> MockAttributes<'a> {
    pub fn new(attrs: Vec<MockAttribute<'a>>) -> Self {
        Self {
            attrs,
            with_checks: None,
        }
    }

    pub fn with_checks(&mut self, val: bool) -> &mut Self {
        self.with_checks = Some(val);
        self
    }
}

impl<'a> Iterator for MockAttributes<'a> {
    type Item = XmlResult<MockAttribute<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        // We read output from Saxon, which will always be valid
        if self.with_checks != Some(false) {
            panic!("MockAttributes expected with_checks false")
        }

        self.attrs.pop().map(|attr| Ok(attr))
    }
}

pub struct MockAttribute<'a> {
    pub key: &'a [u8],
    pub value: Cow<'a, [u8]>,
}

impl<'a> MockAttribute<'a> {
    pub fn new(key: &'a [u8], value: &'a [u8]) -> Self {
        Self {
            key,
            value: Cow::Borrowed(&value[..]),
        }
    }
}

pub struct MockXmlReader<B: BufRead> {
    _reader: B,

    pub check_end: Option<bool>,

    /// Closure yielding the next event for `read_event`.
    ///
    /// This exists exclusively to avoid adding a lifetime parameter to
    ///   the mock when providing stub data.
    /// A closure must be set before calling `read_event` to avoid a
    ///   panic.
    pub next_event: Option<
        Box<dyn for<'a> Fn(&'a mut Vec<u8>, u8) -> XmlResult<MockXmlEvent<'a>>>,
    >,

    pub event_i: u8,

    /// Next string to yield for a text node.
    pub next_text: Option<XmlResult<String>>,

    pub given_text_ele: Option<String>,

    pub read_to_end_name: Option<String>,
}

impl<B: BufRead> MockXmlReader<B> {
    pub fn from_reader(reader: B) -> Self {
        Self {
            _reader: reader,
            check_end: None,
            next_event: None,
            event_i: 0,
            next_text: None,
            given_text_ele: None,
            read_to_end_name: None,
        }
    }

    pub fn check_end_names(&mut self, val: bool) -> &mut Self {
        self.check_end = Some(val);
        self
    }

    pub fn read_event<'a, 'b>(
        &'a mut self,
        buf: &'b mut Vec<u8>,
    ) -> XmlResult<MockXmlEvent<'b>> {
        let result =
            (self.next_event.as_ref().expect("missing mock next_event"))(
                buf,
                self.event_i,
            );

        self.event_i += 1;
        result
    }

    pub fn read_text<K: AsRef<[u8]>>(
        &mut self,
        end: K,
        _buf: &mut Vec<u8>,
    ) -> XmlResult<String> {
        self.given_text_ele =
            Some(String::from_utf8(end.as_ref().to_vec()).unwrap());

        self.next_text.take().expect("missing mock next_text")
    }

    pub fn read_to_end<K: AsRef<[u8]>>(
        &mut self,
        end: K,
        _buf: &mut Vec<u8>,
    ) -> XmlResult<()> {
        self.read_to_end_name =
            Some(String::from_utf8(end.as_ref().to_vec()).unwrap());

        Ok(())
    }
}
