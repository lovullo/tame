// quick_xml mocks
//
//  Copyright (C) 2014-2020 Ryan Specialty Group, LLC.
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
