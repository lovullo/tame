// Tests for XML frontend
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

// NB: Due to the complexity and verbosity of mocking XML events,
//   these tests are coupled with the XML parser.
// Care should be taken to try to mitigate minor changes to the library's
//   output so as not to make these tests overly fragile.

use super::*;

type Sut<B> = XmlFrontendParser<B>;

// TODO: Just for initial testing; empty files shouldn't be valid, since
// they don't give the parser enough information as to what type of file it
// is.
#[test]
fn emits_eof_for_empty_file() {
    let stub_data: &[u8] = &[];
    let mut sut = Sut::new(stub_data);

    let result = sut.parse_next();

    assert!(matches!(result, Ok(FrontendEvent::Eof)));
}

// Until the parser is complete, we need raw tokens so that we can echo them
// back out.
#[test]
fn produces_raw_xml_events_as_tokens() -> Result<(), Box<dyn std::error::Error>>
{
    let stub_data: &[u8] = r#"<valid-xml xmlns="foons" />"#.as_bytes();
    let mut sut = Sut::new(stub_data);

    loop {
        match sut.parse_next()? {
            FrontendEvent::Token(Token {
                kind: XmlToken::RawXmlEvent((ns, ev)),
                lexeme: _,
                interval,
            }) => {
                if ns.is_none() {
                    continue;
                }

                // Interval should be the starting byte offset to the offset
                // of the final byte, not the byte after it.
                assert!(matches!(
                    interval,
                    Some(ClosedByteInterval(0, hi))
                        if hi == stub_data.len() - 1
                ));

                if let XmlEvent::Empty(start) = ev {
                    assert_eq!(start.name(), b"valid-xml");
                    break;
                }
            }

            x => panic!("Unexpected: {:?}", x),
        }
    }

    Ok(())
}

#[test]
fn produces_error_on_xml_parse_failure() {
    let stub_data: &[u8] = b"<ok /><!-- EOF in comment";
    let mut sut = Sut::new(stub_data);

    loop {
        match sut.parse_next() {
            Ok(FrontendEvent::Eof) => panic!("Expected error"),

            Err(e) => match e {
                FrontendError::UnrecoverableError {
                    source: XmlFrontendError::XmlError(_),
                    interval: ClosedByteInterval(x, y),
                } if x == 6 && y >= x => break,

                _ => panic!("Error mismatch: {:?}", e),
            },

            _ => continue,
        }
    }
}
