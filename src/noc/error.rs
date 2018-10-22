// Copyright (c) 2018, [Ribose Inc](https://www.ribose.com).
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Error {
    ParseError {
        positions: Vec<(usize, usize)>,
        reason: &'static str,
    },
    ConversionError {
        keys: Vec<&'static str>,
        expected: &'static str,
        found: &'static str,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::ParseError { reason, positions } => {
                write!(f, "{} at ", reason);
                let mut it = positions.iter().peekable();
                while it.peek().is_some() {
                    let (l, c) = it.next().unwrap();
                    write!(f, "(line: {}, char {})", l, c);
                    if it.peek().is_some() {
                        write!(f, " while parsing ");
                    }
                }
            }
            Error::ConversionError {
                keys,
                expected,
                found,
            } => {
                write!(
                    f,
                    "Expected {} but found {} while parsing the value for ",
                    expected, found
                );
                let mut it = keys.iter().peekable();
                while it.peek().is_some() {
                    let k = it.next().unwrap();
                    if it.peek().is_some() {
                        write!(f, "\"{}\", ", k);
                    } else {
                        write!(f, "\"{}\"", k);
                    }
                }
            }
        }
        Ok(())
    }
}

impl Error {
    pub fn parse_error(msg: &'static str, line_col: Vec<(usize, usize)>) -> Self {
        Error::ParseError {
            reason: msg,
            positions: line_col,
        }
    }

    pub fn conversion_error(expected: &'static str, found: &'static str) -> Self {
        Error::ConversionError {
            keys: Vec::new(),
            expected,
            found,
        }
    }

    pub fn unshift_key(self, key: &'static str) -> Self {
        match self {
            Error::ConversionError {
                mut keys,
                expected,
                found,
            } => {
                keys.insert(0, key);
                Error::ConversionError {
                    keys,
                    expected,
                    found,
                }
            }
            Error::ParseError { .. } => unreachable!(),
        }
    }
}
