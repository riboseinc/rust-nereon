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
pub enum NocError {
    Parse(ParseError),
    Convert(ConversionError),
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub positions: Vec<(usize, usize)>,
    pub reason: &'static str,
}

#[derive(Debug, PartialEq)]
pub struct ConversionError {
    pub keys: Vec<&'static str>,
    pub expected: &'static str,
    pub found: &'static str,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at ", self. reason);
        let mut it = self.positions.iter().peekable();
        while it.peek().is_some() {
            let (l, c) = it.next().unwrap();
            write!(f, "(line: {}, char {})", l, c);
            if it.peek().is_some() {
                write!(f, " while parsing ");
            }
        }
        Ok(())
    }
}

impl fmt::Display for ConversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Expected {} but found {} while parsing the value for ",
            self.expected, self.found
        );
        let mut it = self.keys.iter().peekable();
        while it.peek().is_some() {
            let k = it.next().unwrap();
            if it.peek().is_some() {
                write!(f, "\"{}\", ", k);
            } else {
                write!(f, "\"{}\"", k);
            }
        }
        Ok(())
    }
}

impl fmt::Display for NocError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            NocError::Parse(e) => write!(f, "{}", e),
            NocError::Convert(e) => write!(f, "{}", e),
        }
    }
}

impl ParseError {
    pub fn new(msg: &'static str, line_col: (usize, usize)) -> Self {
        Self {
            reason: msg,
            positions: vec![line_col],
        }
    }
    pub fn push_position(mut self, pos: (usize, usize)) -> Self {
        self.positions.push(pos);
        self
    }
}

impl ConversionError {
    pub fn new(expected: &'static str, found: &'static str) -> Self {
        Self {
            keys: Vec::new(),
            expected,
            found,
        }
    }

    pub fn unshift_key(mut self, key: &'static str) -> Self {
        self.keys.insert(0, key);
        self
    }
}
