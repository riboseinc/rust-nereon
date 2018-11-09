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

use super::{parse_noc, ConversionError, FromValue, Value};

#[derive(FromValue, Debug, PartialEq)]
pub struct Command {
    pub command: Vec<(String, Command)>,
    pub option: Vec<(String, UserOption)>,
}

#[derive(FromValue, Debug, PartialEq)]
pub struct UserOption {
    pub flags: Vec<String>,
    pub short: Option<String>,
    pub long: Option<String>,
    pub env: Option<String>,
    pub default_arg: Option<String>,
    pub default: Option<String>,
    pub hint: Option<String>,
    pub usage: String,
    pub key: Vec<String>,
}

#[derive(FromValue, Debug, PartialEq)]
pub struct Nos {
    pub name: String,
    pub authors: Vec<String>,
    pub version: String,
    pub license: String,
    pub command: Vec<(String, Command)>,
    pub option: Vec<(String, UserOption)>,
}

impl<'a> From<&'a str> for Nos {
    fn from(s: &str) -> Self {
        parse_noc::<Nos>(s).unwrap_or_else(|e| panic!("Invalid NOS string: {:?}", e))
    }
}

impl From<Value> for Nos {
    fn from(v: Value) -> Self {
        Nos::from_value(v).unwrap_or_else(|e| panic!("Invalid NOS Value: {:?}", e))
    }
}
