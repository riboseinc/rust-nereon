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
// ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NO/T
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

use std::ffi::OsString;
use std::io;
use std::path::Path;

mod libnereon;

#[derive(Debug, PartialEq)]
pub enum NocData {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Noc>),
    Float(f64),
    Object(Vec<Noc>),
}

#[derive(Debug, PartialEq)]
pub struct Noc {
    key: String,
    data: NocData,
}

#[derive(Debug, PartialEq)]
pub enum NosData {
    Int(i64),
    Bool(bool),
    String(String),
    IpPort(i32),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub struct Nos {
    name: String,
    data: NosData,
    is_set: bool,
    sw_short: String,
    sw_long: String,
    desc_short: String,
    desc_long: String,
    env: String,
    noc_key: String,
}

pub fn nereon<'a, I>(
    nos: Option<&str>,
    noc: Option<&Path>,
    argv: I,
) -> io::Result<(Vec<Nos>, Option<Noc>)>
where
    I: IntoIterator<Item = OsString>,
{
    libnereon::nereon(nos, noc, argv)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
