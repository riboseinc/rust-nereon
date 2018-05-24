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
pub enum CfgData {
    Int(i64),
    Bool(bool),
    String(String),
    Array(Vec<Cfg>),
    Float(f64),
    Object(Vec<Cfg>),
}

#[derive(Debug, PartialEq)]
pub enum MetaData {
    Int(i64),
    Bool(bool),
    String(String),
    IpPort(i32),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub struct Meta {
    name: String,
    data: MetaData,
    helper: bool,
    sw_short: String,
    sw_long: String,
    desc_short: String,
    desc_long: String,
    cfg_env: String,
    cfg_key: String,
}

#[derive(Debug, PartialEq)]
pub struct Cfg {
    key: String,
    data: CfgData,
}

pub fn nereon<'a, I>(
    cfg: Option<&Path>,
    meta: Option<&Path>,
    argv: I,
) -> io::Result<(Option<Cfg>, Vec<Meta>)>
where
    I: IntoIterator<Item = OsString>,
{
    libnereon::nereon(cfg, meta, argv)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
