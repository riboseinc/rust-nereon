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

extern crate getopts;
extern crate regex;

#[derive(Clone)]
pub struct Opt {
    node: String,
    short: Option<String>,
    long: Option<String>,
    pub env: Option<String>,
    flags: u32,
    pub default: Option<String>,
    pub format: Option<String>,
    usage: Option<String>,
}

pub enum OptFlag {
    Optional = 1,
    Multiple = 2,
    NoArg = 4,
    OptionalArg = 8,
}

impl Opt {
    pub fn new(
        node: &str,
        short: Option<&str>,
        long: Option<&str>,
        env: Option<&str>,
        flags: u32,
        default: Option<&str>,
        format: Option<&str>,
        usage: Option<&str>,
    ) -> Opt {
        Opt {
            node: node.to_owned(),
            short: short.map(String::from),
            long: long.map(String::from),
            env: env.map(String::from),
            flags: flags,
            default: default.map(String::from),
            format: format.map(String::from),
            usage: usage.map(String::from),
        }
    }

    pub fn to_getopts(&self, options: &mut getopts::Options) {
        if self.short.is_some() || self.long.is_some() {
            let mut hasarg = getopts::HasArg::Yes;
            if self.flags & OptFlag::NoArg as u32 != 0 {
                hasarg = getopts::HasArg::No;
            } else if self.flags & OptFlag::OptionalArg as u32 != 0 {
                hasarg = getopts::HasArg::Maybe;
            }

            options.opt(
                self.short.as_ref().map_or("", String::as_str),
                self.long.as_ref().map_or("", String::as_str),
                self.usage.as_ref().map_or("", String::as_str),
                "",
                hasarg,
                getopts::Occur::Multi,
            );
        }
    }

    pub fn node_depth(&self) -> usize {
        self.node.matches('.').count()
    }

    pub fn get_branch_keys(&self) -> Vec<String> {
        let keys = self.node.split('.').collect::<Vec<_>>();
        keys[..keys.len() - 1]
            .iter()
            .map(|&s| s.to_owned())
            .collect()
    }

    pub fn get_leaf_key(&self) -> Option<String> {
        match self.node.as_ref() {
            "" => None,
            _ => Some(self.node.split('.').last().unwrap().to_owned())
        }
    }

    pub fn get_name(&self) -> &Option<String> {
        match self.long {
            Some(_) => &self.long,
            _ => &self.short,
        }
    }
}
