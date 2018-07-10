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

#[derive(Clone, Debug)]
pub struct Opt {
    /// Dot (`.`) separated path of destination node for this option.
    pub node: String,
    /// Short option as single character string `"s"` matches `-s`.
    pub short: Option<String>,
    /// Long option `"long"` matches `--long`.
    pub long: Option<String>,
    /// Environment variable to use if option not parsed from command line.
    pub env: Option<String>,
    /// OptFlag values as u32 or'd together
    pub flags: u32,
    /// Value to use if not parsed from command line or environment variable.
    pub default: Option<String>,
    /// Format the option. This is a simple string format where the first `{}` will be replaced by the parsed value.
    pub format: Option<String>,
    /// Description of option used to generate the usage message.
    pub usage: Option<String>,
}

pub enum OptFlag {
    /// Option is not required.
    Optional = 1,
    /// Option can appear more than once.
    Multiple = 2,
    /// Option doesn;t take an argument.
    NoArg = 4,
    /// Option may be used with or without an argument.
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

    fn to_getopts(&self, options: &mut getopts::Options) {
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
                // getopts occurrence handling is a bit broken
                // as it disallows (opt AND multi) or (req AND multi)
                // so we use Multi and deal with it later
                getopts::Occur::Multi,
            );
        }
    }
}

pub fn opt_to_getopts(opt: &Opt, options: &mut getopts::Options) {
    opt.to_getopts(options);
}
