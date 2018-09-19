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

extern crate clap;
//extern crate noc;

use noc::{FromValue, Value};
use std::collections::HashMap;

pub struct Command {
    pub commands: HashMap<String, Command>,
    pub options: HashMap<String, UserOption>,
}

pub struct UserOption {
    pub short: Option<String>,
    pub long: Option<String>,
    pub env: Option<String>,
    pub default_arg: Option<String>,
    pub default: Option<String>,
    pub usage: String,
    pub key: String,
}

pub struct Nos {
    pub name: String,
    pub author: String,
    pub version: String,
    pub license: String,
    pub commands: HashMap<String, Command>,
    pub options: HashMap<String, UserOption>,
}

impl FromValue for Command {
    fn from_value(value: &Value) -> Result<Self, String> {
        value.get_dict(vec!["command"]).and_then(|commands| {
            commands
                .iter()
                .try_fold(HashMap::new(), |mut cs, (k, v)| {
                    Command::from_value(v).map(|c| {
                        cs.insert(k.to_owned(), c);
                        cs
                    })
                })
                .and_then(|commands| {
                    value.get_dict(vec!["option"]).and_then(|options| {
                        options.iter()
                            .try_fold(HashMap::new(), |mut os, (k, v)| {
                                UserOption::from_value(v).map(|o| {
                                    os.insert(k.to_owned(), o);
                                    os
                                })
                            })
                            .map(|options| Command { commands, options })
                    })
                })
        })
    }
}

impl FromValue for UserOption {
    fn from_value(value: &Value) -> Result<Self, String> {
        String::from_kv_optional(vec!["short"], value).and_then(|short| {
            String::from_kv_optional(vec!["long"], value).and_then(|long| {
                String::from_kv_optional(vec!["env"], value).and_then(|env| {
                    String::from_kv_optional(vec!["default_arg"], value).and_then(|default_arg| {
                        String::from_kv_optional(vec!["default"], value).and_then(|default| {
                            String::from_kv(vec!["usage"], value).and_then(|usage| {
                                String::from_kv(vec!["key"], value).map(|key| UserOption {
                                    short,
                                    long,
                                    env,
                                    default_arg,
                                    default,
                                    usage,
                                    key,
                                })
                            })
                        })
                    })
                })
            })
        })
    }
}

impl FromValue for Nos {
    fn from_value(value: &Value) -> Result<Self, String> {
        value.get_string(vec!["name"]).and_then(|name| {
            value.get_string(vec!["author"]).and_then(|author| {
                value.get_string(vec!["version"]).and_then(|version| {
                    value.get_string(vec!["license"]).and_then(|license| {
                        value.get_dict(vec!["command"]).and_then(|commands| {
                            commands.iter()
                                .try_fold(HashMap::new(), |mut cs, (k, v)| {
                                    Command::from_value(v).map(|c| {
                                        cs.insert(k.to_owned(), c);
                                        cs
                                    })
                                })
                                .and_then(|commands| {
                                    value.get_dict(vec!["option"]).and_then(|options| {
                                        options.iter()
                                            .try_fold(HashMap::new(), |mut os, (k, v)| {
                                                UserOption::from_value(v).map(|o| {
                                                    os.insert(k.to_owned(), o);
                                                    os
                                                })
                                            })
                                            .map(|options| Nos {
                                                name,
                                                author,
                                                version,
                                                license,
                                                commands,
                                                options,
                                            })
                                    })
                                })
                        })
                    })
                })

            })
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct Opt {
    /// Dot separated path of destination node for this option. eg. `"root.leaf"`.
    pub key: Vec<String>,
    /// Short option as single character string `"s"` matches `-s`.
    pub short: Option<String>,
    /// Long option `"long"` matches `--long`.
    pub long: Option<String>,
    /// Environment variable to use if option not parsed from command line.
    pub env: Option<String>,
    /// Value to use if option is present without an arg
    pub default_arg: Option<String>,
    /// Value to use if not parsed from command line or environment variable.
    pub default: Option<String>,
    /// Description of option used to generate the usage message.
    pub usage: String,
}

impl Opt {
    /// Creates a new `Opt` instance for use with [nereon_init](fn.nereon_init.html)
    ///
    /// # Arguments
    /// * `key` - Dot separated path of destination node for this option. eg. `"root.leaf"`.
    /// * `short` - Short option as single character string `"s"` matches `-s`.
    /// * `long` - Long option `"long"` matches `--long`.
    /// * `env` - Environment variable to use if option not parsed from command line.
    /// * `default_arg` Value to use if option is present but has no arg.
    /// * `default` - Value to use if not parsed from command line or environment variable.
    /// * `usage` - Description of option used to generate the usage message.
    pub fn new(
        key: &[&str],
        short: Option<&str>,
        long: Option<&str>,
        env: Option<&str>,
        default_arg: Option<&str>,
        default: Option<&str>,
        usage: &str,
    ) -> Opt {
        Opt {
            key: { key.iter().map(|k| (*k).to_owned()).collect() },
            short: short.map(String::from),
            long: long.map(String::from),
            env: env.map(String::from),
            default_arg: default_arg.map(String::from),
            default: default.map(String::from),
            usage: usage.to_owned(),
        }
    }

    /*
    pub fn to_getopts<'a>(&self, mut options: getopts::Options) -> getopts::Options {
        if self.short.is_some() || self.long.is_some() {
            let hasarg = if let Some(_) = self.default_arg {
                getopts::HasArg::Maybe
            } else {
                getopts::HasArg::Yes
            };

            options.opt(
                self.short.as_ref().map_or("", String::as_str),
                self.long.as_ref().map_or("", String::as_str),
                self.usage.as_str(),
                "",
                hasarg,
                getopts::Occur::Optional,
            );
        }
        options
    }*/
}
