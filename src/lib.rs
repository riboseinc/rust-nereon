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
extern crate serde_json;

use libucl::ucl_to_json;
use regex::Regex;
use serde_json::{map::Map, Value};
use std::{env, fs::File};

pub mod libucl;
mod nos;

pub use nos::{Opt, OptFlag};

pub fn nereon_json<T, U>(options: T, args: U) -> Result<String, String>
where
    T: IntoIterator<Item = Opt>,
    U: IntoIterator<Item = String>,
{
    match nereon_init(options, args) {
        Ok(v) => match serde_json::to_string(&v) {
            Ok(s) => Ok(s),
            Err(m) => return Err(m.to_string()),
        },
        Err(m) => return Err(m),
    }
}

pub fn nereon_init<T, U>(options: T, args: U) -> Result<Value, String>
where
    T: IntoIterator<Item = Opt>,
    U: IntoIterator<Item = String>,
{
    // collect options and sort by node depth
    let mut options = options.into_iter().collect::<Vec<_>>();
    options.sort_by(|a, b| a.node_depth().cmp(&b.node_depth()));

    // get command line options
    let mut getopts_options = getopts::Options::new();

    for o in options.iter() {
        o.to_getopts(&mut getopts_options);
    }

    let matches = match getopts_options.parse(args) {
        Ok(ms) => ms,
        Err(e) => return Err(format!("{:?}", e)),
    };

    // build the config tree
    let mut config = Value::from(Map::new());
    for o in options.iter() {
        let mut subtree = &mut config;
        let mut node = "".to_owned();

        for key in o.get_branch_keys() {
            node = node + "." + &key;
            let old_subtree = subtree;
            subtree = match old_subtree {
                Value::Object(m) => {
                    if !m.contains_key(&key) {
                        m.insert(key.clone(), Value::from(Map::new()));
                    }
                    m.get_mut(&key).unwrap()
                }
                _ => return Err(format!("Config tree already has value at {}", node)),
            };
        }

        // get default option value
        let mut value = o.default.to_owned();

        // environment overrides default
        if let Some(ref name) = o.env {
            if let Ok(s) = env::var(name) {
                value = Some(s);
            }
        }

        // command arg overrides environment
        if let Some(name) = o.get_name() {
            if let Some(s) = matches.opt_str(name) {
                value = Some(s);
            }
        }

        // add leaf node
        if value.is_some() {
            let mut value = value.unwrap();

            // format the value if necessary
            if let Some(f) = &o.format {
                let re = Regex::new("(.*[^\\\\])\\{\\}(.*)").unwrap();
                if let Some(c) = re.captures(f) {
                    value = format!(
                        "{}{}{}",
                        c.get(1).unwrap().as_str(),
                        value,
                        c.get(2).unwrap().as_str()
                    );
                }
            }

            let value = match value.starts_with('@') {
                true => match load_ucl(&value[1..]) {
                    Ok(config) => config,
                    Err(e) => {
                        return Err(format!("Failed to load config file {}: {}", &value[1..], e))
                    }
                },
                false => Value::String(value),
            };

            if subtree.is_object() {
                match o.get_leaf_key() {
                    Some(k) => {
                        if let Value::Object(m) = subtree {
                            m.insert(k, value);
                        }
                    }
                    None => *subtree = value,
                }
            }
        }
    }

    Ok(config)
}

fn load_ucl(file: &str) -> Result<Value, String> {
    let mut file = match File::open(file) {
        Ok(f) => f,
        Err(_) => return Err(format!("Couldn't open file")),
    };
    let value: Value = match ucl_to_json(&mut file) {
        Ok(json) => match serde_json::from_str::<Value>(&json) {
            Ok(v) => v,
            Err(e) => {
                return Err(format!(
                    "Failed to parse json from libucl: {:?} [json: {}]",
                    e, json
                ))
            }
        },
        Err(e) => return Err(format!("Parse failure (libucl): {:?}", e)),
    };

    Ok(value)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
