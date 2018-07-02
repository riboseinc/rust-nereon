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
#[macro_use]
extern crate serde_json;

use libucl::ucl_to_json;
use regex::Regex;
use serde_json::{map::Map, Value};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io;

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

fn value_into_string(v: Value) -> String {
    match v {
        Value::Object(_) => "".to_owned(),
        Value::String(s) => s,
        _ => format!("{}", v),
    }
}

fn add_values(one: Value, other: Value) -> Value {
    match one {
        Value::String(s) => match s.len() {
            0 => other,
            _ => Value::String(s + &value_into_string(other)),
        },
        _ => match other {
            Value::String(s) => match s.len() {
                0 => one,
                _ => Value::String(value_into_string(one) + &s),
            },
            _ => Value::String(value_into_string(one) + &value_into_string(other)),
        },
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

pub fn ucl_to_value(src: &mut io::Read) -> Result<Value, String> {
    match ucl_to_json(src) {
        Err(e) => Err(format!("{}", e)),
        Ok(s) => match serde_json::from_str::<Value>(&s) {
            Err(e) => Err(format!("{}", e)),
            Ok(mut root) => {
                // collect all strings that require expansion
                let mut expansions = HashMap::new();
                get_expansions(&root, &mut expansions);

                // perform up to 2 passes so nodes expanded in the
                // first pass are available in second
                for _ in 1..2 {
                    if expansions.len() == 0 {
                        break;
                    }
                    // attempt to expand expansions
                    for (k, v) in expansions.iter_mut() {
                        if v.is_none() {
                            *v = expand_str(k, &root);
                        }
                    }
                    // substitute expansion results into tree
                    substitute_expansions(&mut root, &expansions);
                    expansions = expansions
                        .into_iter()
                        .filter(|(_, v)| v.is_none())
                        .collect();
                }

                Ok(root)
            }
        },
    }
}

fn get_expansions(node: &Value, expansions: &mut HashMap<String, Option<Value>>) -> () {
    match node {
        Value::String(s) => {
            if s.contains('$') {
                expansions.insert(s.clone(), None);
            }
        }
        Value::Object(o) => o.values().for_each(|n| get_expansions(n, expansions)),
        _ => (),
    }
}

fn substitute_expansions(node: &mut Value, expansions: &HashMap<String, Option<Value>>) -> () {
    if node.is_string() {
        if let Some(Some(v)) = expansions.get(node.as_str().unwrap()) {
            *node = v.clone();
        }
    } else if node.is_object() {
        for (_, n) in node.as_object_mut().unwrap().iter_mut() {
            substitute_expansions(n, expansions);
        }
    }
}

fn expand_str(s: &str, root: &Value) -> Option<Value> {
    match expand(&s.replace("$$", "\0"), &root) {
        Some(Value::String(s)) => Some(Value::String(s.replace('\0', "$"))),
        v => v,
    }
}

fn expand(s: &str, root: &Value) -> Option<Value> {
    match s.find("${") {
        None => Some(Value::String(s.to_owned())),
        Some(p) => {
            let prefix = Value::String(s[..p].to_owned());
            match expand(&s[(p + 2)..], root) {
                Some(Value::String(s)) => match s.find("}") {
                    None => Some(Value::String(s)),
                    Some(e) => {
                        let suffix = Value::String(s[(e + 1)..].to_owned());
                        match expand_var(&s[..e], root) {
                            Some(e) => Some(add_values(add_values(prefix, e), suffix)),
                            None => None,
                        }
                    }
                },
                v => v,
            }
        }
    }
}

fn expand_var(v: &str, root: &Value) -> Option<Value> {
    match v.find(':') {
        None => None,
        Some(p) => {
            let (k, mut v) = v.split_at(p);
            v = &v[1..];
            match k.as_ref() {
                "env" => expand(
                    &env::var(&v).unwrap_or("".to_owned()).replace("$$", "\0"),
                    &root,
                ),
                "file" => match File::open(&v) {
                    // TODO: this silently fails if the file doesn't open or is invalid
                    Ok(mut f) => match ucl_to_value(&mut f) {
                        Ok(v) => Some(v),
                        _ => None,
                    },
                    _ => None,
                },
                "node" => match v.len() {
                    0 => Some(root.clone()),
                    _ => match get_node(root, &v.split('.').collect::<Vec<_>>().as_ref()) {
                        Some(v) => Some(v.clone()),
                        None => None,
                    },
                },
                _ => None,
            }
        }
    }
}

fn get_node<'a>(node: &'a Value, keys: &[&str]) -> Option<&'a Value> {
    match keys.len() {
        0 => Some(node),
        _ => match node {
            Value::Object(o) => match o.get(keys[0]) {
                Some(v) => get_node(v, &keys[1..]),
                _ => None,
            },
            _ => None,
        },
    }
}

fn load_ucl(file: &str) -> Result<Value, String> {
    match File::open(file) {
        Err(e) => Err(format!("Couldn't open file {}: {}", file, e)),
        Ok(ref mut f) => match ucl_to_json(f) {
            Err(e) => Err(format!("Failed to parse ucl {}: {}", file, e)),
            Ok(json) => match serde_json::from_str::<Value>(&json) {
                Err(e) => Err(format!("Failed to parse json from ucl {}: {}", file, e)),
                Ok(v) => Ok(v),
            },
        },
    }
}

#[cfg(test)]
mod tests {
    extern crate serde_json;

    use serde_json::Value;
    use std::env;

    #[test]
    fn test_expand() {
        env::remove_var("TEST");
        assert_eq!(
            super::expand_var("env:TEST", &Value::Bool(false)),
            Some(Value::String("".to_owned()))
        );
        env::set_var("TEST", "test");
        assert_eq!(
            super::expand_var("env:TEST", &Value::Bool(false)),
            Some(Value::String("test".to_owned()))
        );
        assert_eq!(
            super::expand_var("file:tests/not_exist", &Value::Bool(false)),
            None
        );
        assert_eq!(
            super::expand_var("file:tests/hello-who.conf", &Value::Bool(false)),
            Some(json!({"greeting":"olá"}))
        );
        assert_eq!(
            super::expand_var("node:farewell", &json!({"greeting":"olá"})),
            None
        );
        assert_eq!(
            super::expand_var("node:", &json!({"greeting":"olá"})),
            Some(json!({"greeting":"olá"}))
        );
        assert_eq!(
            super::expand_var("node:greeting", &json!({"greeting":"olá"})),
            Some(Value::String("olá".to_owned()))
        );
        assert_eq!(
            super::expand_str("", &Value::Null),
            Some(Value::String("".to_owned()))
        );
        assert_eq!(
            super::expand_str("$$", &Value::Null),
            Some(Value::String("$".to_owned()))
        );
        assert_eq!(
            super::expand_str("${node:greeting}", &json!({"greeting":"olá"})),
            Some(Value::String("olá".to_owned()))
        );
        assert_eq!(
            super::expand_str(" ${node:greeting}", &json!({"greeting":"olá"})),
            Some(Value::String(" olá".to_owned()))
        );
        assert_eq!(
            super::expand_str("$$${node:greeting}", &json!({"greeting":"olá"})),
            Some(Value::String("$olá".to_owned()))
        );
        assert_eq!(
            super::expand_str("${node:greeting}$$", &json!({"greeting":"olá"})),
            Some(Value::String("olá$".to_owned()))
        );
        assert_eq!(
            super::expand_str("$$", &Value::Null),
            Some(Value::String("$".to_owned()))
        );
        env::set_var("TEST", "$$");
        assert_eq!(
            super::expand_str("${env:TEST}", &Value::Null),
            Some(Value::String("$".to_owned()))
        );
        env::set_var("TEST", "${node:greeting}");
        assert_eq!(
            super::expand_str("${env:TEST}", &json!({"greeting":"olá"})),
            Some(Value::String("olá".to_owned()))
        );
        let src = "\
                   greeting1 = ${env:TEST}\n\
                   greeting = olá";
        let expected = json!({"greeting":"olá", "greeting1":"olá"});
        assert_eq!(super::ucl_to_value(&mut src.as_bytes()), Ok(expected));
    }
}
