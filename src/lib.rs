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

//! `nereon` is an option parser which creates a JSON representation of data
//! parsed from a combination of command line options and environment variables.
//!
//! Cammand line options are described using [`struct Opt`](struct.Opt.html)
//!
//! [`nereon_init`](fn.nereon_init.html) is used to parse the command line options
//! and returns a [`serde_json::value::Value`](https://docs.serde.rs/serde_json/) tree.
//!
//! [`nereon_json`](fn.nereon_json.html) does the same as [`nereon_init`](fn.nereon_init.html)
//! except it returns a JSON string.
//!
//! Variable interpolation is performed so an option passed on the command line
//! can, for example, be expanded to the contents of a file, which itself will be parsed
//! and inserted into the JSON structure. Files included in this way can be JSON or
//! [UCL](https://www.rspamd.com/doc/configuration/ucl.html).
//!
//! Options are processed in order and variable interpolation is performed as options are
//! processed. Later options can therefore use values created by earlier option processing
//! (as can be seen in the following example).
//!
//! For more details on variable interpolation see [`expand_vars`](fn.expand_vars.html).
//!
//! *Note:* `nereon` depends on [libucl](https://github.com/vstakhov/libucl). Libucl is a C
//! that must be installed and accessible to the dynamic linker (via `LD_LIBRARY_PATH`,
//! `DYLD_LIBRARY_PATH` or similar).
//!
//! # Examples
//!
//! ```
//! # #[macro_use] extern crate serde_json;
//! extern crate nereon;
//! use nereon::{Opt, nereon_init, ucl_to_value};
//!
//! // create an example config file with UCL syntax
//! std::fs::write("/tmp/nereon_test", r#"
//!     user "admin" {
//!         permissions = "${env:nereon_permissions}"
//!     }
//! "#);
//!
//! // UCL can be loaded and converted into JSON using ucl_to_value
//! assert_eq!(
//!     ucl_to_value(&mut std::fs::File::open("/tmp/nereon_test").unwrap()),
//!     Ok( json!(
//!         {
//!             "user" : {
//!                 "admin" : {
//!                     // note: ${env:nereon_permissions} expands to ""
//!                     // as the environment variable isn't set
//!                     "permissions" : ""
//!                 }
//!             }
//!         }
//!     ))
//! );
//!
//! // .. or can be expanded during option processing with nereon_init
//! let options = vec![
//!     Opt::new(
//!         "",
//!         None,
//!         None,
//!         Some("nereon_config"),
//!         0,
//!         None,
//!         Some("${file:{}}"),
//!         Some("Config file"),
//!     ),
//!     Opt::new(
//!         "user.admin.uid",
//!         Some("u"),
//!         None,
//!         None,
//!         0,
//!         None,
//!         None,
//!         Some("UID of admin user"),
//!     ),
//! ];
//!
//! let args = "-u 100".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
//!
//! std::env::set_var("nereon_config", "/tmp/nereon_test");
//! std::env::set_var("nereon_permissions", "read,write");
//!
//! assert_eq!(nereon_init(options, args), Ok(json!(
//!     {
//!         "user" : {
//!             "admin" : {
//!                 "uid" : "100",
//!                 "permissions" : "read,write"
//!             }
//!         }
//!     }))
//! );
//! std::fs::remove_file("/tmp/nereon_test");
//! ```

extern crate getopts;
extern crate regex;
#[cfg_attr(test, macro_use)]
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

use nos::opt_to_getopts;
pub use nos::{Opt, OptFlag};

pub mod noc;
/// Parse command-line options into JSON formatted configuration.
///
/// # Examples
///
/// ```
/// # extern crate nereon;
/// # use nereon::{Opt, OptFlag};
/// # use nereon::nereon_json;
/// let mut opt1: Opt = Default::default();
/// opt1.node = String::from("username");
/// opt1.short = Some(String::from("u"));
/// opt1.default = Some(String::from("admin"));
/// opt1.usage = Some(String::from("User name"));
/// let args = "-u root".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
/// assert_eq!(nereon_json(vec![opt1], args), Ok("{\"username\":\"root\"}".to_owned()));
/// ```

pub fn nereon_json<T, U>(options: T, args: U) -> Result<String, String>
where
    T: IntoIterator<Item = Opt>,
    U: IntoIterator<Item = String>,
{
    match nereon_init(options, args) {
        Ok(v) => match serde_json::to_string(&v) {
            Ok(s) => Ok(s),
            Err(m) => Err(m.to_string()),
        },
        Err(m) => Err(m),
    }
}

/// Parse command-line options into a serde_json
/// [`Value`](https://docs.serde.rs/serde_json/value/enum.Value.html).
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate serde_json;
/// # extern crate nereon;
/// # use nereon::{Opt, OptFlag};
/// # use nereon::nereon_init;
/// let options = vec![
///     Opt::new(
///         "username",
///         Some("u"),
///         Some("user"),
///         Some("NEREON_USER"),
///         0,
///         Some("admin"),
///         None,
///         Some("User name"),
///     ),
/// ];
/// let args = "-u root".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
/// assert_eq!(nereon_init(options, args), Ok(json!({"username" : "root"})));
/// ```

pub fn nereon_init<T, U>(options: T, args: U) -> Result<Value, String>
where
    T: IntoIterator<Item = Opt>,
    U: IntoIterator<Item = String>,
{
    // collect options
    let options = options.into_iter().collect::<Vec<_>>();

    // get command line options
    let mut getopts_options = getopts::Options::new();

    for o in &options {
        opt_to_getopts(&o, &mut getopts_options);
    }

    let matches = match getopts_options.parse(args) {
        Ok(ms) => ms,
        Err(e) => return Err(format!("{:?}", e)),
    };

    // build the config tree
    let mut config = Value::from(Map::new());
    for o in &options {
        let name = match o.long {
            Some(_) => &o.long,
            None => &o.short,
        };

        // explicit option is highest priority
        let mut values = match name {
            Some(n) => matches.opt_strs(&n),
            None => Vec::new(),
        };

        // use environment if no value from options
        if values.is_empty() {
            if let Some(ref name) = o.env {
                if let Ok(s) = env::var(name) {
                    values.push(s.to_owned());
                }
            }
        }

        // and finally use default if still no value
        if values.is_empty() {
            if let Some(ref s) = o.default {
                values.push(s.to_owned());
            }
        }

        if values.is_empty() {
            if o.flags & OptFlag::Optional as u32 != 0 {
                continue;
            } else if let Some(ref n) = name {
                return Err(format!("Option is required ({})", n));
            } else {
                return Err("Required option not supplied".to_owned());
            }
        }

        // format the values if necessary
        if let Some(f) = &o.format {
            let re = Regex::new("(.*[^\\\\])\\{\\}(.*)").unwrap();
            if let Some(c) = re.captures(f) {
                let prefix = c.get(1).unwrap().as_str();
                let suffix = c.get(2).unwrap().as_str();
                values = values
                    .iter()
                    .map(|v| format!("{}{}{}", prefix, v, suffix))
                    .collect();
            }
        }

        // convert to serde_json `String`s
        let mut values = values.drain(..).map(Value::String).collect::<Vec<_>>();

        // convert to `String` or `Array` depending on flags
        let mut values = match o.flags & OptFlag::Multiple as u32 {
            0 => values.remove(0),
            _ => Value::Array(values),
        };

        // expand with variable interpolation
        if let Err(s) = expand_vars(&mut values) {
            return Err(s);
        }

        // insert into tree
        if let Err(s) = insert_value(&mut config, &o.node, values) {
            return Err(s);
        }
    }
    Ok(config)
}

/// Converts UCL formatted data into serde_json
/// [`Value`](https://docs.serde.rs/serde_json/value/enum.Value.html).
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate serde_json;
/// # extern crate nereon;
/// # use nereon::ucl_to_value;
/// let mut ucl = r#"
///     user "root" {
///         uid 0
///     }
/// "#;
///
/// let value = json!({"user" : {"root" : {"uid" : 0 }}});
///
/// assert_eq!(ucl_to_value(&mut ucl.as_bytes()).unwrap(), value);
/// ```
pub fn ucl_to_value(src: &mut io::Read) -> Result<Value, String> {
    match ucl_to_json(src) {
        Err(e) => Err(format!("{}", e)),
        Ok(s) => match serde_json::from_str::<Value>(&s) {
            Err(e) => Err(format!("{}", e)),
            Ok(mut root) => match expand_vars(&mut root) {
                Ok(_) => Ok(root),
                Err(e) => Err(e),
            },
        },
    }
}

/// Perform variable interpolation within serde_json
/// [`Value`](https://docs.serde.rs/serde_json/value/enum.Value.html).
///
/// Recursively searches all strings within `root` for variables that
/// can be expanded and expands these varialbles in place.
///
/// There are three types of expansion:
/// * `${node:some.node}` - substitutes `some.node` from root. `some.node` is the absolute
/// path of the node within `root`.
/// * `${file:some/file} - substitutes the expanded contents of `some/file`
/// * `${ENV:env_var}` - substitutes the environment variable `env_var`
///
/// `$$` expands to `$`
///
/// *Note*: `node` and `file` type substitutions can cause `expand_vars` to return an `Err`
/// if the corresponding node or file isn't found. `env` substitutions will always succeed
/// and use the empty string `""` in place of the environment variable if it isn't set.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate serde_json;
/// # extern crate nereon;
/// # use nereon::expand_vars;
/// use std::env;
///
/// // substitute entire string with another
/// let mut value = json!({"user" : "${env:nereon_user}"});
/// env::set_var("nereon_user", "root");
/// expand_vars(&mut value);
/// assert_eq!(value, json!({"user" : "root"}));
///
/// // substitute a string for part of a string
/// let mut value = json!({"user" : "User is ${env:nereon_user}"});
/// expand_vars(&mut value);
/// assert_eq!(value, json!({"user" : "User is root"}));
///
/// // substitute a node for an entire string
/// let mut value = json!({
///     "allowed" : ["root", "admin"],
///     "users" : "${node:allowed}"
/// });
/// expand_vars(&mut value);
/// assert_eq!(value, json!({
///     "allowed" : ["root", "admin"],
///     "users" : ["root", "admin"]
/// }));
///
/// // this one fails ...
/// let mut value = json!({
///     "user" : "${node:missing}",
///     "file" : "${file:no-such-file}",
/// });
/// assert!(expand_vars(&mut value).is_err());
///
/// // ... but this succeeds
/// let mut value = json!({"env" : "${env:no_such_env_var}"});
/// expand_vars(&mut value);
/// assert_eq!(value, json!({"env" : ""}));
///
/// // $$ expansion
/// let mut value = json!({"node" : "$${node:example}"});
/// expand_vars(&mut value);
/// assert_eq!(value, json!({"node" : "${node:example}"}));
/// ```
pub fn expand_vars(root: &mut Value) -> Result<(), String> {
    // collect all strings that require expansion
    let mut expansions = HashMap::new();
    get_expansions(&root, &mut expansions);

    // perform up to 2 passes so nodes expanded in the
    // first pass are available in second
    for _ in 1..2 {
        if expansions.is_empty() {
            break;
        }
        // attempt to expand expansions
        for (k, v) in &mut expansions {
            if v.is_none() {
                *v = expand_str(k, &root);
            }
        }
        // substitute expansion results into tree
        substitute_expansions(root, &expansions);
        expansions = expansions
            .into_iter()
            .filter(|(_, v)| v.is_none())
            .collect();
    }

    if expansions.is_empty() {
        Ok(())
    } else {
        Err(format!("Expansions failed for {:?}", expansions.keys()))
    }
}

fn value_into_string(v: Value) -> String {
    match v {
        Value::Object(_) => "".to_owned(),
        Value::String(s) => s,
        _ => format!("{}", v),
    }
}

fn concatenate_values(one: Value, other: Value) -> Value {
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

fn insert_value(config: &mut Value, path: &str, value: Value) -> Result<(), String> {
    let mut subtree = config;

    let mut branch = if !path.is_empty() {
        path.split('.').map(String::from).collect()
    } else {
        Vec::new()
    };
    let leaf = branch.pop();
    let mut path = String::new();

    // find / create branch
    for key in branch {
        if subtree.is_object() {
            subtree = { subtree }
                .as_object_mut()
                .unwrap()
                .entry(key.clone())
                .or_insert(Value::Object(Map::new()));
        }
        if !subtree.is_object() {
            return Err(format!("Config tree already has value at {}", path));
        }
        path = path + "." + &key;
    }

    // add leaf
    if let Some(k) = leaf {
        subtree.as_object_mut().unwrap().insert(k, value);
    } else {
        *subtree = value;
    }

    Ok(())
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
                Some(Value::String(s)) => match s.find('}') {
                    None => Some(Value::String(s)),
                    Some(e) => {
                        let suffix = Value::String(s[(e + 1)..].to_owned());
                        match expand_var(&s[..e], root) {
                            Some(e) => {
                                Some(concatenate_values(concatenate_values(prefix, e), suffix))
                            }
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
            match k {
                "env" => expand(
                    &env::var(&v)
                        .unwrap_or_else(|_| "".to_owned())
                        .replace("$$", "\0"),
                    &root,
                ),
                "file" => match File::open(&v) {
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

#[cfg(test)]
mod tests {
    //    extern crate serde_json;

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
        let src = "greeting1 = ${env:TEST}\n\
                   greeting = olá";
        let expected = json!({"greeting":"olá", "greeting1":"olá"});
        assert_eq!(super::ucl_to_value(&mut src.as_bytes()), Ok(expected));

        // this one fails
        let mut value = json!({
            "user" : "${node:missing}",
            "file" : "${file:no-such-file}",
        });
        let result = super::expand_vars(&mut value);
        println!("{:?}", result);
        assert!(result.is_err());
    }
}
