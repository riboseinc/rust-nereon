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

//! `nereon` is an option parser which creates a configuration tree
//! representing data parsed from a combination of command line
//! options and environment variables.
//!
//! Command line options are described using [`struct Opt`](struct.Opt.html)
//!
//! [`nereon_init`](fn.nereon_init.html) is used to parse the command line options
//! and returns a [`noc::Value`](struct.Value.html) tree.
//!
//! # Examples
//!
//! ```
//! # extern crate nereon;
//! use nereon::{configure, parse_noc, Value, Nos, FromValue};
//! use std::collections::HashMap;
//!
//! let noc = r#"user admin {uid 1000}"#;
//! let expected = Value::Dict(HashMap::new())
//!     .insert(vec!["user", "admin", "uid"], Value::from("1000"));
//! assert_eq!(parse_noc(noc), Ok(expected));
//!
//! let nos = r#"
//!     name "Nereon test"
//!     authors ["John Doe <john@doe.me>"]
//!     license Free
//!     version "0.0.1"
//!     option config {
//!         env nereon_config
//!         usage "Config file"
//!         key []
//!     }
//!     option user {
//!         short u
//!         key [user, admin, uid]
//!         usage "User's UID"
//!         hint USER
//!     }
//!     option permissions {
//!         env nereon_permissions
//!         key [user, admin, permissions]
//!         usage "Permissions for user"
//!     }"#;
//!
//! // create an example NOC config file
//! ::std::fs::write("/tmp/nereon_test", "user admin permissions read").unwrap();
//!
//! ::std::env::set_var("nereon_config", "/tmp/nereon_test");
//! ::std::env::set_var("nereon_permissions", "read,write");
//! let expected = parse_noc(r#"
//!     user admin uid 100
//!     user admin permissions "read,write""#
//! ).unwrap();
//! let nos = Nos::from_value(&parse_noc(nos).unwrap()).unwrap();
//! assert_eq!(configure(&nos, &["program", "-u", "100"]), Ok(expected));
//! ::std::fs::remove_file("/tmp/nereon_test").unwrap();
//! ```

extern crate clap;

#[macro_use]
extern crate pest_derive;
extern crate pest;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate nereon_derive;

use std::collections::HashMap;
use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::Read;

mod nos;

pub use nos::{Nos, UserOption};

mod noc;

pub use noc::{parse_noc, FromValue, Value};

/// Parse command-line options into a
/// [`noc::Value`](https://docs.serde.rs/serde_json/value/enum.Value.html).
///
/// # Examples
///
/// ```
/// # extern crate nereon;
/// use std::collections::HashMap;
/// use nereon::{Nos, Value, parse_noc, configure, FromValue};
/// let nos = r#"
///     name "Nereon test"
///     authors ["John Doe <john@doe.me>"]
///     version "0.0.1"
///     license Free
///     option username {
///         short u
///         long user
///         env NEREON_USER
///         default admin
///         hint USER
///         usage "User name"
///         key [username]
///     }"#;
/// let expected = Value::Dict(HashMap::new())
///     .insert(vec!["username"], Value::from("root"));
/// let nos = Nos::from_value(&parse_noc(nos).unwrap()).unwrap();
/// assert_eq!(configure(&nos, &["program", "-u", "root"]), Ok(expected));
/// ```

pub fn configure<'a, U, I>(nos: &Nos, args: U) -> Result<Value, String>
where
    U: IntoIterator<Item = I>,
    I: Into<OsString> + Clone,
{
    if nos.option.is_none() {
        return Ok(Value::Dict(HashMap::new()));
    }

    let options = nos.option.as_ref().unwrap();

    // get command line options
    let mut clap_app = clap::App::new(nos.name.to_owned())
        .version(nos.version.as_str())
        .about(nos.license.as_str());

    for a in nos.authors.iter() {
        clap_app = clap_app.author(a.as_str());
    }

    for (n, o) in options.iter() {
        let mut arg = clap::Arg::with_name(n.as_str()).required(true);
        if let Some(ref s) = o.short {
            arg = arg.short(s);
        }
        if let Some(ref l) = o.long {
            arg = arg.long(l);
        }
        if let Some(ref d) = o.default {
            arg = arg.default_value(d);
        }
        if o.default_arg.is_none() {
            arg = arg.takes_value(true);
        }
        if let Some(ref e) = o.env {
            arg = arg.env(e);
        }
        if let Some(ref h) = o.hint {
            arg = arg.value_name(h);
        }
        clap_app = clap_app.arg(arg);
    }

    let matches = clap_app
        .get_matches_from_safe(args)
        .map_err(|e| format!("{}", e))?;

    fn key_to_strs(option: &UserOption) -> Vec<&str> {
        option.key.iter().map(|k| k.as_str()).collect()
    }

    // read the config file if there is one
    let mut config = Value::Dict(HashMap::new());
    if let Some(n) = matches.value_of_os("config") {
        let mut buffer = String::new();
        config = File::open(&n)
            .and_then(|ref mut f| f.read_to_string(&mut buffer))
            .map_err(|e| format!("{:?}", e))
            .and_then(|_| parse_noc(&buffer))
            .and_then(|v| {
                Ok({
                    let keys = key_to_strs(&options.get("config").unwrap());
                    config.insert(keys, v)
                })
            })?
    };

    // build the config tree
    config = options.iter().fold(config, |mut config, (name, option)| {
        if name != "config" {
            let value = if matches.occurrences_of(name) == 0 {
                option
                    .env
                    .as_ref()
                    .and_then(|e| env::var_os(e))
                    .map(Value::from)
                    .or_else(|| {
                        config
                            .lookup_value(key_to_strs(&option))
                            .map_or_else(|| option.default.clone().map(Value::from), |_| None)
                    })
            } else {
                matches
                    .value_of_os(name)
                    .map(Value::from)
                    .or_else(|| option.default_arg.clone().map(Value::from))
            };
            if let Some(v) = value {
                let keys = key_to_strs(option);
                config = config.insert(keys, v);
            }
        }
        config
    });
    Ok(config)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_configure() {
        use super::{configure, parse_noc, FromValue, Nos, Value};
        use std::collections::HashMap;
        let nos = r#"
name "Nereon test"
authors ["John Doe <john@doe.me>"]
version "0.0.1"
license Free
option username {
    short u
    long user
    env NEREON_USER
    default admin
    hint USER
    usage "User name"
    key [username]
}"#;
        let expected = Value::Dict(HashMap::new()).insert(vec!["username"], Value::from("root"));
        let nos = Nos::from_value(&parse_noc(nos).unwrap()).unwrap();
        assert_eq!(configure(&nos, &["program", "-u", "root"]), Ok(expected));
    }

    #[test]
    fn test_configure1() {
        use super::{configure, parse_noc, FromValue, Nos, Value};
        use std::collections::HashMap;

        let noc = r#"user admin {uid 1000}"#;
        let expected =
            Value::Dict(HashMap::new()).insert(vec!["user", "admin", "uid"], Value::from("1000"));
        assert_eq!(parse_noc(noc), Ok(expected));

        let nos = r#"
name "Nereon test"
authors ["John Doe <john@doe.me>"]
license Free
version "0.0.1"
option config {
    env nereon_config
    usage "Config file"
    key []
}
option user {
    short u
    key [user, admin, uid]
    usage "User's UID"
    hint USER
}
option permissions {
    env nereon_permissions
    key [user, admin, permissions]
    usage "Permissions for user"
}"#;

        // create an example NOC config file
        ::std::fs::write("/tmp/nereon_test", "user admin permissions read").unwrap();

        ::std::env::set_var("nereon_config", "/tmp/nereon_test");
        ::std::env::set_var("nereon_permissions", "read,write");
        let expected = parse_noc(
            r#"
            user admin uid 100
            user admin permissions "read,write""#,
        ).unwrap();
        let nos = Nos::from_value(&parse_noc(nos).unwrap()).unwrap();
        assert_eq!(configure(&nos, &["program", "-u", "100"]), Ok(expected));
        ::std::fs::remove_file("/tmp/nereon_test").unwrap();
    }
}
