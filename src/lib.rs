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

//! Use `nereon` for application configuration and option parsing.
//!
//! Configuration written in
//! [NOC](https://github.com/riboseinc/nereon-syntax) syntax can be
//! parsed into a [`Value`](enum.Value.html) using the
//! [`parse_noc`](fn.parse_noc.html) function.
//!
//! ```
//! extern crate nereon;
//! use nereon::{parse_noc, Table, Value};
//!
//! let noc = r#"
//!     user admin {
//!         uid 1000
//!         name "John Doe"
//!     }"#;
//!
//! let expected = Value::Table(Table::new())
//!     .insert(vec!["user", "admin", "uid"], Value::from("1000"))
//!     .insert(vec!["user", "admin", "name"], Value::from("John Doe"));
//!
//! assert_eq!(parse_noc::<Value>(noc), Ok(expected));
//! ```
//!
//! A Nereon [`Value`](enum.Value.html) can be converted back into a NOC string:
//!
//! ```
//! extern crate nereon;
//! use nereon::{parse_noc, Value};
//!
//! let noc = r#"
//!     user admin {
//!         name "John Doe"
//!         uid 1000 + 10
//!     }"#;
//!
//! let expected = r#""user" {"admin" {"name" "John Doe","uid" "1010"}}"#
//!     .to_owned();
//!
//! assert_eq!(parse_noc::<Value>(noc).map(|v| v.as_noc_string()), Ok(expected));
//! ```
//!
//! By using the [`nereon-derive`](../nereon_derive/index.html) crate, a
//! Nereon [`Value`](enum.Value.html) can be converted into another type
//! using the [`FromValue`](trait.FromValue.html) trait.
//!
//! ```
//! #[macro_use]
//! extern crate nereon_derive;
//! extern crate nereon;
//! use nereon::{parse_noc, NocError, ConversionError, FromValue, Value};
//!
//! # fn main() {
//! #[derive(FromValue, PartialEq, Debug)]
//! struct User {
//!     uid: u32,
//!     name: String,
//! }
//!
//! let noc = r#"
//!     uid 1000 + 10
//!     name "John Doe"
//! "#;
//!
//! let expected = User { uid: 1010, name: "John Doe".to_owned() };
//! let user = parse_noc::<User>(noc);
//! assert_eq!(user, Ok(expected));
//! # }
//! ```
//!
//! Nereon can also be used to parse command lines. Command line
//! options are described with a NOS configuration in NOC syntax.
//! Arguments from the command line are inserted into the resulting
//! [`Value`](enum.Value.html). NOS accepts environment variables as
//! option defaults and can optionally load further defaults from a
//! configuration file.
//!
//! ```
//! # extern crate nereon;
//! use nereon::{configure, parse_noc, Value, FromValue};
//! use std::collections::HashMap;
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
//! // create an example NOC config file and environment variables
//! ::std::fs::write("/tmp/nereon_test", "user admin permissions read").unwrap();
//! ::std::env::set_var("nereon_config", "/tmp/nereon_test");
//! ::std::env::set_var("nereon_permissions", "read,write");
//!
//! let expected = parse_noc::<Value>(r#"
//!     user admin uid 100
//!     user admin permissions "read,write""#
//! ).unwrap();
//!
//! assert_eq!(configure(nos, &["program", "-u", "100"]), Ok(expected));
//!
//! # ::std::fs::remove_file("/tmp/nereon_test").unwrap();
//! ```

extern crate clap;

#[macro_use]
extern crate pest_derive;
extern crate pest;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate nereon_derive;

use clap::{App, ArgMatches};
use std::env;
use std::ffi::OsString;
use std::fs::File;
use std::io::Read;

mod nos;
use nos::{Command, Nos, UserOption};

mod noc;
pub use noc::{parse_noc, ConversionError, FromValue, NocError, ParseError, Table, Value};

/// Parse command-line options into a [`Value`](enum.Value.html).
///
/// # Examples
///
/// ```
/// # extern crate nereon;
/// use nereon::{Table, Value, parse_noc, configure, FromValue};
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
/// let expected = Value::Table(Table::new())
///     .insert(vec!["username"], Value::from("root"));
/// assert_eq!(configure(nos, &["program", "-u", "root"]), Ok(expected));
/// ```
/// # Panics
///
/// `configure` panics if `nos` string is not valid NOS. This is
/// considered a programming error: the NOS is asserted to be valid
/// and considered a part of the program source.
pub fn configure<T, N, U, I>(nos: N, args: U) -> Result<T, String>
where
    T: FromValue,
    N: Into<Nos>,
    U: IntoIterator<Item = I>,
    I: Into<OsString> + Clone,
{
    let nos = nos.into();

    // get command line options
    let mut clap_app = clap::App::new(nos.name.as_str())
        .version(nos.version.as_str())
        .about(nos.license.as_str());

    for a in nos.authors.iter() {
        clap_app = clap_app.author(a.as_str());
    }

    clap_app = clap_app_init(clap_app, &nos.option, &nos.command);

    let matches = clap_app.get_matches_from(args);

    // read the config file if there is one
    let mut config = Value::Table(Table::new());
    if let Some(n) = matches.value_of_os("config") {
        let mut buffer = String::new();
        config = File::open(&n)
            .and_then(|ref mut f| f.read_to_string(&mut buffer))
            .map_err(|e| format!("{}", e))
            .and_then(|_| parse_noc::<Value>(&buffer).map_err(|e| format!("{:?}", e)))
            .and_then(|v| {
                Ok({
                    let keys =
                        key_to_strs(&nos.option.iter().find(|(k, _)| k == "config").unwrap().1);
                    config.insert(keys, v)
                })
            })?
    };

    // build the config tree
    config = update_config(config, &nos.option, &nos.command, &matches);

    T::from_value(config).map_err(|e| format!("{}", e))
}

fn clap_app_init<'a, 'b>(
    mut app: App<'a, 'b>,
    options: &'a [(String, UserOption)],
    subcommands: &'a [(String, Command)],
) -> App<'a, 'b> {
    for (n, o) in options.iter() {
        let mut arg = clap::Arg::with_name(n.as_str());
        for f in o.flags.iter() {
            match f.as_ref() {
                "required" => arg = arg.required(true),
                "multiple" => arg = arg.multiple(true),
                "takesvalue" => arg = arg.takes_value(true),
                _ => panic!("No such argument flag ({})", f),
            }
        }
        if let Some(ref s) = o.short {
            arg = arg.short(s);
        }
        if let Some(ref l) = o.long {
            arg = arg.long(l);
        }
        if let Some(ref d) = o.default {
            arg = arg.default_value(d);
        }
        if let Some(ref e) = o.env {
            arg = arg.env(e);
        }
        if let Some(ref h) = o.hint {
            arg = arg.value_name(h);
        }
        app = app.arg(arg);
    }
    for (n, c) in subcommands.iter() {
        app = app.subcommand(clap_app_init(
            clap::SubCommand::with_name(n),
            &c.option,
            &c.command,
        ));
    }
    app
}

fn update_config(
    mut config: Value,
    options: &[(String, UserOption)],
    subcommands: &[(String, Command)],
    matches: &ArgMatches,
) -> Value {
    config = options.iter().fold(config, |mut config, (name, option)| {
        if name != "config" {
            let value = if matches.occurrences_of(name) == 0 {
                option
                    .env
                    .as_ref()
                    .and_then(env::var_os)
                    .map(Value::from)
                    .or_else(|| {
                        config
                            .lookup(key_to_strs(&option))
                            .map_or_else(|| option.default.clone().map(Value::from), |_| None)
                    })
            } else if option.flags.iter().any(|ref f| f.as_str() == "multiple") {
                matches
                    .values_of_os(name)
                    .map(|vs| Value::from(vs.collect::<Vec<_>>()))
                    .or_else(|| option.default_arg.clone().map(Value::from))
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
    if let Some(name) = matches.subcommand_name() {
        if let Some(matches) = matches.subcommand_matches(name) {
            let subcommand = &subcommands.iter().find(|(k, _)| k == name).unwrap().1;
            config = update_config(config, &subcommand.option, &subcommand.command, matches);
        }
    }
    config
}

fn key_to_strs(option: &UserOption) -> Vec<&str> {
    option.key.iter().map(|k| k.as_str()).collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_configure() {
        use super::{configure, Table, Value};
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
        let expected = Value::Table(Table::new()).insert(vec!["username"], Value::from("root"));
        assert_eq!(configure(nos, &["program", "-u", "root"]), Ok(expected));
    }

    #[test]
    fn test_configure1() {
        use super::{configure, parse_noc, Table, Value};

        let noc = r#"user admin {uid 1000}"#;
        let expected =
            Value::Table(Table::new()).insert(vec!["user", "admin", "uid"], Value::from("1000"));
        assert_eq!(parse_noc::<Value>(noc), Ok(expected));

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
        let expected = parse_noc::<Value>(
            r#"
            user admin uid 100
            user admin permissions "read,write""#,
        ).unwrap();
        assert_eq!(configure(nos, &["program", "-u", "100"]), Ok(expected));
        ::std::fs::remove_file("/tmp/nereon_test").unwrap();
    }

    #[test]
    fn test_configure2() {
        use super::{configure, Table, Value};
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
}
command sub {
    option one {
        short o
        long one
        flags [takesvalue],
        usage "Supply one"
        key [sub, one]
    }
}"#;
        let actual = configure(nos, &["program", "-u", "root", "sub", "-o", "1"]);
        let expected = Ok(Value::Table(Table::new())
            .insert(vec!["username"], Value::from("root"))
            .insert(vec!["sub", "one"], Value::from("1")));
        assert_eq!(actual, expected);
    }

}
