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
//! extern crate nereon;
//! use nereon::{Opt, nereon_init, Value};
//! # use std::collections::HashMap;
//! # use std::iter::FromIterator;
//!
//! let noc = r#"user "admin" {uid 1000}"#;
//! let mut expected = Value::Dict(HashMap::new());
//! expected.insert(
//!     vec!["user".to_owned(),"admin".to_owned(), "uid".to_owned()],
//!     Value::String("1000".to_owned()));
//!
//! assert_eq!(noc.parse(), Ok(expected));
//!
//! // .. or can be expanded during option processing with nereon_init
//! let options = vec![
//!     Opt::new(
//!         &[],
//!         None,
//!         None,
//!         Some("nereon_config"),
//!         0,
//!         None,
//!         "Config file",
//!     ),
//!     Opt::new(
//!         &["user", "admin", "uid"],
//!         Some("u"),
//!         None,
//!         None,
//!         0,
//!         None,
//!         "UID of admin user",
//!     ),
//!     Opt::new(
//!         &["user", "admin", "permissions"],
//!         None,
//!         None,
//!         Some("nereon_permissions"),
//!         0,
//!         None,
//!         "Permissions for admin user",
//!     ),
//! ];
//!
//! let args = "-u 100".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
//!
//! // create an example NOC config file
//! std::fs::write("/tmp/nereon_test", r#"
//!     user "admin" {
//!         permissions read
//!     }
//! "#);
//!
//! std::env::set_var("nereon_config", "/tmp/nereon_test");
//! std::env::set_var("nereon_permissions", "read,write");
//!
//! let mut expected = Value::Dict(HashMap::new());
//! expected.insert(
//!     vec!["user".to_owned(), "admin".to_owned(), "uid".to_owned()],
//!     Value::String("100".to_owned()));
//! expected.insert(
//!     vec!["user".to_owned(), "admin".to_owned(), "permissions".to_owned()],
//!     Value::String("read,write".to_owned()));
//!
//! assert_eq!(nereon_init(options, args), Ok(expected));
//! std::fs::remove_file("/tmp/nereon_test");
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
use std::ffi::OsString;

mod nos;

pub use nos::{Nos, UserOption};

mod noc;

pub use noc::{FromValue, Value};

/// Parse command-line options into a
/// [`noc::Value`](https://docs.serde.rs/serde_json/value/enum.Value.html).
///
/// # Examples
///
/// ```
/// # extern crate nereon;
/// # use nereon::{nereon_init, Opt, OptFlag, Value};
/// # use std::collections::HashMap;
/// let options = vec![
///     Opt::new(
///         &["username"],
///         Some("u"),
///         Some("user"),
///         Some("NEREON_USER"),
///         0,
///         Some("admin"),
///         "User name",
///     ),
/// ];
/// let args = "-u root".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
/// let mut expected = Value::Dict(HashMap::new());
/// expected.insert(vec!["username".to_owned()], Value::String("root".to_owned()));
/// assert_eq!(nereon_init(options, args), Ok(expected));
/// ```

pub fn nereon_init<'a, U, I>(nos: &Nos, args: U) -> Result<Value, String>
where
    U: IntoIterator<Item = I>,
    I: Into<OsString> + Clone,
{
    /*
    fn get_arg<'a>(
        opt: &Opt,
        matches: &'a Matches,
        config: Option<&Value>,
    ) -> Result<String, String> {
        opt.long.as_ref().and_then(|n| matches.opt_str(&n))
            .or_else(|| opt.short.as_ref().and_then(|n| matches.opt_str(&n)))
            .or_else(|| {
                opt.env
                    .as_ref()
                    .and_then(|n| env::var(&n).ok())
                    .or_else(|| {
                        config.and_then(|c| {
                            c.get(opt.key.clone())
                                .filter(|v| v.is_string())
                                .and_then(|v| v.as_string().map(String::from))
                        })
                    })
                    .or_else(|| opt.default.as_ref().map(|v| v.to_owned()))
            })
            .map_or_else(
                || {
                    Err(opt.long.as_ref()
                        .or(opt.short.as_ref())
                        .map_or(format!("Required option not supplied {:?}", opt), |n| {
                            format!("Option is required ({})", n)
                        }))
                },
                |v| Ok(v),
            )
    }
*/
    // get command line options
    let mut clap_app = clap::App::new(nos.name.to_owned())
        .version(nos.version.as_str())
        .about(nos.license.as_str());
    for a in nos.authors.iter() {
        clap_app = clap_app.author(a.as_str());
    }
    for (n, o) in nos.option.iter() {
        let mut arg = clap::Arg::with_name(n.as_str());
        if let Some(ref s) = o.short {
            arg = arg.short(s);
        }
        if let Some(ref l) = o.long {
            arg = arg.long(l);
        }
        if o.default_arg.is_none() {
            arg = arg.takes_value(true);
        }
        if let Some(ref e) = o.env {
            arg = arg.env(e);
        }
        if o.default.is_none() && o.env.is_none() {
            arg = arg.required(true);
        }
        clap_app = clap_app.arg(arg);
    }

    let _matches = clap_app.get_matches_from(args);
    /*
    // read the config file if there is one
    let mut config = if options
        .get("config")
        .filter(|o| !o.key.is_empty())
        .is_some()
    {
        options
            .remove("config")
            .ok_or_else(|| unreachable!())
            .and_then(|ref o| {
                get_arg(o, &matches, None).and_then(|n| {
                    let mut buffer = String::new();
                    File::open(&n)
                        .and_then(|ref mut f| f.read_to_string(&mut buffer))
                        .map_err(|e| format!("{:?}", e))
                        .and_then(|_| buffer.parse())
                })
            })
    } else {
        Ok(Value::Dict(HashMap::new()))
    }?;

    // build the config tree
    options
        .values()
        .try_fold(&mut config, |mut config, option| {
            get_arg(option, &matches, Some(&mut config)).and_then(|v| {
                config.insert(option.key.clone(), Value::String(v.to_owned()));
                Ok(config)
            })
        })?;
*/
    Ok(Value::Dict(HashMap::new()))
}

#[cfg(test)]
mod tests {}
