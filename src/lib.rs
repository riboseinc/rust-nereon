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
//!         Some("Config file"),
//!     ),
//!     Opt::new(
//!         &["user", "admin", "uid"],
//!         Some("u"),
//!         None,
//!         None,
//!         0,
//!         None,
//!         Some("UID of admin user"),
//!     ),
//!     Opt::new(
//!         &["user", "admin", "permissions"],
//!         None,
//!         None,
//!         Some("nereon_permissions"),
//!         0,
//!         None,
//!         Some("Permissions for admin user"),
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

extern crate getopts;

#[macro_use]
extern crate pest_derive;
extern crate pest;

#[macro_use]
extern crate lazy_static;

use std::collections::{HashMap};
use std::env;
use std::fs::File;
use std::io::Read;

mod nos;

use nos::opt_to_getopts;
pub use nos::{Opt, OptFlag};

pub mod noc;

pub use noc::Value;

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
///         Some("User name"),
///     ),
/// ];
/// let args = "-u root".split(" ").map(|a| a.to_owned()).collect::<Vec<_>>();
/// let mut expected = Value::Dict(HashMap::new());
/// expected.insert(vec!["username".to_owned()], Value::String("root".to_owned()));
/// assert_eq!(nereon_init(options, args), Ok(expected));
/// ```

pub fn nereon_init<T, U>(options: T, args: U) -> Result<Value, String>
where
    T: IntoIterator<Item = Opt>,
    U: IntoIterator<Item = String>,
{
    // collect options
    let mut options = options.into_iter().collect::<Vec<_>>();

    // get command line options
    let mut getopts_options = getopts::Options::new();

    for o in &options {
        opt_to_getopts(&o, &mut getopts_options);
    }

    let matches = match getopts_options.parse(args) {
        Ok(ms) => ms,
        Err(e) => return Err(format!("{:?}", e)),
    };

    // rearrange options to put file option first
    // file option, if any, has 0 length key
    if let Some(idx) = options.iter().position(|o| o.key.is_empty()) {
        let t = options.swap_remove(idx);
        options.push(t);
        let t = options.swap_remove(0);
        options.push(t);
    }

    // build the config tree
    let mut config = Value::Dict(HashMap::new());
    for o in options {
        let long_opt = o.long.as_ref();
        let short_opt = o.short.as_ref();
        let env = o.env.as_ref();

        // get values for each option, first looking in command line arguments
        // then the environment and finally use default
        let mut values = long_opt
            .map(|n| matches.opt_strs(&n))
            .or_else(|| short_opt.map(|n| matches.opt_strs(&n)))
            .filter(|vs| !vs.is_empty())
            .or_else(|| {
                env.and_then(|n| env::var(&n).ok())
                    .or_else(|| o.default.as_ref().map(|v| v.to_owned()))
                    .map(|v| vec![v])
            });

        let mut values = match values {
            Some(vs) => vs,
            None => {
                return Err(long_opt
                    .or(short_opt)
                           .map_or(format!("Required option not supplied {:?}", o), |n| {
                        format!("Option is required ({})", n)
                    }))
            }
        };

        // read config file if this option is file_opt
        // file option, if any, has 0 length key
        if o.key.is_empty() {
            config = {
                let mut buffer = String::new();
                let c = File::open(&values[0])
                    .and_then(|ref mut f| f.read_to_string(&mut buffer))
                    .map_err(|e| format!("{:?}", e))
                    .and_then(|_| buffer.parse());
                if c.is_err() {
                    return c;
                } else {
                    c.unwrap()
                }
            };
            continue;
        }

        // convert to `noc::Value::String`s
        let mut values = values.drain(..).map(Value::String).collect::<Vec<_>>();

        // convert to `String` or `List` depending on flags
        let mut value = match o.flags & OptFlag::Multiple as u32 {
            0 => values.swap_remove(0),
            _ => Value::List(values),
        };

        config.insert(o.key, value);
    }
    Ok(config)
}

#[cfg(test)]
mod tests {}
