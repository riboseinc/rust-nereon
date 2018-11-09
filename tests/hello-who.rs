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

extern crate nereon;
#[macro_use]
extern crate nereon_derive;

extern crate toml;

#[macro_use]
extern crate lazy_static;

use nereon::{configure, ConversionError, FromValue, Value};
use std::env;

static CARGO: &str = include_str!("../Cargo.toml");

lazy_static! {
    static ref NOS: String = {
        let cargo = CARGO.parse::<toml::Value>().unwrap();
        let package = cargo.get("package").unwrap();
        format!(
            r#"
name "test-who"
authors {}
version {}
license {}
option config {{
    short c
    long config
    hint FILE
    usage "Config file"
    key []
}}
option greeting {{
    short g
    long greeting
    default Hello
    env GREETING
    flags [takesvalue]
    usage "How to greet"
    key [greeting]
}}
option who {{
    short w
    long who
    hint NAME
    flags [takesvalue, required]
    usage "Entity to greet"
    key [who]
}}
"#,
            package.get("authors").unwrap(),
            package.get("version").unwrap(),
            package.get("license").unwrap(),
        )
    };
}

#[derive(FromValue, Debug, PartialEq)]
struct Config {
    greeting: String,
    who: String,
}

//#[test]
//fn test_unknown_arg() {
//    let config = configure::<Config, _, _, _>(NOS.as_ref(), &vec!["program", "-u"]);
//    assert!(config.is_err());
//}

//#[test]
//fn test_no_possible_value() {
//    let config = configure::<Config, _, _, _>(NOS.as_ref(), &vec!["program"]);
//    assert!(config.is_err());
//
//    let config = configure::<Config, _, _, _>(NOS.as_ref(), &vec!["program", "-w"]);
//    assert!(config.is_err());
//}

#[test]
fn test_overrides() {
    // these tests are merged as env vars are available across threads

    // test default greeting
    env::remove_var("GREETING");
    let config = configure::<Config, _, _, _>(NOS.as_ref(), &vec!["program", "-w", "World"]);
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Hello".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure config overrides default
    env::remove_var("GREETING");
    let config = configure::<Config, _, _, _>(
        NOS.as_ref(),
        &vec!["program", "-c", "tests/hello-who.conf", "-w", "World"],
    );
    assert_eq!(
        config,
        Ok(Config {
            greeting: "olá".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure explicit overrides default
    env::remove_var("GREETING");
    let config = configure::<Config, _, _, _>(
        NOS.as_ref(),
        &vec![
            "program",
            "-c",
            "tests/hello-who.conf",
            "-w",
            "World",
            "-g",
            "Welcome",
        ],
    );
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Welcome".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure env overrides default
    env::set_var("GREETING", "Chow");
    let config = configure::<Config, _, _, _>(NOS.as_ref(), &vec!["program", "-w", "World"]);
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Chow".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure env overrides config
    env::set_var("GREETING", "Chow");
    let config = configure::<Config, _, _, _>(
        NOS.as_ref(),
        &vec!["program", "-c", "tests/hello-who.conf", "-w", "World"],
    );
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Chow".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure explicit overrides config
    env::remove_var("GREETING");
    let config = configure::<Config, _, _, _>(
        NOS.as_ref(),
        &vec![
            "program",
            "-c",
            "tests/hello-who.conf",
            "-w",
            "World",
            "-g",
            "Welcome",
        ],
    );
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Welcome".to_owned(),
            who: "World".to_owned()
        })
    );

    // ensure explicit overrides env
    env::set_var("GREETING", "Chow");
    let config = configure::<Config, _, _, _>(
        NOS.as_ref(),
        &vec![
            "program",
            "-c",
            "tests/hello-who.conf",
            "-w",
            "World",
            "-g",
            "Welcome",
        ],
    );
    assert_eq!(
        config,
        Ok(Config {
            greeting: "Welcome".to_owned(),
            who: "World".to_owned()
        })
    );
}
