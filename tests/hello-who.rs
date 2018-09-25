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
extern crate toml;

#[macro_use]
extern crate lazy_static;

use nereon::{configure, FromValue, Nos};
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
option who {{
    short w
    long who
    env TEST_WHO
    default world
    hint NAME
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

#[test]
fn test_nos_option() {
    use nereon::parse_noc;

    let config = configure(
        &Nos::from_value(&parse_noc(&NOS).unwrap()).unwrap(),
        &vec!["program", "--unknown", "-c", "tests/hello-who.conf"],
    );
    assert!(config.is_err());

    let config = configure(
        &Nos::from_value(&parse_noc(&NOS).unwrap()).unwrap(),
        &vec!["program", "-w"],
    );
    assert!(config.is_err());

    env::set_var("TEST_WHO", "guess who?");
    let config = configure(
        &Nos::from_value(&parse_noc(&NOS).unwrap()).unwrap(),
        &vec!["program", "-w", "Arg", "--config", "tests/hello-who.conf"],
    );
    assert!(config.is_ok());
}
