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

use nereon::nereon_init;
use nereon::Opt;
use std::env;
use std::collections::HashMap;

#[test]
fn test_nos_option() {
    let mut options = HashMap::new();
    options.insert(
        "config".to_owned(),
        Opt::new(
            &[],
            Some("c"),
            Some("config"),
            None,
            None,
            None,
            "Config file",
        ),
    );
    options.insert(
        "who".to_owned(),
        Opt::new(
            &["who"],
            Some("w"),
            Some("who"),
            Some("TEST_WHO"),
            None,
            Some("World"),
            "Entity to greet.",
        ),
    );
    let args = |v: Vec<&str>| v.iter().map(|s| s.to_string()).collect::<Vec<_>>();

    let config = nereon_init(
        options.clone(),
        vec!["-p"].iter().map(|s| s.to_string()).collect::<Vec<_>>(),
    );
    assert!(config.is_err());
    let config = nereon_init(options.clone(), args(vec!["-w"]));
    assert!(config.is_err());

    env::set_var("TEST_WHO", "guess who?");
    let config = nereon_init(
        options.clone(),
        args(vec!["-w", "Arg", "--config", "tests/hello-who.conf"]),
    );
    if let Err(ref m) = config {
        println!("Error was: {:?}", m);
    }
    assert!(config.is_ok());
    println!("{:?}", config);
}
