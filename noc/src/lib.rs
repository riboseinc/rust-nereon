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

#[macro_use]
extern crate nereon_derive;
extern crate nereon;
extern crate tiny_http;

mod artifacts;

use nereon::{parse_noc, FromValue, Nos, Value};
use std::env;
use std::io::Write;
use std::io::{self, Read};
use tiny_http::{Method, Request, Response, Server, StatusCode};

const NOS: &str = r#"
name noc
authors ["John Hedges <john@drystone.co.uk>"]
version "0.1.0"
license "BSD-2-Clause"
option port {
    short p
    long "http-port"
    key [port]
    usage "NOC playground http port"
    hint PORT
}"#;

#[derive(FromValue)]
struct Config {
    port: Option<u32>,
}

pub fn main() -> Result<(), String> {
    let nos = Nos::from_value(&parse_noc(NOS).unwrap()).unwrap();
    let config = nereon::configure(&nos, env::args_os())?;
    let config = Config::from_value(&config).unwrap();

    config
        .port
        .map_or_else(|| parse(), |p| playground(p))
        .map_err(|e| format!("{:?}", e))
}

fn parse() -> io::Result<()> {
    let stdin = io::stdin();

    io::stdout().write_all({
        let mut noc = String::new();
        stdin.lock().read_to_string(&mut noc)?;
        parse_noc(&noc).unwrap().as_noc_string().as_ref()
    })?;
    Ok(())
}

fn playground(port: u32) -> io::Result<()> {
    Server::http(format!("{}:{}", "127.0.0.1", port))
        .and_then(|server| loop {
            match server.recv() {
                Ok(req) => handle_request(req)?,
                Err(e) => return Err(Box::new(e)),
            };
        })
        .map_err(|e| *e.downcast::<io::Error>().unwrap())
}

fn handle_request(mut req: Request) -> io::Result<()> {
    if req.method() == &Method::Get || req.method() == &Method::Head {
        match req.url() {
            "/" => req.respond(artifacts::html()),
            "/js" => req.respond(artifacts::js()),
            "/css" => req.respond(artifacts::css()),
            "/favicon.png" => req.respond(artifacts::favicon()),
            _ => req.respond(Response::from_string("Not found").with_status_code(StatusCode(404))),
        }
    } else if req.method() == &Method::Post {
        match req.url() {
            "/parse" => {
                let mut body = String::new();
                if req.as_reader().read_to_string(&mut body).is_ok() {
                    req.respond(Response::from_string(match parse_noc(&body) {
                        Ok(s) => s.as_noc_string_pretty(),
                        Err(e) => format!("{:?}", e),
                    }))
                } else {
                    req.respond(
                        Response::from_string("Non-UTF8 request body")
                            .with_status_code(StatusCode(400)),
                    )
                }
            }
            _ => req.respond(Response::from_string("Not found").with_status_code(StatusCode(404))),
        }
    } else {
        req.respond(Response::from_string("Method not allowed").with_status_code(StatusCode(405)))
    }
}
