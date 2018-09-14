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

extern crate getopts;
extern crate nereon;
extern crate noc;
extern crate tiny_http;

use getopts::Options;
use noc::artifacts;
use std::env;
use std::io::{self, Read};
use std::io::Write;
use std::process::exit;
use tiny_http::{Method, Request, Response, Server, StatusCode};
use nereon::Value;

fn usage(opts: &Options) {
    let progname = env::current_exe()
        .map(|p| p.file_name().unwrap().to_str().unwrap().to_owned())
        .unwrap();

    let brief = format!("Usage: {} [options]", progname);
    print!("{}", opts.usage(&brief));
}

fn main() {
    let mut opts = Options::new();

    opts.optopt("p", "http-port", "NOC playground HTTP server port", "PORT");
    opts.optflag("h", "help", "print this help menu");

    opts.parse(&env::args().collect::<Vec<_>>()[1..])
        .map_err(|e| format!("{:?}", e))
        .and_then(|matches| {
            if matches.opt_present("h") {
                usage(&opts);
                return Ok(());
            }

            match matches.opt_str("p") {
                Some(p) => p
                    .parse::<u32>()
                    .map_err(|_| format!("Bad port [{}]", p))
                    .and_then(|p| playground(p).map_err(|e| format!("{:?}", e))),
                _ => parse().map_err(|e| format!("{:?}", e)),
            }
        }).unwrap_or_else(|e| {
            println!("{}", e);
            usage(&opts);
            exit(1);
        });
}

fn parse() -> io::Result<()> {
    let stdin = io::stdin();

    io::stdout().write_all( {
        let mut noc = String::new();
        stdin.lock().read_to_string(&mut noc)?;
        noc.parse::<Value>()
            .unwrap()
            .as_noc_string()
            .as_ref()
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
        }).map_err(|e| *e.downcast::<io::Error>().unwrap())
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
                    req.respond(Response::from_string(match body.parse::<Value>() {
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
