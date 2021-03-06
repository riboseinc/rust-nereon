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

use tiny_http::{Header, Response, StatusCode};

pub fn html() -> Response<&'static [u8]> {
    artifact("text/html", include_bytes!("noc.html"))
}

pub fn js() -> Response<&'static [u8]> {
    artifact("application/javascript", include_bytes!("noc.js"))
}

pub fn css() -> Response<&'static [u8]> {
    artifact("text/css", include_bytes!("noc.css"))
}

pub fn favicon() -> Response<&'static [u8]> {
    artifact("image/x-icon", include_bytes!("noc.png"))
}

fn artifact<'a>(mime: &str, src: &'a [u8]) -> Response<&'a [u8]> {
    Response::new(
        StatusCode(200),
        vec![Header::from_bytes("Content-Type", mime).unwrap()],
        src,
        Some(src.len()),
        None,
    )
}
