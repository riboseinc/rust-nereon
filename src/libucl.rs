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

extern crate libc;

use std::ffi::{CStr, CString};
use std::io;

#[link(name = "ucl")]
extern "C" {
    fn ucl_parser_new(flags: i32) -> *mut libc::c_void;
    fn ucl_parser_free(p: *mut libc::c_void);
    fn ucl_parser_add_chunk_full(
        p: *mut libc::c_void,
        s: *const i8,
        len: libc::size_t,
        priority: libc::c_int,
        strat: libc::c_int,
        parse_type: libc::c_int,
    ) -> bool;
    fn ucl_parser_get_error(p: *mut libc::c_void) -> *const libc::c_char;
    fn ucl_parser_get_object(p: *mut libc::c_void) -> *mut libc::c_void;
    fn ucl_object_unref(p: *mut libc::c_void);
    fn ucl_object_emit(o: *mut libc::c_void, t: i32) -> *const libc::c_char;
}

enum Ucl {
    Parser(*mut libc::c_void),
    Object(*mut libc::c_void),
}

impl Drop for Ucl {
    fn drop(&mut self) {
        match self {
            Ucl::Parser(h) => unsafe { ucl_parser_free(*h) },
            Ucl::Object(h) => unsafe { ucl_object_unref(*h) },
        }
    }
}

impl Ucl {
    fn unwrap(&self) -> *mut libc::c_void {
        match self {
            Ucl::Parser(h) => *h,
            Ucl::Object(h) => *h,
        }
    }
}

/// Converts UCL content from `src` into a JSON [`String`]
///
/// # Examples
/// Basic usage:
///
/// ```
/// # use nereon::libucl::ucl_to_json;
/// let ucl = "greeting: \"hello\"";
/// let mut reader = ucl.as_bytes();
/// assert_eq!(ucl_to_json(&mut reader).unwrap(), "{\"greeting\":\"hello\"}");
/// ```
///
/// # Remarks
///
/// A minimal wrapper around [libucl](https://github.com/vstakhov/libucl).
///
/// This function does not perform variable substitution. If you need variable
/// substitution consider using [`nereon::ucl_to_serde_json`].
///
/// *Note*: The C string returned from libucl's `ucl_object_emit` cannot safely be
/// freed from Rust and so it's memory is leaked.
///
/// [`String`]: https://doc.rust-lang.org/std/string/struct.String.html
/// [`nereon::ucl_to_serde_json`]: ../fn.ucl_to_serde_json.html
pub fn ucl_to_json(src: &mut io::Read) -> io::Result<String> {
    let mut buffer = String::new();
    src.read_to_string(&mut buffer)?;

    let len = buffer.len();
    let src = match CString::new(buffer) {
        Ok(s) => s,
        Err(_) => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Can't convert UCL to Utf8".to_owned(),
            ))
        }
    };

    let p = match unsafe { ucl_parser_new(0) } {
        h if !h.is_null() => Ucl::Parser(h),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to create UCL parser",
            ))
        }
    };

    if !unsafe { ucl_parser_add_chunk_full(p.unwrap(), src.as_ptr(), len, 0, 1, 0) } {
        return match unsafe { ucl_parser_get_error(p.unwrap()) } {
            cerr if !cerr.is_null() => {
                let err = unsafe { CStr::from_ptr(cerr) }.to_str().unwrap();
                Err(io::Error::new(
                    io::ErrorKind::Other,
                    format!("Failed to parse UCL: {}", err),
                ))
            }
            _ => Err(io::Error::new(io::ErrorKind::Other, "Failed to parse UCL")),
        };
    }

    let o = match unsafe { ucl_parser_get_object(p.unwrap()) } {
        h if !h.is_null() => Ucl::Object(h),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to get UCL object",
            ))
        }
    };

    // TODO: this is currently a memory leak as we can't call
    // rust's free on the string emitted from UCL. Instead we need to
    // create emitter callback functions to build up the emitted string
    // within Rust
    let cstr = unsafe { CStr::from_ptr(ucl_object_emit(o.unwrap(), 1)) };

    Ok(cstr.to_str().unwrap().to_owned())
}

#[cfg(test)]
mod tests {

    #[test]
    fn ucl_to_json() {
        assert_eq!(
            super::ucl_to_json(&mut "hello: \"hello\"".as_bytes()).unwrap(),
            "{\"hello\":\"hello\"}"
        );
    }
}
