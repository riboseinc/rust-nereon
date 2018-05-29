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

use std::ffi::OsString;
use std::ffi::{CStr, CString};
use std::path::Path;
use std::{io, mem, ptr, slice};

#[link(name = "nereon")]
extern "C" {
    fn nereon_ctx_init(ctx: *mut Ctx, nos: *const libc::c_char) -> libc::c_int;
    fn nereon_parse_noc_options(noc: *const libc::c_char, noc_opts: *mut libc::c_void) -> libc::c_int;
    fn nereon_parse_cmdline(ctx: *mut Ctx, n: libc::c_int, p: *const *const libc::c_char);
    fn nereon_ctx_finalize(ctx: *mut Ctx) -> ();
}

const CFG_MAX_NAME: usize = 64;
const CFG_MAX_LONG_SWITCH: usize = 32;
const CFG_MAX_SHORT_DESC: usize = 32;
const CFG_MAX_LONG_DESC: usize = 128;
const CFG_MAX_ENV_NAME: usize = 64;
const CFG_MAX_KEY_NAME: usize = 128;

const NEREON_TYPE_INT: libc::c_int = 0;
const NEREON_TYPE_BOOL: libc::c_int = 1;
const NEREON_TYPE_STRING: libc::c_int = 2;
const NEREON_TYPE_ARRAY: libc::c_int = 3;
const NEREON_TYPE_IPPORT: libc::c_int = 4;
const NEREON_TYPE_FLOAT: libc::c_int = 5;
const NEREON_TYPE_OBJECT: libc::c_int = 6;

#[repr(C)]
struct Ctx {
    nos: *const Nos,
    nos_count: libc::c_int,
    noc: *mut Noc,
}

#[repr(C)]
struct Noc {
    key: [libc::c_char; CFG_MAX_KEY_NAME],
    data_type: libc::c_int,
    childs: *const Noc,
    next: *const Noc,
    data: f64,
}

#[repr(C)]
struct Nos {
    name: [libc::c_char; CFG_MAX_NAME],
    data_type: libc::c_int,
    is_set: bool,
    sw_short: [libc::c_char; 2],
    sw_long: [libc::c_char; CFG_MAX_LONG_SWITCH],
    desc_short: [libc::c_char; CFG_MAX_SHORT_DESC],
    desc_long: [libc::c_char; CFG_MAX_LONG_DESC],
    env: [libc::c_char; CFG_MAX_ENV_NAME],
    noc_key: [libc::c_char; CFG_MAX_KEY_NAME],
    data: f64,
}

pub fn nereon<'a, I>(
    nos: Option<&str>,
    noc: Option<&Path>,
    args: I,
) -> io::Result<(Vec<super::Nos>, Option<super::Noc>)>
where
    I: IntoIterator<Item = OsString>,
{
    // new empty context for libnereon
    let mut ctx = Ctx {
        nos: ptr::null(),
        nos_count: 0,
        noc: ptr::null_mut(),
    };

    // c_nos_ptr is valid or NULL
    let c_nos;
    let mut c_nos_ptr = ptr::null();
    if let Some(s) = nos {
        c_nos = CString::new(s).unwrap();
        c_nos_ptr = c_nos.as_ptr();
    }

    // initialise our libnereon context from config files
    if unsafe { nereon_ctx_init(&mut ctx, c_nos_ptr) } == -1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Failed to parse nos.",
        ));
    }

    // read noc if necessary
    if let Some(path) = noc {
        let c_noc = CString::new(path.to_str().unwrap()).unwrap();
        let c_noc_ptr = c_noc.as_ptr();
        if unsafe { nereon_parse_noc_options(c_noc_ptr, &mut ctx.noc as *mut _ as *mut libc::c_void) } == -1 {
            unsafe { nereon_ctx_finalize(&mut ctx) };
            return Err(io::Error::new(
                io::ErrorKind::Other,
                "Failed to parse noc.",
            ));
        }
    }
        
    // process command line options
    // collect args as CString and parallel Vec of ptr
    let args = args.into_iter()
        .map(|a| CString::new(a.to_str().unwrap()).unwrap())
        .collect::<Vec<_>>();
    let c_args = args.iter().map(|a| a.as_ptr()).collect::<Vec<_>>();

    // update nereon context with command line args
    if c_args.len() > 1 {
        unsafe {
            nereon_parse_cmdline(&mut ctx, c_args.len() as libc::c_int, c_args.as_ptr());
        }
    }

    // collect nereon context into Rust structs
    let result = Ok((
        match ctx.nos_count as usize {
            0 => vec![],
            c => unsafe { slice::from_raw_parts(ctx.nos, c) }
                .iter()
                .map(get_nos)
                .collect::<Vec<_>>(),
        },
        match ctx.noc {
            p if !p.is_null() => Some(get_noc(unsafe { &*p })),
            _ => None,
        },
    ));

    // destroy context and return
    unsafe { nereon_ctx_finalize(&mut ctx) };

    result
}

fn get_nos(m: &Nos) -> super::Nos {
    super::Nos {
        name: copy_c_string(m.name.as_ptr()),
        data: match m.data_type {
            NEREON_TYPE_INT => super::NosData::Int(i64_from_data(m.data)),
            NEREON_TYPE_BOOL => super::NosData::Bool(bool_from_data(m.data)),
            NEREON_TYPE_FLOAT => super::NosData::Float(m.data),
            NEREON_TYPE_STRING => super::NosData::String(string_from_data(m.data)),
            NEREON_TYPE_IPPORT => super::NosData::IpPort(i64_from_data(m.data) as i32),
            n => panic!("Unknown NEREON_TYPE_ {}", n),
        },
        is_set: m.is_set,
        sw_short: copy_c_string(m.sw_short.as_ptr()),
        sw_long: copy_c_string(m.sw_long.as_ptr()),
        desc_short: copy_c_string(m.desc_short.as_ptr()),
        desc_long: copy_c_string(m.desc_long.as_ptr()),
        env: copy_c_string(m.env.as_ptr()),
        noc_key: copy_c_string(m.noc_key.as_ptr()),
    }
}

fn get_noc(c: &Noc) -> super::Noc {
    super::Noc {
        key: copy_c_string(c.key.as_ptr()),
        data: match c.data_type {
            NEREON_TYPE_INT => super::NocData::Int(i64_from_data(c.data)),
            NEREON_TYPE_BOOL => super::NocData::Bool(bool_from_data(c.data)),
            NEREON_TYPE_FLOAT => super::NocData::Float(c.data),
            NEREON_TYPE_STRING => super::NocData::String(string_from_data(c.data)),
            NEREON_TYPE_ARRAY => super::NocData::Array(get_noc_childs(c)),
            NEREON_TYPE_OBJECT => super::NocData::Object(get_noc_childs(c)),
            n => panic!("Unknown NEREON_TYPE_ {}", n),
        },
    }
}

fn get_noc_childs(c: &Noc) -> Vec<super::Noc> {
    let mut nocs = vec![];
    let mut addr = c.childs;

    while !addr.is_null() {
        let c = unsafe { ptr::read(addr) };
        nocs.push(get_noc(&c));
        addr = c.next;
    }
    nocs
}

fn copy_c_string(p: *const libc::c_char) -> String {
    match p.is_null() {
        false => unsafe { CStr::from_ptr(p) }.to_str().unwrap().to_owned(),
        true => "".to_owned(),
    }
}

fn string_from_data(p: f64) -> String {
    let s: *const libc::c_char = unsafe { mem::transmute(p) };
    copy_c_string(s)
}

fn bool_from_data(p: f64) -> bool {
    i64_from_data(p) & 0xff != 0
}

fn i64_from_data(p: f64) -> i64 {
    unsafe { mem::transmute(p) }
}

#[cfg(test)]
mod tests {
    extern crate tempfile;

    use self::tempfile::NamedTempFile;
//    use super::super::{Noc, NocData, Nos, NosData};
    use super::super::{Noc, NocData, Nos};
    use super::nereon;
//    use std::env;
    use std::ffi::OsString;
    use std::io;
    use std::io::Write;
//    use std::path::Path;

    #[test]
    fn test() {
        // empty everything
        assert!(test_nereon(nereon(None, None, vec![]), (vec![], None)));
        // just some args
        assert!(test_nereon(
            nereon(None, None, args(vec!["", "-a", "apples", "-p"])),
            (vec![], None)
        ));
        // types

        type_test("true", NocData::Bool(true));
        type_test("10", NocData::Int(10));
        type_test("42.0", NocData::Float(42.0));
        type_test("\"42\"", NocData::String("42".to_owned()));
    }

    fn type_test(v: &str, d: NocData) {
        let mut f = NamedTempFile::new().unwrap();
        let _t = writeln!(f, "a:{}", v);
        let r = nereon(None, Some(f.path()), vec![]);
        let c = config_item("", NocData::Object(vec![config_item("a", d)]));
        assert!(test_nereon(r, (vec![], Some(c))));
    }

    fn test_nereon(r: io::Result<(Vec<Nos>, Option<Noc>)>, t: (Vec<Nos>, Option<Noc>)) -> bool {
        match r {
            Ok(ref rr) if rr == &t => true,
            _ => {
                println!("{:?}", r);
                println!("Ok({:?})", t);
                false
            }
        }
    }

    fn args(a: Vec<&str>) -> Vec<OsString> {
        a.iter().map(|a| OsString::from(a)).collect()
    }

    fn config_item(k: &str, d: NocData) -> Noc {
        Noc {
            key: k.to_owned(),
            data: d,
        }
    }
}
