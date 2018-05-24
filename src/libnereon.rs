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
    fn nereon_ctx_init(
        ctx: *mut Ctx,
        cfg_path: *const libc::c_char,
        meta_path: *const libc::c_char,
    ) -> libc::c_int;
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
    meta: *const Meta,
    meta_count: libc::c_int,
    cfg: *const Cfg,
}

#[repr(C)]
struct Cfg {
    cfg_key: [libc::c_char; CFG_MAX_KEY_NAME],
    cfg_type: libc::c_int,
    childs: *const Cfg,
    next: *const Cfg,
    cfg_data: f64,
}

#[repr(C)]
struct Meta {
    cfg_name: [libc::c_char; CFG_MAX_NAME],
    cfg_type: libc::c_int,
    helper: bool,
    sw_short: [libc::c_char; 2],
    sw_long: [libc::c_char; CFG_MAX_LONG_SWITCH],
    desc_short: [libc::c_char; CFG_MAX_SHORT_DESC],
    desc_long: [libc::c_char; CFG_MAX_LONG_DESC],
    cfg_env: [libc::c_char; CFG_MAX_ENV_NAME],
    cfg_key: [libc::c_char; CFG_MAX_KEY_NAME],
    cfg_data: f64,
}

pub fn nereon<'a, I>(
    cfg: Option<&Path>,
    meta: Option<&Path>,
    args: I,
) -> io::Result<(Option<super::Cfg>, Vec<super::Meta>)>
where
    I: IntoIterator<Item = OsString>,
{
    // new empty context for libnereon
    let mut ctx = Ctx {
        meta: ptr::null_mut(),
        meta_count: 0,
        cfg: ptr::null(),
    };

    // c_cfg_ptr is valid or NULL
    let c_cfg;
    let mut c_cfg_ptr = ptr::null();
    if let Some(path) = cfg {
        c_cfg = CString::new(path.to_str().unwrap()).unwrap();
        c_cfg_ptr = c_cfg.as_ptr();
    }

    // c_meta_ptr is valid or NULL
    let c_meta;
    let mut c_meta_ptr = ptr::null();
    if let Some(path) = meta {
        c_meta = CString::new(path.to_str().unwrap()).unwrap();
        c_meta_ptr = c_meta.as_ptr();
    }

    // initialise our libnereon context from config files
    if unsafe { nereon_ctx_init(&mut ctx, c_cfg_ptr, c_meta_ptr) } == -1 {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Failed to get nereon configuration.",
        ));
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
        match ctx.cfg {
            p if !p.is_null() => Some(get_cfg(unsafe { &*p })),
            _ => None,
        },
        match ctx.meta_count as usize {
            0 => vec![],
            c => unsafe { slice::from_raw_parts(ctx.meta, c) }
                .iter()
                .map(get_meta)
                .collect::<Vec<_>>(),
        },
    ));

    // destroy context and return
    unsafe { nereon_ctx_finalize(&mut ctx) };

    result
}

fn get_meta(m: &Meta) -> super::Meta {
    super::Meta {
        name: copy_c_string(m.cfg_name.as_ptr()),
        data: match m.cfg_type {
            NEREON_TYPE_INT => super::MetaData::Int(i64_from_data(m.cfg_data)),
            NEREON_TYPE_BOOL => super::MetaData::Bool(bool_from_data(m.cfg_data)),
            NEREON_TYPE_FLOAT => super::MetaData::Float(m.cfg_data),
            NEREON_TYPE_STRING => super::MetaData::String(string_from_data(m.cfg_data)),
            NEREON_TYPE_IPPORT => super::MetaData::IpPort(i64_from_data(m.cfg_data) as i32),
            n => panic!("Unknown NEREON_TYPE_ {}", n),
        },
        helper: m.helper,
        sw_short: copy_c_string(m.sw_short.as_ptr()),
        sw_long: copy_c_string(m.sw_long.as_ptr()),
        desc_short: copy_c_string(m.desc_short.as_ptr()),
        desc_long: copy_c_string(m.desc_long.as_ptr()),
        cfg_env: copy_c_string(m.cfg_env.as_ptr()),
        cfg_key: copy_c_string(m.cfg_key.as_ptr()),
    }
}

fn get_cfg(c: &Cfg) -> super::Cfg {
    super::Cfg {
        key: copy_c_string(c.cfg_key.as_ptr()),
        data: match c.cfg_type {
            NEREON_TYPE_INT => super::CfgData::Int(i64_from_data(c.cfg_data)),
            NEREON_TYPE_BOOL => super::CfgData::Bool(bool_from_data(c.cfg_data)),
            NEREON_TYPE_FLOAT => super::CfgData::Float(c.cfg_data),
            NEREON_TYPE_STRING => super::CfgData::String(string_from_data(c.cfg_data)),
            NEREON_TYPE_ARRAY => super::CfgData::Array(get_cfg_childs(c)),
            NEREON_TYPE_OBJECT => super::CfgData::Object(get_cfg_childs(c)),
            n => panic!("Unknown NEREON_TYPE_ {}", n),
        },
    }
}

fn get_cfg_childs(c: &Cfg) -> Vec<super::Cfg> {
    let mut cfgs = vec![];
    let mut addr = c.childs;

    while !addr.is_null() {
        let c = unsafe { ptr::read(addr) };
        cfgs.push(get_cfg(&c));
        addr = c.next;
    }
    cfgs
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
    use super::super::{Cfg, CfgData, Meta, MetaData};
    use super::nereon;
    use std::env;
    use std::ffi::OsString;
    use std::io;
    use std::io::Write;
    use std::path::Path;

    #[test]
    fn test() {
        // empty everything
        assert!(test_nereon(nereon(None, None, vec![]), (None, vec![])));
        // just some args
        assert!(test_nereon(
            nereon(None, None, args(vec!["", "-a", "apples", "-p"])),
            (None, vec![])
        ));
        // types

        type_test("true", CfgData::Bool(true));
        type_test("10", CfgData::Int(10));
        type_test("42.0", CfgData::Float(42.0));
        type_test("\"42\"", CfgData::String("42".to_owned()));
    }

    fn type_test(v: &str, d: CfgData) {
        let mut f = NamedTempFile::new().unwrap();
        let _t = writeln!(f, "a:{}", v);
        let r = nereon(Some(f.path()), None, vec![]);
        let c = config_item("", CfgData::Object(vec![config_item("a", d)]));
        assert!(test_nereon(r, (Some(c), vec![])));
    }

    fn test_nereon(r: io::Result<(Option<Cfg>, Vec<Meta>)>, t: (Option<Cfg>, Vec<Meta>)) -> bool {
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

    fn config_item(k: &str, d: CfgData) -> Cfg {
        Cfg {
            key: k.to_owned(),
            data: d,
        }
    }
}
