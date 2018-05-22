extern crate libc;

use std::ffi::{CStr, CString};
use std::io;

#[link(name = "ucl")]
extern "C" {
    fn ucl_parser_new(flags: i32) -> *mut libc::c_void;
    fn ucl_parser_free(p: *mut libc::c_void);
    fn ucl_parser_add_chunk(p: *mut libc::c_void, s: *const i8, len: libc::size_t);
    fn ucl_parser_get_object(p: *mut libc::c_void) -> *mut libc::c_void;
    fn ucl_object_unref(p: *mut libc::c_void);
    fn ucl_object_emit(o: *mut libc::c_void, t: i32) -> *const libc::c_char;
}

struct UclParser {
    h: *mut libc::c_void,
}

impl UclParser {
    fn new() -> Option<UclParser> {
        let h = unsafe { ucl_parser_new(0) };
        match h.is_null() {
            false => Some(UclParser{ h: h }),
            true => None
        }
    }
}

impl Drop for UclParser {
    fn drop(&mut self) {
        unsafe {
            ucl_parser_free(self.h);
        }
    }
}

struct UclObject {
    h: *mut libc::c_void,
}

impl UclObject {
    fn new(h: *mut libc::c_void) -> UclObject {
        UclObject { h: h }
    }
}

impl Drop for UclObject {
    fn drop(&mut self) {
        unsafe {
            ucl_object_unref(self.h);
        }
    }
}

pub fn ucl_to_json(src: &mut io::Read) -> io::Result<String> {
    // TODO: this is currently a memory leak as we can't call
    // rust's free on the string emitted from UCL. Instead we need to
    // create emitter callback functions to build up the emitted string
    // within Rust
    let mut buffer = String::new();
    src.read_to_string(&mut buffer)?;

    let parser = UclParser::new();

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

    unsafe {
        ucl_parser_add_chunk(parser.h, src.as_ptr(), len);
    }

    let o = UclObject::new(unsafe { ucl_parser_get_object(parser.h) });
    let cstr = unsafe { CStr::from_ptr(ucl_object_emit(o.h, 1)) };

    Ok(cstr.to_str().unwrap().to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ucl_to_json() {
        assert_eq!(
            ucl_to_json(&mut "hello: \"hello\"".as_bytes()).unwrap(),
            "{\"hello\":\"hello\"}"
        );
    }
}
