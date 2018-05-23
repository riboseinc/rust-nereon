extern crate libc;

use std::ffi::{CStr, CString};
use std::io;

#[link(name = "ucl")]
extern "C" {
    fn ucl_parser_new(flags: i32) -> *mut libc::c_void;
    fn ucl_parser_free(p: *mut libc::c_void);
    fn ucl_parser_add_chunk(p: *mut libc::c_void, s: *const i8, len: libc::size_t) -> bool;
    fn ucl_parser_get_error(p: *mut libc::c_void) -> *const libc::c_char;
    fn ucl_parser_get_object(p: *mut libc::c_void) -> *mut libc::c_void;
    fn ucl_object_unref(p: *mut libc::c_void);
    fn ucl_object_emit(o: *mut libc::c_void, t: i32) -> *const libc::c_char;
}

enum Ucl {
    Parser(*mut libc::c_void),
    Object(*mut libc::c_void)
}

impl Drop for Ucl {
    fn drop(&mut self) {
        match self {
            Ucl::Parser(h) => unsafe { ucl_parser_free(*h) },
            Ucl::Object(h) => unsafe { ucl_object_unref(*h) }
        }
    }
}

impl Ucl {
    fn unwrap(&self) -> *mut libc::c_void {
        match self {
            Ucl::Parser(h) => *h,
            Ucl::Object(h) => *h
        }
    }
}

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
        _ => return Err(io::Error::new(io::ErrorKind::Other, "Failed to create UCL parser"))
    };
            
    if ! unsafe { ucl_parser_add_chunk(p.unwrap(), src.as_ptr(), len) } {
        return match unsafe { ucl_parser_get_error(p.unwrap()) } {
            cerr if !cerr.is_null() => {
                let err = unsafe { CStr::from_ptr(cerr) }.to_str().unwrap() ;
                Err(io::Error::new(io::ErrorKind::Other, format!("Failed to parse UCL: {}", err)))
            }
            _ => {
                Err(io::Error::new(io::ErrorKind::Other, "Failed to parse UCL"))
            }
        }
    }

    let o = match unsafe { ucl_parser_get_object(p.unwrap()) } {
        h if !h.is_null() => Ucl::Object(h),
        _ => return Err(io::Error::new(io::ErrorKind::Other, "Failed to get UCL object"))
    };
    
    // TODO: this is currently a memory leak as we can't call
    // rust's free on the string emitted from UCL. Instead we need to
    // create emitter callback functions to build up the emitted string
    // within Rust
    let cstr = unsafe { CStr::from_ptr( ucl_object_emit(o.unwrap(), 1)) };

    Ok(cstr.to_str().unwrap().to_owned())
}

#[cfg(test)]
mod tests {
//    use super::*;

    #[test]
    fn ucl_to_json() {
        assert_eq!(
            super::ucl_to_json(&mut "hello: \"hello\"".as_bytes()).unwrap(),
            "{\"hello\":\"hello\"}"
        );
    }
}
