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

use super::{ErrorKind, Value};

pub fn evaluate(name: &str, eval_args: &[Value], args: &[Value]) -> Result<Value, ErrorKind> {
    match name {
        "add" => unimplemented!(),
        "arg" => arg(eval_args, args),
        _ => Err(ErrorKind::UnknownEval(name.to_owned())),
    }
}

fn arg(eval_args: &[Value], args: &[Value]) -> Result<Value, ErrorKind> {
    match eval_args.len() {
        1 => Ok(&eval_args[0]),
        _ => Err(ErrorKind::BadArg("arg(int)".to_owned())),
    }.and_then(|arg| match arg.as_string() {
        Some(arg) => Ok(arg),
        None => Err(ErrorKind::BadArg(format!("arg(int): Not an int {:?}", arg))),
    })
        .and_then(|num| match num.parse::<usize>() {
            Ok(num) => Ok(num),
            Err(e) => Err(ErrorKind::BadArg(format!("arg(int): {:?}", e))),
        })
        .and_then(|num| match args.get(num) {
            Some(value) => Ok((*value).clone()),
            None => Err(ErrorKind::BadArg(format!("arg({}): not enough args", num))),
        })
}
