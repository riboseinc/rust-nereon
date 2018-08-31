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

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Dict(HashMap<String, Value>),
    Array(Vec<Value>),
}

impl Value {
    pub fn insert(&mut self, mut keys: Vec<String>, mut value: Value) {
        let key = keys.remove(0);
        let map = self.as_dict_mut().unwrap();
        let old_value = map.remove(&key).filter(|v| v.is_dict());

        map.insert(
            key,
            match keys.len() {
                0 => {
                    // single key so insert in current node
                    match (value.is_dict(), old_value) {
                        (true, Some(Value::Dict(mut existing))) => {
                            for (k, v) in value.as_dict_mut().unwrap().drain() {
                                existing.insert(k, v);
                            }
                            Value::Dict(existing)
                        }
                        _ => value,
                    }
                }
                _ => {
                    let mut node = old_value.unwrap_or_else(|| Value::Dict(HashMap::new()));
                    node.insert(keys, value);
                    node
                }
            },
        );
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s.as_ref()),
            _ => None,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Value::String(s) => s,
            _ => panic!(),
        }
    }

    pub fn as_dict<'a>(&'a self) -> Option<&'a HashMap<String, Value>> {
        match self {
            Value::Dict(ref map) => Some(map),
            _ => None,
        }
    }

    pub fn as_dict_mut<'a>(&'a mut self) -> Option<&'a mut HashMap<String, Value>> {
        match self {
            Value::Dict(ref mut map) => Some(map),
            _ => None,
        }
    }

    pub fn is_dict(&self) -> bool {
        match self {
            Value::Dict(_) => true,
            _ => false,
        }
    }

    pub fn as_array<'a>(&'a self) -> Option<&'a Vec<Value>> {
        match self {
            Value::Array(ref vec) => Some(vec),
            _ => None,
        }
    }

    pub fn as_array_mut<'a>(&'a mut self) -> Option<&'a mut Vec<Value>> {
        match self {
            Value::Array(ref mut vec) => Some(vec),
            _ => None,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            Value::Array(_) => true,
            _ => false,
        }
    }

    pub fn as_noc_string(&self) -> String {
        match self {
            Value::String(s) => format!("\"{}\"", s),
            Value::Array(v) => {
                let values = v.iter().map(|v| v.as_noc_string()).collect::<Vec<_>>();
                format!("[{}]", values.join(" "))
            }
            Value::Dict(m) => {
                let values = m
                    .iter()
                    .map(|(k, v)| format!("\"{}\" {}", k, v.as_noc_string()))
                    .collect::<Vec<_>>();
                format!("{{{}}}", values.join("\n"))
            }
        }
    }

    pub fn as_noc_string_pretty(&self) -> String {
        self.as_noc_string_indented("")
    }

    fn as_noc_string_indented(&self, indent: &str) -> String {
        match self {
            Value::String(s) => format!("\"{}\"", s),
            Value::Array(v) => {
                let values = v
                    .iter()
                    .map(|v| v.as_noc_string_indented(&(indent.to_owned() + "\t")))
                    .collect::<Vec<_>>();
                format!("[{}]", values.join(" "))
            }
            Value::Dict(m) => {
                let next_indent = indent.to_owned() + "\t";
                let values = m
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "{}\"{}\" {}",
                            next_indent,
                            k,
                            v.as_noc_string_indented(&next_indent)
                        )
                    }).collect::<Vec<_>>();
                format!("{{\n{}\n{}}}", values.join("\n"), indent)
            }
        }
    }
}
