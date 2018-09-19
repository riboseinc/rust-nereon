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

use std::collections::{BTreeMap, HashMap};
use std::iter::{self, FromIterator};
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Dict(HashMap<String, Value>),
    List(Vec<Value>),
}

pub trait FromValue<OK = Self> {
    fn from_value(value: &Value) -> Result<OK, String>;
    fn from_kv<'a, I>(keys: I, value: &Value) -> Result<OK, String>
    where
        I: IntoIterator<Item = &'a str>,
    {
        value
            .get(keys)
            .map_or_else(|| Err("No such value".to_owned()), Self::from_value)
    }
    fn from_kv_optional<'a, I>(keys: I, value: &Value) -> Result<Option<OK>, String>
    where
        I: IntoIterator<Item = &'a str>,
    {
        value
            .get(keys)
            .map_or_else(|| Ok(None), |value| Self::from_value(value).map(Some))
    }
}

impl Value {
    pub fn insert<I>(&mut self, keys: I, value: Value)
    where
        I: IntoIterator<Item = String>,
    {
        let mut keys = keys.into_iter().peekable();
        let key = keys.next().unwrap();
        let map = self.as_dict_mut().unwrap();
        let old_value = map.remove(&key).filter(|v| v.is_dict());

        map.insert(
            key,
            if keys.peek().is_none() {
                // single key so insert in current node
                match (value, old_value) {
                    (Value::Dict(mut new), Some(Value::Dict(mut existing))) => {
                        for (k, v) in new.drain() {
                            existing.insert(k, v);
                        }
                        Value::Dict(existing)
                    }
                    (v, _) => v,
                }
            } else {
                let mut node = old_value.unwrap_or_else(|| Value::Dict(HashMap::new()));
                node.insert(keys.collect::<Vec<_>>(), value);
                node
            },
        );
    }

    pub fn get<'a, I>(&self, keys: I) -> Option<&Value>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let mut keys = keys.into_iter();
        let key = keys.next();
        key.map_or_else(
            || Some(self),
            |k| {
                self.as_dict()
                    .and_then(|d| d.get(k))
                    .and_then(|v| v.get(keys))
            },
        )
    }

    pub fn get_str<'a, I>(&self, keys: I) -> Result<&str, String>
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.get(keys).map_or_else(
            || Err("No such value".to_owned()),
            |value| {
                value
                    .as_str()
                    .ok_or_else(|| "Value is not a string".to_owned())
            },
        )
    }

    pub fn get_string<'a, I>(&self, keys: I) -> Result<String, String>
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.get_str(keys).map(String::from)
    }

    pub fn get_dict<'a, I>(&'a self, keys: I) -> Result<&'a HashMap<String, Value>, String>
    where
        I: IntoIterator<Item = &'a str>,
    {
        self.get(keys).map_or_else(
            || Err("No such value".to_owned()),
            |value| {
                value
                    .as_dict()
                    .ok_or_else(|| "Value is not a dict".to_owned())
            },
        )
    }

    pub fn as_str(&self) -> Option<&str> {
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

    pub fn into_string(self) -> Option<String> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
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

    pub fn as_list<'a>(&'a self) -> Option<&'a Vec<Value>> {
        match self {
            Value::List(ref vec) => Some(vec),
            _ => None,
        }
    }

    pub fn as_list_mut<'a>(&'a mut self) -> Option<&'a mut Vec<Value>> {
        match self {
            Value::List(ref mut vec) => Some(vec),
            _ => None,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Value::List(_) => true,
            _ => false,
        }
    }

    pub fn as_noc_string(&self) -> String {
        match self {
            Value::String(s) => format!("\"{}\"", s),
            Value::List(v) => {
                let values = v
                    .iter()
                    .map(|v| match v {
                        Value::Dict(_) => format!("{{{}}}", v.as_noc_string()),
                        Value::List(_) => format!("[{}]", v.as_noc_string()),
                        Value::String(_) => v.as_noc_string(),
                    })
                    .collect::<Vec<_>>();
                values.join(",")
            }
            Value::Dict(m) => {
                let values = m
                    .iter()
                    .map(|(k, v)| match v {
                        Value::Dict(_) => format!("\"{}\" {{{}}}", k, v.as_noc_string()),
                        Value::List(_) => format!("\"{}\" [{}]", k, v.as_noc_string()),
                        Value::String(_) => format!("\"{}\" {}", k, v.as_noc_string()),
                    })
                    .collect::<Vec<_>>();
                values.join("\n")
            }
        }
    }

    pub fn as_noc_string_pretty(&self) -> String {
        self.as_s_indent(0)
    }

    fn as_s_indent(&self, indent: usize) -> String {
        let tabs = iter::repeat('\t').take(indent).collect::<String>();
        match self {
            Value::String(s) => format!("\"{}\"", s),
            Value::List(v) => v
                .iter()
                .map(|v| {
                    let s = v.as_s_indent(indent + 1);
                    match v {
                        Value::Dict(_) => format!("{}{{\n{}\n{}}}", tabs, s, tabs),
                        Value::List(_) => format!("{}[\n{}\n{}]", tabs, s, tabs),
                        Value::String(_) => format!("{}{}", tabs, s),
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
            Value::Dict(m) => BTreeMap::from_iter(m.iter()) // sort
                .iter()
                .map(|(k, v)| {
                    let s = v.as_s_indent(indent + 1);
                    match v {
                        Value::Dict(_) => format!("{}\"{}\" {{\n{}\n{}}}", tabs, k, s, tabs),
                        Value::List(_) => format!("{}\"{}\" [\n{}\n{}]", tabs, k, s, tabs),
                        Value::String(_) => format!("{}\"{}\" {}", tabs, k, s),
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }
}

impl FromStr for Value {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, String> {
        super::parse(input)
    }
}

impl FromValue for String {
    fn from_value(value: &Value) -> Result<Self, String> {
        value
            .as_str()
            .map(String::from)
            .map_or_else(|| Err("Value is not a string".to_owned()), Ok)
    }
}
