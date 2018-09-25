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
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::iter::{self, FromIterator};
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Dict(HashMap<String, Value>),
    List(Vec<Value>),
}

impl<'a> From<&'a str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.to_owned())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl<'a> From<&'a OsStr> for Value {
    fn from(s: &OsStr) -> Self {
        Value::String(String::from(s.to_string_lossy()))
    }
}

impl From<OsString> for Value {
    fn from(s: OsString) -> Self {
        Value::from(s.as_os_str())
    }
}

impl<S, V> From<HashMap<S, V>> for Value
where
    Value: From<V>,
    String: From<S>,
    S: Eq + Hash,
{
    fn from(mut m: HashMap<S, V>) -> Self {
        Value::Dict(
            m.drain()
                .map(|(k, v)| (String::from(k), Value::from(v)))
                .collect(),
        )
    }
}

impl<V> From<Vec<V>> for Value
where
    Value: From<V>,
{
    fn from(mut v: Vec<V>) -> Self {
        Value::List(v.drain(..).map(Value::from).collect())
    }
}

impl Value {
    pub fn insert<'a, I, V>(&mut self, keys: I, value: V)
    where
        I: IntoIterator<Item = &'a str>,
        V: Into<Value>,
    {
        let value = value.into();
        let mut keys = keys.into_iter().peekable();

        if let Some(key) = keys.next() {
            let map = self.as_dict_mut().unwrap();
            let old_value = map.remove(key).filter(|v| v.is_dict());

            map.insert(
                key.to_owned(),
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
        } else {
            *self = value;
        }
    }

    pub fn get<T>(&self, key: &str) -> Result<T, String>
    where
        T: FromValue,
    {
        self.get_value(key)
            .map_or_else(T::from_no_value, T::from_value)
    }

    pub fn get_value<'a>(&'a self, key: &str) -> Option<&'a Value> {
        self.as_dict().and_then(|d| d.get(key))
    }

    pub fn lookup<'a, I, T>(&'a self, keys: I) -> Result<T, String>
    where
        I: IntoIterator<Item = &'a str>,
        T: FromValue,
    {
        self.lookup_value(keys)
            .map_or_else(T::from_no_value, T::from_value)
    }

    pub fn lookup_value<'a, I>(&'a self, keys: I) -> Option<&'a Value>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let keys = keys.into_iter().collect::<Vec<_>>();
        if keys.is_empty() {
            Some(self)
        } else {
            self.get_value(keys[0])
                .and_then(|v| v.lookup_value(keys[1..].to_vec()))
        }
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
                let values = BTreeMap::from_iter(m.iter())
                    .iter()
                    .map(|(k, v)| match v {
                        Value::Dict(_) => format!("\"{}\" {{{}}}", k, v.as_noc_string()),
                        Value::List(_) => format!("\"{}\" [{}]", k, v.as_noc_string()),
                        Value::String(_) => format!("\"{}\" {}", k, v.as_noc_string()),
                    })
                    .collect::<Vec<_>>();
                values.join(",")
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

pub trait FromValue<OK = Self> {
    fn from_value(value: &Value) -> Result<OK, String>;
    // this is a kludge so missing Values can be converted
    // into None
    fn from_no_value() -> Result<OK, String> {
        Err("No such key".to_owned())
    }
}

macro_rules! from_value_for {
    ($type:ident) => {
        impl FromValue for $type {
            fn from_value(value: &Value) -> Result<Self, String> {
                value.as_str().map_or_else(
                    || Err("Value is not a String".to_owned()),
                    |s| {
                        s.parse().map_err(|e| {
                            format!(
                                "Failed to parse: {} (\"{}\" -> {})",
                                e,
                                s,
                                stringify!($type)
                            )
                        })
                    },
                )
            }
        }
    };
}

from_value_for!(u8);
from_value_for!(u16);
from_value_for!(u32);
from_value_for!(u64);
from_value_for!(i8);
from_value_for!(i16);
from_value_for!(i32);
from_value_for!(i64);
from_value_for!(f32);
from_value_for!(f64);

impl FromValue for String {
    fn from_value(value: &Value) -> Result<Self, String> {
        value
            .as_str()
            .map(String::from)
            .map_or_else(|| Err("Value is not a string".to_owned()), Ok)
    }
}

impl<T> FromValue for Option<T>
where
    T: FromValue,
{
    fn from_value(value: &Value) -> Result<Self, String> {
        T::from_value(value).map(Some).or_else(|_| Ok(None))
    }
    fn from_no_value() -> Result<Self, String> {
        Ok(None)
    }
}

impl<T, S: ::std::hash::BuildHasher + Default> FromValue for HashMap<String, T, S>
where
    T: FromValue,
{
    fn from_value(value: &Value) -> Result<Self, String> {
        value
            .as_dict()
            .ok_or_else(|| "Couldn't convert".to_owned())
            .and_then(|d| {
                d.iter().try_fold(HashMap::default(), |mut m, (k, v)| {
                    T::from_value(v).map(|v| {
                        m.insert(k.to_owned(), v);
                        m
                    })
                })
            })
    }
}

impl<T> FromValue for Vec<T>
where
    T: FromValue,
{
    fn from_value(value: &Value) -> Result<Self, String> {
        value
            .as_list()
            .ok_or_else(|| "Couldn't convert".to_owned())
            .and_then(|d| {
                d.iter().try_fold(Vec::new(), |mut m, v| {
                    T::from_value(v).map(|v| {
                        m.push(v);
                        m
                    })
                })
            })
    }
}

#[cfg(test)]
mod tests {
    use super::{FromValue, Value};
    use std::collections::HashMap;
    use std::str::FromStr;

    #[test]
    fn test_value_from() {
        assert_eq!(
            Value::from(HashMap::<String, Value>::new()),
            Value::Dict(HashMap::new())
        );
        assert_eq!(Value::from(Vec::<&str>::new()), Value::List(Vec::new()));
        assert_eq!(Value::from("hello"), Value::String("hello".to_owned()));
        assert_eq!(
            Value::from("hello".to_owned()),
            Value::String("hello".to_owned())
        );
    }

    #[test]
    fn test_value_insert() {
        let mut v = Value::from(HashMap::<String, Value>::new());
        v.insert(vec!["a"], "a");
        assert_eq!(v.as_noc_string(), r#""a" "a""#);
        v.insert(vec!["b"], "b");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b""#);
        v.insert(vec!["c", "c", "c"], "c");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b","c" {"c" {"c" "c"}}"#);
        v.insert(vec!["c"], "c");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b","c" "c""#);
    }

    #[test]
    fn test_value_get() {
        let value = Value::from_str(r#"a a, b 1, c c, e {}, f []"#).unwrap();
        assert_eq!(value.get("a"), Ok("a".to_owned()));
        assert_eq!(value.get("a"), Ok(Some("a".to_owned())));
        assert_eq!(value.get("b"), Ok(1u8));
        assert_eq!(value.get("b"), Ok(1u16));
        assert_eq!(value.get("b"), Ok(1u32));
        assert_eq!(value.get("b"), Ok(1u64));
        assert_eq!(value.get("b"), Ok(1i8));
        assert_eq!(value.get("b"), Ok(1i16));
        assert_eq!(value.get("b"), Ok(1i32));
        assert_eq!(value.get("b"), Ok(1i64));
        assert_eq!(value.get("b"), Ok(1f32));
        assert_eq!(value.get("b"), Ok(1f64));
        assert_eq!(value.get("c"), Ok("c".to_owned()));
        assert!(value.get::<String>("d").is_err());
        assert_eq!(value.get::<Option<String>>("d"), Ok(None));
        assert_eq!(
            value.get::<HashMap<String, String>>("e"),
            Ok(HashMap::new())
        );
        assert_eq!(value.get::<Vec<String>>("f"), Ok(Vec::new()));
    }

    #[test]
    fn test_value_get_value() {
        let value = Value::from_str(r#"a a, b b, c c, e {}, f []"#).unwrap();
        assert_eq!(value.get_value("a"), Some(&Value::String("a".to_owned())));
        assert_eq!(value.get_value("b"), Some(&Value::String("b".to_owned())));
        assert_eq!(value.get_value("c"), Some(&Value::String("c".to_owned())));
        assert_eq!(value.get_value("d"), None);
        assert_eq!(value.get_value("f"), Some(&Value::List(Vec::new())));
        assert_eq!(value.get_value("e"), Some(&Value::Dict(HashMap::new())));
    }

    #[test]
    fn test_value_is_as_into() {
        let v = Value::from("a");
        assert_eq!(v.as_str(), Some("a"));
        assert_eq!(v.as_dict(), None);
        assert_eq!(v.is_string(), true);
        assert_eq!(v.is_dict(), false);
        let v = Value::from(vec![""]);
        assert_eq!(v.as_str(), None);
        assert_eq!(v.as_list(), Some(&vec![Value::from("")]));
        assert_eq!(v.is_string(), false);
        assert_eq!(v.is_list(), true);
        let v = Value::from(HashMap::<String, Value>::new());
        assert_eq!(v.as_list(), None);
        assert_eq!(v.as_dict(), Some(&HashMap::new()));
        assert_eq!(v.is_list(), false);
        assert_eq!(v.is_dict(), true);
    }

    #[test]
    fn test_value_to_noc_string() {
        let v = Value::from_str(r#"a a, b b, c c, e {a a, b b}, f [a,b,c,d]"#).unwrap();
        let noc = v.as_noc_string();
        assert_eq!(v, Value::from_str(&noc).unwrap());
        assert_eq!(
            v.as_noc_string_pretty(),
            r#""a" "a"
"b" "b"
"c" "c"
"e" {
	"a" "a"
	"b" "b"
}
"f" [
	"a"
	"b"
	"c"
	"d"
]"#
        );
    }

    #[test]
    fn test_from_value() {
        assert_eq!(
            String::from_value(&Value::from("hello")),
            Ok("hello".to_owned())
        );
        assert_eq!(
            Vec::from_value(&Value::from(vec!["a", "b"])),
            Ok(vec!["a".to_owned(), "b".to_owned()])
        );
        assert_eq!(
            HashMap::from_value(&Value::from(HashMap::<&str, &str>::new())),
            Ok(HashMap::<String, String>::new())
        );
    }
}
