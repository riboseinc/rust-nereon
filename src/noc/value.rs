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

/// Main `Value` enum with variants for strings, tables, and lists.
///
/// There is currently no constructor for Value instances though
/// they can be created from `str`, `String`, `Vec` and `HashMap` instances
///
/// ```
/// # extern crate nereon;
/// # use std::collections::HashMap;
/// # use nereon::Value;
/// assert_eq!(Value::String("42".to_owned()), Value::from("42"));
/// assert_eq!(Value::Table(HashMap::new()), Value::from(HashMap::<String, Value>::new()));
/// assert_eq!(Value::List(vec![]), Value::from(Vec::<Value>::new()));
/// ```
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Table(HashMap<String, Value>),
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
        Value::Table(
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
    /// `insert` a [`Value`](enum.Value.html) at the end of the branch
    /// described by a list of keys. Branch nodes are `Table` variants
    /// of `Value`.
    ///
    /// Missing branch nodes are automatically created. Any `String`
    /// and `List` nodes along the path are overwritten.
    ///
    /// Inserting a `Table` where a `Table` already exists causes the
    /// tables to be merged. Old table entries are overwritten by new
    /// entries with the same key.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use std::collections::HashMap;
    /// # use nereon::{Value, parse_noc};
    /// let mut v = Value::Table(HashMap::new());
    /// assert_eq!(v, parse_noc::<Value>("").unwrap());
    /// v = v.insert(vec!["forename"], Value::from("John"));
    /// assert_eq!(v, parse_noc::<Value>("forename John").unwrap());
    /// v = v.insert(vec!["surname"], Value::from("Doe"));
    /// assert_eq!(v, parse_noc::<Value>("forename John, surname Doe").unwrap());
    /// v = v.insert(vec!["forename"], Value::from(vec!["John", "Reginald"]));
    /// assert_eq!(v, parse_noc::<Value>("forename [John, Reginald], surname Doe").unwrap());
    /// v = v.insert(vec!["forename", "first"], Value::from("John"));
    /// assert_eq!(v, parse_noc::<Value>("forename { first John }, surname Doe").unwrap());
    /// v = v.insert(vec!["forename", "middle"], Value::from("Reginald"));
    /// assert_eq!(v, parse_noc::<Value>(
    ///     "forename { first John, middle Reginald }, surname Doe").unwrap());
    /// ```
    pub fn insert<'a, I, V>(self, keys: I, value: V) -> Self
    where
        I: IntoIterator<Item = &'a str>,
        V: Into<Value>,
    {
        let value = value.into();
        let mut keys = keys.into_iter().peekable();
        if keys.peek().is_none() {
            value
        } else {
            let key = keys.next().unwrap();
            if let Value::Table(mut map) = self {
                let old_value = map.remove(key).filter(|v| v.is_dict());
                map.insert(
                    key.to_owned(),
                    if keys.peek().is_none() {
                        // single key so insert in current node
                        match (value, old_value) {
                            (Value::Table(mut new), Some(Value::Table(mut existing))) => {
                                for (k, v) in new.drain() {
                                    existing.insert(k, v);
                                }
                                Value::Table(existing)
                            }
                            (v, _) => v,
                        }
                    } else {
                        let mut node = old_value.unwrap_or_else(|| Value::Table(HashMap::new()));
                        node.insert(keys.collect::<Vec<_>>(), value)
                    },
                );
                Value::Table(map)
            } else {
                unreachable!()
            }
        }
    }

    /// Get and convert a value from a `Value::Table` by `key`
    ///
    /// The return type `T` inplements `FromValue` so this method is used
    /// to get the `Value` and convert to `T` in one call
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use std::collections::HashMap;
    /// # use nereon::{Value, parse_noc};
    /// assert_eq!(parse_noc::<Value>("number 42").and_then(|v| v.get("number")), Ok("42".to_owned()));
    /// assert_eq!(parse_noc::<Value>("number 42").and_then(|v| v.get("number")), Ok(42));
    /// ```
    pub fn get<T>(&self, key: &str) -> Result<T, String>
    where
        T: FromValue,
    {
        self.get_value(key)
            .map_or_else(T::from_no_value, T::from_value)
    }

    /// Get a `Value` from a `Value::Table` by `key`
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use std::collections::HashMap;
    /// # use nereon::{Value, parse_noc};
    /// let v = parse_noc::<Value>("number 42").unwrap();
    /// assert_eq!(v.get_value("number"), Some(&Value::String("42".to_owned())));
    /// ```
    pub fn get_value<'a>(&'a self, key: &str) -> Option<&'a Value> {
        self.as_dict().and_then(|d| d.get(key))
    }

    /// Lookup and convert a `Value` in a tree by `keys` list
    ///
    /// The return type `T` inplements `FromValue` so this method is used
    /// to get the `Value` and convert to `T` in one call
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use std::collections::HashMap;
    /// # use nereon::{Value, parse_noc};
    /// assert_eq!(
    ///     parse_noc::<Value>("a { b { c 42 } }")
    ///         .and_then(|v| v.lookup(vec!["a", "b", "c"])),
    ///     Ok("42".to_owned()));
    /// assert_eq!(parse_noc::<Value>("a { b { c 42 } }")
    ///    .and_then(|v| v.lookup(vec!["a", "b", "c"])), Ok(42));
    /// ```
    pub fn lookup<'a, I, T>(&'a self, keys: I) -> Result<T, String>
    where
        I: IntoIterator<Item = &'a str>,
        T: FromValue,
    {
        self.lookup_value(keys)
            .map_or_else(T::from_no_value, T::from_value)
    }

    /// Lookup a `Value` in a tree by `keys` list
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use std::collections::HashMap;
    /// # use nereon::{Value, parse_noc};
    /// let v = parse_noc::<Value>("a { b { c 42 } }").unwrap();
    /// assert_eq!(
    ///     v.lookup_value(vec!["a", "b", "c"]),
    ///     Some(&Value::String("42".to_owned())));
    /// ```
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

    /// Get a reference to contained `String` from `Value`.
    ///
    /// Returns `None` if value isn't `Value::String` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// assert_eq!(Value::from("42").as_str(), Some("42"));
    /// assert_eq!(Value::List(vec![]).as_str(), None);
    /// ```
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s.as_ref()),
            _ => None,
        }
    }

    /// Test whether a `Value` is a `Value::String` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// assert_eq!(Value::from("42").is_string(), true);
    /// assert_eq!(Value::List(vec![]).is_string(), false);
    /// ```
    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
    }

    /// Get a reference to contained `HashMap` from `Value`
    ///
    /// Returns `None` if value isn't `Value::Table` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// # use std::collections::HashMap;
    /// assert_eq!(Value::from("42").as_dict(), None);
    /// assert_eq!(Value::Table(HashMap::new()).as_dict(), Some(&HashMap::new()));
    /// ```
    pub fn as_dict<'a>(&'a self) -> Option<&'a HashMap<String, Value>> {
        match self {
            Value::Table(ref map) => Some(map),
            _ => None,
        }
    }

    /// Get a mutable reference to contained `HashMap` from `Value`
    ///
    /// Returns `None` if value isn't `Value::Table` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// # use std::collections::HashMap;
    /// assert_eq!(Value::from("42").as_dict(), None);
    /// assert_eq!(Value::Table(HashMap::new()).as_dict(), Some(&HashMap::new()));
    /// ```
    pub fn as_dict_mut<'a>(&'a mut self) -> Option<&'a mut HashMap<String, Value>> {
        match self {
            Value::Table(ref mut map) => Some(map),
            _ => None,
        }
    }

    /// Test whether a `Value` is a `Value::Table` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// # use std::collections::HashMap;
    /// assert_eq!(Value::Table(HashMap::new()).is_dict(), true);
    /// assert_eq!(Value::from("42").is_dict(), false);
    /// ```
    pub fn is_dict(&self) -> bool {
        match self {
            Value::Table(_) => true,
            _ => false,
        }
    }

    /// Get a reference to contained `Vec` from `Value`
    ///
    /// Returns `None` if value isn't `Value::List` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// assert_eq!(Value::List(vec![]).as_list(), Some(&vec![]));
    /// assert_eq!(Value::from("42").as_list(), None);
    /// ```
    pub fn as_list<'a>(&'a self) -> Option<&'a Vec<Value>> {
        match self {
            Value::List(ref vec) => Some(vec),
            _ => None,
        }
    }

    /// Get a mutable reference to contained `Vec` from `Value`
    ///
    /// Returns `None` if value isn't `Value::List` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// assert_eq!(Value::List(vec![]).as_list(), Some(&vec![]));
    /// assert_eq!(Value::from("42").as_list(), None);
    /// ```
    pub fn as_list_mut<'a>(&'a mut self) -> Option<&'a mut Vec<Value>> {
        match self {
            Value::List(ref mut vec) => Some(vec),
            _ => None,
        }
    }

    /// Test whether a `Value` is a `Value::List` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::Value;
    /// assert_eq!(Value::List(vec![]).is_list(), true);
    /// assert_eq!(Value::from("42").is_list(), false);
    /// ```
    pub fn is_list(&self) -> bool {
        match self {
            Value::List(_) => true,
            _ => false,
        }
    }

    /// Convert a `Value` into a NOC `String`
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{parse_noc, Value};
    /// assert_eq!(parse_noc::<Value>(r#"
    ///     forenames [John, Reginald]
    ///     surname Doe
    /// "#).map(|v| v.as_noc_string()),
    ///     Ok(r#""forenames" ["John","Reginald"],"surname" "Doe""#.to_owned()));
    /// ```
    pub fn as_noc_string(&self) -> String {
        match self {
            Value::String(s) => format!("\"{}\"", s),
            Value::List(v) => {
                let values = v
                    .iter()
                    .map(|v| match v {
                        Value::Table(_) => format!("{{{}}}", v.as_noc_string()),
                        Value::List(_) => format!("[{}]", v.as_noc_string()),
                        Value::String(_) => v.as_noc_string(),
                    }).collect::<Vec<_>>();
                values.join(",")
            }
            Value::Table(m) => {
                let values = BTreeMap::from_iter(m.iter())
                    .iter()
                    .map(|(k, v)| match v {
                        Value::Table(_) => format!("\"{}\" {{{}}}", k, v.as_noc_string()),
                        Value::List(_) => format!("\"{}\" [{}]", k, v.as_noc_string()),
                        Value::String(_) => format!("\"{}\" {}", k, v.as_noc_string()),
                    }).collect::<Vec<_>>();
                values.join(",")
            }
        }
    }

    /// Convert a `Value` into a pretty NOC `String`
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{parse_noc, Value};
    /// assert_eq!(parse_noc::<Value>("forenames [John, Reginald], surname Doe")
    ///         .map(|v| v.as_noc_string_pretty()),
    ///     Ok("\"forenames\" [\n\t\"John\"\n\t\"Reginald\"\n]\n\"surname\" \"Doe\"".to_owned()));
    /// ```
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
                        Value::Table(_) => format!("{}{{\n{}\n{}}}", tabs, s, tabs),
                        Value::List(_) => format!("{}[\n{}\n{}]", tabs, s, tabs),
                        Value::String(_) => format!("{}{}", tabs, s),
                    }
                }).collect::<Vec<_>>()
                .join("\n"),
            Value::Table(m) => BTreeMap::from_iter(m.iter()) // sort
                .iter()
                .map(|(k, v)| {
                    let s = v.as_s_indent(indent + 1);
                    match v {
                        Value::Table(_) => format!("{}\"{}\" {{\n{}\n{}}}", tabs, k, s, tabs),
                        Value::List(_) => format!("{}\"{}\" [\n{}\n{}]", tabs, k, s, tabs),
                        Value::String(_) => format!("{}\"{}\" {}", tabs, k, s),
                    }
                })
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }
}

/// Trait for conversion from `Value` into arbitrary types.
///
/// This trait is used by the [`get`](enum.Value.html#method.get) and
/// [`lookup`](enum.Value.html#method.lookup)
/// [`Value`](enum.Value.html) methods.
///
/// Implementations exist for several standard types.
///
/// Implemnataions can be automatically derived by using the
/// [`nereon_derive`](../nereon_derive/index.html) crate.
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

impl FromValue for Value {
    fn from_value(value: &Value) -> Result<Self, String> {
        Ok(match value {
            Value::String(s) => Value::String(s.to_owned()),
            Value::List(v) => {
                Value::List(v.iter().map(|v| Value::from_value(&v).unwrap()).collect())
            }
            Value::Table(m) => Value::Table(
                m.iter()
                    .map(|(k, v)| (k.to_owned(), Value::from_value(&v).unwrap()))
                    .collect(),
            ),
        })
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
    use super::super::parse_noc;
    use super::{FromValue, Value};
    use std::collections::HashMap;

    #[test]
    fn test_value_from() {
        assert_eq!(
            Value::from(HashMap::<String, Value>::new()),
            Value::Table(HashMap::new())
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
        v = v.insert(vec!["a"], "a");
        assert_eq!(v.as_noc_string(), r#""a" "a""#);
        v = v.insert(vec!["b"], "b");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b""#);
        v = v.insert(vec!["c", "c", "c"], "c");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b","c" {"c" {"c" "c"}}"#);
        v = v.insert(vec!["c"], "c");
        assert_eq!(v.as_noc_string(), r#""a" "a","b" "b","c" "c""#);
    }

    #[test]
    fn test_value_get() {
        let value = parse_noc::<Value>(r#"a a, b 1, c c, e {}, f []"#).unwrap();
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
        let value = parse_noc::<Value>(r#"a a, b b, c c, e {}, f []"#).unwrap();
        assert_eq!(value.get_value("a"), Some(&Value::String("a".to_owned())));
        assert_eq!(value.get_value("b"), Some(&Value::String("b".to_owned())));
        assert_eq!(value.get_value("c"), Some(&Value::String("c".to_owned())));
        assert_eq!(value.get_value("d"), None);
        assert_eq!(value.get_value("f"), Some(&Value::List(Vec::new())));
        assert_eq!(value.get_value("e"), Some(&Value::Table(HashMap::new())));
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
        let v = parse_noc::<Value>(r#"a a, b b, c c, e {a a, b b}, f [a,b,c,d]"#).unwrap();
        let noc = v.as_noc_string();
        assert_eq!(v, parse_noc::<Value>(&noc).unwrap());
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
