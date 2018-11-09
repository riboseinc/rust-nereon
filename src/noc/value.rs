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

use noc::ConversionError;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::hash::Hash;
use std::iter::{self, FromIterator};
use std::slice;
use std::vec::Drain;

/// Main `Value` enum with variants for strings, tables, and lists.
///
/// There is currently no constructor for Value instances though
/// they can be created from `str`, `String`, `Vec` and `HashMap` instances
///
/// ```
/// # extern crate nereon;
/// # use std::collections::HashMap;
/// # use nereon::{Table, Value};
/// assert_eq!(Value::String("42".to_owned()), Value::from("42"));
/// assert_eq!(Value::Table(Table::new()), Value::from(HashMap::<String, Value>::new()));
/// assert_eq!(Value::List(vec![]), Value::from(Vec::<Value>::new()));
/// ```

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Table {
    inner: Vec<(String, Value)>,
}

impl Table {
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }
    pub fn remove(&mut self, k: &str) -> Option<Value> {
        self.inner
            .iter()
            .position(|(kk, _)| kk == k)
            .map(|pos| self.inner.remove(pos).1)
    }
    pub fn insert(&mut self, k: String, v: Value) -> Option<Value> {
        let ret = self.remove(&k);
        self.inner.push((k, v));
        ret
    }
    pub fn drain(&mut self) -> Drain<(String, Value)> {
        self.inner.drain(..)
    }
    pub fn get(&self, k: &str) -> Option<&Value> {
        self.inner.iter().find(|(kk, _)| kk == k).map(|(_, v)| v)
    }
    pub fn iter(&self) -> slice::Iter<(String, Value)> {
        self.inner.iter()
    }
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }
}

impl FromIterator<(String, Value)> for Table {
    fn from_iter<I>(it: I) -> Self
    where
        I: IntoIterator<Item = (String, Value)>,
    {
        Self {
            inner: Vec::from_iter(it),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Table(Table),
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
    /// # use nereon::{Table, Value, parse_noc};
    /// let mut v = Value::Table(Table::new());
    /// assert_eq!(v, parse_noc::<Value>("").unwrap());
    /// v = v.insert(vec!["forename"], Value::from("John"));
    /// assert_eq!(v, parse_noc::<Value>("forename John").unwrap());
    /// v = v.insert(vec!["surname"], Value::from("Doe"));
    /// assert_eq!(v, parse_noc::<Value>("forename John, surname Doe").unwrap());
    /// v = v.insert(vec!["forename"], Value::from(vec!["John", "Reginald"]));
    /// assert_eq!(v, parse_noc::<Value>("surname Doe, forename [John, Reginald]").unwrap());
    /// v = v.insert(vec!["forename", "first"], Value::from("John"));
    /// assert_eq!(v, parse_noc::<Value>("surname Doe, forename { first John }").unwrap());
    /// v = v.insert(vec!["forename", "middle"], Value::from("Reginald"));
    /// assert_eq!(v, parse_noc::<Value>(
    ///     "surname Doe, forename { first John, middle Reginald }").unwrap());
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
                let old_value = map.remove(key).filter(|v| v.is_table());
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
                        let mut node = old_value.unwrap_or_else(|| Value::Table(Table::new()));
                        node.insert(keys.collect::<Vec<_>>(), value)
                    },
                );
                Value::Table(map)
            } else {
                unreachable!()
            }
        }
    }

    /// Convert a `Value` into any type implementing FromValue
    ///
    /// This function takes an `Option<Value>`. With some types, such
    /// as Option<T>, `None` may be converted successfully.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// use nereon::Value;
    /// let v = Value::from("42");
    /// assert_eq!(Value::convert(Some(v)), Ok(42));
    /// ```
    pub fn convert<T: FromValue>(v: Option<Value>) -> Result<T, ConversionError> {
        v.map_or_else(T::from_no_value, T::from_value)
    }

    /// Get a `Value` from a `Value::Table` by `key`
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{Value, parse_noc};
    /// let v = parse_noc::<Value>("number 42").unwrap();
    /// assert_eq!(v.get("number"), Some(&Value::String("42".to_owned())));
    /// ```
    pub fn get<'a>(&'a self, key: &str) -> Option<&'a Value> {
        self.as_table().and_then(|d| d.get(key))
    }

    /// Lookup a `Value` in a tree by `keys` list
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{Value, parse_noc};
    /// let v = parse_noc::<Value>("a { b { c 42 } }").unwrap();
    /// assert_eq!(
    ///     v.lookup(vec!["a", "b", "c"]),
    ///     Some(&Value::String("42".to_owned())));
    /// ```
    pub fn lookup<'a, I>(&'a self, keys: I) -> Option<&'a Value>
    where
        I: IntoIterator<Item = &'a str>,
    {
        let keys = keys.into_iter().collect::<Vec<_>>();
        if keys.is_empty() {
            Some(self)
        } else {
            self.get(keys[0]).and_then(|v| v.lookup(keys[1..].to_vec()))
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

    /// Get a reference to contained `Table` from `Value`
    ///
    /// Returns `None` if value isn't `Value::Table` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{Table, Value};
    /// assert_eq!(Value::from("42").as_table(), None);
    /// assert_eq!(Value::Table(Table::new()).as_table(), Some(&Table::new().into()));
    /// ```
    pub fn as_table(&self) -> Option<&Table> {
        match self {
            Value::Table(ref map) => Some(map),
            _ => None,
        }
    }

    /// Get a mutable reference to contained `Table` from `Value`
    ///
    /// Returns `None` if value isn't `Value::Table` variant.
    ///
    /// # Example
    /// ```
    /// # extern crate nereon;
    /// # use nereon::{Table, Value};
    /// assert_eq!(Value::from("42").as_table(), None);
    /// assert_eq!(Value::Table(Table::new()).as_table(), Some(&Table::new()));
    /// ```
    pub fn as_table_mut(&mut self) -> Option<&mut Table> {
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
    /// # use nereon::{Table, Value};
    /// assert_eq!(Value::Table(Table::new()).is_table(), true);
    /// assert_eq!(Value::from("42").is_table(), false);
    /// ```
    pub fn is_table(&self) -> bool {
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
                let values = m
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
            Value::Table(m) => m
                .iter()
                .map(|(k, v)| {
                    let s = v.as_s_indent(indent + 1);
                    match v {
                        Value::Table(_) => format!("{}\"{}\" {{\n{}\n{}}}", tabs, k, s, tabs),
                        Value::List(_) => format!("{}\"{}\" [\n{}\n{}]", tabs, k, s, tabs),
                        Value::String(_) => format!("{}\"{}\" {}", tabs, k, s),
                    }
                }).collect::<Vec<_>>()
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
    fn from_value(value: Value) -> Result<OK, ConversionError>;
    // this is a kludge so missing Values can be converted
    // into None
    fn from_no_value() -> Result<OK, ConversionError> {
        Err(ConversionError::new("value", "nothing"))
    }
}

macro_rules! from_value_for {
    ($type:ident) => {
        impl FromValue for $type {
            fn from_value(value: Value) -> Result<Self, ConversionError> {
                match value {
                    Value::String(s) => s
                        .parse()
                        .map_err(|_| ConversionError::new(stringify!($type), "string")),
                    Value::List(_) => Err(ConversionError::new("string", "list")),
                    Value::Table(_) => Err(ConversionError::new("string", "table")),
                }
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
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        Ok(value)
    }
}

impl FromValue for String {
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        match value {
            Value::String(s) => Ok(s),
            Value::Table(_) => Err(ConversionError::new("string", "table")),
            Value::List(_) => Err(ConversionError::new("string", "list")),
        }
    }
}

impl<T> FromValue for Option<T>
where
    T: FromValue,
{
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        Ok(Some(T::from_value(value)?))
    }
    fn from_no_value() -> Result<Self, ConversionError> {
        Ok(None)
    }
}

impl<T, S: ::std::hash::BuildHasher + Default> FromValue for HashMap<String, T, S>
where
    T: FromValue,
{
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        match value {
            Value::Table(mut d) => d.drain().try_fold(HashMap::default(), |mut m, (k, v)| {
                T::from_value(v).map(|v| {
                    m.insert(k, v);
                    m
                })
            }),
            Value::String(_) => Err(ConversionError::new("table", "string")),
            Value::List(_) => Err(ConversionError::new("table", "list")),
        }
    }

    fn from_no_value() -> Result<Self, ConversionError> {
        Ok(HashMap::default())
    }
}

impl<T> FromValue for Vec<T>
where
    T: FromValue,
{
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        match value {
            Value::List(mut l) => l.drain(..).try_fold(Vec::new(), |mut m, v| {
                T::from_value(v).map(|v| {
                    m.push(v);
                    m
                })
            }),
            Value::String(_) => Err(ConversionError::new("list", "string")),
            Value::Table(_) => Err(ConversionError::new("list", "table")),
        }
    }
    fn from_no_value() -> Result<Self, ConversionError> {
        Ok(Vec::default())
    }
}

impl<T> FromValue for Vec<(String, T)>
where
    T: FromValue,
{
    fn from_value(value: Value) -> Result<Self, ConversionError> {
        match value {
            Value::Table(mut l) => l.drain().try_fold(Vec::new(), |mut m, (k, v)| {
                T::from_value(v).map(|v| {
                    m.push((k, v));
                    m
                })
            }),
            Value::String(_) => Err(ConversionError::new("table", "string")),
            Value::List(_) => Err(ConversionError::new("table", "list")),
        }
    }
    fn from_no_value() -> Result<Self, ConversionError> {
        Ok(Vec::default())
    }
}

#[cfg(test)]
mod tests {
    use super::super::parse_noc;
    use super::{FromValue, Table, Value};
    use std::collections::HashMap;

    #[test]
    fn test_value_from() {
        assert_eq!(
            Value::from(HashMap::<String, Value>::new()),
            Value::Table(Table::new())
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
        let value = parse_noc::<Value>(r#"a a, b b, c c, e {}, f []"#).unwrap();
        assert_eq!(value.get("a"), Some(&Value::String("a".to_owned())));
        assert_eq!(value.get("b"), Some(&Value::String("b".to_owned())));
        assert_eq!(value.get("c"), Some(&Value::String("c".to_owned())));
        assert_eq!(value.get("d"), None);
        assert_eq!(value.get("f"), Some(&Value::List(Vec::new())));
        assert_eq!(value.get("e"), Some(&Value::Table(Table::new())));
    }

    #[test]
    fn test_value_lookup() {
        let value = parse_noc::<Value>(r#"a {a a, b b, c c, e {}, f []}"#).unwrap();
        assert_eq!(
            value.lookup(vec!["a", "a"]),
            Some(&Value::String("a".to_owned()))
        );
        assert_eq!(
            value.lookup(vec!["a", "b"]),
            Some(&Value::String("b".to_owned()))
        );
        assert_eq!(
            value.lookup(vec!["a", "c"]),
            Some(&Value::String("c".to_owned()))
        );
        assert_eq!(value.lookup(vec!["a", "d"]), None);
        assert_eq!(value.lookup(vec!["a", "f"]), Some(&Value::List(Vec::new())));
        assert_eq!(
            value.lookup(vec!["a", "e"]),
            Some(&Value::Table(Table::new()))
        );
    }

    #[test]
    fn test_value_is_as_into() {
        let v = Value::from("a");
        assert_eq!(v.as_str(), Some("a"));
        assert_eq!(v.as_table(), None);
        assert_eq!(v.is_string(), true);
        assert_eq!(v.is_table(), false);
        let v = Value::from(vec![""]);
        assert_eq!(v.as_str(), None);
        assert_eq!(v.as_list(), Some(&vec![Value::from("")]));
        assert_eq!(v.is_string(), false);
        assert_eq!(v.is_list(), true);
        let v = Value::from(HashMap::<String, Value>::new());
        assert_eq!(v.as_list(), None);
        assert_eq!(v.as_table(), Some(&Table::new()));
        assert_eq!(v.is_list(), false);
        assert_eq!(v.is_table(), true);
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
            String::from_value(Value::from("hello")),
            Ok("hello".to_owned())
        );
        assert_eq!(
            Vec::<Value>::from_value(Value::from(vec!["a", "b"])),
            Ok(vec!["a".into(), "b".into()])
        );
        assert_eq!(
            HashMap::from_value(Value::from(HashMap::<&str, &str>::new())),
            Ok(HashMap::<String, String>::new())
        );
        assert_eq!(
            Vec::<(String, Value)>::from_value(Value::Table(Table::new()).insert(vec!["a"], "a")),
            Ok(vec![("a".to_owned(), "a".into())])
        );
    }
}
