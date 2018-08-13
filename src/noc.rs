use std::collections::HashMap;
use std::io;
use std::iter::{FromIterator, Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Value {
    String(String),
    Dict(HashMap<String, Value>),
    Array(Vec<Value>),
}

impl Value {
    fn insert(&mut self, mut keys: Vec<String>, mut value: Value) {
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
                format!("[{}]", values.join(","))
            }
            Value::Dict(m) => {
                let values = m
                    .iter()
                    .map(|(k, v)| format!("\"{}\" {}", k, v.as_noc_string()))
                    .collect::<Vec<_>>();
                format!("{{{}}}", values.join(","))
            }
        }
    }
}

struct Parser<'a> {
    src: &'a mut Peekable<Chars<'a>>,
    row: u32,
    clm: u32,
    templates: Vec<(String, Value)>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unexpected(char),
    NoKey,
    HasKey,
    Missing(char),
}

#[derive(Debug, PartialEq)]
pub struct Error(u32, u32, ErrorKind);

pub fn from_str(s: &str) -> Result<Value, Error> {
    Parser {
        src: &mut s.chars().peekable(),
        row: 0,
        clm: 0,
        templates: Vec::new(),
    }.parse()
}

pub fn from_read(s: &mut io::Read) -> io::Result<Result<Value, Error>> {
    let mut buffer = String::new();
    s.read_to_string(&mut buffer)?;
    Ok(from_str(&buffer))
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Value, Error> {
        let result = self.parse_dict();
        match self.src.peek() {
            None => result,
            Some(c) => Err(Error(self.row, self.clm, ErrorKind::Unexpected(*c))),
        }
    }

    fn parse_dict(&mut self) -> Result<Value, Error> {
        let mut result = Value::Dict(HashMap::new());
        loop {
            match self.parse_item() {
                Ok(Some((keys, value))) => {
                    if keys.is_empty() {
                        return Err(Error(self.row, self.clm, ErrorKind::NoKey));
                    }
                    result.insert(keys, value);
                }
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }

    fn parse_array(&mut self) -> Result<Value, Error> {
        let mut result = Vec::new();
        loop {
            match self.parse_item() {
                Ok(Some((keys, value))) => {
                    if ! keys.is_empty() {
                        return Err(Error(self.row, self.clm, ErrorKind::HasKey));
                    }
                    result.push(value);
                }
                Ok(None) => break,
                Err(e) => return Err(e),
            }
        }
        Ok(Value::Array(result))
    }

    fn parse_item(&mut self) -> Result<Option<(Vec<String>, Value)>, Error> {
        // skip empty items
        loop {
            match self.skip_space() {
                Some(c) if c == ']' || c == '}' || c == ')' => return Ok(None),
                Some(c) if c == ',' || c == '\n' => {
                    self.skip();
                    continue;
                }
                None => return Ok(None),
                _ => break,
            }
        }

        let mut values = Vec::new();
        loop {
            match self.parse_value() {
                Ok(Some(value @ Value::String(_))) => values.push(value),
                Ok(Some(value @ Value::Dict(_))) => {
                    values.push(value);
                    break;
                }
                Ok(Some(value @ Value::Array(_))) => {
                    values.push(value);
                    break;
                }
                Ok(None) => break,
                Err(e) => return Err(e),
            }
            if let Some(c) = self.skip_space() {
                if c == ',' || c == '\n' {
                    self.skip();
                    break;
                }
            }
        }

        // skip item delimiter if any
        if let Some(c) = self.peek() {
            if c == ',' || c == '\n' {
                self.skip();
            }
        }

        if values.is_empty() {
            return Ok(None);
        }

        let value = values.pop().unwrap();
        let values = values
            .iter()
            .map(|v| v.as_string().unwrap().to_owned())
            .collect();

        Ok(Some((values, value)))
    }

    fn parse_value(&mut self) -> Result<Option<Value>, Error> {
        match self.skip_space() {
            None => Ok(None),
            Some('{') => {
                self.skip();
                match self.parse_dict() {
                    Ok(value) => match self.skip_space() {
                        Some('}') => {
                            self.skip();
                            Ok(Some(value))
                        }
                        _ => Err(Error(self.row, self.clm, ErrorKind::Missing('}'))),
                    },
                    Err(e) => Err(e),
                }
            }
            Some('[') => {
                self.skip();
                match self.parse_array() {
                    Ok(value) => match self.skip_space() {
                        Some(']') => {
                            self.skip();
                            Ok(Some(value))
                        }
                        _ => Err(Error(self.row, self.clm, ErrorKind::Missing(']'))),
                    },
                    Err(e) => Err(e),
                }
            }
            _ => match self.parse_expression() {
                Ok(v) => Ok(Some(v)),
                Err(e) => Err(e),
            },
        }
    }

    fn parse_expression(&mut self) -> Result<Value, Error> {
        match self.peek() {
            Some(c) if c == '\"' => {
                self.skip();
                let value = self.parse_quoted_string();
                match self.peek() {
                    Some('\"') => {
                        self.skip();
                        Ok(value)
                    }
                    _ => Err(Error(self.row, self.clm, ErrorKind::Missing('\"'))),
                }
            }
            _ => self.parse_bare_string(),
        }
    }

    fn parse_bare_string(&mut self) -> Result<Value, Error> {
        let mut result = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) if c == ' ' || c == '\t' || c == ',' || c == '\n' => break,
                Some(c) if c >= 'a' && c <= 'z' => result.push(c),
                Some(c) if c >= 'A' && c <= 'Z' => result.push(c),
                Some(c) if c >= '0' && c <= '9' => result.push(c),
                Some(c) if c == '_' && c <= 'z' => result.push(c),
                Some(c) => return Err(Error(self.row, self.clm, ErrorKind::Unexpected(c))),
            }
            self.skip();
        }
        Ok(Value::String(result))
    }

    fn parse_quoted_string(&mut self) -> Value {
        let escapes: HashMap<char, char> = HashMap::from_iter(vec![
            ('a', 0x07 as char),
            ('b', 0x08 as char),
            ('f', 0x0c as char),
            ('n', '\n'),
            ('r', '\r'),
            ('t', '\t'),
            ('v', 0x0b as char),
            ('\\', '\\'),
            ('\'', '\''),
            ('\"', '\"'),
            ('?', '?'),
        ]);
        let mut result = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) if c == '\"' => break,
                Some(c) if c == '\\' => {
                    self.skip();
                    match self.peek() {
                        Some(c) if escapes.contains_key(&c) => {
                            result.push(escapes[&c])
                        }
                        Some(c) => {
                            result.push('\\');
                            result.push(c);
                        }
                        None => break,
                    }
                }
                Some(c) => {
                    result.push(c);
                    self.skip();
                }
            }
        }
        Value::String(result)
    }

    fn skip_space(&mut self) -> Option<char> {
        loop {
            match self.peek() {
                Some(c) if c == ' ' || c == '\t' => self.skip(),
                x => return x,
            };
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.src.peek().cloned()
    }

    fn skip(&mut self) {
        match self.src.next() {
            Some('\n') => {
                self.row += 1;
                self.clm = 0;
            }
            Some(_) => self.clm += 1,
            _ => (),
        }
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn test_parse() {
        use super::{from_str, Error, ErrorKind, Value};
        use std::collections::HashMap;
        use std::iter::FromIterator;

        assert_eq!(from_str("").unwrap(), Value::Dict(HashMap::new()));
        assert_eq!(from_str("fail"), Err(Error(0, 4, ErrorKind::NoKey)));
        assert_eq!(
            from_str("key value").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
        assert_eq!(
            from_str(",,,,key value,,,,").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
        assert_eq!(
            from_str("key value\nkey value1").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value1".to_owned()),
            )]))
        );
        assert_eq!(
            from_str("key value\nkey2 value2").unwrap(),
            Value::Dict(HashMap::from_iter(vec![
                ("key".to_owned(), Value::String("value".to_owned())),
                ("key2".to_owned(), Value::String("value2".to_owned())),
            ]))
        );
        let a = r#""key1" "value1""#;
        assert_eq!(from_str(a).unwrap().as_noc_string(), format!("{{{}}}", a));
    }
}
