use std::collections::HashMap;
use std::io;
use std::iter::{FromIterator, Peekable};
use std::str::Chars;

mod eval;

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
    templates: HashMap<String, String>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unexpected(char),
    NoKey,
    BadKey,
    HasKey,
    Missing(char),
    EOF,
    BadTemplate,
    UnknownEval(String),
    Eval(String, String),
}

#[derive(Debug, PartialEq)]
pub struct Error(u32, u32, ErrorKind);

pub fn from_str(s: &str) -> Result<Value, Error> {
    Parser {
        src: &mut s.chars().peekable(),
        row: 0,
        clm: 0,
        templates: HashMap::new(),
    }.parse()
}

pub fn from_read(s: &mut io::Read) -> io::Result<Result<Value, Error>> {
    let mut buffer = String::new();
    s.read_to_string(&mut buffer)?;
    Ok(from_str(&buffer))
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) -> Result<Value, Error> {
        match self.parse_dict(&[]) {
            ok @ Ok(_) => match self.peek() {
                None => ok,
                Some(c) => Err(self.error(ErrorKind::Unexpected(c))),
            },
            err => err,
        }
    }

    fn parse_dict(&mut self, args: &[Value]) -> Result<Value, Error> {
        let mut result = Value::Dict(HashMap::new());
        loop {
            match self.parse_values(args) {
                Ok(mut values) => {
                    if values.is_empty() {
                        if self.at_eof() || self.peek().unwrap() == '}' {
                            break;
                        }
                        continue;
                    }
                    let value = values.pop().unwrap();
                    if values.is_empty() {
                        return Err(self.error(ErrorKind::NoKey));
                    }
                    if !values.iter().all(|v| v.is_string()) {
                        return Err(self.error(ErrorKind::BadKey));
                    }
                    result.insert(values.drain(..).map(|v| v.into_string()).collect(), value);
                }
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }

    fn parse_array(&mut self, args: &[Value]) -> Result<Value, Error> {
        let mut result = Vec::new();
        loop {
            match self.parse_values(args) {
                Ok(values) => {
                    if values.is_empty() {
                        if self.at_eof() || "])".contains(self.peek().unwrap()) {
                            break;
                        }
                        continue;
                    }
                    for value in values {
                        result.push(value);
                    }
                }
                Err(e) => return Err(e),
            }
        }
        Ok(Value::Array(result))
    }

    fn parse_values(&mut self, args: &[Value]) -> Result<Vec<Value>, Error> {
        let mut values = Vec::new();

        loop {
            if let Some(c) = self.skip_space() {
                if c == ',' || c == '\n' {
                    self.skip();
                    break;
                }
            }
            match self.parse_value(args) {
                Ok(Some(value)) => values.push(value),
                Ok(None) => {
                    if self.at_eof() {
                        break;
                    }
                }
                Err(e) => return Err(e),
            }
        }
        Ok(values)
    }

    fn parse_value(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.skip_space() {
            None => Ok(None),
            Some('{') => {
                self.skip();
                match self.parse_dict(args) {
                    Ok(value) => match self.skip_space() {
                        Some('}') => {
                            self.skip();
                            Ok(Some(value))
                        }
                        _ => Err(self.error(ErrorKind::Missing('}'))),
                    },
                    Err(e) => Err(e),
                }
            }
            Some('[') => {
                self.skip();
                match self.parse_array(args) {
                    Ok(value) => match self.skip_space() {
                        Some(']') => {
                            self.skip();
                            Ok(Some(value))
                        }
                        _ => Err(self.error(ErrorKind::Missing(']'))),
                    },
                    Err(e) => Err(e),
                }
            }
            _ => self.parse_expression(args),
        }
    }

    fn parse_expression(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.peek() {
            Some(c) if c == '\"' => {
                self.skip();
                let value = self.parse_quoted_string();
                match self.peek() {
                    Some('\"') => {
                        self.skip();
                        Ok(Some(value))
                    }
                    _ => Err(self.error(ErrorKind::Missing('\"'))),
                }
            }
            _ => self.parse_bare_string(args),
        }
    }

    fn parse_bare_string(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        let mut result = String::new();
        loop {
            match self.peek() {
                None => break,
                Some(c) if c == ' ' || c == '\t' || c == ',' || c == '\n' => break,
                Some(c) if c == '}' || c == ']' || c == ')' => break,
                Some(c) if c >= 'a' && c <= 'z' => result.push(c),
                Some(c) if c >= 'A' && c <= 'Z' => result.push(c),
                Some(c) if c >= '0' && c <= '9' => result.push(c),
                Some(c) if c == '_' => result.push(c),
                Some(c) if c == '(' => return self.parse_macro(&result, args),
                Some(c) => return Err(self.error(ErrorKind::Unexpected(c))),
            }
            self.skip();
        }
        Ok(Some(Value::String(result)))
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
                        Some(c) if escapes.contains_key(&c) => result.push(escapes[&c]),
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

    fn parse_macro(&mut self, name: &str, args: &[Value]) -> Result<Option<Value>, Error> {
        self.skip();
        if name == "let" {
            let id;
            loop {
                match self.parse_values(args) {
                    Ok(mut values) => match values.len() {
                        0 => {
                            if let Some(c) = self.peek() {
                                if c == ']' || c == '}' || c == ')' {
                                    return Err(self.error(ErrorKind::Unexpected(c)));
                                }
                            } else {
                                return Err(self.error(ErrorKind::EOF));
                            }
                        }
                        1 => {
                            if values[0].is_string() {
                                id = values.remove(0).into_string();
                                break;
                            } else {
                                return Err(self.error(ErrorKind::BadTemplate));
                            }
                        }
                        _ => return Err(self.error(ErrorKind::BadTemplate)),
                    },
                    Err(e) => return Err(e),
                }
            }

            let template = match self.read_template() {
                Ok(t) => t,
                Err(e) => return Err(e),
            };

            self.templates.insert(id, template);
            Ok(None)
        } else {
            match self.parse_array(args) {
                Ok(args) => match self.peek() {
                    Some(c) if c == ')' => match self.evaluate(name, args.as_array().unwrap()) {
                        Ok(Some(value)) => {
                            self.skip();
                            Ok(Some(value))
                        }
                        Ok(None) => {
                            self.skip();
                            Ok(None)
                        }
                        Err(e) => Err(e),
                    },
                    _ => Err(self.error(ErrorKind::Missing(')'))),
                },
                Err(e) => Err(e),
            }
        }
    }

    fn read_template(&mut self) -> Result<String, Error> {
        // read next value and return as a string
        let mut braces = Vec::new();
        let mut result = String::new();
        loop {
            if self.at_eof() {
                return Err(self.error(ErrorKind::EOF));
            }
            let c = self.peek().unwrap();
            if c == ')' && braces.is_empty() {
                break;
            }
            self.skip();
            if (c == ',' || c == '\n') && braces.is_empty() {
                break;
            }
            result.push(c);
            if c == '\"' {
                while !self.at_eof() {
                    let c = self.get().unwrap();
                    result.push(c);
                    if c == '\"' {
                        break;
                    }
                    if c == '\\' && !self.at_eof() {
                        result.push(self.get().unwrap());
                    }
                }
                continue;
            }
            if c == '{' || c == '[' || c == '(' {
                braces.push(c);
            }
            if (c == '}' || c == ']' || c == ')') && braces.pop() != Some(c) {
                return Err(self.error(ErrorKind::Unexpected(c)));
            }
        }
        Ok(result)
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
        let _ = self.get();
    }

    fn get(&mut self) -> Option<char> {
        let result = self.src.next();
        if let Some(c) = result {
            if c == '\n' {
                self.row += 1;
                self.clm = 0;
            } else {
                self.clm += 1;
            }
        }
        result
    }

    fn at_eof(&mut self) -> bool {
        self.peek() == None
    }

    fn evaluate(&mut self, name: &str, args: &[Value]) -> Result<Option<Value>, Error> {
        match name {
            "let" => Ok(None),
            _ => match eval::evaluate(name, args) {
                Ok(value) => Ok(Some(value)),
                Err(e) => Err(self.error(e)),
            },
        }
    }

    fn error(&self, error: ErrorKind) -> Error {
        Error(self.row, self.clm, error)
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
