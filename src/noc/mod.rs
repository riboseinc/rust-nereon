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

use std::collections::{HashMap, VecDeque};
use std::io;
use std::iter::{FromIterator, Peekable};
use std::str::Chars;

mod eval;
mod value;

use self::value::Value;

struct Parser<'a> {
    src: &'a mut Peekable<Chars<'a>>,
    row: u32,
    clm: u32,
    templates: Option<HashMap<String, String>>,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unexpected(char),
    BadKey,
    Missing(char),
    EOF,
    BadTemplate,
    UnknownEval(String),
    Eval(String, String),
    Temp,
}

#[derive(Debug, PartialEq)]
pub struct Error(u32, u32, ErrorKind);

impl Error {
    pub fn row(&self) -> u32 {
        let Error(row, _, _) = self;
        *row
    }

    pub fn clm(&self) -> u32 {
        let Error(_, clm, _) = self;
        *clm
    }

    pub fn kind(&self) -> &ErrorKind {
        let Error(_, _, kind) = self;
        kind
    }
}

pub fn from_str(s: &str) -> Result<Value, Error> {
    Parser {
        src: &mut s.chars().peekable(),
        row: 0,
        clm: 0,
        templates: Some(HashMap::new()),
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
            match self.parse_keyed_value(args) {
                Ok(mut values) => {
                    if !values.is_empty() {
                        let value = values.pop_back().unwrap();
                        if values.is_empty() {
                            return Err(self.error(ErrorKind::BadKey));
                        }
                        if !values.iter().all(|v| v.is_string()) {
                            return Err(self.error(ErrorKind::BadKey));
                        }
                        result.insert(values.drain(..).map(|v| v.into_string()).collect(), value);
                    }
                    match self.peek() {
                        Some(c) if is_sep(c) => self.skip(),
                        Some(c) if is_close(c) => break,
                        Some(_) => (),
                        None => break,
                    }
                }
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }

    fn parse_keyed_value(&mut self, args: &[Value]) -> Result<VecDeque<Value>, Error> {
        match self.parse_value(args) {
            Ok(None) => match self.peek() {
                Some(c) if is_sep(c) => {
                    self.skip();
                    Ok(VecDeque::new())
                }
                Some(c) if is_close(c) => Ok(VecDeque::new()),
                None => Ok(VecDeque::new()),
                _ => self.parse_keyed_value(args),
            },
            Ok(Some(value)) => match self.parse_keyed_value(args) {
                Ok(mut values) => {
                    values.push_front(value);
                    Ok(values)
                }
                e => e,
            },
            Err(e) => Err(e),
        }
    }

    fn parse_value(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.skip_space() {
            Some(c) if is_close(c) || is_sep(c) => Ok(None),
            None => Ok(None),
            Some('{') => {
                self.skip();
                match self.parse_dict(args) {
                    Ok(value) => match self.peek() {
                        Some('}') => {
                            self.skip();
                            self.skip_space();
                            Ok(Some(value))
                        }
                        Some(c) => Err(self.error(ErrorKind::Unexpected(c))),
                        None => Err(self.error(ErrorKind::Missing('}'))),
                    },
                    Err(e) => Err(e),
                }
            }
            Some('[') => {
                self.skip();
                match self.parse_list(args) {
                    Ok(mut values) => match self.peek() {
                        Some(']') => {
                            self.skip();
                            self.skip_space();
                            Ok(Some(Value::Array(Vec::from_iter(values.drain(..)))))
                        }
                        Some(c) => Err(self.error(ErrorKind::Unexpected(c))),
                        None => Err(self.error(ErrorKind::EOF)),
                    },
                    Err(e) => Err(e),
                }
            }
            _ => {
                let result = self.parse_expression(args);
                if result.is_ok() {
                    self.skip_space();
                }
                result
            }
        }
    }

    fn parse_expression(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.peek() {
            Some(c) if c == '\"' => {
                self.skip();
                let value = self.parse_quoted_string();
                match self.get() {
                    // Some('\"') or None
                    Some(_) => Ok(Some(value)),
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
                Some(c) if c == ' ' || c == '\t' || is_sep(c) || is_close(c) => break,
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
                Some(c) if c == '\\' => match self.get() {
                    Some(c) if escapes.contains_key(&c) => result.push(escapes[&c]),
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => break,
                },
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
        match name {
            "let" => self.create_template(args),
            "apply" => self.apply_template(args),
            s => self.evaluate(s, args),
        }
    }

    // parse all values up to but not including the next block terminator '}', ']', ')' or EOF
    // ',' and '\n' are treated as whitespace
    fn parse_list(&mut self, args: &[Value]) -> Result<VecDeque<Value>, Error> {
        match self.parse_value(args) {
            Ok(value) => {
                if let Some(c) = self.peek() {
                    if is_sep(c) {
                        self.skip();
                    }
                }
                match value {
                    None => match self.peek() {
                        Some(c) if is_close(c) => Ok(VecDeque::new()),
                        None => Ok(VecDeque::new()),
                        _ => self.parse_list(args),
                    },
                    Some(value) => match self.parse_list(args) {
                        Ok(mut values) => {
                            values.push_front(value);
                            Ok(values)
                        }
                        e => e,
                    },
                }
            }
            Err(e) => Err(e),
        }
    }

    fn parse_args(&mut self, args: &[Value]) -> Result<VecDeque<Value>, Error> {
        match self.parse_list(args) {
            result @ Ok(_) => match self.get() {
                Some(')') => result,
                Some(c) => Err(self.error(ErrorKind::Unexpected(c))),
                None => Err(self.error(ErrorKind::EOF)),
            },
            e => e,
        }
    }

    fn parse_arg(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.parse_value(args) {
            Ok(None) => match self.peek() {
                Some(c) if is_sep(c) => {
                    self.skip();
                    self.parse_arg(args)
                }
                _ => Ok(None),
            },
            v => v,
        }
    }

    fn create_template(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        let name = match self.parse_arg(args) {
            Ok(Some(v)) => {
                if v.is_string() {
                    v.into_string()
                } else {
                    return Err(self.error(ErrorKind::BadKey));
                }
            }
            Ok(_) => return Err(self.error(ErrorKind::BadKey)),
            error => return error,
        };
        println!("name [{}], next [{:?}]", name, self.peek());

        let template = match self.read_template() {
            Ok(t) => t,
            Err(e) => return Err(e),
        };
        println!("template [{}]", template);
        self.templates.as_mut().unwrap().insert(name, template);
        Ok(None)
    }

    fn apply_template(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        unimplemented!();
        //match self.parse_args(args);
        //match self.templates.map(|templates| templates.contains(
        //    let templates = self.templates.take().unwrap();

        //    let parser = Parser::new()
    }

    fn evaluate(&mut self, name: &str, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.parse_args(args) {
            Ok(mut args) => match self.peek() {
                Some(c) if c == ')' => {
                    match eval::evaluate(name, &args.drain(..).collect::<Vec<_>>()[..]) {
                        Ok(value) => {
                            self.skip();
                            Ok(Some(value))
                        }
                        Err(e) => Err(self.error(e)),
                    }
                }
                _ => Err(self.error(ErrorKind::Missing(')'))),
            },
            Err(e) => Err(e),
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
                self.skip();
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

    fn error(&self, error: ErrorKind) -> Error {
        Error(self.row, self.clm, error)
    }
}

fn is_sep(c: char) -> bool {
    c == ',' || c == '\n'
}

fn is_close(c: char) -> bool {
    c == '}' || c == ']' || c == ')'
}

#[cfg(test)]
mod test {

    #[test]
    fn test_parse() {
        use super::{from_str, Error, ErrorKind, Value};
        use std::collections::HashMap;
        use std::iter::FromIterator;

        assert_eq!(from_str("").unwrap(), Value::Dict(HashMap::new()));
        assert_eq!(from_str("fail"), Err(Error(0, 4, ErrorKind::BadKey)));
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

        let a = "test {]";
        assert_eq!(from_str(a).unwrap_err().kind(), &ErrorKind::Unexpected(']'));

        let a = r#"let(template, value)
key apply(template)
"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }
}
