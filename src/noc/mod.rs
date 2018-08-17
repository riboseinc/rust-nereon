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
    templates: Option<HashMap<String, Template>>,
}

#[derive(Debug, Clone)]
struct Template {
    row: u32,
    clm: u32,
    template: String,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Unexpected(char),
    BadKey,
    Missing(char),
    EOF,
    BadTemplate,
    MissingTemplate(String),
    BadArg(String),
    UnknownEval(String),
    BadEval(String, String),
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
        self.parse_dict(&[]).and_then(|v| self.expect(v, None))
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
                    match self.skip_sep() {
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
        self.parse_value(args).and_then(|v| match v {
            None => match self.peek() {
                Some(c) if is_sep(c) || is_close(c) => Ok(VecDeque::new()),
                None => Ok(VecDeque::new()),
                _ => self.parse_keyed_value(args),
            },
            Some(value) => self.parse_keyed_value(args).and_then(|mut values| {
                values.push_front(value);
                Ok(values)
            }),
        })
    }

    fn parse_value(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        let result = match self.skip_space() {
            Some(c) if is_close(c) || is_sep(c) => Ok(None),
            None => Ok(None),
            Some('{') => {
                self.skip();
                self.parse_dict(args)
                    .and_then(|v| self.expect(v, Some('}')))
                    .and_then(|v| Ok(Some(v)))
            }
            Some('[') => {
                self.skip();
                self.parse_list(args)
                    .and_then(|v| self.expect(v, Some(']')))
                    .and_then(|mut v| Ok(Some(Value::Array(Vec::from_iter(v.drain(..))))))
            }
            _ => self.parse_expression(args),
        };
        if result.is_ok() {
            self.skip_space();
        }
        result
    }

    fn parse_expression(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        match self.peek() {
            Some(c) if c == '\"' => {
                self.skip();
                Ok(self.parse_quoted_string())
                    .and_then(|v| self.expect(v, Some('\"')))
                    .and_then(|v| Ok(Some(v)))
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
        self.parse_value(args).and_then(|value| {
            self.skip_sep();
            match value {
                None => match self.peek() {
                    Some(c) if is_close(c) => Ok(VecDeque::new()),
                    None => Ok(VecDeque::new()),
                    _ => self.parse_list(args),
                },
                Some(value) => self.parse_list(args).and_then(|mut values| {
                    values.push_front(value);
                    Ok(values)
                }),
            }
        })
    }

    fn parse_args(&mut self, args: &[Value]) -> Result<VecDeque<Value>, Error> {
        self.parse_list(args)
            .and_then(|v| self.expect(v, Some(')')))
    }

    fn parse_arg(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        self.parse_value(args).and_then(|arg| match arg {
            None => match self.skip_sep() {
                None => Ok(None),
                Some(c) if is_close(c) => Ok(None),
                _ => self.parse_arg(args),
            },
            _ => Ok(arg),
        })
    }

    fn create_template(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        self.parse_arg(args).and_then(|name| {
            match name.filter(|name| name.is_string()) {
                Some(name) => Ok(name.into_string()),
                _ => Err(self.error(ErrorKind::BadKey)),
            }.and_then(|name| {
                self.skip_sep();
                let row = self.row;
                let clm = self.clm;
                self.read_template()
                    .and_then(|template| self.expect(template, Some(')')))
                    .and_then(|template| {
                        self.templates.as_mut().unwrap().insert(
                            name,
                            Template {
                                row: row,
                                clm: clm,
                                template: template,
                            },
                        );
                        Ok(None)
                    })
            })
        })
    }

    fn apply_template(&mut self, args: &[Value]) -> Result<Option<Value>, Error> {
        let bad_name = "Template name should be a string";
        self.parse_args(args)
            .and_then(|mut apply_args| match apply_args.pop_front() {
                None => Err(self.error(ErrorKind::BadArg(bad_name.to_owned()))),
                Some(name) => match name.as_string() {
                    Some(name) => Ok(name),
                    None => Err(self.error(ErrorKind::BadArg(bad_name.to_owned()))),
                }.and_then(|name| {
                    let templates = self.templates.take().unwrap();
                    if let Some(template) = templates.get(name).and_then(|v| Some(v.clone())) {
                        let mut parser = Parser {
                            src: &mut template.template.chars().peekable(),
                            row: template.row,
                            clm: template.clm,
                            templates: Some(templates),
                        };
                        let result = parser
                            .parse_value(&Vec::from_iter(apply_args.drain(..)))
                            .and_then(|value| match parser.skip_sep() {
                                None => Ok(value),
                                Some(c) => Err(parser.error(ErrorKind::Unexpected(c))),
                            });
                        self.templates = parser.templates.take();
                        result
                    } else {
                        Err(self.error(ErrorKind::MissingTemplate(name.to_owned())))
                    }
                }),
            })
    }

    fn evaluate(&mut self, name: &str, args: &[Value]) -> Result<Option<Value>, Error> {
        self.parse_args(args)
            .and_then(|mut eval_args| {
                eval::evaluate(name, &Vec::from_iter(eval_args.drain(..))[..], args)
                    .or_else(|e| Err(self.error(e)))
            })
            .and_then(|v| Ok(Some(v)))
    }

    fn read_template(&mut self) -> Result<String, Error> {
        // read next value and return as a string
        let mut braces = Vec::new();
        let mut result = String::new();
        loop {
            match self.peek() {
                None => return Err(self.error(ErrorKind::EOF)),
                Some(c) if (is_sep(c) || c == ')') && braces.is_empty() => break,
                Some(c) if c == '{' || c == '[' || c == '(' => {
                    result.push(c);
                    braces.push(c);
                }
                Some(c) if c == '}' || c == ']' || c == ')' => {
                    result.push(c);
                    match braces.pop() {
                        Some('{') if c == '}' => (),
                        Some('[') if c == ']' => (),
                        Some('(') if c == ')' => (),
                        _ => return Err(self.error(ErrorKind::Unexpected(c))),
                    }
                }
                Some(c) if c == '\"' => {
                    result.push(c);
                    self.skip();
                    while let Some(c) = self.peek() {
                        result.push(c);
                        if c == '\"' {
                            break;
                        }
                        self.skip();
                        if c == '\\' {
                            self.skip();
                            if let Some(c) = self.peek() {
                                result.push(c);
                            }
                        }
                    }
                }
                Some(c) => result.push(c),
            }
            self.skip();
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

    fn skip_sep(&mut self) -> Option<char> {
        loop {
            match self.skip_space() {
                Some(c) if is_sep(c) => self.skip(),
                o => return o,
            }
        }
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

    fn error(&self, error: ErrorKind) -> Error {
        Error(self.row, self.clm, error)
    }

    fn expect<V>(&mut self, value: V, c: Option<char>) -> Result<V, Error> {
        match self.peek() {
            cc if cc == c => {
                self.skip();
                Ok(value)
            }
            Some(cc) => Err(self.error(ErrorKind::Unexpected(cc))),
            None => Err(self.error(ErrorKind::EOF)),
        }
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

        let a = r#"let(template, value),
 key apply(template)"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );

        let a = r#"let(template, [value])
                   key apply(template)"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Array(vec!(Value::String("value".to_owned()))),
            )]))
        );

        let a = r#"let(template, arg(0))
                   key apply(template [value])"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Array(vec!(Value::String("value".to_owned()))),
            )]))
        );
    }
}
