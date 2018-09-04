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

use nom::types::CompleteStr;
use nom::{is_alphanumeric, multispace0, space0, space1, ErrorKind, IResult, Needed};
use std::collections::{HashMap, VecDeque};
use std::io;

mod eval;
mod value;

use self::value::Value;

#[derive(Debug, Clone)]
struct Template {
    row: u32,
    clm: u32,
    template: String,
}

struct State {
    templates: HashMap<String, Template>,
    args: Vec<Value>,
}

pub fn from_read(input: &mut io::Read) -> Result<Value, String> {
    let mut buffer = String::new();
    input
        .read_to_string(&mut buffer)
        .map_err(|e| format!("{:?}", e))
        .and_then(|_| from_str(&buffer))
}

pub fn from_str(input: &str) -> Result<Value, String> {
    let (input, _) = space0(CompleteStr::from(input)).unwrap();
    apply!(
        input,
        parse_dict,
        &mut State {
            templates: HashMap::new(),
            args: Vec::new(),
        }
    ).map_err(|e| format!("Parse error: {:?}", e))
        .and_then(|v| match v {
            (CompleteStr(""), v) => Ok(v),
            _ => Err("Trailing characters".to_owned()),
        })
}

named_args!(parse_dict<'a>(state: &mut State)<CompleteStr<'a>, Value>,
    fold_many0!(
        do_parse!(
            result: apply!(parse_keyed_value, state)
                >> many0!(alt!(tag!(",") | multispace0))
                >> (result)
        ),
        Value::Dict(HashMap::new()),
        |mut acc: Value, o| {
            if let Some((keys, value)) = o {
                acc.insert(keys, value);
            }
            acc
        }
    )
);

named_args!(parse_keyed_value<'a>(state: &mut State)<CompleteStr<'a>, Option<(Vec<String>, Value)>>,
    map_res!(
        separated_list!(space1, apply!(parse_value, state)),
        |vs: Vec<Option<Value>>| {
            let mut vs: Vec<_> = vs.into_iter().filter_map(|v| v).collect();
            if !vs.is_empty() {
                let value = vs.pop().unwrap();
                if !vs.is_empty() && vs.iter().all(|k| k.is_string()) {
                    Ok(Some((
                        vs.drain(..).map(|k| k.into_string()).collect(),
                        value,
                    )))
                } else {
                    Err(ErrorKind::Custom(0))
                }
            } else {
                Ok(None)
            }
        }
    )
);

named_args!(parse_value<'a>(state: &mut State)<CompleteStr<'a>, Option<Value>>,
    alt!(
        map!(
            delimited!(tag!("{"), apply!(parse_dict, state), tag!("}")),
            Some
        )
            | map!(
                delimited!(tag!("["), apply!(parse_list, state), tag!("]")),
                |list| Some(Value::Array(list))
            ) | apply!(parse_expression, state)
    )
);

named_args!(parse_expression<'a>(state: &mut State)<CompleteStr<'a>, Option<Value>>,
    alt!(
        value!(None, apply!(parse_let, state)) |
        apply!(parse_apply, state) |
        apply!(parse_macro, state) |
        apply!(parse_parens, state) |
        apply!(parse_binop, state) |
        map!(parse_quoted_string, Some) |
        map!(
            take_while1_s!(|c| is_alphanumeric(c as u8) || c == '_' || c == '.'),
            |v| Some(Value::String(v.0.to_owned()))
        )
    )
);

named_args!(parse_let<'a>(state: &mut State)<CompleteStr<'a>, ()>,
    map_res!(
        tuple!(
            apply!(parse_value, state),
            parse_template
        ), |(name, template): (Option<Value>, String)| {
            if let Some(name) = name.map(|v| v.into_string()) {
                state.templates.insert(name, Template { row: 0, clm: 0, template });
                Ok(())
            } else {
                Err(ErrorKind::Custom(1))
            }
        }
    )
);

named_args!(parse_apply<'a>(state: &mut State)<CompleteStr<'a>, Option<Value>>,
    map_res!(value!(None), |_: Option<Value>| Err(ErrorKind::Custom(2)))
);

named_args!(parse_parens<'a>(state: &mut State)<CompleteStr<'a>, Option<Value>>,
    map_res!(value!(None), |_: Option<Value>| Err(ErrorKind::Custom(2)))
);

named_args!(parse_binop<'a>(state: &mut State)<CompleteStr<'a>, Option<Value>>,
    map_res!(value!(None), |_: Option<Value>| Err(ErrorKind::Custom(2)))
);

named!(parse_template(CompleteStr) -> String,
    value!(String::new())
);

named!(parse_quoted_string(CompleteStr) -> Value,
    delimited!(
        tag!("\""),
        map!(
            escaped_transform!(
                none_of!("\""),
                '\\',
                alt!(
                    tag!("a") => { |_| "\x07" }
                    | tag!("b") => { |_| "\x08" }
                    | tag!("f") => { |_| "\x0c" }
                    | tag!("n") => { |_| "\n" }
                    | tag!("r") => { |_| "\r" }
                    | tag!("t") => { |_| "\t" }
                    | tag!("v") => { |_| "\x0b" }
                    | tag!("\\") => { |_| "\\" }
                    | tag!("\'") => { |_| "\'" }
                    | tag!("\"") => { |_| "\"" }
                    | tag!("?") => { |_| "?" }
                )
            ),
            Value::String
        ),
        tag!("\"")
    )
);

fn parse_macro<'a>(_input: CompleteStr, _state: &State) -> IResult<CompleteStr<'a>, Option<Value>> {
    unimplemented!()
    /*
    self.skip();
    match name {
        "let" => self.create_template(args),
        "apply" => self.apply_template(args),
        _ => self
            .parse_args(args)
            .and_then(|mut eval_args| {
                eval::evaluate(name, &Vec::from_iter(eval_args.drain(..))[..], args)
                    .map_err(|e| self.error(e))
            })
            .map(|v| Some(v)),
    }
*/
}

// parse all values up to but not including the next block terminator '}', ']', ')' or EOF
// ',' and '\n' are treated as whitespace
named_args!(parse_list<'a>(state: &mut State)<CompleteStr<'a>, Vec<Value>>,
    map!(
        fold_many0!(
            apply!(parse_value, state),
            Vec::new(),
            |mut list: Vec<_>, value| {
                list.push(value);
                list
            }
        ),
        |list| list.into_iter().filter_map(|v| v).collect()
    )
);

fn parse_args<'a>(
    _input: CompleteStr,
    _state: &'a State,
) -> IResult<CompleteStr<'a>, VecDeque<Value>> {
    unimplemented!()
    /*
    self.parse_list(args)
        .and_then(|v| self.expect(v, Some(')')))
*/
}

fn parse_arg<'a>(_input: CompleteStr, _state: &State) -> IResult<CompleteStr<'a>, Option<Value>> {
    unimplemented!()
    /*
    self.parse_value(args).and_then(|arg| match arg {
        None => match self.skip_sep() {
            None => Ok(None),
            Some(c) if is_close(c) => Ok(None),
            _ => self.parse_arg(args),
        },
        _ => Ok(arg),
    })
*/
}

fn apply_template(_input: CompleteStr, _state: &State) -> Result<Option<Value>, String> {
    unimplemented!()
    /*
    let bad_name = "Template name should be a string";
    self.parse_args(args)
        .and_then(|mut apply_args| match apply_args.pop_front() {
            None => Err(self.error(ErrorKind::BadArg(bad_name.to_owned()))),
            Some(name) => match name.as_string() {
                Some(name) => Ok(name),
                None => Err(self.error(ErrorKind::BadArg(bad_name.to_owned()))),
            }.and_then(|name| {
                let templates = self.templates.take().unwrap();
                if let Some(template) = templates.get(name).map(|v| v.clone()) {
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
*/
}

fn parse_template_string<'a>(_input: CompleteStr) -> IResult<CompleteStr, String> {
    unimplemented!()
    /*
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
*/
}

#[cfg(test)]
mod test {

    #[test]
    fn test_parse() {
        use super::{from_str, Value};
        use std::collections::HashMap;
        use std::iter::FromIterator;

        assert_eq!(from_str("").unwrap(), Value::Dict(HashMap::new()));
        assert_eq!(from_str("fail"), Err("Trailing characters".to_owned()));

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
        println!("--------------------------------------");
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
        assert_eq!(from_str(a).unwrap_err(), "Trailing characters");

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
                Value::Array(vec![Value::String("value".to_owned())]),
            )]))
        );

        let a = r#"let(template, arg(0))
                   key apply(template [value])"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Array(vec![Value::String("value".to_owned())]),
            )]))
        );
    }
}
