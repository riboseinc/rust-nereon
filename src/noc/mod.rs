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

#[cfg(debug_assertions)]
const _GRAMMAR: &'static str = include_str!("../nereon.pest");

use pest::iterators::{Pair, Pairs};
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use std::char::from_u32;
use std::collections::HashMap;
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

#[derive(Parser)]
#[grammar = "nereon.pest"]
struct NereonParser;

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> =
        PrecClimber::new(vec![Operator::new(Rule::infix, Assoc::Left)]);
}

pub fn from_read(input: &mut io::Read) -> Result<Value, String> {
    let mut buffer = String::new();
    input
        .read_to_string(&mut buffer)
        .map_err(|e| format!("{:?}", e))
        .and_then(|_| from_str(&buffer))
}

pub fn from_str(input: &str) -> Result<Value, String> {
    NereonParser::parse(Rule::root, input)
        .map_err(|e| format!("{:?}", e))
        .and_then(|mut pairs| mk_value(pairs.next().unwrap()))
}

fn mk_value(pair: Pair<Rule>) -> Result<Value, String> {
    match pair.as_rule() {
        Rule::kv_list => mk_dict(pair.into_inner()),
        Rule::expression => evaluate(pair.into_inner()),
        Rule::bare_string => Ok(Value::String(pair.into_span().as_str().to_owned())),
        Rule::quoted_string => mk_quoted(pair.into_inner()),
        _ => unimplemented!(),
    }
}

fn mk_dict(pairs: Pairs<Rule>) -> Result<Value, String> {
    pairs
        .flatten()
        .filter(|p| p.as_rule() == Rule::key_value)
        .map(|p| {
//            println!("---\n{}\n---", p);
            p
        })
        .try_fold(Value::Dict(HashMap::new()), |mut dict, kv| {
            println!("----------");
            let mut values: Vec<Pair<Rule>> = kv
                .into_inner()
                .filter(|p| p.as_rule() == Rule::expression)
                .collect();
            for p in values.clone() {
                println!("{}", p.as_str());
                println!("{}", p);
            }
            mk_value(values.pop().unwrap()).and_then(|value| {
                values
                    .into_iter()
                    .try_fold(Vec::new(), |mut keys, e| {
                        mk_value(e).and_then(|key| {
                            if key.is_string() {
                                keys.push(key.into_string());
                                Ok(keys)
                            } else {
                                Err("Key is not a string".to_owned())
                            }
                        })
                    })
                    .and_then(|keys| {
                        dict.insert(keys, value);
                        Ok(dict)
                    })
            })
        })
}

fn evaluate(expression: Pairs<Rule>) -> Result<Value, String> {
    CLIMBER.climb(
        expression
            .flatten()
            .filter(|p| p.as_rule() == Rule::value || p.as_rule() == Rule::infix),
        |value| mk_value(value.into_inner().next().unwrap()),
        |_lhs, op, _rhs| match op.as_rule() {
            _ => unimplemented!(),
        },
    )
}

fn mk_quoted(quoted: Pairs<Rule>) -> Result<Value, String> {
    quoted
        .flatten()
        .try_fold(String::new(), |mut s, pair| {
            match pair.as_rule() {
                Rule::quoted_chars => s.push_str(pair.into_span().as_str()),
                Rule::esc => {
                    let mut esc = pair.into_span().as_str();
                    let code = esc.get(2..).unwrap();
                    s.push(match esc.chars().skip(1).next().unwrap() {
                        'r' => '\r',
                        'n' => '\n',
                        't' => '\t',
                        c if c == '\\' || c == '\'' || c == '\"' => c,
                        '0' => char::from(u8::from_str_radix(code, 8).unwrap()),
                        'x' => char::from(u8::from_str_radix(code, 16).unwrap()),
                        'u' | 'U' => match from_u32(u32::from_str_radix(code, 16).unwrap()) {
                            Some(c) => c,
                            _ => return Err("Invalid unicode".to_owned()),
                        },
                        _ => unreachable!(),
                    })
                }
                _ => (),
            };
            Ok(s)
        })
        .map(|s| Value::String(s))
}

#[cfg(test)]
mod test {
    use super::{from_str, mk_value, NereonParser, Parser, Rule, Value};
    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn test_empty() {
        assert_eq!(from_str("").unwrap(), Value::Dict(HashMap::new()));
    }

    #[test]
    fn test_key_no_value() {
        assert!(from_str("fail").is_err());
    }

    #[test]
    fn test_key_value() {
        assert_eq!(
            from_str("key value").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_nested_dict() {
        assert_eq!(
            from_str("key { key value }").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Dict(HashMap::from_iter(vec![(
                    "key".to_owned(),
                    Value::String("value".to_owned()),
                )]))
            )]))
        );
    }

    #[test]
    fn test_sep_key_value_sep() {
        assert_eq!(
            from_str(",,,,key value,,,,").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_duplicate_key() {
        assert_eq!(
            from_str("key value,key value1").unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value1".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_multi_kv() {
        assert_eq!(
            from_str("key value\nkey2 value2").unwrap(),
            Value::Dict(HashMap::from_iter(vec![
                ("key".to_owned(), Value::String("value".to_owned())),
                ("key2".to_owned(), Value::String("value2".to_owned())),
            ]))
        );
    }

    #[test]
    fn test_quoted_kv() {
        let a = r#""key\n1" "value\n1""#;
        let b = "\"key\n1\" \"value\n1\"";
        assert_eq!(from_str(a).unwrap().as_noc_string(), format!("{{{}}}", b));
    }

    #[test]
    fn test_bad_escape() {
        let a = r#""key\n1" "value\1""#;
        assert!(from_str(a).is_err());
    }

    #[test]
    fn test_unalanced() {
        let a = "test {]";
        assert!(from_str(a).is_err());
    }

    #[test]
    fn test_quoted() {
        let mut ps = NereonParser::parse(Rule::value, r#""\x20\040\u0020\U00000020""#).unwrap();
        assert_eq!(
            mk_value(ps.next().unwrap().into_inner().next().unwrap()),
            Ok(Value::String("    ".to_owned()))
        );
    }

    #[test]
    fn test_bad_unicode_parse() {
        assert!(
            NereonParser::parse(Rule::value, r#""\U0000020""#).is_err();
        );
    }

    #[test]
    #[ignore]
    fn test_template_string() {
        let a = r#"let(template, value),
 key apply(template)"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    #[ignore]
    fn test_template_list() {
        let a = r#"let(template, [value])
                   key apply(template)"#;
        assert_eq!(
            from_str(a).unwrap(),
            Value::Dict(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Array(vec![Value::String("value".to_owned())]),
            )]))
        );
    }

    #[test]
    #[ignore]
    fn test_template_array_arg() {
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
