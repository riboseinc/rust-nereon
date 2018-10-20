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
const _GRAMMAR: &str = include_str!("pest");

use pest::iterators::Pair;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Parser;
use std::char::from_u32;
use std::collections::HashMap;

mod functions;
mod value;

pub use self::value::{FromValue, Value};

#[derive(Debug)]
struct State<'a> {
    templates: Vec<(String, Pair<'a, Rule>)>,
    args: Vec<Value>,
}

#[derive(Parser)]
#[grammar = "noc/pest"]
struct NocParser;

lazy_static! {
    static ref CLIMBER: PrecClimber<Rule> = {
        let plus = Operator::new(Rule::plus, Assoc::Left);
        let minus = Operator::new(Rule::minus, Assoc::Left);
        let times = Operator::new(Rule::times, Assoc::Left);
        let divide = Operator::new(Rule::divide, Assoc::Left);
        let modulus = Operator::new(Rule::modulus, Assoc::Left);
        let intdiv = Operator::new(Rule::intdiv, Assoc::Left);
        let power = Operator::new(Rule::power, Assoc::Left);
        PrecClimber::new(vec![plus | minus, times | divide | modulus | intdiv, power])
    };
}

/// Parse a NOC string and convert into any type that implements
/// [`FromValue`](trait.FromValue.html)
///
/// ```
/// extern crate nereon;
/// use nereon::{parse_noc, Value};
/// use std::collections::HashMap;
///
/// let noc = r#"
///     user admin {
///         uid 1000 + 10
///         name "John Doe"
///     }"#;
///
/// let expected = Value::Table(HashMap::new())
///     .insert(vec!["user", "admin", "uid"], Value::from("1010"))
///     .insert(vec!["user", "admin", "name"], Value::from("John Doe"));
///
/// assert_eq!(parse_noc(noc), Ok(expected));
/// ```
pub fn parse_noc<T>(input: &str) -> Result<T, String>
where
    T: FromValue,
{
    NocParser::parse(Rule::root, input)
        .map_err(|e| format!("{:?}", e))
        .and_then(|mut pairs| {
            mk_value(
                pairs.next().unwrap(),
                &mut State {
                    templates: Vec::new(),
                    args: Vec::new(),
                },
            )
        }).and_then(T::from_value)
}

fn mk_value<'a>(pair: Pair<'a, Rule>, state: &mut State<'a>) -> Result<Value, String> {
    match pair.as_rule() {
        Rule::table => mk_table(pair, state),
        Rule::list => mk_list(pair, state).map(Value::List),
        Rule::expression => evaluate(pair, state),
        Rule::bare_string => mk_bare(pair),
        Rule::quoted_string => mk_quoted(pair),
        Rule::function => apply_function(pair, state),
        _ => unreachable!(),
    }
}

fn mk_table<'a>(pair: Pair<'a, Rule>, state: &mut State<'a>) -> Result<Value, String> {
    pair.into_inner()
        .try_fold(Value::Table(HashMap::new()), |dict, pair| {
            match pair.as_rule() {
                Rule::key_value => {
                    let mut expressions: Vec<Pair<Rule>> = pair.into_inner().collect();
                    assert!(expressions.iter().all(|e| e.as_rule() == Rule::expression));
                    mk_value(expressions.pop().unwrap(), state).and_then(|value| {
                        expressions
                            .into_iter()
                            .try_fold(Vec::new(), |mut keys, e| {
                                mk_value(e, state).and_then(|key| {
                                    String::from_value(key).map(|key| {
                                        keys.push(key);
                                        keys
                                    })
                                })
                            }).map(|keys| dict.insert(keys.iter().map(|s| s.as_ref()), value))
                    })
                }
                Rule::template => {
                    mk_template(pair, state);
                    Ok(dict)
                }
                _ => unreachable!(),
            }
        })
}

fn mk_list<'a>(pair: Pair<'a, Rule>, state: &mut State<'a>) -> Result<Vec<Value>, String> {
    pair.into_inner()
        .try_fold(Vec::new(), |mut list, pair| match pair.as_rule() {
            Rule::expression => mk_value(pair, state).map(|value| {
                list.push(value);
                list
            }),
            Rule::template => {
                mk_template(pair, state);
                Ok(list)
            }
            _ => unreachable!(),
        })
}

fn evaluate<'a>(expression: Pair<'a, Rule>, state: &mut State<'a>) -> Result<Value, String> {
    CLIMBER.climb(
        expression.into_inner(),
        |value| mk_value(value.into_inner().next().unwrap(), state),
        |lhs, op, rhs| {
            lhs.and_then(|lhs| rhs.map(|rhs| [lhs, rhs]))
                .and_then(|args| match op.as_rule() {
                    Rule::plus => functions::add(&args),
                    Rule::minus => functions::subtract(&args),
                    Rule::times => functions::multiply(&args),
                    Rule::divide => functions::divide(&args),
                    Rule::intdiv => functions::intdiv(&args),
                    Rule::modulus => functions::modulus(&args),
                    Rule::power => functions::power(&args),
                    _ => unimplemented!(),
                })
        },
    )
}

fn mk_bare(bare: Pair<Rule>) -> Result<Value, String> {
    bare.into_inner()
        .try_fold(String::new(), |mut s, pair| match pair.as_rule() {
            Rule::bare_chars => {
                s.push_str(pair.into_span().as_str());
                Ok(s)
            }
            Rule::esc => unescape(pair.into_span().as_str()).map(|c| {
                s.push(c);
                s
            }),
            Rule::special_esc => {
                let mut esc = pair.into_span().as_str();
                s.push(esc.chars().nth(1).unwrap());
                Ok(s)
            }
            _ => unreachable!(),
        }).map(Value::String)
}

fn mk_quoted(quoted: Pair<Rule>) -> Result<Value, String> {
    quoted
        .into_inner()
        .try_fold(String::new(), |mut s, pair| match pair.as_rule() {
            Rule::quoted_chars => {
                s.push_str(pair.into_span().as_str());
                Ok(s)
            }
            Rule::esc => unescape(pair.into_span().as_str()).map(|c| {
                s.push(c);
                s
            }),
            _ => unreachable!(),
        }).map(Value::String)
}

fn unescape(s: &str) -> Result<char, String> {
    let code = s.get(2..).unwrap();
    match s.chars().nth(1).unwrap() {
        'r' => Ok('\r'),
        'n' => Ok('\n'),
        't' => Ok('\t'),
        c if c == '\\' || c == '\'' || c == '\"' => Ok(c),
        '0' => Ok(char::from(u8::from_str_radix(code, 8).unwrap())),
        'x' => Ok(char::from(u8::from_str_radix(code, 16).unwrap())),
        'u' | 'U' => from_u32(u32::from_str_radix(code, 16).unwrap())
            .ok_or_else(|| "Invalid unicode".to_owned()),
        _ => unreachable!(),
    }
}

fn mk_template<'a>(pair: Pair<'a, Rule>, state: &mut State<'a>) {
    let mut iter = pair.into_inner();
    let name = mk_value(iter.next().unwrap(), state)
        .and_then(String::from_value)
        .unwrap();
    state.templates.push((name, iter.next().unwrap()));
}

fn apply_function<'a>(pair: Pair<'a, Rule>, state: &mut State<'a>) -> Result<Value, String> {
    let mut iter = pair.into_inner();
    let name = iter.next().unwrap();
    if name.as_str() == "$" {
        let name = mk_value(iter.next().unwrap(), state).and_then(String::from_value)?;
        apply_template(&name, &[], state)
    } else {
        let name = mk_value(name, state).and_then(String::from_value)?;
        mk_list(iter.next().unwrap(), state).and_then(|args| match name.as_ref() {
            "apply" => args[0]
                .as_str()
                .ok_or_else(|| "Template name isn't a string".to_owned())
                .and_then(|name| apply_template(name, &args[1..], state)),
            "arg" => match args.len() {
                1 => args[0]
                    .as_str()
                    .ok_or(())
                    .and_then(|arg| arg.parse::<usize>().map_err(|_| ()))
                    .and_then(|n| state.args.get(n).ok_or(()))
                    .map(|v| v.clone()),
                _ => Err(()),
            }.map_err(|_| "arg(n): bad argument n".to_owned()),
            _ => functions::apply(&name, &args[0..]),
        })
    }
}

fn apply_template(name: &str, args: &[Value], state: &mut State) -> Result<Value, String> {
    state
        .templates
        .iter()
        .rposition(|(template_name, _)| template_name == name)
        .ok_or_else(|| "No such template".to_owned())
        .and_then(|idx| {
            let mut new_state = State {
                templates: state.templates[0..idx].to_vec(),
                args: args.to_vec(),
            };
            mk_value(state.templates[idx].1.clone(), &mut new_state)
        })
}

#[cfg(test)]
mod tests {
    use super::{parse_noc, Value};
    use std::collections::HashMap;
    use std::iter::FromIterator;

    #[test]
    fn test_empty() {
        assert_eq!(
            parse_noc::<Value>("").unwrap(),
            Value::Table(HashMap::new())
        );
    }

    #[test]
    fn test_key_no_value() {
        assert!(parse_noc::<Value>("fail").is_err());
    }

    #[test]
    fn test_key_value() {
        assert_eq!(
            parse_noc::<Value>("key value").unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_nested_dict() {
        assert_eq!(
            parse_noc::<Value>("key { key value }").unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::Table(HashMap::from_iter(vec![(
                    "key".to_owned(),
                    Value::String("value".to_owned()),
                )])),
            )]))
        );
    }

    #[test]
    fn test_sep_key_value_sep() {
        assert_eq!(
            parse_noc::<Value>(",,,,key value,,,,").unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_duplicate_key() {
        assert_eq!(
            parse_noc::<Value>("key value,key value1").unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value1".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_multi_kv() {
        assert_eq!(
            parse_noc::<Value>("key value\nkey2 value2").unwrap(),
            Value::Table(HashMap::from_iter(vec![
                ("key".to_owned(), Value::String("value".to_owned())),
                ("key2".to_owned(), Value::String("value2".to_owned())),
            ]))
        );
    }

    #[test]
    fn test_quoted_kv() {
        let a = r#""key\n1" "value\n1""#;
        let b = "\"key\n1\" \"value\n1\"";
        assert_eq!(parse_noc::<Value>(a).unwrap().as_noc_string(), b);
    }

    #[test]
    fn test_bad_escape() {
        assert!(parse_noc::<Value>(r#""key\n1" "value\1""#).is_err());
    }

    #[test]
    fn test_unbalanced() {
        assert!(parse_noc::<Value>("test {]").is_err());
    }

    #[test]
    fn bare() {
        vec![
            ("a abcABC123", r#""a" "abcABC123""#),
            (r#"a \{\}\[\]\(\)\ \,"#, r#""a" "{}[]() ,""#),
            (r#"a \x20"#, r#""a" " ""#),
            (r#"a \040"#, r#""a" " ""#),
            (r#"a \u0020"#, r#""a" " ""#),
            (r#"a \U00000020"#, r#""a" " ""#),
            (r#"a Ψฒ≨🚲"#, r#""a" "Ψฒ≨🚲""#),
            (r#"a \{b\ c\}"#, r#""a" "{b c}""#),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(&parse_noc::<Value>(a).unwrap().as_noc_string(), b);
        });
    }

    #[test]
    fn test_quoted() {
        vec![
            (r#"a "\x20""#, r#""a" " ""#),
            (r#"a "\040""#, r#""a" " ""#),
            (r#"a "\u0020""#, r#""a" " ""#),
            (r#"a "\U00000020""#, r#""a" " ""#),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(&parse_noc::<Value>(a).unwrap().as_noc_string(), b);
        });
    }

    #[test]
    fn test_bad_unicode_parse() {
        vec![
            r#"a "\x20"#,
            r#"a "\04""#,
            r#"a "\u020""#,
            r#"a "\U00000g0""#,
        ].iter()
        .for_each(|a| assert!(&parse_noc::<Value>(a).is_err()));
    }

    #[test]
    fn test_list() {
        vec![
            ("a []", r#""a" []"#),
            ("a [1,2]", r#""a" ["1","2"]"#),
            ("a [{}]", r#""a" [{}]"#),
            ("a [\n\n]", r#""a" []"#),
            ("a [\n1\n,2\n]", r#""a" ["1","2"]"#),
            ("a [,,1,,2]", r#""a" ["1","2"]"#),
            ("a [1,,2,,]", r#""a" ["1","2"]"#),
            ("a [{b [1,2]}]", r#""a" [{"b" ["1","2"]}]"#),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(&parse_noc::<Value>(a).unwrap().as_noc_string(), b);
        });
    }

    #[test]
    fn test_template_string() {
        let a = r#"let(template, value),
 key apply(template)"#;
        assert_eq!(
            parse_noc::<Value>(a).unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::String("value".to_owned()),
            )]))
        );
    }

    #[test]
    fn test_template_list() {
        let a = r#"let(template, [value])
                   key apply(template)"#;
        assert_eq!(
            parse_noc::<Value>(a).unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::List(vec![Value::String("value".to_owned())]),
            )]))
        );
    }

    #[test]
    fn test_template_array_arg() {
        let a = r#"let(template, arg(0))
                   key apply(template, [value])"#;
        assert_eq!(
            parse_noc::<Value>(a).unwrap(),
            Value::Table(HashMap::from_iter(vec![(
                "key".to_owned(),
                Value::List(vec![Value::String("value".to_owned())]),
            )]))
        );
    }

    #[test]
    fn calculate() {
        vec![
            ("a 1 + 1", r#""a" "2""#),
            ("a 1 - -1", r#""a" "2""#),
            ("a 1  *  -10", r#""a" "-10""#),
            ("a \"-1\" * 10", r#""a" "-10""#),
            ("a (2 + 3) * 4", r#""a" "20""#),
            ("a 2 + 3 * 4", r#""a" "14""#),
            ("a 180 / 3.14", r#""a" "57.324840764331206""#),
            ("a 1 / 2", r#""a" "0.5""#),
            ("a 1 \\ 2", r#""a" "0""#),
            ("a 5 % 2", r#""a" "1""#),
            ("a 10 ^ 2", r#""a" "100""#),
            ("a 10 ^ (2 + 1)", r#""a" "1000""#),
            ("a 10 ^ -1", r#""a" "0.1""#),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(&parse_noc::<Value>(a).unwrap().as_noc_string(), b);
        });
    }

    #[test]
    fn calculate_fail_missing_whitespace() {
        // these don't fail but behave somewhat unexpectedly
        vec![
            ("a 1+1", r#"a "1+1""#),
            ("a 1 --1", r#"a{1 "--1"}"#),
            ("a 1*  -10", r#"a {"1*" "-10"}"#),
            (r#"a "-1"*10"#, r#"a{"-1" "*10"}"#),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap(),
            )
        });
    }

    #[test]
    fn comments() {
        vec![
            ("a {} # comment", "a {}"),
            ("a { # comment\n}", "a {}"),
            ("a [] #comment", "a []"),
            ("a test #comment", "a test"),
            ("a test#comment", "a test"),
            ("a [1 #comment\n, 2]", "a [1,2]"),
            ("a {b c#comment\n}", "a {b c}"),
            ("\n# comment\na {\n#comment\n}", "a{}"),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap()
            );
        });
    }

    #[test]
    fn templates() {
        vec![
            ("let(t, 1), a apply(t)", "a 1"),
            ("let(t, 1), a $t", "a 1"),
            ("let(t, 1), a $t + 1", "a 2"),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap()
            );
        });
    }

    #[test]
    fn functions() {
        vec![
            ("a add(1, 1)", "a 2"),
            ("let(t, 1), a add($t, 1)", "a 2"),
            ("a subtract(1, 1)", "a 0"),
            ("let(t, 1), a subtract($t, 1)", "a 0"),
            ("a divide(1, 1)", "a 1"),
            ("let(t, 1), a divide($t, $t)", "a 1"),
            ("a multiply(2, 2)", "a 4"),
            ("let(t, 2), a add($t, $t)", "a 4"),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap()
            );
        });
    }

    #[test]
    fn concat() {
        vec![
            ("a concat(a, b, c)", "a abc"),
            ("a concat()", "a \"\""),
            ("a concat(a)", "a a"),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap()
            );
        });
    }
    #[test]
    fn join() {
        vec![
            ("a join(., a, b, c)", "a a.b.c"),
            ("a join(.)", "a \"\""),
            ("a join(., a)", "a a"),
        ].iter()
        .for_each(|(a, b)| {
            assert_eq!(
                parse_noc::<Value>(a).unwrap(),
                parse_noc::<Value>(b).unwrap()
            );
        });
    }
}
