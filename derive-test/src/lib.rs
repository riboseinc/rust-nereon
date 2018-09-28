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

#[macro_use]
#[allow(unused_imports)]
extern crate nereon_derive;
extern crate nereon;

#[cfg(test)]
mod tests {
    extern crate nereon;
    use self::nereon::{parse_noc, FromValue, Value};

    #[test]
    fn test_string_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            a: String,
        }
        assert_eq!(
            A::from_value(parse_noc::<Value>("a apple").unwrap()),
            Ok(A {
                a: "apple".to_owned(),
            })
        );
    }

    #[test]
    fn test_unsigned_struct() {
        #[derive(Default, Debug, FromValue, PartialEq)]
        struct A {
            a: u64,
            b: u32,
            c: u16,
            d: u8,
        }
        let tests = &[
            ("a 0, b 0, c 0, d 0", Ok(Default::default())),
            (
                "a 18446744073709551615, b 4294967295, c 65535, d 255",
                Ok(A {
                    a: u64::max_value(),
                    b: u32::max_value(),
                    c: u16::max_value(),
                    d: u8::max_value(),
                }),
            ),
        ];
        for (a, b) in tests {
            assert_eq!(&A::from_value(parse_noc::<Value>(a).unwrap()), b);
        }
        let tests = &[
            "a 18446744073709551617, b 0, c 0, d 0",
            "a 0, b 4294967296, c 0, d 0",
            "a 0, b 0, c 65536, d 0",
            "a 0, b 0, c 0, d 256",
            "a (-1), b 0, c 0, d 0",
            "a 0, b (-1), c 0, d 0",
            "a 0, b 0, c (-1), d 0",
            "a 0, b 0, c 0, d (-1)",
            "a 0.0, b 0, c 0, d 0",
            "a 0, b 0.0, c 0, d 0",
            "a 0, b 0, c 0.0, d 0",
            "a 0, b 0, c 0, d 0.0",
        ];
        for a in tests {
            assert!(&A::from_value(parse_noc::<Value>(a).unwrap()).is_err());
        }
    }

    #[test]
    fn test_signed_struct() {
        #[derive(Default, Debug, FromValue, PartialEq)]
        struct A {
            a: i64,
            b: i32,
            c: i16,
            d: i8,
        }
        let tests = &[
            ("a 0, b 0, c 0, d 0", Ok(Default::default())),
            (
                "a 9223372036854775807, b 2147483647, c 32767, d 127",
                Ok(A {
                    a: i64::max_value(),
                    b: i32::max_value(),
                    c: i16::max_value(),
                    d: i8::max_value(),
                }),
            ),
            (
                "a (-9223372036854775808), b (-2147483648), c (-32768), d (-128)",
                Ok(A {
                    a: i64::min_value(),
                    b: i32::min_value(),
                    c: i16::min_value(),
                    d: i8::min_value(),
                }),
            ),
        ];
        for (a, b) in tests {
            assert_eq!(&A::from_value(parse_noc::<Value>(a).unwrap()), b);
        }
        let tests = &[
            "a 9223372036854775808, b 0, c 0, d 0",
            "a 0, b 2147483648, c 0, d 0",
            "a 0, b 0, c 32768, d 0",
            "a 0, b 0, c 0, d 128",
            "a (-9223372036854775809), b 0, c 0, d 0",
            "a 0, b (-2147483649), c 0, d 0",
            "a 0, b 0, c (-32769), d 0",
            "a 0, b 0, c 0, d (-129)",
            "a 0.0, b 0, c 0, d 0",
            "a 0, b 0.0, c 0, d 0",
            "a 0, b 0, c 0.0, d 0",
            "a 0, b 0, c 0, d 0.0",
        ];
        for a in tests {
            assert!(&A::from_value(parse_noc::<Value>(a).unwrap()).is_err());
        }
    }

    #[test]
    fn test_float_struct() {
        #[derive(Default, Debug, FromValue, PartialEq)]
        struct A {
            a: f64,
            b: f32,
        }
        let tests = &[
            ("a 0, b 0", Ok(Default::default())),
            ("a 1.2, b 1.2", Ok(A { a: 1.2, b: 1.2 })),
            ("a (-1.2), b (-1.2)", Ok(A { a: -1.2, b: -1.2 })),
            ("a 1.2e6, b 1.2e6", Ok(A { a: 1.2e6, b: 1.2e6 })),
            (
                "a (-1.2e6), b (-1.2e6)",
                Ok(A {
                    a: -1.2e6,
                    b: -1.2e6,
                }),
            ),
        ];
        for (a, b) in tests {
            assert_eq!(&A::from_value(parse_noc::<Value>(a).unwrap()), b);
        }
        let tests = &["a not, b 0", "a 0, b not"];
        for a in tests {
            assert!(&A::from_value(parse_noc::<Value>(a).unwrap()).is_err());
        }
    }

    #[test]
    fn test_option_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            a: Option<u8>,
        }
        assert_eq!(
            A::from_value(parse_noc::<Value>("b apple").unwrap()),
            Ok(A { a: None })
        );
        assert_eq!(
            A::from_value(parse_noc::<Value>("a 200").unwrap()),
            Ok(A { a: Some(200) })
        );
    }

    #[test]
    fn test_nested_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            a: String,
        }
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
            b: String,
        }
        assert_eq!(
            B::from_value(parse_noc::<Value>("a { a apple }, b banana").unwrap()),
            Ok(B {
                a: A {
                    a: "apple".to_owned()
                },
                b: "banana".to_owned(),
            })
        );
    }

    #[test]
    fn test_enum() {
        #[derive(Debug, FromValue, PartialEq)]
        enum AA {
            A,
            B,
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: AA,
        }
        assert_eq!(parse_noc::<B>("a a"), Ok(B { a: AA::A }));
    }

    #[test]
    fn test_enum_with_named_variant() {
        impl FromValue for AA {
            fn from_value(v: Value) -> Result<Self, String> {
                #[inline]
                fn convert<T: FromValue>(v: Value) -> Result<T, String> {
                    T::from_value(v)
                }
                #[inline]
                fn convert_no_value<T: FromValue>() -> Result<T, String> {
                    T::from_no_value()
                }
                let variant = match v {
                    Value::String(ref s) => Ok(s.clone()),
                    Value::Table(ref t) if t.len() == 1 => Ok(t.iter().next().unwrap().0.clone()),
                    _ => Err(format!(
                        "Cannot convert to {}: not String or single value Table",
                        stringify!(AA)
                    )),
                }?;
                match variant.as_ref() {
                    "a" => Ok(AA::A),
                    "b" => {
                        use std::collections::HashMap;
                        let mut v:HashMap<String, Value> = HashMap::from_value(v)
                            .map(|mut v: HashMap<String, Value>| v.remove("b").unwrap())
                            .and_then(|v| {
                                HashMap::from_value(v).map_err(|_| {
                                    format!(
                                        "Cannot convert to {}: \"{}\" is not a Table",
                                        stringify!(AA::B),
                                        stringify!("b")
                                    )
                                })
                            })?;
                        Ok(AA::B {
                            b: v.remove(stringify!(b)).map_or_else(
                                || {
                                    convert_no_value().map_err(|e| {
                                        format!(
                                            "Cannot convert to {}. Field {}: {}",
                                            stringify!("# name :: vname"),
                                            stringify!(b),
                                            e
                                        )
                                    })
                                },
                                convert,
                            )?,
                        })
                    }
                    _ => Err(format!(
                        "Cannot convert to {}: no such variant \"{}\"",
                        stringify!(AA),
                        variant
                    )),
                }
            }
        }
        #[derive(Debug, PartialEq)]
        enum AA {
            A,
            B { b: u32 },
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: AA,
        }
        assert_eq!(parse_noc::<B>("a a"), Ok(B { a: AA::A }));
    }
}
