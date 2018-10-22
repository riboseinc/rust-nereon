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
    use super::nereon::{parse_noc, Error, FromValue, Value};

    fn conversion_error<T>(
        keys: &[&'static str],
        expected: &'static str,
        found: &'static str,
    ) -> Result<T, Error> {
        Err(Error::ConversionError {
            keys: keys.to_vec(),
            expected,
            found,
        })
    }

    #[test]
    fn test_string_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            a: String,
        }
        assert_eq!(
            parse_noc::<A>("a apple"),
            Ok(A {
                a: "apple".to_owned(),
            })
        );
        assert_eq!(
            parse_noc::<A>("b apple"),
            conversion_error(&["a"], "value", "nothing")
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
            assert_eq!(&parse_noc::<A>(a), b);
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
            assert!(parse_noc::<A>(a).is_err());
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
            assert_eq!(&parse_noc::<A>(a), b);
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
            assert!(parse_noc::<A>(a).is_err());
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
            assert_eq!(&parse_noc::<A>(a), b);
        }
        let tests = &["a not, b 0", "a 0, b not"];
        for a in tests {
            assert!(&parse_noc::<A>(a).is_err());
        }
    }

    #[test]
    fn test_option_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            a: Option<u8>,
        }
        assert_eq!(parse_noc::<A>("b apple"), Ok(A { a: None }));
        assert_eq!(parse_noc::<A>("a 200"), Ok(A { a: Some(200) }));
        assert_eq!(
            parse_noc::<A>("a apple"),
            conversion_error(&["a"], "u8", "string")
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
            parse_noc::<B>("a { a apple }, b banana"),
            Ok(B {
                a: A {
                    a: "apple".to_owned()
                },
                b: "banana".to_owned(),
            })
        );
        assert_eq!(
            parse_noc::<B>("a { a apple }"),
            conversion_error(&["b"], "value", "nothing")
        );
        assert_eq!(
            parse_noc::<B>("a { c apple }, b banana"),
            conversion_error(&["a", "a"], "value", "nothing")
        );
    }

    #[test]
    fn test_tuple_struct() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A(i8, i8, i8);
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert_eq!(parse_noc::<B>("a [1,2,3]"), Ok(B { a: A(1, 2, 3) }));
        assert_eq!(
            parse_noc::<B>("a [1,forty-two,3]"),
            conversion_error(&["a", "field#1"], "i8", "string")
        );
    }

    #[test]
    fn test_enum() {
        #[derive(Debug, FromValue, PartialEq)]
        enum A {
            A,
            B,
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert_eq!(parse_noc::<B>("a a"), Ok(B { a: A::A }));
        assert_eq!(parse_noc::<B>("a b"), Ok(B { a: A::B }));
        assert_eq!(
            parse_noc::<B>("a c"),
            conversion_error(&["a"], "one of [a, b]", "value")
        );
    }

    #[test]
    fn test_enum_with_named_variant() {
        #[derive(Debug, FromValue, PartialEq)]
        enum A {
            A { a: i8 },
            B { b: u32 },
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert_eq!(parse_noc::<B>("a a {a 42}"), Ok(B { a: A::A { a: 42 } }));
        assert_eq!(parse_noc::<B>("a b {b 42}"), Ok(B { a: A::B { b: 42 } }));
        assert_eq!(
            parse_noc::<B>("a b {b bakers-dozen}"),
            conversion_error(&["a", "b", "b"], "u32", "string")
        );
        assert_eq!(
            parse_noc::<B>("a b {c 42}"),
            conversion_error(&["a", "b", "b"], "value", "nothing")
        );
        assert_eq!(
            parse_noc::<B>("a c {b 42}"),
            conversion_error(&["a"], "one of [a, b]", "value")
        );
    }

    #[test]
    fn test_enum_with_unnamed_variant() {
        #[derive(Debug, FromValue, PartialEq)]
        enum A {
            A(i8, i8, i8),
            B(u32),
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert_eq!(parse_noc::<B>("a a [1,2,3]"), Ok(B { a: A::A(1, 2, 3) }));
        assert_eq!(parse_noc::<B>("a b [41 + 1]"), Ok(B { a: A::B(42) }));
        assert_eq!(
            parse_noc::<B>("a c {b 42}"),
            conversion_error(&["a"], "one of [a, b]", "value")
        );
        assert_eq!(parse_noc::<B>("a b [orangutan]"),
            conversion_error(&["a", "b", "field#0"], "u32", "string")
        );
        assert_eq!(parse_noc::<B>("a a [1,2,orangutan]"),
            conversion_error(&["a", "a", "field#2"], "i8", "string")
        );
    }

    #[test]
    fn test_mixed_enum() {
        #[derive(Debug, FromValue, PartialEq)]
        enum A {
            A,
            B { b: u32 },
            C(u8),
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert_eq!(parse_noc::<B>("a a"), Ok(B { a: A::A }));
        assert_eq!(parse_noc::<B>("a b {b 42}"), Ok(B { a: A::B { b: 42 } }));
        assert_eq!(parse_noc::<B>("a c [41 + 1]"), Ok(B { a: A::C(42) }));
    }

    #[test]
    fn test_invalid_enums() {
        #[derive(Debug, FromValue, PartialEq)]
        enum A {
            A,
            B { b: u32 },
            C(u8),
        };
        #[derive(Debug, FromValue, PartialEq)]
        struct B {
            a: A,
        }
        assert!(parse_noc::<B>("a a [42]").is_err());
        assert!(parse_noc::<B>("a a []").is_err());
        assert!(parse_noc::<B>("a b {b:42}").is_err());
        assert!(parse_noc::<B>("a b [42]").is_err());
        assert!(parse_noc::<B>("a b").is_err());
        assert!(parse_noc::<B>("a b {c:42}").is_err());
        assert!(parse_noc::<B>("a b {}").is_err());
        assert!(parse_noc::<B>("a d").is_err());
        assert!(parse_noc::<B>("a d {d:42}").is_err());
        assert!(parse_noc::<B>("a d").is_err());
        assert!(parse_noc::<B>("a d {d:42}").is_err());
        assert!(parse_noc::<B>("a d [42]").is_err());
    }

    #[test]
    fn test_missing_vec() {
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            vec: Vec<String>,
        }
        assert_eq!(
            parse_noc::<A>(""),
            Ok(A {
                vec: Vec::default()
            })
        );
    }

    #[test]
    fn test_missing_map() {
        use std::collections::HashMap;
        #[derive(Debug, FromValue, PartialEq)]
        struct A {
            map: HashMap<String, String>,
        }
        assert_eq!(
            parse_noc::<A>(""),
            Ok(A {
                map: HashMap::default()
            })
        );
    }
}
