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

//! A Nereon [Value](../nereon/enum.Value.html) can be converted into
//! another type using the [FromValue](../nereon/trait.FromValue.html)
//! trait. See the [`nereon`](../nereon/index.html) crate for
//! example usage.
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

#[doc(hidden)]
#[proc_macro_derive(FromValue)]
pub fn derive_from_value(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let body = match ast.data {
        syn::Data::Struct(ref s) => body_from_from_value_struct(&name, s),
        syn::Data::Enum(ref e) => body_from_from_value_enum(&name, e),
        _ => unreachable!(),
    };
    let gen = quote! {
        impl FromValue for #name {
            fn from_value(mut v: Value) -> Result<Self, String> {
                #body
            }
        }
    };
    gen.into()
}

fn body_from_from_value_struct(name: &syn::Ident, data: &syn::DataStruct) -> TokenStream2 {
    match data.fields {
        syn::Fields::Named(_) => body_from_from_value_normal_struct(name, &data.fields),
        syn::Fields::Unnamed(_) => body_from_from_value_tuple_struct(name, &data.fields),
        syn::Fields::Unit => unreachable!(),
    }
}

fn body_from_from_value_normal_struct(name: &syn::Ident, fields: &syn::Fields) -> TokenStream2 {
    let fields: TokenStream2 = named_fields(fields);
    let err = format!("Cannot convert to {}, not a Table", name);
    quote! {
        match v {
            Value::Table(mut v) => Ok(#name { #fields }),
            _ => Err(#err.to_owned())
        }
    }
}

fn body_from_from_value_tuple_struct(
    name: &syn::Ident,
    fields: &syn::Fields,
) -> TokenStream2 {
    let fields = fields.iter().fold(quote!{}, |q, _| quote! {
        #q
        Value::convert(v.next())?,
    });
    let err = format!("Cannot convert to {}, not a List", name);
    quote! {
        match v {
            Value::List(mut v) => {
                let mut v = v.drain(..);
                Ok(#name(
                    #fields
                ))
            }
            _ => Err(#err.to_owned()),
        }
    }
}

fn body_from_from_value_enum(name: &syn::Ident, data: &syn::DataEnum) -> TokenStream2 {
    let branches = data.variants.iter().fold(quote!{}, |q, data| {
        let vname = &data.ident;
        let match_name = (&data.ident).to_string().to_lowercase();
        let unit_err = format!("Key \"{}\" doesn't expect a value", match_name);
        let list_err = format!("Key \"{}\" expects a List value", match_name);
        let table_err = format!("Key \"{}\" expects a Table value", match_name);
        let branch = match data.fields {
            syn::Fields::Named(_) => {
                let fields = named_fields(&data.fields);
                quote! {
                    match v {
                        Some(Value::Table(mut v)) => Ok(#name::#vname {
                            #fields
                        }),
                        _ => Err(#table_err.to_owned()),
                    }
                }
            }
            syn::Fields::Unnamed(_) => {
                let fields = data.fields.iter().fold(quote!{}, |q, _| quote! {
                    #q
                    Value::convert(v.next())?,
                });
                quote! {
                    match v {
                        Some(Value::List(mut v)) => {
                            let mut v = v.drain(..);
                            Ok(#name::#vname(
                                #fields
                            ))
                        }
                        _ => Err(#list_err.to_owned()),
                    }
                }
            }
            syn::Fields::Unit => quote! {
                v.map_or_else(
                    || Ok(#name::#vname),
                    |_| Err(#unit_err.to_owned()),
                ),
            },
        };
        quote! {
            #q
            #match_name => #branch
        }
    });
    quote! {
        match v {
            Value::String(s) => Ok((s, None)),
            Value::Table(ref mut t) if t.len() == 1 => {
                Ok(t.drain().next().map(|(k, v)| (k, Some(v))).unwrap())
            }
            _ => Err("Not an enum Variant".to_owned()),
        }.and_then(|(n, v)| match n.as_ref() {
            #branches
            _ => Err(format!("No such variant \"{}\"", n)),
        })
    }
}

fn named_fields(data: &syn::Fields) -> TokenStream2 {
    data.iter()
        .fold(quote!{}, |q, f| {
            let n = f.ident.as_ref().unwrap();
            quote! {
                #q
                #n: Value::convert(v.remove(stringify!(#n)))?,
            }
        })
}
