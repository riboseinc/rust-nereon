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
//! trait.
//!
//! ```
//! #[macro_use]
//! extern crate nereon_derive;
//! extern crate nereon;
//! use nereon::{parse_noc, FromValue, Value};
//!
//! # fn main() {
//! #[derive(FromValue, PartialEq, Debug)]
//! struct User {
//!     uid: u32,
//!     name: String,
//! }
//!
//! let noc = r#"
//!     uid 1000 + 10
//!     name "John Doe"
//! "#;
//!
//! let expected = User { uid: 1010, name: "John Doe".to_owned() };
//! let user = parse_noc(noc).and_then(|v| User::from_value(&v));
//! assert_eq!(user, Ok(expected));
//! # }
//! ```

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
        syn::Data::Struct(ref s) => impl_derive_from_value_struct(s),
        syn::Data::Enum(ref e) => impl_derive_from_value_enum(e),
        _ => unreachable!(),
    };
    let gen = quote! {
        impl FromValue for #name {
            fn from_value(v: &Value) -> Result<Self, String> {
                Ok(#name {
                    #body
                })
            }
        }
    };
    gen.into()
}

fn impl_derive_from_value_struct(data: &syn::DataStruct) -> TokenStream2 {
    match data.fields {
        syn::Fields::Named(_) => impl_derive_from_value_struct_named(&data.fields),
        syn::Fields::Unnamed(_) => impl_derive_from_value_struct_unnamed(&data.fields),
        syn::Fields::Unit => impl_derive_from_value_struct_unit(),
    }
}

fn impl_derive_from_value_struct_named(fields: &syn::Fields) -> TokenStream2 {
    fields
        .iter()
        .fold(quote!{}, |q, f| {
            let name = f.ident.as_ref().unwrap();
            quote! {
                #q
                #name: v.get(stringify!(#name))?,
            }
        })
        .into()
}

fn impl_derive_from_value_struct_unnamed(_fields: &syn::Fields) -> TokenStream2 {
    unimplemented!()
}

fn impl_derive_from_value_struct_unit() -> TokenStream2 {
    unimplemented!()
}

fn impl_derive_from_value_enum(_data: &syn::DataEnum) -> TokenStream2 {
    unimplemented!()
}
