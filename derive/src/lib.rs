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
        syn::Data::Struct(ref s) => impl_derive_from_value_struct(&name, s),
        syn::Data::Enum(ref e) => impl_derive_from_value_enum(&name, e),
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

fn impl_derive_from_value_struct(name: &syn::Ident, data: &syn::DataStruct) -> TokenStream2 {
    match data.fields {
        syn::Fields::Named(_) => impl_derive_from_value_struct_named(name, &data.fields),
        syn::Fields::Unnamed(_) => impl_derive_from_value_struct_unnamed(name, &data.fields),
        syn::Fields::Unit => impl_derive_from_value_struct_unit(name),
    }
}

fn impl_derive_from_value_struct_named(name: &syn::Ident, fields: &syn::Fields) -> TokenStream2 {
    let fields: TokenStream2 = fields
        .iter()
        .fold(quote!{}, |q, f| {
            let n = f.ident.as_ref().unwrap();
            quote! {
                #q
                #n: table.remove(stringify!(#n)).map_or_else(
                    || convert_no_value()
                        .map_err(|e| format!("Cannot convert to {}. Field {}: {}", stringify!(#name), stringify!(#n), e)),
                    convert
                )?,
            }
        }).into();
    quote! {
        #[inline]
        fn convert<T: FromValue>(v: Value) -> Result<T, String> {
            T::from_value(v)
        }
        #[inline]
        fn convert_no_value<T: FromValue>() -> Result<T, String> {
            T::from_no_value()
        }
        let table = v.as_table_mut().ok_or_else(
            || format!("Cannot convert to {}: not a Table", stringify!(#name))
        )?;
        Ok( #name {
            #fields
        })
    }
}

fn impl_derive_from_value_struct_unnamed(
    _name: &syn::Ident,
    _fields: &syn::Fields,
) -> TokenStream2 {
    unimplemented!()
}

fn impl_derive_from_value_struct_unit(_name: &syn::Ident) -> TokenStream2 {
    unimplemented!()
}

fn impl_derive_from_value_enum(_name: &syn::Ident, _data: &syn::DataEnum) -> TokenStream2 {
    unimplemented!()
}
