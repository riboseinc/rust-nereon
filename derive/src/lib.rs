extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;

#[proc_macro_derive(FromValue)]
pub fn derive_from_value(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<syn::DeriveInput>(input).unwrap();
    let name = &ast.ident;
    let body = match ast.data {
        syn::Data::Struct(ref s) => impl_derive_from_value_struct(s),
        syn::Data::Enum(ref e) => impl_derive_from_value_enum(e),
        _ => unreachable!()
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
    fields.iter().fold(quote!{}, |q, f| {
        let name = f.ident.as_ref().unwrap();
        quote! {
            #q
            #name: v.get(stringify!(#name))?,
        }
    }).into()
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
