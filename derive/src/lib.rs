extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro::TokenStream as TokenStream2;

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
//                #body
            }
        }
    };
    gen.into()
}

fn impl_derive_from_value_struct(_data: &syn::DataStruct) -> TokenStream2 {
    let gen = quote! {
        v.get("command").and_then(|commands| {
            v.get("option")
                .map(|options| Command { commands, options })
        })
    };
    gen.into()
}

fn impl_derive_from_value_enum(_data: &syn::DataEnum) -> TokenStream2 {
    unimplemented!()
}
