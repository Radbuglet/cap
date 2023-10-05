use std::sync::atomic::{AtomicUsize, Ordering};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

// TODO: Version handling

pub fn new_unique_ident() -> Ident {
    const COMPILATION_TAG: u32 = const_random::const_random!(u32);

    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let unique_id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let ident_string = format!("__cap_random_ident_{COMPILATION_TAG}_{unique_id}");
    Ident::new(ident_string.as_str(), Span::call_site())
}

pub fn make_macro_exporter(data: TokenStream) -> (TokenStream, Ident) {
    let ident = new_unique_ident();

    (
        quote! {
            #[doc(hidden)]
            #[macro_export]
            macro_rules! #ident {
                (@__extract_macro_data $path:path => $($args:tt)*) => {
                    $path!(@__extract_macro_data $($args)* #data);
                };
            }
        },
        ident,
    )
}

pub fn make_macro_importer(base_args: TokenStream, macros: &[TokenStream]) -> TokenStream {
    if let Some((first_macro, rest)) = macros.split_first() {
        quote! {
            #first_macro!(@__extract_macro_data #(#rest)=>* => #base_args);
        }
    } else {
        TokenStream::default()
    }
}

syn::custom_keyword!(__extract_macro_data);

#[derive(Clone)]
pub struct MacroImporterBegin;

impl Parse for MacroImporterBegin {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![@]>()?;
        input.parse::<__extract_macro_data>()?;
        Ok(Self)
    }
}
