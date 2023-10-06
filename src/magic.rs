use std::sync::atomic::{AtomicUsize, Ordering};

use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    Token,
};

// TODO: Version mixing handling

// === Ident construction === //

pub fn new_unique_ident() -> Ident {
    const COMPILATION_TAG: u32 = const_random::const_random!(u32);

    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    let unique_id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let ident_string = format!("__cap_random_ident_{COMPILATION_TAG}_{unique_id}");
    Ident::new(ident_string.as_str(), Span::call_site())
}

// === Exporter-importer protocol === //

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

pub fn make_macro_importer(base_args: TokenStream, macro_chain: &[TokenStream]) -> TokenStream {
    if let Some((first_macro, rest)) = macro_chain.split_first() {
        quote! {
            #first_macro!(@__extract_macro_data #(#rest)=>* => #base_args);
        }
    } else {
        TokenStream::default()
    }
}

syn::custom_keyword!(__extract_macro_data);

// === Import Parsing === //

#[derive(Clone)]
pub struct MacroImporterBegin;

impl Parse for MacroImporterBegin {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![@]>()?;
        input.parse::<__extract_macro_data>()?;
        Ok(Self)
    }
}

#[derive(Clone)]
pub struct ImportedMacroInfo<U, D> {
    pub supplied: U,
    pub collected: Vec<D>,
}

impl<U: Parse, D: Parse> Parse for ImportedMacroInfo<U, D> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<MacroImporterBegin>()?;

        // Parse supplied data
        let supplied;
        braced!(supplied in input);
        let supplied = U::parse(&supplied)?;

        // Parse collected data
        let mut collected = Vec::new();
        while !input.is_empty() {
            let collected_unit;
            braced!(collected_unit in input);
            let collected_unit = D::parse(&collected_unit)?;
            collected.push(collected_unit);
        }

        Ok(Self {
            supplied,
            collected,
        })
    }
}
