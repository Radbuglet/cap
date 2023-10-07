use std::sync::atomic::{AtomicUsize, Ordering};

use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Brace,
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

// === Braced === //

#[derive(Clone)]
pub struct Braced<V> {
    pub brace: Brace,
    pub value: V,
}

impl<V: Parse> Parse for Braced<V> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let value;

        Ok(Self {
            brace: braced!(value in input),
            value: value.parse()?,
        })
    }
}

impl<V: ToTokens> ToTokens for Braced<V> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut group = Group::new(Delimiter::Brace, self.value.to_token_stream());
        group.set_span(self.brace.span.join());
        tokens.extend([TokenTree::Group(group)]);
    }
}

// === StructuredKv === //

pub struct StructuredKv<K, V> {
    pub key: K,
    pub eq: Token![=],
    pub value: Braced<V>,
}

impl<K, V> Clone for StructuredKv<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            eq: self.eq,
            value: self.value.clone(),
        }
    }
}

impl<K: Parse, V: Parse> Parse for StructuredKv<K, V> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            key: input.parse()?,
            eq: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl<K: ToTokens, V: ToTokens> ToTokens for StructuredKv<K, V> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.key.to_tokens(tokens);
        self.eq.to_tokens(tokens);
        self.value.to_tokens(tokens)
    }
}

// === StructuredArray === //

pub struct StructuredArray<V> {
    pub values: Punctuated<Braced<V>, Token![,]>,
}

impl<V> Clone for StructuredArray<V>
where
    V: Clone,
{
    fn clone(&self) -> Self {
        Self {
            values: self.values.clone(),
        }
    }
}

impl<V: Parse> Parse for StructuredArray<V> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            values: Punctuated::parse_terminated(input)?,
        })
    }
}

impl<V: ToTokens> ToTokens for StructuredArray<V> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.values.to_tokens(tokens);
    }
}
