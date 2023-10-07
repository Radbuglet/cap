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

// === Structured === //

#[doc(hidden)]
#[allow(dead_code)]
pub(crate) mod structured_macro_internals {
    use syn::Token;

    #[allow(unused_imports)]
    pub(crate) use super::structured;

    pub use {
        proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree},
        quote::ToTokens,
        std::{
            iter::Extend,
            result::Result::{Err, Ok},
            stringify,
        },
        syn::{
            braced,
            parse::{Parse, ParseStream},
            Result as SynResult,
        },
    };

    pub fn parse_dyn_kw(input: ParseStream, key: &str) -> syn::Result<Ident> {
        if input
            .cursor()
            .ident()
            .is_some_and(|(ident, _)| ident == key)
        {
            Ok(input.parse().unwrap())
        } else {
            Err(input.error(format!("Expected identifier {key:?}")))
        }
    }

    pub fn parse_grouped<V: Parse>(input: ParseStream) -> syn::Result<V> {
        let braced;
        braced!(braced in input);
        let result = braced.parse::<V>()?;
        if !braced.is_empty() {
            return Err(braced.error("Unexpected."));
        }

        Ok(result)
    }

    pub fn parse_kv<V: Parse>(input: ParseStream, key: &str) -> syn::Result<V> {
        parse_dyn_kw(input, key)?;
        input.parse::<Token![=]>()?;
        parse_grouped(input)
    }
}

#[allow(unused_macros)]
macro_rules! structured {
    () => {
        /* muncher base case */
    };
    (
		$(#[$attr:meta])*
		$vis:vis struct $name:ident {
			$(
				$(#[$f_attr:meta])*
				$f_vis:vis $f_name:ident: $f_ty:ty
			),*
			$(,)?
		}

		$($rest:tt)*
	) => {
		$(#[$attr])*
		$vis struct $name {
			$(
				$(#[$f_attr])*
				$f_vis $f_name: $f_ty
			),*
		}

        impl $crate::magic::structured_macro_internals::Parse for $name {
			#[allow(unused)]
			fn parse(input: $crate::magic::structured_macro_internals::ParseStream) -> $crate::magic::structured_macro_internals::SynResult<Self> {
				let input_inner;
				$crate::magic::structured_macro_internals::braced!(input_inner in input);

				$crate::magic::structured_macro_internals::Ok(Self {
					$($f_name: $crate::magic::structured_macro_internals::parse_kv(
						&input_inner,
						$crate::magic::structured_macro_internals::stringify!($f_name),
					)?,)*
				})
			}
		}

		impl $crate::magic::structured_macro_internals::ToTokens for $name {
			fn to_tokens(&self, tokens: &mut $crate::magic::structured_macro_internals::TokenStream) {
				#[allow(unused_mut)]
				let mut inner = $crate::magic::structured_macro_internals::TokenStream::new();

				$($crate::magic::structured_macro_internals::ToTokens::to_tokens(&self.$f_name, &mut inner);)*

				$crate::magic::structured_macro_internals::Extend::extend(
					&mut *tokens,
					[$crate::magic::structured_macro_internals::TokenTree::Group(
						$crate::magic::structured_macro_internals::Group::new(
							$crate::magic::structured_macro_internals::Delimiter::Brace,
							inner,
						),
					)],
				);
			}
		}

		$crate::magic::structured_macro_internals::structured!($($rest)*);
    };
	(
		$(#[$attr:meta])*
		$vis:vis enum $name:ident {
			$($f_name:ident($f_vis:vis $f_ty:ty)),*
			$(,)?
		}

		$($rest:tt)*
	) => {
		$(#[$attr])*
		$vis enum $name {
			$($f_name($f_vis $f_ty)),*
		}

        impl $crate::magic::structured_macro_internals::Parse for $name {
			#[allow(unused)]
			fn parse(input: $crate::magic::structured_macro_internals::ParseStream) -> $crate::magic::structured_macro_internals::SynResult<Self> {
				$(if $crate::magic::structured_macro_internals::parse_dyn_kw(input, $crate::magic::structured_macro_internals::stringify!($f_name)).is_ok() {
					return $crate::magic::structured_macro_internals::parse_grouped(input);
				})*

				$crate::magic::structured_macro_internals::Err(input.error("Unexpected enum variant."))
			}
		}

		impl $crate::magic::structured_macro_internals::ToTokens for $name {
			fn to_tokens(&self, tokens: &mut $crate::magic::structured_macro_internals::TokenStream) {
				match self {
					$(Self::$f_name(inner) => {
						$crate::magic::structured_macro_internals::Extend::extend(
							&mut *tokens,
							[
								$crate::magic::structured_macro_internals::TokenTree::Ident(
									$crate::magic::structured_macro_internals::Ident::new(
										$crate::magic::structured_macro_internals::stringify!($f_name),
										$crate::magic::structured_macro_internals::Span::call_site(),
									),
								),
								$crate::magic::structured_macro_internals::TokenTree::Group(
									$crate::magic::structured_macro_internals::Group::new(
										$crate::magic::structured_macro_internals::Delimiter::Brace,
										$crate::magic::structured_macro_internals::ToTokens::into_token_stream(inner),
									),
								),
							],
						);
					},)*
				}
			}
		}

		$crate::magic::structured_macro_internals::structured!($($rest)*);
    };
}

pub(crate) use structured;

#[derive(Clone)]
pub struct Nop;

impl Parse for Nop {
    fn parse(_input: ParseStream) -> syn::Result<Self> {
        Ok(Self)
    }
}

impl ToTokens for Nop {
    fn to_tokens(&self, _tokens: &mut TokenStream) {}
}

pub struct StructuredArray<T> {
    pub contents: Punctuated<T, Token![,]>,
}

impl<T: Clone> Clone for StructuredArray<T> {
    fn clone(&self) -> Self {
        Self {
            contents: self.contents.clone(),
        }
    }
}

impl<T: Parse> Parse for StructuredArray<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            contents: Punctuated::parse_terminated(input)?,
        })
    }
}

impl<T: ToTokens> ToTokens for StructuredArray<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.contents.to_tokens(tokens);
    }
}
