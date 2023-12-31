use core::fmt;
use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut},
};

use passing_macro::UNIQUE_IDENT_PREFIX;
use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{
    braced, bracketed,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Brace,
    Token,
};

// === IDs === //

pub fn combine_idents<I: Borrow<Ident>>(span: Span, idents: impl IntoIterator<Item = I>) -> Ident {
    let mut id = UNIQUE_IDENT_PREFIX.to_string();
    for ident in idents {
        let ident = ident.borrow().to_string();
        assert!(ident.starts_with(UNIQUE_IDENT_PREFIX));
        id.push_str(&ident[UNIQUE_IDENT_PREFIX.len()..]);
    }

    Ident::new(&id, span)
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

    pub fn write_dyn_kw(stream: &mut TokenStream, key: &str) {
        stream.extend([TokenTree::Ident(Ident::new(key, Span::call_site()))]);
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

    pub fn write_group(stream: &mut TokenStream, f: impl FnOnce(&mut TokenStream)) {
        stream.extend([TokenTree::Group(Group::new(Delimiter::Brace, {
            let mut inner = TokenStream::new();
            f(&mut inner);
            inner
        }))]);
    }

    pub fn parse_kv<V: Parse>(input: ParseStream, key: &str) -> syn::Result<V> {
        parse_dyn_kw(input, key)?;
        input.parse::<Token![=]>()?;
        parse_grouped(input)
    }

    pub fn write_kv(stream: &mut TokenStream, key: &str, value: &impl ToTokens) {
        write_dyn_kw(stream, key);
        Token![=](Span::call_site()).to_tokens(stream);
        write_group(stream, |stream| value.to_tokens(stream));
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

        impl $crate::util::structured_macro_internals::Parse for $name {
            #[allow(unused)]
            fn parse(input: $crate::util::structured_macro_internals::ParseStream) -> $crate::util::structured_macro_internals::SynResult<Self> {
                $crate::util::structured_macro_internals::parse_dyn_kw(
                    input,
                    $crate::util::structured_macro_internals::stringify!($name),
                )?;

                let input_inner;
                $crate::util::structured_macro_internals::braced!(input_inner in input);

                $crate::util::structured_macro_internals::Ok(Self {
                    $($f_name: $crate::util::structured_macro_internals::parse_kv(
                        &input_inner,
                        $crate::util::structured_macro_internals::stringify!($f_name),
                    )?,)*
                })
            }
        }

        impl $crate::util::structured_macro_internals::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut $crate::util::structured_macro_internals::TokenStream) {
                $crate::util::structured_macro_internals::write_dyn_kw(
                    tokens,
                    $crate::util::structured_macro_internals::stringify!($name),
                );

                $crate::util::structured_macro_internals::write_group(tokens, |tokens| {
                    $($crate::util::structured_macro_internals::write_kv(
                        tokens,
                        &$crate::util::structured_macro_internals::stringify!($f_name),
                        &self.$f_name
                    );)*
                });
            }
        }

        $crate::util::structured_macro_internals::structured!($($rest)*);
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
        #[allow(dead_code)]
        $vis enum $name {
            $($f_name($f_vis $f_ty)),*
        }

        impl $crate::util::structured_macro_internals::Parse for $name {
            #[allow(unused)]
            fn parse(input: $crate::util::structured_macro_internals::ParseStream) -> $crate::util::structured_macro_internals::SynResult<Self> {
                $(if $crate::util::structured_macro_internals::parse_dyn_kw(input, $crate::util::structured_macro_internals::stringify!($f_name)).is_ok() {
                    return $crate::util::structured_macro_internals::Ok(
                        Self::$f_name($crate::util::structured_macro_internals::parse_grouped(input)?)
                    );
                })*

                $crate::util::structured_macro_internals::Err(input.error("unexpected enum variant"))
            }
        }

        impl $crate::util::structured_macro_internals::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut $crate::util::structured_macro_internals::TokenStream) {
                match self {
                    $(Self::$f_name(inner) => {
                        $crate::util::structured_macro_internals::write_dyn_kw(
                            tokens,
                            $crate::util::structured_macro_internals::stringify!($f_name)
                        );

                        $crate::util::structured_macro_internals::write_group(tokens, |tokens| {
                            $crate::util::structured_macro_internals::ToTokens::to_tokens(inner, tokens);
                        });
                    },)*
                    #[allow(unreachable_patterns)]  // Used for empty enums
                    _ => { let _ = tokens; },
                }
            }
        }

        $crate::util::structured_macro_internals::structured!($($rest)*);
    };
}

pub(crate) use structured;

// === Structured Helpers === //

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

pub struct SynArray<T> {
    pub contents: Punctuated<T, Token![,]>,
}

impl<T> Deref for SynArray<T> {
    type Target = Punctuated<T, Token![,]>;

    fn deref(&self) -> &Self::Target {
        &self.contents
    }
}

impl<T> DerefMut for SynArray<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.contents
    }
}

impl<T: Clone> Clone for SynArray<T> {
    fn clone(&self) -> Self {
        Self {
            contents: self.contents.clone(),
        }
    }
}

impl<T> FromIterator<T> for SynArray<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut contents = Punctuated::new();
        contents.extend(iter);
        Self { contents }
    }
}

impl<T: Parse> Parse for SynArray<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        bracketed!(inner in input);

        Ok(Self {
            contents: Punctuated::parse_terminated(&inner)?,
        })
    }
}

impl<T: ToTokens> ToTokens for SynArray<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend([TokenTree::Group(Group::new(
            Delimiter::Bracket,
            self.contents.to_token_stream(),
        ))])
    }
}

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

// === Formatter === //

pub struct FnFormatter<F>(pub F);

impl<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result> fmt::Display for FnFormatter<F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

pub fn format_fn<F: Fn(&mut fmt::Formatter<'_>) -> fmt::Result>(f: F) -> FnFormatter<F> {
    FnFormatter(f)
}

pub fn format_list<'a, I>(iter: I, sep: &'a impl fmt::Display) -> impl fmt::Display + 'a
where
    I: 'a + IntoIterator + Clone,
    I::Item: fmt::Display,
{
    format_fn(move |f| {
        let mut is_subsequent = false;
        for item in iter.clone() {
            if is_subsequent {
                fmt::Display::fmt(sep, f)?;
            }
            is_subsequent = true;
            fmt::Display::fmt(&item, f)?;
        }
        Ok(())
    })
}
