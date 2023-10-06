use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Path, Token, Type, TypeParamBound, Visibility,
};

// === CapMacroArg === //

#[derive(Clone)]
pub struct CapMacroArg {
    pub list: Punctuated<CapDecl, Token![;]>,
}

#[derive(Clone)]
pub struct CapDecl {
    pub visibility: Visibility,
    pub name: Ident,
    pub kind: CapDeclKind,
}

#[derive(Clone)]
pub enum CapDeclKind {
    EqualsTy(Type),
    ImplsTrait(Punctuated<TypeParamBound, Token![+]>),
    Inherits(Punctuated<CapDeclInheritedElement, Token![,]>),
}

#[derive(Clone)]
pub enum CapDeclInheritedElement {
    Ref(Path),
    Mut(Path),
    Bundle(Path),
}

impl CapDeclInheritedElement {
    pub fn mode(&self) -> ResolvedBundleExpectedMode {
        match self {
            CapDeclInheritedElement::Ref(_) => ResolvedBundleExpectedMode::Ref,
            CapDeclInheritedElement::Mut(_) => ResolvedBundleExpectedMode::Mut,
            CapDeclInheritedElement::Bundle(_) => ResolvedBundleExpectedMode::Bundle,
        }
    }

    pub fn path(&self) -> &Path {
        use CapDeclInheritedElement::*;

        match self {
            Ref(p) | Mut(p) | Bundle(p) => p,
        }
    }
}

impl Parse for CapMacroArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            list: input.parse_terminated(CapDecl::parse, Token![;])?,
        })
    }
}

impl Parse for CapDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            visibility: input.parse()?,
            name: input.parse()?,
            kind: input.parse()?,
        })
    }
}

impl Parse for CapDeclKind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<Token![=>]>().is_ok() {
            Ok(Self::Inherits(Punctuated::parse_separated_nonempty(input)?))
        } else if input.parse::<Token![=]>().is_ok() {
            Ok(Self::EqualsTy(input.parse()?))
        } else if input.parse::<Token![:]>().is_ok() {
            Ok(Self::ImplsTrait({
                // Copied from `WherePredicate`'s parsing logic
                let mut bounds = Punctuated::new();
                loop {
                    if input.is_empty()
                        || input.peek(syn::token::Brace)
                        || input.peek(Token![,])
                        || input.peek(Token![;])
                        || input.peek(Token![:])
                        || input.peek(Token![=])
                    {
                        break;
                    }
                    let value = input.parse()?;
                    bounds.push_value(value);
                    if !input.peek(Token![+]) {
                        break;
                    }
                    let punct = input.parse()?;
                    bounds.push_punct(punct);
                }
                bounds
            }))
        } else {
            Err(input.error("Expected =, :, or =>."))
        }
    }
}

impl Parse for CapDeclInheritedElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![ref]) {
            Ok(Self::Ref(input.parse()?))
        } else if input.peek(Token![mut]) {
            Ok(Self::Mut(input.parse()?))
        } else {
            Ok(Self::Bundle(input.parse()?))
        }
    }
}

// === ResolvedBundleExpectedMode === //

syn::custom_keyword!(bundle);

#[derive(Clone)]
pub enum ResolvedBundleExpectedMode {
    Ref,
    Mut,
    Bundle,
}

impl Parse for ResolvedBundleExpectedMode {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<Token![ref]>().is_ok() {
            Ok(Self::Ref)
        } else if input.parse::<Token![mut]>().is_ok() {
            Ok(Self::Mut)
        } else if input.parse::<bundle>().is_ok() {
            Ok(Self::Bundle)
        } else {
            Err(input.error("unknown bundle mode"))
        }
    }
}

impl ToTokens for ResolvedBundleExpectedMode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let span = proc_macro2::Span::call_site();
        match self {
            ResolvedBundleExpectedMode::Ref => Token![ref](span).to_tokens(tokens),
            ResolvedBundleExpectedMode::Mut => Token![mut](span).to_tokens(tokens),
            ResolvedBundleExpectedMode::Bundle => bundle(span).to_tokens(tokens),
        }
    }
}
