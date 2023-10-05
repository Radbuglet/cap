use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Path, Token, Type, TypeParamBound, Visibility,
};

// === Main Macro Syntax === //

#[derive(Clone)]
pub struct CapabilityDeclList {
    pub list: Punctuated<CapabilityDecl, Token![;]>,
}

#[derive(Clone)]
pub struct CapabilityDecl {
    pub visibility: Visibility,
    pub name: Ident,
    pub kind: CapabilityDeclKind,
}

#[derive(Clone)]
pub enum CapabilityDeclKind {
    EqualsTy(Type),
    ImplsTrait(Punctuated<TypeParamBound, Token![+]>),
    Inherits(Punctuated<CapabilityDeclInherited, Token![,]>),
}

#[derive(Clone)]
pub enum CapabilityDeclInherited {
    Ref(Path),
    Mut(Path),
    Bundle(Path),
}

impl CapabilityDeclInherited {
    pub fn mode(&self) -> BundleArgMode {
        match self {
            CapabilityDeclInherited::Ref(_) => BundleArgMode::Ref,
            CapabilityDeclInherited::Mut(_) => BundleArgMode::Mut,
            CapabilityDeclInherited::Bundle(_) => BundleArgMode::Bundle,
        }
    }

    pub fn path(&self) -> &Path {
        use CapabilityDeclInherited::*;

        match self {
            Ref(p) | Mut(p) | Bundle(p) => p,
        }
    }
}

impl Parse for CapabilityDeclList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            list: input.parse_terminated(CapabilityDecl::parse, Token![;])?,
        })
    }
}

impl Parse for CapabilityDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            visibility: input.parse()?,
            name: input.parse()?,
            kind: input.parse()?,
        })
    }
}

impl Parse for CapabilityDeclKind {
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

impl Parse for CapabilityDeclInherited {
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

// === Bundle Generation Syntax === //

syn::custom_keyword!(bundle);
syn::custom_keyword!(ty_equals);
syn::custom_keyword!(ty_implements);

#[derive(Clone)]
pub struct BundleInput {
    pub modes_per_input: Vec<BundleArgMode>,
    pub imported: Vec<BundleImportedInfo>,
}

#[derive(Clone)]
pub enum BundleArgMode {
    Ref,
    Mut,
    Bundle,
}

#[derive(Clone)]
pub enum BundleImportedInfo {
    Equals(Ident, Type),
    Implements(Ident, Punctuated<CapabilityDeclInherited, Token![,]>),
}

impl Parse for BundleArgMode {
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

impl ToTokens for BundleArgMode {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let span = proc_macro2::Span::call_site();
        match self {
            BundleArgMode::Ref => Token![ref](span).to_tokens(tokens),
            BundleArgMode::Mut => Token![mut](span).to_tokens(tokens),
            BundleArgMode::Bundle => bundle(span).to_tokens(tokens),
        }
    }
}

impl Parse for BundleImportedInfo {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<ty_implements>().is_ok() {
            todo!()
        } else if input.parse::<ty_equals>().is_ok() {
            todo!()
        } else {
            Err(input.error("unknown bundle imported info header"))
        }
    }
}
