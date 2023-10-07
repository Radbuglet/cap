use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Ident, Path, Token, Type, TypeParamBound, Visibility,
};

use crate::magic::{structured, Nop, StructuredArray};

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
            CapDeclInheritedElement::Ref(_) => ResolvedBundleExpectedMode::Ref(Nop),
            CapDeclInheritedElement::Mut(_) => ResolvedBundleExpectedMode::Mut(Nop),
            CapDeclInheritedElement::Bundle(_) => ResolvedBundleExpectedMode::Bundle(Nop),
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

// === CapProbe === //

structured! {
    #[derive(Clone)]
    pub struct CapProbeArgs {
        pub expected: StructuredArray<ResolvedBundleExpectedMode>,
    }

    #[derive(Clone)]
    pub enum ResolvedBundleExpectedMode {
        Ref(Nop),
        Mut(Nop),
        Bundle(Nop),
    }

    #[derive(Clone)]
    pub enum CapProbeResult {
        ExactType(Nop),
        ImplsTrait(Nop),
        Bundle(CapProbeBundleResult),
    }

    #[derive(Clone)]
    pub struct CapProbeBundleResult {

    }
}
