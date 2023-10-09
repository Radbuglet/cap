use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Ident, LitBool, Path, Token, Type, TypeParamBound, Visibility,
};

use crate::magic::{structured, Nop, SynArray};

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
    pub fn mode(&self) -> BundleElementMode {
        match self {
            CapDeclInheritedElement::Ref(_) => BundleElementMode::Ref(Nop),
            CapDeclInheritedElement::Mut(_) => BundleElementMode::Mut(Nop),
            CapDeclInheritedElement::Bundle(_) => BundleElementMode::Bundle(Nop),
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
        if input.parse::<Token![ref]>().is_ok() {
            Ok(Self::Ref(input.parse()?))
        } else if input.parse::<Token![mut]>().is_ok() {
            Ok(Self::Mut(input.parse()?))
        } else {
            Ok(Self::Bundle(input.parse()?))
        }
    }
}

// === CapProbe === //

structured! {
    // Supplied
    #[derive(Clone)]
    pub struct CapProbeSupplied {
        pub name: Ident,
        pub visibility: Visibility,
        pub expected: SynArray<CapProbeArgReq>,
    }

    #[derive(Clone)]
    pub struct CapProbeArgReq {
        pub path: Path,
        pub mode: BundleElementMode,
    }

    #[derive(Clone)]
    pub enum BundleElementMode {
        Ref(Nop),
        Mut(Nop),
        Bundle(Nop),
    }

    // Collected
    #[derive(Clone)]
    pub enum CapProbeEntry {
        Component(CapProbeComponent),
        Bundle(CapProbeBundle),
    }

    #[derive(Clone)]
    pub struct CapProbeComponent {
        pub id: Ident,
        pub is_trait: LitBool,
    }

    #[derive(Clone)]
    pub struct CapProbeBundle {
        pub members: SynArray<CapProbeBundleMember>,
    }

    #[derive(Clone)]
    pub struct CapProbeBundleMember {
        pub id: Ident,
        pub is_mutable: LitBool,
        pub is_trait: LitBool,
        pub re_exported_as: Ident,
    }
}

// === CxMacroArg === //

#[derive(Clone)]
pub struct CxMacroArg {
    pub expr: Expr, // FIXME: Limit to place expressions
    pub arr: Token![=>],
    pub optional_mut: Option<Token![mut]>,
    pub path: Path,
}

impl Parse for CxMacroArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            expr: input.parse()?,
            arr: input.parse()?,
            optional_mut: input.parse()?,
            path: input.parse()?,
        })
    }
}

impl ToTokens for CxMacroArg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.expr.to_tokens(tokens);
        self.arr.to_tokens(tokens);
        self.optional_mut.to_tokens(tokens);
        self.path.to_tokens(tokens);
    }
}

// === CxProbe === //

pub type CxProbeSupplied = CxMacroArg;
pub type CxProbeCollected = CapProbeEntry;
