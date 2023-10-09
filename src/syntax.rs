use proc_macro2::{Group, TokenTree};
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token::Brace,
    Expr, Ident, LitBool, LitInt, Path, Token, Type, TypeParamBound, Visibility,
};

use crate::magic::{structured, ImportedMacroInfo, Nop, SynArray};

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
pub enum CxMacroArg {
    Fetch(CxMacroArgFetch),
    Construct(CxMacroArgConstruct),
}

impl Parse for CxMacroArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let place = input.parse::<PlaceExpr>()?;

        match input.parse::<Token![=>]>() {
            Ok(arr) => Ok(Self::Fetch(CxMacroArgFetch {
                expr: place,
                arr,
                optional_mut: input.parse()?,
                path: input.parse()?,
            })),
            Err(err) => {
                let Some(path) = place.as_path() else {
                    return Err(err);
                };

                let (brace, input) = match parse_brace(input) {
                    Ok(inner) => inner,
                    Err(_) => return Err(input.error("expected => or {")),
                };

                Ok(Self::Construct(CxMacroArgConstruct {
                    path: path.clone(),
                    brace,
                    fields: Punctuated::parse_terminated(&input)?,
                }))
            }
        }
    }
}

impl ToTokens for CxMacroArg {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            CxMacroArg::Fetch(v) => v.to_tokens(tokens),
            CxMacroArg::Construct(v) => v.to_tokens(tokens),
        }
    }
}

fn parse_brace(input: ParseStream) -> syn::Result<(Brace, ParseBuffer)> {
    let stream;
    Ok((braced!(stream in input), stream))
}

// Fetch
#[derive(Clone)]
pub struct CxMacroArgFetch {
    pub expr: PlaceExpr,
    pub arr: Token![=>],
    pub optional_mut: Option<Token![mut]>,
    pub path: Path,
}

impl Parse for CxMacroArgFetch {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            expr: input.parse()?,
            arr: input.parse()?,
            optional_mut: input.parse()?,
            path: input.parse()?,
        })
    }
}

impl ToTokens for CxMacroArgFetch {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.expr.to_tokens(tokens);
        self.arr.to_tokens(tokens);
        self.optional_mut.to_tokens(tokens);
        self.path.to_tokens(tokens);
    }
}

// Construct
#[derive(Clone)]
pub struct CxMacroArgConstruct {
    pub path: Path,
    pub brace: Brace,
    pub fields: Punctuated<CxMacroArgConstructField, Token![,]>,
}

#[derive(Clone)]
pub struct CxMacroArgConstructField {
    pub path: Path,
    pub equals: Token![=],
    pub take_from: Expr,
}

impl Parse for CxMacroArgConstruct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let braced;
        Ok(Self {
            path: input.parse()?,
            brace: braced!(braced in input),
            fields: Punctuated::parse_terminated(&braced)?,
        })
    }
}

impl ToTokens for CxMacroArgConstruct {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.path.to_tokens(tokens);
        tokens.extend([TokenTree::Group(Group::new(
            proc_macro2::Delimiter::Brace,
            self.fields.to_token_stream(),
        ))]);
    }
}

impl Parse for CxMacroArgConstructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            path: input.parse()?,
            equals: input.parse()?,
            take_from: input.parse()?,
        })
    }
}

impl ToTokens for CxMacroArgConstructField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.path.to_tokens(tokens);
        self.equals.to_tokens(tokens);
        self.take_from.to_tokens(tokens);
    }
}

// Helpers
#[derive(Clone)]
pub struct PlaceExpr {
    parts: Punctuated<PathOrInt, Token![.]>,
}

impl PlaceExpr {
    pub fn as_path(&self) -> Option<&Path> {
        if self.parts.len() != 1 {
            return None;
        }

        let path = match &self.parts[0] {
            PathOrInt::Path(path) => path,
            PathOrInt::Int(_) => return None,
        };

        Some(path)
    }
}

impl Parse for PlaceExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            parts: Punctuated::parse_separated_nonempty(input)?,
        })
    }
}

impl ToTokens for PlaceExpr {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.parts.to_tokens(tokens);
    }
}

#[derive(Clone)]
pub enum PathOrInt {
    Path(Path),
    Int(LitInt),
}

impl Parse for PathOrInt {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(ident) = input.parse::<Path>() {
            Ok(Self::Path(ident))
        } else if let Ok(lit) = input.parse::<LitInt>() {
            Ok(Self::Int(lit))
        } else {
            Err(input.error("expected a path, an identifier, self, or a literal integer"))
        }
    }
}

impl ToTokens for PathOrInt {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            PathOrInt::Path(path) => path.to_tokens(tokens),
            PathOrInt::Int(lit) => lit.to_tokens(tokens),
        }
    }
}

// === CxProbe === //

pub type CxFetchProbeInfo = ImportedMacroInfo<CxFetchProbeSupplied, CxFetchProbeCollected>;
pub type CxFetchProbeSupplied = CxMacroArgFetch;
pub type CxFetchProbeCollected = CapProbeEntry;

pub type CxConstructProbeInfo =
    ImportedMacroInfo<CxConstructProbeSupplied, CxConstructProbeCollected>;

pub type CxConstructProbeSupplied = CxMacroArgConstruct;
pub type CxConstructProbeCollected = CapProbeEntry;
