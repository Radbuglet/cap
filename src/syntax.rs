use proc_macro2::{Group, TokenStream, TokenTree};
use quote::ToTokens;
use syn::{
    braced,
    ext::IdentExt,
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token::Brace,
    Expr, Ident, LitBool, LitInt, Path, Token, Type, TypeParamBound, Visibility,
};

use crate::util::{structured, SynArray};

// === ItemDef === //

structured! {
    #[derive(Clone)]
    pub enum ItemDef {
        Component(ComponentDef),
        Bundle(BundleDef),
    }

    #[derive(Clone)]
    pub struct ComponentDef {
        pub id: Ident,
        pub name: Ident,
        pub is_trait: LitBool,
    }

    #[derive(Clone)]
    pub struct BundleDef {
        pub members: SynArray<BundleMemberDef>,
    }

    #[derive(Clone)]
    pub struct BundleMemberDef {
        pub id: Ident,
        pub name: Ident,
        pub is_mutable: LitBool,
        pub is_trait: LitBool,
        pub re_exported_as: Ident,
    }
}

// === CapMacroArg === //

#[derive(Clone)]
pub struct CapMacroArg {
    pub list: Punctuated<CapDecl, Token![;]>,
}

#[derive(Clone)]
pub struct CapDecl {
    pub vis: Visibility,
    pub name: Ident,
    pub generics: SimpleGenerics,
    pub kind: CapDeclKind,
}

#[derive(Clone)]
pub enum CapDeclKind {
    CompTy(Token![=], Type),
    CompTrait(Token![:], Punctuated<TypeParamBound, Token![+]>),
    Bundle(Token![=>], Punctuated<CapDeclBundleElement, Token![,]>),
}

#[derive(Clone)]
pub enum CapDeclBundleElement {
    Component(CapDeclBundleElementMut, PlainGenericPath),
    Bundle(Path),
}

#[derive(Clone)]
pub enum CapDeclBundleElementMut {
    Ref(Token![ref]),
    Mut(Token![mut]),
}

impl Parse for CapMacroArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            list: input.parse_terminated(CapDecl::parse, Token![;])?,
        })
    }
}

impl ToTokens for CapMacroArg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.list.to_tokens(tokens);
    }
}

impl Parse for CapDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            vis: input.parse()?,
            name: input.parse()?,
            generics: input.parse()?,
            kind: input.parse()?,
        })
    }
}

impl ToTokens for CapDecl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.vis.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.generics.to_tokens(tokens);
        self.kind.to_tokens(tokens);
    }
}

impl Parse for CapDeclKind {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(sep) = input.parse::<Token![=>]>() {
            Ok(Self::Bundle(
                sep,
                Punctuated::parse_separated_nonempty(input)?,
            ))
        } else if let Ok(sep) = input.parse::<Token![=]>() {
            Ok(Self::CompTy(sep, input.parse()?))
        } else if let Ok(sep) = input.parse::<Token![:]>() {
            Ok(Self::CompTrait(sep, {
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

impl ToTokens for CapDeclKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CapDeclKind::CompTy(sep, v) => {
                sep.to_tokens(tokens);
                v.to_tokens(tokens);
            }
            CapDeclKind::CompTrait(sep, v) => {
                sep.to_tokens(tokens);
                v.to_tokens(tokens);
            }
            CapDeclKind::Bundle(sep, v) => {
                sep.to_tokens(tokens);
                v.to_tokens(tokens);
            }
        }
    }
}

impl Parse for CapDeclBundleElement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(kw) = input.parse::<Token![ref]>() {
            Ok(Self::Component(
                CapDeclBundleElementMut::Ref(kw),
                input.parse()?,
            ))
        } else if let Ok(kw) = input.parse::<Token![mut]>() {
            Ok(Self::Component(
                CapDeclBundleElementMut::Mut(kw),
                input.parse()?,
            ))
        } else {
            Ok(Self::Bundle(input.parse()?))
        }
    }
}

impl ToTokens for CapDeclBundleElement {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CapDeclBundleElement::Component(kw, path) => {
                kw.to_tokens(tokens);
                path.to_tokens(tokens);
            }
            CapDeclBundleElement::Bundle(path) => {
                path.to_tokens(tokens);
            }
        }
    }
}

impl ToTokens for CapDeclBundleElementMut {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            CapDeclBundleElementMut::Ref(kw) => kw.to_tokens(tokens),
            CapDeclBundleElementMut::Mut(kw) => kw.to_tokens(tokens),
        }
    }
}

impl CapDeclBundleElement {
    pub fn path(&self) -> TokenStream {
        match self {
            CapDeclBundleElement::Component(_, path) => path.to_token_stream(),
            CapDeclBundleElement::Bundle(path) => path.to_token_stream(),
        }
    }
}

impl CapDeclBundleElementMut {
    pub fn is_mutable(&self) -> bool {
        matches!(self, CapDeclBundleElementMut::Mut(_))
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
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
    pub spread: Option<Token![...]>,
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
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
            spread: input.parse()?,
            path: input.parse()?,
            equals: input.parse()?,
            take_from: input.parse()?,
        })
    }
}

impl ToTokens for CxMacroArgConstructField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.spread.to_tokens(tokens);
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
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            PathOrInt::Path(path) => path.to_tokens(tokens),
            PathOrInt::Int(lit) => lit.to_tokens(tokens),
        }
    }
}

#[derive(Clone)]
pub struct PlainGenericPath {
    pub root_prefix: Option<Token![::]>,
    pub segments: Punctuated<PlainPathPart, Token![::]>,
    pub generics: SimpleGenerics,
}

#[derive(Clone)]
pub struct PlainPathPart(pub Ident);

#[derive(Clone)]
pub struct SimpleGenerics {
    pub params: Option<(Token![<], Punctuated<Ident, Token![,]>, Token![>])>,
}

impl Parse for PlainGenericPath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            root_prefix: input.parse()?,
            segments: Punctuated::parse_separated_nonempty(input)?,
            generics: input.parse()?,
        })
    }
}

impl ToTokens for PlainGenericPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.root_prefix.to_tokens(tokens);
        self.segments.to_tokens(tokens);
        self.generics.to_tokens(tokens);
    }
}

impl Parse for PlainPathPart {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![crate]) || input.peek(Token![super]) || input.peek(Token![self]) {
            Ok(Self(input.call(Ident::parse_any)?))
        } else if let Ok(ident) = input.parse::<Ident>() {
            Ok(Self(ident))
        } else {
            Err(input.error("expected crate, super, self, or an identifier"))
        }
    }
}

impl ToTokens for PlainPathPart {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

impl Parse for SimpleGenerics {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(open) = input.parse::<Token![<]>() {
            Ok(Self {
                params: Some((
                    open,
                    Punctuated::parse_separated_nonempty(input)?,
                    input.parse::<Token![>]>()?,
                )),
            })
        } else {
            Ok(Self { params: None })
        }
    }
}

impl ToTokens for SimpleGenerics {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some((open, list, close)) = &self.params {
            open.to_tokens(tokens);
            list.to_tokens(tokens);
            close.to_tokens(tokens);
        }
    }
}

impl SimpleGenerics {
    pub fn is_empty(&self) -> bool {
        match &self.params {
            Some((_, v, _)) if v.is_empty() => true,
            None => true,
            _ => false,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Ident> + '_ {
        self.params.iter().flat_map(|(_, v, _)| v.iter())
    }
}
