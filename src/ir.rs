use passing_macro::{ComputedLazyImport, LazyImportGroup};
use proc_macro2::{Ident, TokenStream};
use syn::{spanned::Spanned, LitBool};

use crate::{
    syntax::GenericPlainPath,
    util::{structured, SynArray},
};

// === Base IR === //

structured! {
    #[derive(Clone)]
    pub enum ItemDef {
        Component(ComponentDef),
        Bundle(BundleDef),
    }

    #[derive(Clone)]
    pub struct ComponentDef {
        pub id: Ident,
        pub name: TokenStream,
        pub is_trait: LitBool,
        pub generics: SynArray<Ident>,
    }

    #[derive(Clone)]
    pub struct BundleDef {
        pub members: SynArray<BundleMemberDef>,
    }

    #[derive(Clone)]
    pub struct BundleMemberDef {
        pub id: Ident,
        pub name: TokenStream,
        pub is_mutable: LitBool,
        pub is_trait: LitBool,
        pub re_exported_as: Ident,
        pub last_applied_generic: Option<Ident>,
    }
}

// === Ephemeral IR === //

pub struct GenericItemDef<'a> {
    pub base: ComputedLazyImport<'a, ItemDef>,
    pub params: Vec<GenericItemDef<'a>>,
}

impl<'a> GenericItemDef<'a> {
    pub fn new(group: &mut LazyImportGroup<'a, syn::Error>, path: &'a GenericPlainPath) -> Self {
        Self {
            base: group.import(&path.base, make_def_parser(&path.base)),
            params: path
                .iter_generics()
                .map(|part| Self::new(group, part))
                .collect(),
        }
    }
}

// === Helpers === //

pub fn make_def_parser(span: impl Spanned) -> impl FnOnce(TokenStream) -> syn::Result<ItemDef> {
    let span = span.span();
    move |v| syn::parse2(v).map_err(|_| syn::Error::new(span, "expected cap item"))
}
