use passing_macro::{ComputedLazyImport, LazyImportGroup};
use proc_macro2::{Ident, Span, TokenStream};
use syn::{spanned::Spanned, Error, LitBool, LitStr};

use crate::{
    syntax::{GenericPlainPath, PlainPath},
    util::{combine_idents, format_list, structured, SynArray},
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
        pub name: LitStr,
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
        pub name: LitStr,
        pub is_mutable: LitBool,
        pub is_trait: LitBool,
        pub re_exported_as: Ident,
        pub last_applied_generic: Option<Ident>,
    }
}

// === Ephemeral IR === //

pub struct GenericItemDef<'a> {
    pub full_path: &'a GenericPlainPath,
    pub base_def: ComputedLazyImport<'a, ItemDef>,
    pub params: Vec<GenericItemDef<'a>>,
}

impl<'a> GenericItemDef<'a> {
    pub fn new(group: &mut LazyImportGroup<'a, syn::Error>, path: &'a GenericPlainPath) -> Self {
        Self {
            full_path: &path,
            base_def: group.import(&path.base, make_def_parser(&path.base)),
            params: path
                .iter_generics()
                .map(|part| Self::new(group, part))
                .collect(),
        }
    }

    pub fn base_path(&self) -> &'a PlainPath {
        &self.full_path.base
    }

    pub fn validate_comp(&self, errors: &mut TokenStream) {
        match &*self.base_def {
            ItemDef::Component(comp) => {
                if comp.generics.len() != self.params.len() {
                    if self.params.len() < comp.generics.len() {
                        errors.extend(
                            Error::new(
                                self.full_path.span(),
                                format!(
                                    "missing generic parameters: {}",
                                    format_list(
                                        comp.generics.iter().skip(self.params.len() - 1),
                                        &','
                                    )
                                ),
                            )
                            .into_compile_error(),
                        );
                    } else {
                        for param in &self.params[comp.generics.len()..] {
                            errors.extend(
                                Error::new(param.full_path.span(), "unnecessary generic parameter")
                                    .into_compile_error(),
                            );
                        }
                    }
                }

                for para in &self.params {
                    para.validate_comp(errors);
                }
            }
            ItemDef::Bundle(_) => errors.extend(
                Error::new(
                    self.full_path.span(),
                    "expected a component, bound a bundle",
                )
                .into_compile_error(),
            ),
        }
    }

    pub fn compute_id(&self) -> Ident {
        let base_id = match &*self.base_def {
            ItemDef::Component(comp) => comp.id.clone(),
            ItemDef::Bundle(_) => unreachable!(),
        };

        combine_idents(
            Span::call_site(),
            [base_id]
                .into_iter()
                .chain(self.params.iter().map(|para| para.compute_id())),
        )
    }

    pub fn compute_name(&self) -> LitStr {
        let mut buffer = String::new();
        self.compute_name_inner(&mut buffer);
        LitStr::new(&buffer, Span::call_site())
    }

    fn compute_name_inner(&self, buffer: &mut String) {
        let ItemDef::Component(base_def) = &*self.base_def else {
            unreachable!()
        };

        buffer.push_str(&base_def.name.value());
        if !self.params.is_empty() {
            buffer.push('<');
            for para in &self.params {
                para.compute_name_inner(buffer);
            }
            buffer.push('>');
        }
    }
}

// === Helpers === //

pub fn make_def_parser(span: impl Spanned) -> impl FnOnce(TokenStream) -> syn::Result<ItemDef> {
    let span = span.span();
    move |v| syn::parse2(v).map_err(|_| syn::Error::new(span, "expected cap item"))
}
