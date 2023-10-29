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
    pub fn new(
        group: &mut LazyImportGroup<'a, syn::Error>,
        full_path: &'a GenericPlainPath,
    ) -> Self {
        Self {
            full_path,
            base_def: group.import(&full_path.base, make_def_parser(&full_path.base)),
            params: full_path
                .iter_generics()
                .map(|part| Self::new(group, part))
                .collect(),
        }
    }

    pub fn base_path(&self) -> &'a PlainPath {
        &self.full_path.base
    }

    // === Component handling === //

    pub fn comp_validate(&self, errors: &mut TokenStream) -> Result<(), ()> {
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
                                        comp.generics.iter().skip(self.params.len()),
                                        &", "
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

                    return Err(());
                }

                for para in &self.params {
                    para.comp_validate(errors)?;
                }

                Ok(())
            }
            ItemDef::Bundle(_) => {
                errors.extend(
                    Error::new(
                        self.full_path.span(),
                        "expected a component, bound a bundle",
                    )
                    .into_compile_error(),
                );
                Err(())
            }
        }
    }

    pub fn comp_def(&self) -> &ComponentDef {
        match &*self.base_def {
            ItemDef::Component(base_def) => base_def,
            ItemDef::Bundle(_) => unreachable!(),
        }
    }

    pub fn comp_id(&self) -> Ident {
        let base_id = match &*self.base_def {
            ItemDef::Component(comp) => comp.id.clone(),
            ItemDef::Bundle(_) => unreachable!(),
        };

        combine_idents(
            Span::call_site(),
            [base_id]
                .into_iter()
                .chain(self.params.iter().map(|para| para.comp_id())),
        )
    }

    pub fn comp_name(&self) -> LitStr {
        let mut buffer = String::new();
        self.comp_name_inner(&mut buffer);
        LitStr::new(&buffer, Span::call_site())
    }

    fn comp_name_inner(&self, buffer: &mut String) {
        let base_def = self.comp_def();

        buffer.push_str(&base_def.name.value());
        if !self.params.is_empty() {
            buffer.push('<');
            let mut is_subsequent = false;
            for para in &self.params {
                if is_subsequent {
                    buffer.push_str(", ");
                }
                is_subsequent = true;
                para.comp_name_inner(buffer);
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
