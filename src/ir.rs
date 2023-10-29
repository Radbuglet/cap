use passing_macro::{ComputedLazyImport, LazyImportGroup};
use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
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
        pub last_applied_generic: Option<Ident>,
    }
}

// === Ephemeral IR === //

pub struct GenericItemDef<'a> {
    pub full_path: &'a GenericPlainPath,
    pub base_def: ComputedLazyImport<'a, ItemDef>,
    pub generics: Vec<GenericItemDef<'a>>,
}

impl<'a> GenericItemDef<'a> {
    pub fn new(
        group: &mut LazyImportGroup<'a, syn::Error>,
        full_path: &'a GenericPlainPath,
    ) -> Self {
        Self {
            full_path,
            base_def: group.import(&full_path.base, make_def_parser(&full_path.base)),
            generics: full_path
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
        self.comp_validate_inner(true, errors)
    }

    fn comp_validate_inner(&self, is_root: bool, errors: &mut TokenStream) -> Result<(), ()> {
        match &*self.base_def {
            ItemDef::Component(comp) => {
                self.validate_generics(comp, errors)?;

                if comp.is_trait.value && !is_root {
                    errors.extend(
                        Error::new(
                            self.full_path.span(),
                            "trait component can only appear in root of component",
                        )
                        .into_compile_error(),
                    );
                    return Err(());
                }

                for para in &self.generics {
                    para.comp_validate_inner(false, errors)?;
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

    fn comp_id_raw(base_id: Ident, generics: impl IntoIterator<Item = Ident>) -> Ident {
        combine_idents(Span::call_site(), [base_id].into_iter().chain(generics))
    }

    pub fn comp_id(&self) -> Ident {
        let base_id = match &*self.base_def {
            ItemDef::Component(comp) => comp.id.clone(),
            ItemDef::Bundle(_) => unreachable!(),
        };

        Self::comp_id_raw(base_id, self.generics.iter().map(Self::comp_id))
    }

    pub fn comp_use_path(&self) -> TokenStream {
        let base_path = self.base_path();
        let generics = self.generics.iter().map(|para| para.comp_use_path());

        quote! { #base_path::CompTy<#(#generics,)*> }
    }

    pub fn comp_name(&self) -> LitStr {
        let mut buffer = String::new();
        self.comp_name_inner(&mut buffer);
        LitStr::new(&buffer, Span::call_site())
    }

    fn comp_name_inner(&self, buffer: &mut String) {
        let base_def = self.comp_def();

        buffer.push_str(&base_def.name.value());
        if !self.generics.is_empty() {
            buffer.push('<');
            let mut is_subsequent = false;
            for para in &self.generics {
                if is_subsequent {
                    buffer.push_str(", ");
                }
                is_subsequent = true;
                para.comp_name_inner(buffer);
            }
            buffer.push('>');
        }
    }

    // === Bundle handling === //

    pub fn bundle_validate_and_collect(
        &self,
        errors: &mut TokenStream,
    ) -> Result<Vec<BundleMemberDefWithReExporter>, ()> {
        let members = self.bundle_validate_and_collect_inner(errors)?;

        if members.is_empty() {
            errors.extend(
                Error::new(self.full_path.span(), "expected bundle, found component")
                    .into_compile_error(),
            );
            return Err(());
        }

        Ok(members)
    }

    fn bundle_validate_and_collect_inner(
        &self,
        errors: &mut TokenStream,
    ) -> Result<Vec<BundleMemberDefWithReExporter>, ()> {
        match &*self.base_def {
            ItemDef::Component(comp) => {
                self.validate_generics(comp, errors)?;

                let mut chosen_bundle = None;
                let mut paras_with_members = Vec::new();

                for (i, para) in self.generics.iter().enumerate() {
                    let members = para.bundle_validate_and_collect_inner(errors)?;
                    if !members.is_empty() {
                        chosen_bundle = Some((i, members));
                        paras_with_members.push(para);
                    }
                }

                if paras_with_members.len() > 1 {
                    for para in paras_with_members {
                        errors.extend(
                            Error::new(para.full_path.span(), "only one generic component parameter can be parameterized by a bundle")
                                .into_compile_error(),
                        );
                    }
                    return Err(());
                }

                if let Some((chosen_bundle_idx, chosen_bundle)) = chosen_bundle {
                    for member in &chosen_bundle {
                        if member.data.is_trait.value {
                            errors.extend(
                                Error::new(
                                    self.base_path().span(),
                                    "trait component can only appear in root of component",
                                )
                                .into_compile_error(),
                            );
                        }
                    }

                    Ok(chosen_bundle
                        .into_iter()
                        .map(|member| {
                            dbg!(&member.data.last_applied_generic);
                            dbg!(&comp.id);
                            if member.data.last_applied_generic.as_ref() == Some(&comp.id) {
                                return member;
                            }

                            let id = Self::comp_id_raw(
                                comp.id.clone(),
                                self.generics
                                    .iter()
                                    .enumerate()
                                    .map(|(other_para_idx, para)| {
                                        if chosen_bundle_idx == other_para_idx {
                                            member.data.id.clone()
                                        } else {
                                            para.comp_id()
                                        }
                                    }),
                            );
                            let mut name = comp.name.value();
                            if !self.generics.is_empty() {
                                name.push('<');

                                let mut is_subsequent = false;
                                for (other_para_idx, para) in self.generics.iter().enumerate() {
                                    if is_subsequent {
                                        name.push_str(", ");
                                    }
                                    is_subsequent = true;

                                    if chosen_bundle_idx == other_para_idx {
                                        name.push_str(&member.data.name.value());
                                    } else {
                                        name.push_str(&para.comp_name().value());
                                    }
                                }

                                name.push('>');
                            }

                            let from_path_generics =
                                self.generics
                                    .iter()
                                    .enumerate()
                                    .map(|(other_para_idx, para)| {
                                        if chosen_bundle_idx == other_para_idx {
                                            member.from_path.clone()
                                        } else {
                                            para.comp_use_path()
                                        }
                                    });

                            let comp_base_adt = self.base_path();

                            BundleMemberDefWithReExporter {
                                data: BundleMemberDef {
                                    id,
                                    name: LitStr::new(&name, Span::call_site()),
                                    is_mutable: member.data.is_mutable,
                                    is_trait: member.data.is_trait,
                                    last_applied_generic: Some(comp.id.clone()),
                                },
                                from_path: quote! { #comp_base_adt::CompTy<#(#from_path_generics,)*> },
                                from_path_is_type: true,
                            }
                        })
                        .collect())
                } else {
                    Ok(Vec::new())
                }
            }
            ItemDef::Bundle(bundle) => Ok(bundle
                .members
                .iter()
                .map(|member| {
                    let base_path = self.base_path();
                    let id = &member.id;
                    BundleMemberDefWithReExporter {
                        data: member.clone(),
                        from_path: quote! { #base_path::#id },
                        from_path_is_type: false,
                    }
                })
                .collect()),
        }
    }

    // === Helpers === //

    pub fn comp_def(&self) -> &ComponentDef {
        match &*self.base_def {
            ItemDef::Component(base_def) => base_def,
            ItemDef::Bundle(_) => unreachable!(),
        }
    }

    fn validate_generics(&self, comp: &ComponentDef, errors: &mut TokenStream) -> Result<(), ()> {
        if comp.generics.len() == self.generics.len() {
            Ok(())
        } else {
            if self.generics.len() < comp.generics.len() {
                errors.extend(
                    Error::new(
                        self.full_path.span(),
                        format!(
                            "missing generic parameters: {}",
                            format_list(comp.generics.iter().skip(self.generics.len()), &", ")
                        ),
                    )
                    .into_compile_error(),
                );
            } else {
                for param in &self.generics[comp.generics.len()..] {
                    errors.extend(
                        Error::new(param.full_path.span(), "unnecessary generic parameter")
                            .into_compile_error(),
                    );
                }
            }

            Err(())
        }
    }
}

pub struct BundleMemberDefWithReExporter {
    pub data: BundleMemberDef,
    pub from_path: TokenStream,
    pub from_path_is_type: bool,
}

// === Helpers === //

pub fn make_def_parser(span: impl Spanned) -> impl FnOnce(TokenStream) -> syn::Result<ItemDef> {
    let span = span.span();
    move |v| syn::parse2(v).map_err(|_| syn::Error::new(span, "expected cap item"))
}
