use std::collections::HashMap;

use passing_macro::{begin_import, export, import, unique_ident, LazyImportGroup};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Error, LitBool, LitStr};

mod ir;
mod syntax;
mod util;

use crate::{
    ir::{make_def_parser, BundleDef, BundleMemberDef, ComponentDef, GenericItemDef, ItemDef},
    syntax::{CapDecl, CapDeclBundleElement, CapMacroArg, CxMacroArg, PlainPath},
    util::SynArray,
};

#[proc_macro]
pub fn cap(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as CapMacroArg);
    let mut output = TokenStream::new();

    for item in &input.list {
        output.extend(quote! { ::cap::__cap_single! { #item } });
    }

    output.into()
}

#[doc(hidden)]
#[proc_macro]
pub fn __cap_single(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let me = &quote! { ::cap::__ra_cap_single_hack };

    begin_import(me, input, |input| {
        // Parse the input
        let CapDecl {
            vis,
            name,
            generics,
            kind,
        } = &match syn::parse2::<CapDecl>(input) {
            Ok(input) => input,
            Err(err) => return err.into_compile_error(),
        };

        // Handle each kind of definition
        match kind {
            syntax::CapDeclKind::CompTy(_, comp) => {
                let id = unique_ident(name.span());
                let (mod_id, exporter) = export(
                    vis,
                    name,
                    ItemDef::Component(ComponentDef {
                        id,
                        name: LitStr::new(&name.to_string(), Span::call_site()),
                        is_trait: LitBool::new(false, Span::call_site()),
                        generics: generics.iter().cloned().collect(),
                    }),
                );

                quote! {
                    #vis mod #mod_id {
                        #[allow(unused)]
                        use super::*;

                        #vis type CompTy = #comp;
                    }

                    #exporter
                }
            }
            syntax::CapDeclKind::CompTrait(_, comp) => {
                if !generics.is_empty() {
                    return Error::new(
                        generics.span(),
                        "trait components cannot have generic parameters currently",
                    )
                    .into_compile_error();
                }

                let id = unique_ident(name.span());
                let (mod_id, exporter) = export(
                    vis,
                    name,
                    ItemDef::Component(ComponentDef {
                        id,
                        name: LitStr::new(&name.to_string(), Span::call_site()),
                        is_trait: LitBool::new(true, Span::call_site()),
                        generics: SynArray::from_iter([]),
                    }),
                );

                quote! {
                    #vis mod #mod_id {
                        #[allow(unused)]
                        use super::*;

                        #vis trait CompTy: #comp {}

                        impl<T: ?Sized + #comp> CompTy for T {}
                    }

                    #exporter
                }
            }
            syntax::CapDeclKind::Bundle(_, bundle) => {
                if !generics.is_empty() {
                    return Error::new(
                        generics.span(),
                        "bundles cannot have generic parameters currently",
                    )
                    .into_compile_error();
                }

                // Load all members
                let mut errors = TokenStream::new();
                let mut group = LazyImportGroup::new();

                let members = bundle
                    .iter()
                    .map(|member| (member, GenericItemDef::new(&mut group, member.path())))
                    .collect::<Vec<_>>();

                eval_group(group, &mut errors);

                if !errors.is_empty() {
                    return errors;
                }

                // Collect the complete set of bundle fields
                let mut fields = HashMap::new();
                let mut re_exports = TokenStream::new();

                for (member, def) in members {
                    match member {
                        CapDeclBundleElement::Component(mutability, full_path) => {
                            // Ensure that this is, indeed, a component.
                            let ItemDef::Component(def) = def.base_def.into_inner() else {
                                errors.extend(
                                    Error::new(
                                        full_path.span(),
                                        "expected component declaration, found bundle declaration",
                                    )
                                    .into_compile_error(),
                                );
                                continue;
                            };

                            // Handle its generics
                            let base_path = &full_path.base;

                            fields
                                .entry(def.id.to_string())
                                .or_insert_with(|| {
                                    let re_exported_as = unique_ident(Span::call_site());

                                    re_exports.extend(quote! {
                                        #vis use #base_path::CompTy as #re_exported_as;
                                    });

                                    BundleMemberDef {
                                        id: def.id.clone(),
                                        name: def.name.clone(),
                                        is_mutable: LitBool::new(
                                            mutability.is_mutable(),
                                            Span::call_site(),
                                        ),
                                        is_trait: def.is_trait.clone(),
                                        re_exported_as,
                                        last_applied_generic: None,
                                    }
                                })
                                .is_mutable
                                .value |= mutability.is_mutable();
                        }
                        CapDeclBundleElement::Bundle(path) => {
                            let ItemDef::Bundle(def) = def.base_def.into_inner() else {
                                errors.extend(
                                    Error::new(
                                        path.span(),
                                        "expected bundle declaration, found component declaration",
                                    )
                                    .into_compile_error(),
                                );
                                continue;
                            };

                            for def in &def.members.contents {
                                fields
                                    .entry(def.id.to_string())
                                    .or_insert_with(|| {
                                        let its_reexport = &def.re_exported_as;
                                        let re_exported_as = unique_ident(Span::call_site());

                                        re_exports.extend(
											quote! { #vis use #path::#its_reexport as #re_exported_as; },
										);

                                        BundleMemberDef {
                                            id: def.id.clone(),
                                            name: def.name.clone(),
                                            is_mutable: def.is_mutable.clone(),
                                            is_trait: def.is_trait.clone(),
                                            re_exported_as,
                                            last_applied_generic: None,
                                        }
                                    })
                                    .is_mutable
                                    .value |= def.is_mutable.value;
                            }
                        }
                    }
                }

                if !errors.is_empty() {
                    return errors;
                }

                // Define bundle
                let mut ty_bundle_decl_body = TokenStream::new();
                let mut ty_bundle_generic_decls = TokenStream::new();
                let mut ty_bundle_generic_tup_fwds = TokenStream::new();
                let mut ty_bundle_generic_body_fwds = TokenStream::new();
                let mut bundle_fields = TokenStream::new();

                for field in fields.values() {
                    let id = &field.id;
                    let re_exported_as = &field.re_exported_as;
                    let mutability = if field.is_mutable.value {
                        quote! { mut }
                    } else {
                        quote! {}
                    };

                    if field.is_trait.value {
                        ty_bundle_decl_body.extend(quote! {
                            #[allow(non_camel_case_types)]
                            type #id: ?Sized + #re_exported_as;
                        });

                        ty_bundle_generic_decls.extend(quote! {
                            #id: ?Sized + #re_exported_as,
                        });

                        ty_bundle_generic_tup_fwds.extend(quote! {
                            ::core::marker::PhantomData<#id>,
                        });

                        ty_bundle_generic_body_fwds.extend(quote! {
                            type #id = #id;
                        });

                        bundle_fields.extend(quote! {
                            #vis #id: &'a #mutability M::#id,
                        });
                    } else {
                        bundle_fields.extend(quote! {
                            #vis #id: &'a #mutability #re_exported_as,
                        });
                    }
                }

                let (mod_id, exporter) = export(
                    vis,
                    name,
                    ItemDef::Bundle(BundleDef {
                        members: SynArray::from_iter(fields.into_values()),
                    }),
                );

                quote! {
                    #exporter

                    #vis mod #mod_id {
                        #[allow(unused_imports)]
                        use super::*;

                        pub trait TyBundle: ::core::marker::Sized {
                            #ty_bundle_decl_body
                        }

                        #[allow(non_camel_case_types)]
                        impl<#ty_bundle_generic_decls> TyBundle for (#ty_bundle_generic_tup_fwds) {
                            #ty_bundle_generic_body_fwds
                        }

                        pub struct Bundle<'a, M: TyBundle> {
                            #vis _ty: ::core::marker::PhantomData<fn(&'a ()) -> M>,
                            #bundle_fields
                        }

                        #re_exports
                    }
                }
            }
        }
    })
    .into()
}

#[proc_macro]
pub fn cx(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let me = &quote! { ::cap::__ra_cx_hack };

    let out = begin_import(me, input, |input| {
        let input = match syn::parse2::<CxMacroArg>(input) {
            Ok(input) => input,
            Err(err) => return err.into_compile_error(),
        };

        match input {
            CxMacroArg::Fetch(input) => {
                let full_path = &input.path;
                let base_path = &full_path.base;
                let target = &input.expr;
                let info = match syn::parse2::<ItemDef>(import(base_path).eval()) {
                    Ok(info) => info,
                    Err(_) => {
                        return Error::new(full_path.span(), "expected a cap item")
                            .into_compile_error();
                    }
                };

                match info {
                    ItemDef::Component(info) => {
                        let prefix = get_reborrow_prefix(input.optional_mut.is_some());
                        let id = &info.id;

                        quote! { #prefix #target.#id }
                    }
                    ItemDef::Bundle(info) => {
                        if input.optional_mut.is_some() {
                            return Error::new(
                                input.optional_mut.span(),
                                "cannot use mut on a bundle",
                            )
                            .into_compile_error();
                        }

                        let field_getters = info.members.iter().map(|member| {
                            let prefix = get_reborrow_prefix(member.is_mutable.value);
                            let id = &member.id;

                            quote! { #id: #prefix #target.#id }
                        });

                        construct_bundle(base_path, &info, field_getters)
                    }
                }
            }
            CxMacroArg::Construct(input) => {
                // Fetch the items of interest
                let mut errors = TokenStream::new();
                let mut group = LazyImportGroup::new();

                let info = group.import(&input.path, make_def_parser(&input.path));

                let fields = input
                    .fields
                    .iter()
                    .map(|field| {
                        (
                            field,
                            group.import(&field.path.base, make_def_parser(&field.path.base)),
                        )
                    })
                    .collect::<Vec<_>>();

                eval_group(group, &mut errors);

                if !errors.is_empty() {
                    return errors;
                }

                // Enure that info is a bundle
                let info = match info.into_inner() {
                    ItemDef::Bundle(info) => info,
                    ItemDef::Component(_) => {
                        return Error::new(input.path.span(), "expected bundle, found component")
                            .to_compile_error()
                    }
                };

                // Determine where we'll get the data from
                #[derive(Debug)]
                enum FieldEntry {
                    Missing,
                    Present(TokenStream, Span),
                    Doubled(Vec<Span>),
                }

                let mut fetch_map = info
                    .members
                    .iter()
                    .map(|v| {
                        (
                            v.id.clone(),
                            (v.is_mutable.value, &v.name, FieldEntry::Missing),
                        )
                    })
                    .collect::<HashMap<_, _>>();

                for (field_req, field_info) in &fields {
                    match &**field_info {
                        ItemDef::Component(field_info) => {
                            if let Some(spread) = field_req.spread {
                                errors.extend(
                                    Error::new(spread.span(), "cannot spread a component")
                                        .into_compile_error(),
                                );
                            }

                            let Some((_, _, req)) = fetch_map.get_mut(&field_info.id) else {
                                continue;
                            };

                            match req {
                                FieldEntry::Missing => {
                                    *req = FieldEntry::Present(
                                        field_req.take_from.to_token_stream(),
                                        field_req.path.span(),
                                    );
                                }
                                FieldEntry::Present(_from, span) => {
                                    *req = FieldEntry::Doubled(vec![*span, field_req.path.span()]);
                                }
                                FieldEntry::Doubled(overlaps) => overlaps.push(field_req.span()),
                            }
                        }
                        ItemDef::Bundle(field_info) => {
                            if field_req.spread.is_some() {
                                for field_info in &*field_info.members {
                                    let Some((_, _, req)) = fetch_map.get_mut(&field_info.id)
                                    else {
                                        continue;
                                    };

                                    if let FieldEntry::Missing = req {
                                        let get_path_full = &field_req.path;
                                        let get_reexport = &field_info.re_exported_as;
                                        let value_getter = &field_req.take_from;

                                        *req = FieldEntry::Present(
                                            quote! {{
                                                use #get_path_full::#get_reexport as TARGET;
                                                #value_getter
                                            }},
                                            field_req.path.span(),
                                        );
                                    }
                                }
                            } else {
                                for field_info in &*field_info.members {
                                    let Some((is_mutable, _, req)) =
                                        fetch_map.get_mut(&field_info.id)
                                    else {
                                        continue;
                                    };

                                    match req {
                                        FieldEntry::Missing => {
                                            let prefix = get_reborrow_prefix(*is_mutable);
                                            let place = &field_req.take_from; // TODO: Ensure that this is, indeed, a place.
                                            let id = &field_info.id;

                                            *req = FieldEntry::Present(
                                                quote! { #prefix #place.#id },
                                                field_req.path.span(),
                                            );
                                        }
                                        FieldEntry::Present(_from, span) => {
                                            *req = FieldEntry::Doubled(vec![
                                                *span,
                                                field_req.path.span(),
                                            ]);
                                        }
                                        FieldEntry::Doubled(overlaps) => {
                                            overlaps.push(field_req.span())
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Construct the structure
                let mut fields = Vec::new();

                for (id, (_, name, entry)) in &fetch_map {
                    // TODO: Improve diagnostics
                    match entry {
                        FieldEntry::Missing => errors.extend(
                            Error::new(
                                input.brace.span.join(),
                                &format!("missing field {}", name.value()),
                            )
                            .into_compile_error(),
                        ),
                        FieldEntry::Present(getter, _) => fields.push(quote! { #id: #getter }),
                        FieldEntry::Doubled(spans) => {
                            for &span in spans {
                                errors.extend(
                                    Error::new(span, "field source ambiguity").into_compile_error(),
                                )
                            }
                        }
                    }
                }

                if !errors.is_empty() {
                    return errors;
                }

                construct_bundle(&input.path, &info, fields.into_iter())
            }
        }
    });

    // HACK: This forces the expression to be parsed like an expression. I don't know why this is
    // necessary.
    quote! {{ #out }}.into()
}

// HACK: Rust-analyzer does not like proc-macros which call themselves so we create a second alias
//  to `cap_single` to get around this limitation.
#[proc_macro]
#[doc(hidden)]
pub fn __ra_cap_single_hack(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    __cap_single(input)
}

#[proc_macro]
#[doc(hidden)]
pub fn __ra_cx_hack(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    cx(input)
}

fn construct_bundle(
    base_path: &PlainPath,
    info: &BundleDef,
    body: impl Iterator<Item = TokenStream>,
) -> TokenStream {
    let infer_bounds = info.members.iter().filter_map(|member| {
        member.is_trait.value.then(|| {
            quote! { ::core::marker::PhantomData<_>, }
        })
    });

    quote! {
        #base_path::Bundle::<(#(#infer_bounds)*)> {
            _ty: ::core::marker::PhantomData,
            #(#body),*
        }
    }
}

fn get_reborrow_prefix(is_mutable: bool) -> TokenStream {
    if is_mutable {
        quote! { &mut * }
    } else {
        quote! { &* }
    }
}

fn eval_group(mut group: LazyImportGroup<'_, syn::Error>, errors: &mut TokenStream) {
    if let Some(err) = group.eval() {
        errors.extend(err.iter().map(syn::Error::to_compile_error));
    }
}
