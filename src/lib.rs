use std::collections::HashMap;

use passing_macro::{begin_import, export, import, unique_ident};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Error, LitBool, Path};

mod syntax;
mod util;

use crate::syntax::{
    CapDecl, CapMacroArg, ComponentDef, ItemDef,
    {BundleDef, BundleMemberDef, CapDeclBundleElement, CxMacroArg},
};
use crate::util::SynArray;

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
        let CapDecl {
            vis, name, kind, ..
        } = &match syn::parse2::<CapDecl>(input) {
            Ok(input) => input,
            Err(err) => return err.into_compile_error(),
        };

        let mut output = TokenStream::new();

        match kind {
            syntax::CapDeclKind::CompTy(_, comp) => {
                let id = unique_ident(name.span());
                let (mod_id, exporter) = export(
                    vis,
                    name,
                    ItemDef::Component(ComponentDef {
                        id,
                        name: name.clone(),
                        is_trait: LitBool::new(false, Span::call_site()),
                    }),
                );

                output.extend(quote! {
                    #vis mod #mod_id {
                        #[allow(unused)]
                        use super::*;

                        #vis type CompTy = #comp;
                    }

                    #exporter
                });
            }
            syntax::CapDeclKind::CompTrait(_, comp) => {
                let id = unique_ident(name.span());
                let (mod_id, exporter) = export(
                    vis,
                    name,
                    ItemDef::Component(ComponentDef {
                        id,
                        name: name.clone(),
                        is_trait: LitBool::new(true, Span::call_site()),
                    }),
                );

                output.extend(quote! {
                    #vis mod #mod_id {
                        #[allow(unused)]
                        use super::*;

                        #vis trait CompTy: #comp {}

                        impl<T: ?Sized + #comp> CompTy for T {}
                    }

                    #exporter
                });
            }
            syntax::CapDeclKind::Bundle(_, bundle) => {
                // Load all members
                let members = bundle
                    .iter()
                    .map(|member| match member {
                        CapDeclBundleElement::Component(_, path) => (member, import(path)),
                        CapDeclBundleElement::Bundle(path) => (member, import(path)),
                    })
                    .collect::<Vec<_>>();

                // Parse all of them
                let mut has_errors = false;
                let members = members
                    .into_iter()
                    .filter_map(|(member, def)| match syn::parse2::<ItemDef>(def.eval()) {
                        Ok(def) => Some((member, def)),
                        Err(_) => {
                            output.extend(
                                Error::new(member.span(), "expected a cap item")
                                    .into_compile_error(),
                            );
                            has_errors = true;
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                if has_errors {
                    return output;
                }

                // Collect the complete set of bundle fields
                let mut fields = HashMap::new();
                let mut re_exports = TokenStream::new();

                for (member, def) in members {
                    match member {
                        CapDeclBundleElement::Component(mutability, path) => {
                            let ItemDef::Component(def) = def else {
                                output.extend(
                                    Error::new(
                                        path.span(),
                                        "expected component declaration, found bundle declaration",
                                    )
                                    .into_compile_error(),
                                );
                                has_errors = true;
                                continue;
                            };

                            fields
                                .entry(def.id.to_string())
                                .or_insert_with(|| {
                                    let re_exported_as = unique_ident(Span::call_site());

                                    re_exports.extend(quote! {
                                        #vis use #path::CompTy as #re_exported_as;
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
                                    }
                                })
                                .is_mutable
                                .value |= mutability.is_mutable();
                        }
                        CapDeclBundleElement::Bundle(path) => {
                            let ItemDef::Bundle(def) = def else {
                                output.extend(
                                    Error::new(
                                        path.span(),
                                        "expected bundle declaration, found component declaration",
                                    )
                                    .into_compile_error(),
                                );
                                has_errors = true;
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
                                        }
                                    })
                                    .is_mutable
                                    .value |= def.is_mutable.value;
                            }
                        }
                    }
                }

                if has_errors {
                    return output;
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

                output.extend(quote! {
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
                });
            }
        }

        output
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
                let path = &input.path;
                let target = &input.expr;
                let info = match syn::parse2::<ItemDef>(import(path).eval()) {
                    Ok(info) => info,
                    Err(_) => {
                        return Error::new(path.span(), "expected a cap item").into_compile_error();
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

                        construct_bundle(path, &info, field_getters)
                    }
                }
            }
            CxMacroArg::Construct(input) => {
                // Fetch the items of interest
                let info = import(&input.path);

                let fields = input
                    .fields
                    .iter()
                    .map(|field| (field, import(&field.path)))
                    .collect::<Vec<_>>();

                // Evaluate and parse them
                let mut errors = TokenStream::new();

                let info = match syn::parse2::<ItemDef>(info.eval()) {
                    Ok(info) => info,
                    Err(_) => {
                        return Error::new(input.path.span(), "expected a cap item")
                            .into_compile_error();
                    }
                };

                let fields = fields
                    .into_iter()
                    .filter_map(|(field, info)| match syn::parse2::<ItemDef>(info.eval()) {
                        Ok(info) => Some((field, info)),
                        Err(_) => {
                            errors.extend(
                                Error::new(field.path.span(), "expected a cap item")
                                    .into_compile_error(),
                            );
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                if !errors.is_empty() {
                    return errors;
                }

                // Enure that info is a bundle
                let info = match info {
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
                    match field_info {
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
                                        let get_path = &field_req.path;
                                        let get_reexport = &field_info.re_exported_as;
                                        let value_getter = &field_req.take_from;

                                        *req = FieldEntry::Present(
                                            quote! {{
                                                use #get_path::#get_reexport as TARGET;
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
                            Error::new(input.brace.span.join(), &format!("missing field {name}",))
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
    base_path: &Path,
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
