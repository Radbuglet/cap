use std::collections::HashMap;

use passing_macro::{begin_import, export, import, unique_ident};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{spanned::Spanned, Error, LitBool};

mod syntax;
mod util;

use crate::syntax::{
    CapComponentDef, CapDecl, CapDef, CapMacroArg,
    {CapBundleDef, CapBundleMemberDef, CapDeclBundleElement, CxMacroArg},
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
        let CapDecl { vis, name, kind } = &match syn::parse2::<CapDecl>(input) {
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
					CapDef::Component(CapComponentDef {
						id,
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
					CapDef::Component(CapComponentDef {
						id,
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
					.filter_map(|(member, def)| match syn::parse2::<CapDef>(def.eval()) {
						Ok(def) => Some((member, def)),
						Err(err) => {
							output.extend(err.into_compile_error());
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
							let CapDef::Component(def) = def else {
								output.extend(Error::new(
									path.span(),
									"expected component declaration, found bundle declaration",
								).into_compile_error());
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

									CapBundleMemberDef {
										id: def.id.clone(),
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
							let CapDef::Bundle(def) = def else {
								output.extend(Error::new(
									path.span(),
									"expected bundle declaration, found component declaration",
								).into_compile_error());
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

										CapBundleMemberDef {
											id: def.id.clone(),
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
						quote! { }
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
					CapDef::Bundle(CapBundleDef {
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
						impl<#ty_bundle_generic_decls> TyBundle for ::core::marker::PhantomData<(#ty_bundle_generic_tup_fwds)> {
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

    begin_import(me, input, |input| {
        let input = match syn::parse2::<CxMacroArg>(input) {
            Ok(input) => input,
            Err(err) => return err.into_compile_error(),
        };

        match input {
            CxMacroArg::Fetch(input) => {
                let target = &input.expr;
                let info = match syn::parse2::<CapDef>(import(&input.path).eval()) {
                    Ok(info) => info,
                    Err(err) => return err.into_compile_error(),
                };

                match info {
                    CapDef::Bundle(info) => todo!(),
                    CapDef::Component(info) => {
                        let prefix = if input.optional_mut.is_some() {
                            quote! { mut }
                        } else {
                            quote! {}
                        };
                        let id = &info.id;

                        quote! { &#prefix #target.#id }
                    }
                }
            }
            CxMacroArg::Construct(input) => {
                todo!()
            }
        }
    })
    .into()
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
