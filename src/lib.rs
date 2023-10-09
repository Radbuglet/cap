use std::collections::{hash_map::Entry, HashMap};

use magic::{make_macro_exporter, make_macro_importer, new_unique_ident};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Error, LitBool};
use syntax::{
    CapProbeArgReq, CapProbeComponent, CapProbeEntry, CapProbeSupplied, CxFetchProbeCollected,
    CxFetchProbeSupplied, CxMacroArg,
};

use crate::{
    magic::SynArray,
    syntax::{CapProbeBundle, CapProbeBundleMember},
};

mod magic;
mod syntax;

#[proc_macro]
pub fn cap(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(inp as syntax::CapMacroArg);
    let mut out = proc_macro2::TokenStream::default();

    for decl in &inp.list {
        let syntax::CapDecl {
            visibility,
            name,
            kind,
        } = decl;

        match kind {
            syntax::CapDeclKind::EqualsTy(ty) => {
                let id = new_unique_ident();
                let export = make_macro_exporter(
                    id.clone(),
                    CapProbeEntry::Component(CapProbeComponent {
                        id: id.clone(),
                        is_trait: LitBool::new(false, Span::call_site()),
                    }),
                );

                out.extend(quote! {
                    #export

                    #visibility mod #id {
                        #[allow(unused_imports)]
                        use super::*;

                        pub type CompTy = #ty;
                    }

                    #visibility use #id as #name;
                });
            }
            syntax::CapDeclKind::ImplsTrait(tb) => {
                let id = new_unique_ident();
                let export = make_macro_exporter(
                    id.clone(),
                    CapProbeEntry::Component(CapProbeComponent {
                        id: id.clone(),
                        is_trait: LitBool::new(true, Span::call_site()),
                    }),
                );

                out.extend(quote! {
                    #export

                    #visibility mod #id {
                        #[allow(unused_imports)]
                        use super::*;

                        pub trait CompTy: #tb {}

                        impl<__Target: ?Sized + #tb> CompTy for __Target {}
                    }

                    #visibility use #id as #name;
                });
            }
            syntax::CapDeclKind::Inherits(inh) => {
                let expected = inh
                    .iter()
                    .map(|v| CapProbeArgReq {
                        path: v.path().clone(),
                        mode: v.mode(),
                    })
                    .collect();

                let mut process_paths = inh
                    .iter()
                    .map(|v| v.path().into_token_stream())
                    .collect::<Vec<_>>();

                process_paths.push(quote! { ::cap::__cap_process_bundle });

                out.extend(make_macro_importer(
                    CapProbeSupplied {
                        visibility: visibility.clone(),
                        name: name.clone(),
                        expected,
                    },
                    &process_paths,
                ));
            }
        }
    }

    out.into()
}

#[doc(hidden)]
#[proc_macro]
pub fn __cap_process_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(
        inp as magic::ImportedMacroInfo<syntax::CapProbeSupplied, syntax::CapProbeEntry>
    );
    let name = &inp.supplied.name;
    let visibility = &inp.supplied.visibility;

    // Collect members
    let mut errors = TokenStream::new();
    let mut members = HashMap::new();

    #[derive(Debug)]
    struct MemberInfo {
        is_mutable: bool,
        is_trait: bool,
        fetch_from: TokenStream,
    }

    for (expected, collected) in inp.supplied.expected.iter().zip(inp.collected) {
        match collected {
            CapProbeEntry::Component(ty) => {
                let mutability = match expected.mode {
                    syntax::BundleElementMode::Ref(_) => false,
                    syntax::BundleElementMode::Mut(_) => true,
                    syntax::BundleElementMode::Bundle(_) => {
                        errors.extend(
                            Error::new(
                                expected.path.span(),
                                "Cannot import component definition directly like a bundle. Please specify its mutability.",
                            )
                            .into_compile_error(),
                        );
                        continue;
                    }
                };

                match members.entry(ty.id.clone()) {
                    Entry::Vacant(entry) => {
                        let macro_path = &expected.path;

                        entry.insert(MemberInfo {
                            is_mutable: mutability,
                            is_trait: ty.is_trait.value,
                            fetch_from: quote! { #macro_path::CompTy },
                        });
                    }
                    Entry::Occupied(entry) => {
                        entry.into_mut().is_mutable |= mutability;
                    }
                }
            }
            CapProbeEntry::Bundle(bu) => {
                if !matches!(expected.mode, syntax::BundleElementMode::Bundle(_)) {
                    errors.extend(
                        Error::new(
                            expected.path.span(),
                            "cannot specify mutability for an entire bundle",
                        )
                        .into_compile_error(),
                    );
                    continue;
                };

                for member in &*bu.members {
                    match members.entry(member.id.clone()) {
                        Entry::Vacant(entry) => {
                            let macro_path = &expected.path;
                            let re_exported_as = &member.re_exported_as;

                            entry.insert(MemberInfo {
                                is_mutable: member.is_mutable.value,
                                is_trait: member.is_trait.value,
                                fetch_from: quote! { #macro_path::#re_exported_as },
                            });
                        }
                        Entry::Occupied(entry) => {
                            entry.into_mut().is_mutable |= member.is_mutable.value;
                        }
                    }
                }
            }
        }
    }

    if !errors.is_empty() {
        return errors.into();
    }

    // Generate macro
    let id = new_unique_ident();
    let mut import_block = TokenStream::new();
    let mut ty_bundle = TokenStream::new();
    let mut struct_bundle = TokenStream::new();

    for (member_id, member) in &members {
        let fetch = &member.fetch_from;

        import_block.extend(quote! {
            #visibility use super::#fetch as #member_id;
        });

        let ref_header = if member.is_mutable {
            quote! { &'a mut }
        } else {
            quote! { &'a }
        };

        if member.is_trait {
            ty_bundle.extend(quote! {
                type #member_id: #member_id;
            });
            struct_bundle.extend(quote! {
                #visibility #member_id: #ref_header B::#member_id,
            });
        } else {
            struct_bundle.extend(quote! {
                #visibility #member_id: #ref_header #member_id,
            });
        }
    }

    let info_macro = make_macro_exporter(
        id.clone(),
        CapProbeEntry::Bundle(CapProbeBundle {
            members: SynArray::from_iter(members.iter().map(|(member_id, member)| {
                CapProbeBundleMember {
                    id: member_id.clone(),
                    is_mutable: LitBool::new(member.is_mutable, Span::call_site()),
                    is_trait: LitBool::new(member.is_trait, Span::call_site()),
                    re_exported_as: member_id.clone(),
                }
            })),
        }),
    );

    quote! {
        #visibility mod #id {
            #[allow(non_camel_case_types)]
            #visibility trait TyBundle {
                #ty_bundle
            }

            #visibility struct Bundle<'a, B: ?Sized + TyBundle> {
                #visibility _ty: [&'a B; 0],
                #struct_bundle
            }

            #import_block
        }

        #info_macro

        #visibility use #id as #name;
    }
    .into()
}

#[proc_macro]
pub fn cx(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(inp as CxMacroArg);

    match inp {
        CxMacroArg::Fetch(inp) => {
            let path = inp.path.to_token_stream();

            let getter =
                make_macro_importer(inp, &[path, quote! { ::cap::__cx_process_fetch_bundle }]);

            quote! {{ #getter }}.into()
        }
        CxMacroArg::Construct(_) => todo!(),
    }
}

#[proc_macro]
pub fn __cx_process_fetch_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(
        inp as magic::ImportedMacroInfo<CxFetchProbeSupplied, CxFetchProbeCollected>
    );

    let expr = &inp.supplied.expr;

    match &inp.collected[0] {
        CapProbeEntry::Component(info) => {
            let prefix = get_reborrow_prefix(inp.supplied.optional_mut.is_some());
            let id = info.id.clone();
            quote! { #prefix #expr.#id }
        }
        CapProbeEntry::Bundle(info) => {
            let base_path = inp.supplied.path;
            let field_getters = info.members.iter().map(|member| {
                let prefix = get_reborrow_prefix(member.is_mutable.value);
                let id = &member.id;

                quote! { #id: #prefix #expr.#id }
            });

            let infer_bounds = info.members.iter().filter_map(|member| {
                member.is_trait.value.then(|| {
                    let id = &member.id;
                    quote! { #id = _ }
                })
            });

            quote! {
                #base_path::Bundle::<dyn #base_path::TyBundle<#(#infer_bounds),*>> {
                    _ty: [],
                    #(#field_getters),*
                }
            }
        }
    }
    .into()
}

fn get_reborrow_prefix(mutable: bool) -> proc_macro2::TokenStream {
    if mutable {
        quote! { &mut * }
    } else {
        quote! { &* }
    }
}
