use std::collections::{hash_map::Entry, HashMap};

use magic::{make_macro_exporter, make_macro_importer, new_unique_ident};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, Error, LitBool, Path};
use syntax::{
    CapProbeArgReq, CapProbeComponent, CapProbeEntry, CapProbeSupplied, CxConstructProbeInfo,
    CxFetchProbeInfo, CxMacroArg,
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
        CxMacroArg::Construct(inp) => {
            let mut paths = Vec::new();
            {
                // Constructor target
                paths.push(inp.path.to_token_stream());

                // Fields
                for field in &inp.fields {
                    paths.push(field.path.to_token_stream());
                }

                // Target
                paths.push(quote! { ::cap::__cx_process_construct_bundle });
            }

            let getter = make_macro_importer(inp, &paths);
            quote! {{ #getter }}.into()
        }
    }
}

#[proc_macro]
pub fn __cx_process_fetch_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(inp as CxFetchProbeInfo);

    let expr = &inp.supplied.expr;

    match &inp.collected[0] {
        CapProbeEntry::Component(info) => {
            let prefix = get_reborrow_prefix(inp.supplied.optional_mut.is_some());
            let id = info.id.clone();
            quote! { #prefix #expr.#id }
        }
        CapProbeEntry::Bundle(info) => {
            let field_getters = info.members.iter().map(|member| {
                let prefix = get_reborrow_prefix(member.is_mutable.value);
                let id = &member.id;

                quote! { #id: #prefix #expr.#id }
            });

            construct_bundle(&inp.supplied.path, info, field_getters)
        }
    }
    .into()
}

#[proc_macro]
pub fn __cx_process_construct_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(inp as CxConstructProbeInfo);

    // Extract requested bundle info
    let target_info = match &inp.collected[0] {
        CapProbeEntry::Component(_) => {
            return Error::new(
                inp.supplied.path.span(),
                "expected a path to the bundle to be constructed; found a component",
            )
            .to_compile_error()
            .into();
        }
        CapProbeEntry::Bundle(bundle) => bundle,
    };

    // Determine where we'll get the data from
    #[derive(Debug)]
    enum FieldEntry {
        Missing,
        Present(TokenStream, Span),
        Doubled(Vec<Span>),
    }

    let mut fetch_map = target_info
        .members
        .iter()
        .map(|v| (v.id.clone(), (v.is_mutable.value, FieldEntry::Missing)))
        .collect::<HashMap<_, _>>();

    for (field_info, field_req) in inp.collected[1..].iter().zip(&inp.supplied.fields) {
        match field_info {
            CapProbeEntry::Component(field_info) => {
                let Some((_, req)) = fetch_map.get_mut(&field_info.id) else {
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
            CapProbeEntry::Bundle(field_info) => {
                for field_info in &*field_info.members {
                    let Some((is_mutable, req)) = fetch_map.get_mut(&field_info.id) else {
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
                            *req = FieldEntry::Doubled(vec![*span, field_req.path.span()]);
                        }
                        FieldEntry::Doubled(overlaps) => overlaps.push(field_req.span()),
                    }
                }
            }
        }
    }

    // Construct the structure
    let mut errors = TokenStream::new();
    let mut fields = Vec::new();

    for (id, (_, entry)) in &fetch_map {
        // TODO: Improve diagnostics
        match entry {
            FieldEntry::Missing => errors.extend(
                Error::new(inp.supplied.fields.span(), "missing field").into_compile_error(),
            ),
            FieldEntry::Present(getter, _) => fields.push(quote! { #id: #getter }),
            FieldEntry::Doubled(spans) => {
                for &span in spans {
                    errors.extend(Error::new(span, "field source ambiguity").into_compile_error())
                }
            }
        }
    }

    if !errors.is_empty() {
        return errors.into();
    }

    construct_bundle(&inp.supplied.path, target_info, fields.into_iter()).into()
}

fn get_reborrow_prefix(mutable: bool) -> proc_macro2::TokenStream {
    if mutable {
        quote! { &mut * }
    } else {
        quote! { &* }
    }
}

fn construct_bundle(
    base_path: &Path,
    info: &CapProbeBundle,
    body: impl Iterator<Item = TokenStream>,
) -> TokenStream {
    let infer_bounds = info.members.iter().filter_map(|member| {
        member.is_trait.value.then(|| {
            let id = &member.id;
            quote! { #id = _ }
        })
    });

    quote! {
        #base_path::Bundle::<dyn #base_path::TyBundle<#(#infer_bounds),*>> {
            _ty: [],
            #(#body),*
        }
    }
}
