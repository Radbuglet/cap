use std::collections::HashMap;

use magic::{make_macro_exporter, make_macro_importer, new_unique_ident};
use quote::{quote, ToTokens};
use syn::parse_macro_input;
use syntax::{CapProbeArgReq, CapProbeArgs, CapProbeDecl, CapProbeEntry};

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
                let (export, export_name) =
                    make_macro_exporter(CapProbeEntry::ExactType(CapProbeDecl { id }));

                out.extend(quote! {
                    #export

                    #visibility mod #export_name {
                        #[allow(unused_imports)]
                        use super::*;

                        pub type ExactType = #ty;
                    }

                    #visibility use #export_name as #name;
                });
            }
            syntax::CapDeclKind::ImplsTrait(tb) => {
                let id = new_unique_ident();
                let (export, export_name) =
                    make_macro_exporter(CapProbeEntry::ImplsTrait(CapProbeDecl { id }));

                out.extend(quote! {
                    #export

                    #visibility mod #export_name {
                        #[allow(unused_imports)]
                        use super::*;

                        pub trait ExactTrait: #tb {}

                        impl<__Target: ?Sized + #tb> ExactTrait for __Target {}
                    }

                    #visibility use #export_name as #name;
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
                    CapProbeArgs { expected },
                    &process_paths,
                ));
            }
        }
    }

    out.into()
}

#[proc_macro]
pub fn __cap_process_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(
        inp as magic::ImportedMacroInfo<syntax::CapProbeArgs, syntax::CapProbeEntry>
    );

    for (expected, collected) in inp.supplied.expected.iter().zip(inp.collected.iter()) {
        match collected {
            CapProbeEntry::ExactType(ty) => {}
            CapProbeEntry::ImplsTrait(tb) => {}
            CapProbeEntry::Bundle(_) => {}
        }
    }

    Default::default()
}
