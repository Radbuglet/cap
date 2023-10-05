use magic::{make_macro_exporter, make_macro_importer, new_unique_ident};
use quote::{quote, ToTokens};
use syn::parse_macro_input;

mod magic;
mod syntax;

#[proc_macro]
pub fn cap(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inp = parse_macro_input!(inp as syntax::CapabilityDeclList);
    let mut out = proc_macro2::TokenStream::default();

    for decl in &inp.list {
        let syntax::CapabilityDecl {
            visibility,
            name,
            kind,
        } = decl;

        match kind {
            syntax::CapabilityDeclKind::EqualsTy(ty) => {
                let id = new_unique_ident();
                let (export, export_name) =
                    make_macro_exporter(quote! {{ id = #id, exact_type = #ty }});

                out.extend(quote! {
                    #export
                    #visibility use #export_name as #name;
                });
            }
            syntax::CapabilityDeclKind::ImplsTrait(tb) => {
                let id = new_unique_ident();
                let (export, export_name) =
                    make_macro_exporter(quote! {{ id = #id, trait_bound = #tb }});

                out.extend(quote! {
                    #export
                    #visibility use #export_name as #name;
                });
            }
            syntax::CapabilityDeclKind::Inherits(inh) => {
                let inh_config = inh
                    .iter()
                    .map(|v| v.mode().to_token_stream())
                    .collect::<Vec<_>>();

                let mut inh_paths = inh
                    .iter()
                    .map(|v| v.path().into_token_stream())
                    .collect::<Vec<_>>();

                inh_paths.push(quote! { ::cap::__cap_process_bundle });

                out.extend(make_macro_importer(quote! { #(#inh_config)* }, &inh_paths));
            }
        }
    }

    out.into()
}

#[proc_macro]
pub fn __cap_process_bundle(inp: proc_macro::TokenStream) -> proc_macro::TokenStream {
    dbg!(inp);
    Default::default()
}
