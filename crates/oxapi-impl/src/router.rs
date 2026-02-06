//! Router generation for axum.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::openapi::HttpMethod;

/// Generator for axum router code.
pub struct RouterGenerator;

impl RouterGenerator {
    /// Generate the body of the map_routes function.
    ///
    /// The methods parameter is a list of (method_name, http_method, path) tuples.
    /// The spec_method parameter is an optional (method_name, endpoint_path) for the spec route.
    pub fn generate_map_routes(
        &self,
        methods: &[(syn::Ident, HttpMethod, String)],
        spec_method: Option<(syn::Ident, String)>,
    ) -> TokenStream {
        let routes = methods.iter().map(|(method_name, http_method, path)| {
            // OpenAPI and axum 0.8+ both use {param} format
            let method_fn = http_method_to_axum_fn(*http_method);

            quote! {
                .route(#path, ::axum::routing::#method_fn(Self::#method_name))
            }
        });

        let spec_route = spec_method.map(|(method_name, path)| {
            quote! {
                .route(#path, ::axum::routing::get(|| async { Self::#method_name() }))
            }
        });

        quote! {
            router
                #(#routes)*
                #spec_route
        }
    }
}

/// Get the axum routing function for an HTTP method.
fn http_method_to_axum_fn(method: HttpMethod) -> syn::Ident {
    format_ident!("{}", method.as_str())
}
