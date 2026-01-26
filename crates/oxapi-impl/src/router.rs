//! Router generation for axum.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::Generator;
use crate::openapi::HttpMethod;

/// Generator for axum router code.
pub struct RouterGenerator;

impl RouterGenerator {
    pub fn new(_generator: &Generator) -> Self {
        Self
    }

    /// Generate the body of the map_routes function.
    ///
    /// The methods parameter is a list of (method_name, http_method, path) tuples.
    pub fn generate_map_routes(&self, methods: &[(syn::Ident, HttpMethod, String)]) -> TokenStream {
        if methods.is_empty() {
            return quote! { router };
        }

        let routes = methods.iter().map(|(method_name, http_method, path)| {
            // OpenAPI and axum 0.8+ both use {param} format
            let method_fn = http_method_to_axum_fn(*http_method);

            quote! {
                .route(#path, ::axum::routing::#method_fn(Self::#method_name))
            }
        });

        quote! {
            router
                #(#routes)*
        }
    }
}

/// Get the axum routing function for an HTTP method.
fn http_method_to_axum_fn(method: HttpMethod) -> syn::Ident {
    format_ident!(
        "{}",
        match method {
            HttpMethod::Get => "get",
            HttpMethod::Post => "post",
            HttpMethod::Put => "put",
            HttpMethod::Delete => "delete",
            HttpMethod::Patch => "patch",
            HttpMethod::Head => "head",
            HttpMethod::Options => "options",
        }
    )
}
