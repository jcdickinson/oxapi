//! Router generation for axum.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::openapi::HttpMethod;
use crate::Generator;

/// Generator for axum router code.
pub struct RouterGenerator<'a> {
    #[allow(dead_code)]
    generator: &'a Generator,
}

impl<'a> RouterGenerator<'a> {
    pub fn new(generator: &'a Generator) -> Self {
        Self { generator }
    }

    /// Generate the body of the map_routes function.
    ///
    /// The methods parameter is a list of (method_name, http_method, path) tuples.
    pub fn generate_map_routes(
        &self,
        methods: &[(syn::Ident, HttpMethod, String)],
    ) -> TokenStream {
        if methods.is_empty() {
            return quote! { router };
        }

        let routes = methods.iter().map(|(method_name, http_method, path)| {
            let axum_path = convert_path_to_axum(path);
            let method_fn = http_method_to_axum_fn(*http_method);

            quote! {
                .route(#axum_path, ::axum::routing::#method_fn(Self::#method_name))
            }
        });

        quote! {
            router
                #(#routes)*
        }
    }
}

/// Convert an OpenAPI path to axum path format.
///
/// OpenAPI uses `{param}` and axum 0.8+ also uses `{param}`.
/// Earlier axum versions used `:param` but 0.8 uses `{param}`.
fn convert_path_to_axum(path: &str) -> String {
    // OpenAPI and axum 0.8+ both use {param} format, so just pass through
    path.to_string()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_path_simple() {
        assert_eq!(convert_path_to_axum("/users"), "/users");
    }

    #[test]
    fn test_convert_path_with_param() {
        assert_eq!(convert_path_to_axum("/users/{id}"), "/users/{id}");
    }

    #[test]
    fn test_convert_path_multiple_params() {
        assert_eq!(
            convert_path_to_axum("/users/{user_id}/posts/{post_id}"),
            "/users/{user_id}/posts/{post_id}"
        );
    }
}
