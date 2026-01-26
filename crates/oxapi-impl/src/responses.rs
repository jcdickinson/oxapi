//! Response enum generation.

use heck::ToUpperCamelCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::openapi::{Operation, ParsedSpec, ResponseStatus};
use crate::types::TypeGenerator;
use crate::{GeneratedTypeKind, ResponseSuffixes, TypeOverride, TypeOverrides};

/// Generator for response enums.
pub struct ResponseGenerator<'a> {
    spec: &'a ParsedSpec,
    type_gen: &'a TypeGenerator,
    overrides: &'a TypeOverrides,
    suffixes: &'a ResponseSuffixes,
}

impl<'a> ResponseGenerator<'a> {
    pub fn new(
        spec: &'a ParsedSpec,
        type_gen: &'a TypeGenerator,
        overrides: &'a TypeOverrides,
        suffixes: &'a ResponseSuffixes,
    ) -> Self {
        Self {
            spec,
            type_gen,
            overrides,
            suffixes,
        }
    }

    /// Generate response enums for all operations.
    pub fn generate_all(&self) -> TokenStream {
        let enums: Vec<_> = self
            .spec
            .operations()
            .map(|op| self.generate_for_operation(op))
            .collect();

        quote! {
            #(#enums)*
        }
    }

    /// Generate Ok and Err enums for a single operation.
    pub fn generate_for_operation(&self, op: &Operation) -> TokenStream {
        let op_name = op
            .operation_id
            .as_deref()
            .unwrap_or(&op.path)
            .to_upper_camel_case();

        let ok_enum = self.generate_ok_enum(op, &op_name);
        let err_enum = self.generate_err_enum(op, &op_name);

        quote! {
            #ok_enum
            #err_enum
        }
    }

    /// Get the enum name, applying any rename override.
    fn get_enum_name(&self, op: &Operation, default_name: &str, kind: GeneratedTypeKind) -> syn::Ident {
        if let Some(TypeOverride::Rename { name, .. }) =
            self.overrides.get(op.method, &op.path, kind)
        {
            format_ident!("{}", name)
        } else {
            format_ident!("{}", default_name)
        }
    }

    /// Get the variant name for a status code, applying any rename override.
    fn get_variant_name(&self, op: &Operation, status: u16, kind: GeneratedTypeKind) -> syn::Ident {
        if let Some(TypeOverride::Rename { variant_overrides, .. }) =
            self.overrides.get(op.method, &op.path, kind)
        {
            if let Some(ov) = variant_overrides.get(&status) {
                return format_ident!("{}", ov.name);
            }
        }
        format_ident!("Status{}", status)
    }

    /// Get the variant attributes for a status code.
    fn get_variant_attrs(&self, op: &Operation, status: u16, kind: GeneratedTypeKind) -> Vec<TokenStream> {
        if let Some(TypeOverride::Rename { variant_overrides, .. }) =
            self.overrides.get(op.method, &op.path, kind)
        {
            if let Some(ov) = variant_overrides.get(&status) {
                return ov.attrs.clone();
            }
        }
        Vec::new()
    }

    /// Get attributes for an enum. If attrs contains a derive, returns only the attrs.
    /// Otherwise prepends the default derives.
    fn get_enum_attrs(&self, op: &Operation, kind: GeneratedTypeKind) -> Vec<TokenStream> {
        if let Some(TypeOverride::Rename { attrs, .. }) =
            self.overrides.get(op.method, &op.path, kind)
        {
            // Check if any attr is a derive
            let has_derive = attrs.iter().any(|a| a.to_string().starts_with("#[derive"));
            if has_derive {
                return attrs.clone();
            } else {
                // Prepend default derives
                let mut result = vec![self.suffixes.default_derives.clone()];
                result.extend(attrs.clone());
                return result;
            }
        }
        Vec::new()
    }

    /// Get the variant name for a default response.
    fn get_default_variant_name(&self, _op: &Operation, _kind: GeneratedTypeKind) -> syn::Ident {
        format_ident!("Default")
    }

    /// Check if the type is replaced.
    fn is_replaced(&self, op: &Operation, kind: GeneratedTypeKind) -> bool {
        self.overrides.is_replaced(op.method, &op.path, kind)
    }

    /// Generate the Ok (success) enum for an operation.
    fn generate_ok_enum(&self, op: &Operation, op_name: &str) -> TokenStream {
        // Skip if replaced
        if self.is_replaced(op, GeneratedTypeKind::Ok) {
            return quote! {};
        }

        let enum_name = self.get_enum_name(
            op,
            &format!("{}{}", op_name, self.suffixes.ok_suffix),
            GeneratedTypeKind::Ok,
        );

        // Collect success responses (2xx status codes)
        let success_responses: Vec<_> = op
            .responses
            .iter()
            .filter(|r| r.status_code.is_success())
            .collect();

        if success_responses.is_empty() {
            // No explicit success responses, create a default
            return quote! {
                #[derive(Debug)]
                pub enum #enum_name {
                    Status200,
                }

                impl ::axum::response::IntoResponse for #enum_name {
                    fn into_response(self) -> ::axum::response::Response {
                        match self {
                            Self::Status200 => {
                                ::axum::http::StatusCode::OK.into_response()
                            }
                        }
                    }
                }
            };
        }

        let variants: Vec<_> = success_responses
            .iter()
            .map(|resp| {
                let status = match &resp.status_code {
                    ResponseStatus::Code(code) => *code,
                    ResponseStatus::Default => 200,
                };
                let variant_name = self.get_variant_name(op, status, GeneratedTypeKind::Ok);
                let variant_attrs = self.get_variant_attrs(op, status, GeneratedTypeKind::Ok);

                if let Some(schema) = &resp.schema {
                    let ty = self
                        .type_gen
                        .type_for_schema(schema, &format!("{}Response{}", op_name, status));
                    (variant_name, Some(ty), status, variant_attrs)
                } else {
                    (variant_name, None, status, variant_attrs)
                }
            })
            .collect();

        let variant_defs = variants.iter().map(|(name, ty, _, attrs)| {
            if let Some(ty) = ty {
                quote! { #(#attrs)* #name(#ty) }
            } else {
                quote! { #(#attrs)* #name }
            }
        });

        let into_response_arms = variants.iter().map(|(name, ty, status, _)| {
            let status_code = status_code_ident(*status);

            if ty.is_some() {
                quote! {
                    Self::#name(body) => {
                        (
                            #status_code,
                            ::axum::Json(body),
                        ).into_response()
                    }
                }
            } else {
                quote! {
                    Self::#name => {
                        #status_code.into_response()
                    }
                }
            }
        });

        let enum_attrs = self.get_enum_attrs(op, GeneratedTypeKind::Ok);
        let default_derives = &self.suffixes.default_derives;

        // If we have override attrs, use them; otherwise use default derives
        let attrs_tokens = if enum_attrs.is_empty() {
            quote! { #default_derives }
        } else {
            quote! { #(#enum_attrs)* }
        };

        quote! {
            #attrs_tokens
            pub enum #enum_name {
                #(#variant_defs,)*
            }

            impl ::axum::response::IntoResponse for #enum_name {
                fn into_response(self) -> ::axum::response::Response {
                    use ::axum::response::IntoResponse;
                    match self {
                        #(#into_response_arms)*
                    }
                }
            }
        }
    }

    /// Generate the Err (error) enum for an operation.
    fn generate_err_enum(&self, op: &Operation, op_name: &str) -> TokenStream {
        // Skip if replaced
        if self.is_replaced(op, GeneratedTypeKind::Err) {
            return quote! {};
        }

        let enum_name = self.get_enum_name(
            op,
            &format!("{}{}", op_name, self.suffixes.err_suffix),
            GeneratedTypeKind::Err,
        );

        // Collect error responses (4xx, 5xx, and default)
        let error_responses: Vec<_> = op
            .responses
            .iter()
            .filter(|r| r.status_code.is_error())
            .collect();

        if error_responses.is_empty() {
            // No explicit error responses, create a default
            return quote! {
                #[derive(Debug)]
                pub enum #enum_name {
                    Status500(String),
                }

                impl ::axum::response::IntoResponse for #enum_name {
                    fn into_response(self) -> ::axum::response::Response {
                        use ::axum::response::IntoResponse;
                        match self {
                            Self::Status500(msg) => {
                                (
                                    ::axum::http::StatusCode::INTERNAL_SERVER_ERROR,
                                    msg,
                                ).into_response()
                            }
                        }
                    }
                }
            };
        }

        let variants: Vec<_> = error_responses
            .iter()
            .map(|resp| {
                let (variant_name, status, is_default) = match &resp.status_code {
                    ResponseStatus::Code(code) => {
                        (self.get_variant_name(op, *code, GeneratedTypeKind::Err), *code, false)
                    }
                    ResponseStatus::Default => {
                        (self.get_default_variant_name(op, GeneratedTypeKind::Err), 500, true)
                    }
                };
                let variant_attrs = self.get_variant_attrs(op, status, GeneratedTypeKind::Err);

                if let Some(schema) = &resp.schema {
                    let ty = self
                        .type_gen
                        .type_for_schema(schema, &format!("{}Error{}", op_name, status));
                    (variant_name, Some(ty), status, is_default, variant_attrs)
                } else {
                    (variant_name, None, status, is_default, variant_attrs)
                }
            })
            .collect();

        let variant_defs = variants.iter().map(|(name, ty, _, is_default, attrs)| {
            if let Some(ty) = ty {
                if *is_default {
                    quote! { #(#attrs)* #name(::axum::http::StatusCode, #ty) }
                } else {
                    quote! { #(#attrs)* #name(#ty) }
                }
            } else if *is_default {
                quote! { #(#attrs)* #name(::axum::http::StatusCode) }
            } else {
                quote! { #(#attrs)* #name }
            }
        });

        let into_response_arms = variants.iter().map(|(name, ty, status, is_default, _)| {
            let status_code = status_code_ident(*status);

            if *is_default {
                if ty.is_some() {
                    quote! {
                        Self::#name(status, body) => {
                            (status, ::axum::Json(body)).into_response()
                        }
                    }
                } else {
                    quote! {
                        Self::#name(status) => {
                            status.into_response()
                        }
                    }
                }
            } else if ty.is_some() {
                quote! {
                    Self::#name(body) => {
                        (
                            #status_code,
                            ::axum::Json(body),
                        ).into_response()
                    }
                }
            } else {
                quote! {
                    Self::#name => {
                        #status_code.into_response()
                    }
                }
            }
        });

        let enum_attrs = self.get_enum_attrs(op, GeneratedTypeKind::Err);
        let default_derives = &self.suffixes.default_derives;

        // If we have override attrs, use them; otherwise use default derives
        let attrs_tokens = if enum_attrs.is_empty() {
            quote! { #default_derives }
        } else {
            quote! { #(#enum_attrs)* }
        };

        quote! {
            #attrs_tokens
            pub enum #enum_name {
                #(#variant_defs,)*
            }

            impl ::axum::response::IntoResponse for #enum_name {
                fn into_response(self) -> ::axum::response::Response {
                    use ::axum::response::IntoResponse;
                    match self {
                        #(#into_response_arms)*
                    }
                }
            }
        }
    }
}

/// Generate a StatusCode expression for a numeric status.
fn status_code_ident(status: u16) -> TokenStream {
    // Use well-known constants where possible
    let name = match status {
        200 => Some("OK"),
        201 => Some("CREATED"),
        202 => Some("ACCEPTED"),
        204 => Some("NO_CONTENT"),
        301 => Some("MOVED_PERMANENTLY"),
        302 => Some("FOUND"),
        304 => Some("NOT_MODIFIED"),
        400 => Some("BAD_REQUEST"),
        401 => Some("UNAUTHORIZED"),
        403 => Some("FORBIDDEN"),
        404 => Some("NOT_FOUND"),
        405 => Some("METHOD_NOT_ALLOWED"),
        409 => Some("CONFLICT"),
        410 => Some("GONE"),
        422 => Some("UNPROCESSABLE_ENTITY"),
        429 => Some("TOO_MANY_REQUESTS"),
        500 => Some("INTERNAL_SERVER_ERROR"),
        501 => Some("NOT_IMPLEMENTED"),
        502 => Some("BAD_GATEWAY"),
        503 => Some("SERVICE_UNAVAILABLE"),
        504 => Some("GATEWAY_TIMEOUT"),
        _ => None,
    };

    if let Some(name) = name {
        let ident = format_ident!("{}", name);
        quote! { ::axum::http::StatusCode::#ident }
    } else {
        quote! { ::axum::http::StatusCode::from_u16(#status).unwrap() }
    }
}
