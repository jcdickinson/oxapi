//! Response enum generation.

use heck::ToUpperCamelCase;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::openapi::{Operation, ParsedSpec, ResponseStatus};
use crate::types::TypeGenerator;

/// Generator for response enums.
pub struct ResponseGenerator<'a> {
    spec: &'a ParsedSpec,
    type_gen: &'a TypeGenerator,
}

impl<'a> ResponseGenerator<'a> {
    pub fn new(spec: &'a ParsedSpec, type_gen: &'a TypeGenerator) -> Self {
        Self { spec, type_gen }
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

    /// Generate the Ok (success) enum for an operation.
    fn generate_ok_enum(&self, op: &Operation, op_name: &str) -> TokenStream {
        let enum_name = format_ident!("{}Ok", op_name);

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
                let variant_name = format_ident!("Status{}", status);

                if let Some(schema) = &resp.schema {
                    let ty = self
                        .type_gen
                        .type_for_schema(schema, &format!("{}Response{}", op_name, status));
                    (variant_name, Some(ty), status)
                } else {
                    (variant_name, None, status)
                }
            })
            .collect();

        let variant_defs = variants.iter().map(|(name, ty, _)| {
            if let Some(ty) = ty {
                quote! { #name(#ty) }
            } else {
                quote! { #name }
            }
        });

        let into_response_arms = variants.iter().map(|(name, ty, status)| {
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

        quote! {
            #[derive(Debug)]
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
        let enum_name = format_ident!("{}Err", op_name);

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
                let (variant_name, status) = match &resp.status_code {
                    ResponseStatus::Code(code) => (format_ident!("Status{}", code), *code),
                    ResponseStatus::Default => (format_ident!("Default"), 500),
                };

                if let Some(schema) = &resp.schema {
                    let ty = self
                        .type_gen
                        .type_for_schema(schema, &format!("{}Error{}", op_name, status));
                    (
                        variant_name,
                        Some(ty),
                        status,
                        resp.status_code == ResponseStatus::Default,
                    )
                } else {
                    (
                        variant_name,
                        None,
                        status,
                        resp.status_code == ResponseStatus::Default,
                    )
                }
            })
            .collect();

        let variant_defs = variants.iter().map(|(name, ty, _, is_default)| {
            if let Some(ty) = ty {
                if *is_default {
                    // Default needs to carry the status code
                    quote! { #name(::axum::http::StatusCode, #ty) }
                } else {
                    quote! { #name(#ty) }
                }
            } else if *is_default {
                quote! { #name(::axum::http::StatusCode) }
            } else {
                quote! { #name }
            }
        });

        let into_response_arms = variants.iter().map(|(name, ty, status, is_default)| {
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

        quote! {
            #[derive(Debug)]
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
