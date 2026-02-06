//! Response enum generation.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};

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

    /// Check if a response matches the given kind (Ok for success codes, Err for error codes).
    fn response_matches_kind(status: &ResponseStatus, kind: GeneratedTypeKind) -> bool {
        match kind {
            GeneratedTypeKind::Ok => status.is_success(),
            GeneratedTypeKind::Err => status.is_error(),
            GeneratedTypeKind::Query => false,
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
        let op_name = op.name();

        let ok_enum = self.generate_response_enum(op, &op_name, GeneratedTypeKind::Ok);
        let err_enum = self.generate_response_enum(op, &op_name, GeneratedTypeKind::Err);

        quote! {
            #ok_enum
            #err_enum
        }
    }

    /// Get the enum name, applying any rename override.
    fn get_enum_name(
        &self,
        op: &Operation,
        default_name: &str,
        kind: GeneratedTypeKind,
    ) -> syn::Ident {
        if let Some(TypeOverride::Rename { name, .. }) =
            self.overrides.get(op.method, &op.path, kind)
        {
            name.clone()
        } else {
            format_ident!("{}", default_name)
        }
    }

    /// Get a value from a variant override, or return a default.
    fn get_variant_override<T, F>(
        &self,
        op: &Operation,
        status: u16,
        kind: GeneratedTypeKind,
        getter: F,
        default: T,
    ) -> T
    where
        F: FnOnce(&crate::VariantOverride) -> T,
    {
        if let Some(TypeOverride::Rename {
            variant_overrides, ..
        }) = self.overrides.get(op.method, &op.path, kind)
            && let Some(ov) = variant_overrides.get(&status)
        {
            return getter(ov);
        }
        default
    }

    /// Get the variant name for a status code, applying any rename override.
    fn get_variant_name(&self, op: &Operation, status: u16, kind: GeneratedTypeKind) -> syn::Ident {
        self.get_variant_override(
            op,
            status,
            kind,
            |ov| ov.name.clone(),
            format_ident!("Status{}", status),
        )
    }

    /// Get the inner type name override for a status code, if specified.
    fn get_inner_type_name(
        &self,
        op: &Operation,
        status: u16,
        kind: GeneratedTypeKind,
    ) -> Option<syn::Ident> {
        self.get_variant_override(op, status, kind, |ov| ov.inner_type_name.clone(), None)
    }

    /// Get the variant attributes for a status code.
    fn get_variant_attrs(
        &self,
        op: &Operation,
        status: u16,
        kind: GeneratedTypeKind,
    ) -> Vec<TokenStream> {
        self.get_variant_override(op, status, kind, |ov| ov.attrs.clone(), Vec::new())
    }

    /// Validate that all variant overrides reference status codes that exist in the spec.
    /// Returns compile errors for any invalid status codes.
    fn validate_variant_overrides(&self, op: &Operation, kind: GeneratedTypeKind) -> TokenStream {
        let Some(TypeOverride::Rename {
            variant_overrides, ..
        }) = self.overrides.get(op.method, &op.path, kind)
        else {
            return quote! {};
        };

        // Collect valid status codes from the spec
        let valid_codes: std::collections::HashSet<u16> = op
            .responses
            .iter()
            .filter(|r| Self::response_matches_kind(&r.status_code, kind))
            .filter_map(|r| match &r.status_code {
                ResponseStatus::Code(code) => Some(*code),
                _ => None,
            })
            .collect();

        // Check each override
        let errors: Vec<_> = variant_overrides
            .iter()
            .filter(|(status, _)| !valid_codes.contains(status))
            .map(|(status, ov)| {
                let valid_list: Vec<_> = valid_codes.iter().collect();
                let kind_name = match kind {
                    GeneratedTypeKind::Ok => "success",
                    GeneratedTypeKind::Err => "error",
                    GeneratedTypeKind::Query => "query",
                };
                let msg = format!(
                    "status code {} does not exist in the OpenAPI spec for {} {} (valid {} codes: {:?})",
                    status, op.method, op.path, kind_name, valid_list
                );
                quote_spanned! { ov.name.span() =>
                    compile_error!(#msg);
                }
            })
            .collect();

        quote! { #(#errors)* }
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

    /// Generate a response enum (Ok or Err) for an operation.
    fn generate_response_enum(
        &self,
        op: &Operation,
        op_name: &str,
        kind: GeneratedTypeKind,
    ) -> TokenStream {
        // Skip if replaced
        if self.overrides.is_replaced(op.method, &op.path, kind) {
            return quote! {};
        }

        // Validate variant overrides reference valid status codes
        let validation_errors = self.validate_variant_overrides(op, kind);

        let suffix = match kind {
            GeneratedTypeKind::Ok => &self.suffixes.ok_suffix,
            GeneratedTypeKind::Err => &self.suffixes.err_suffix,
            GeneratedTypeKind::Query => unreachable!(),
        };

        let enum_name = self.get_enum_name(op, &format!("{}{}", op_name, suffix), kind);

        // Collect relevant responses based on kind
        let responses: Vec<_> = op
            .responses
            .iter()
            .filter(|r| Self::response_matches_kind(&r.status_code, kind))
            .collect();

        // Handle empty responses with a default enum
        if responses.is_empty() {
            return self.generate_empty_fallback(&enum_name, kind);
        }

        let mut errors = Vec::new();
        let mut inline_definitions = Vec::new();

        // Process each response into variant info
        let variants: Vec<VariantInfo> = responses
            .iter()
            .map(|resp| {
                let (variant_name, status, is_default) = match &resp.status_code {
                    ResponseStatus::Code(code) => {
                        (self.get_variant_name(op, *code, kind), *code, false)
                    }
                    ResponseStatus::Default => {
                        // Only error enums should have Default responses
                        (format_ident!("Default"), 500, true)
                    }
                };

                let variant_attrs = self.get_variant_attrs(op, status, kind);
                let inner_type_override = self.get_inner_type_name(op, status, kind);

                let body_type = if let Some(schema) = &resp.schema {
                    // Validate: inner type override only allowed for inline schemas
                    if let Some(ref inner_name) = inner_type_override
                        && !TypeGenerator::is_inline_schema(schema)
                    {
                        errors.push(quote_spanned! { inner_name.span() =>
                            compile_error!("inner type name override can only be used with inline schemas, not $ref");
                        });
                    }

                    // Use override name or default name hint
                    let name_hint = inner_type_override
                        .as_ref()
                        .map(|i| i.to_string())
                        .unwrap_or_else(|| format!("{}{}", enum_name, variant_name));
                    let generated = self
                        .type_gen
                        .type_for_schema_with_definitions(schema, &name_hint);
                    inline_definitions.extend(generated.definitions);
                    Some(generated.type_ref)
                } else {
                    None
                };

                VariantInfo {
                    name: variant_name,
                    body_type,
                    status,
                    is_default,
                    attrs: variant_attrs,
                }
            })
            .collect();

        // Generate variant definitions
        let variant_defs = variants.iter().map(|v| {
            let name = &v.name;
            let attrs = &v.attrs;
            match (&v.body_type, v.is_default) {
                (Some(ty), true) => quote! { #(#attrs)* #name(::axum::http::StatusCode, #ty) },
                (Some(ty), false) => quote! { #(#attrs)* #name(#ty) },
                (None, true) => quote! { #(#attrs)* #name(::axum::http::StatusCode) },
                (None, false) => quote! { #(#attrs)* #name },
            }
        });

        // Generate IntoResponse match arms
        let into_response_arms = variants.iter().map(|v| {
            let name = &v.name;
            let status_code = status_code_ident(v.status);

            match (&v.body_type, v.is_default) {
                (Some(_), true) => quote! {
                    Self::#name(status, body) => {
                        (status, ::axum::Json(body)).into_response()
                    }
                },
                (Some(_), false) => quote! {
                    Self::#name(body) => {
                        (#status_code, ::axum::Json(body)).into_response()
                    }
                },
                (None, true) => quote! {
                    Self::#name(status) => {
                        status.into_response()
                    }
                },
                (None, false) => quote! {
                    Self::#name => {
                        #status_code.into_response()
                    }
                },
            }
        });

        let enum_attrs = self.get_enum_attrs(op, kind);
        let default_derives = &self.suffixes.default_derives;

        // If we have override attrs, use them; otherwise use default derives
        let attrs_tokens = if enum_attrs.is_empty() {
            quote! { #default_derives }
        } else {
            quote! { #(#enum_attrs)* }
        };

        // Use the enum name's span for the definition so error messages
        // point to the user's enum definition, not the macro attribute
        let enum_span = enum_name.span();

        let enum_def = quote_spanned! { enum_span =>
            #attrs_tokens
            pub enum #enum_name {
                #(#variant_defs,)*
            }
        };

        quote! {
            #validation_errors

            #(#errors)*

            #(#inline_definitions)*

            #enum_def

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

    /// Generate a fallback enum when no responses are defined in the spec.
    /// For Ok: generates a default Status200 variant.
    /// For Err: returns empty (no error type generated when spec has no errors).
    fn generate_empty_fallback(
        &self,
        enum_name: &syn::Ident,
        kind: GeneratedTypeKind,
    ) -> TokenStream {
        let span = enum_name.span();
        match kind {
            GeneratedTypeKind::Ok => quote_spanned! { span =>
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
            },
            // No error type generated when the spec defines no error responses
            GeneratedTypeKind::Err => quote! {},
            GeneratedTypeKind::Query => unreachable!(),
        }
    }
}

/// Information about a response variant.
struct VariantInfo {
    name: syn::Ident,
    body_type: Option<TokenStream>,
    status: u16,
    is_default: bool,
    attrs: Vec<TokenStream>,
}

/// Well-known HTTP status codes with their constant names.
const STATUS_CODE_NAMES: &[(u16, &str)] = &[
    (200, "OK"),
    (201, "CREATED"),
    (202, "ACCEPTED"),
    (204, "NO_CONTENT"),
    (301, "MOVED_PERMANENTLY"),
    (302, "FOUND"),
    (304, "NOT_MODIFIED"),
    (400, "BAD_REQUEST"),
    (401, "UNAUTHORIZED"),
    (403, "FORBIDDEN"),
    (404, "NOT_FOUND"),
    (405, "METHOD_NOT_ALLOWED"),
    (409, "CONFLICT"),
    (410, "GONE"),
    (422, "UNPROCESSABLE_ENTITY"),
    (429, "TOO_MANY_REQUESTS"),
    (500, "INTERNAL_SERVER_ERROR"),
    (501, "NOT_IMPLEMENTED"),
    (502, "BAD_GATEWAY"),
    (503, "SERVICE_UNAVAILABLE"),
    (504, "GATEWAY_TIMEOUT"),
];

/// Generate a StatusCode expression for a numeric status.
fn status_code_ident(status: u16) -> TokenStream {
    // Use well-known constants where possible
    if let Some((_, name)) = STATUS_CODE_NAMES.iter().find(|(code, _)| *code == status) {
        let ident = format_ident!("{}", name);
        quote! { ::axum::http::StatusCode::#ident }
    } else {
        quote! { ::axum::http::StatusCode::from_u16(#status).unwrap() }
    }
}
