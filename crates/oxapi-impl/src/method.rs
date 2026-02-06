//! Method transformation for trait methods.

use openapiv3::{ReferenceOr, SchemaKind, Type as OapiType};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{FnArg, GenericArgument, PathArguments, Type};

use crate::openapi::Operation;
use crate::types::TypeGenerator;
use crate::{GeneratedTypeKind, Generator, TypeOverride};

/// Role of a parameter in a handler function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamRole {
    /// Path parameters (e.g., `/items/{id}`)
    Path,
    /// Query parameters (e.g., `?foo=bar`)
    Query,
    /// Request body
    Body,
    /// Other extractors (State, custom extractors, etc.)
    Other,
}

/// Transforms user-defined trait methods based on OpenAPI operations.
pub struct MethodTransformer<'a> {
    generator: &'a Generator,
    types_mod: &'a syn::Ident,
}

impl<'a> MethodTransformer<'a> {
    pub fn new(generator: &'a Generator, types_mod: &'a syn::Ident) -> Self {
        Self {
            generator,
            types_mod,
        }
    }

    /// Resolve a response type (Ok or Err) for an operation, applying any overrides.
    fn resolve_response_type(
        &self,
        op: &Operation,
        kind: GeneratedTypeKind,
        default_suffix: &str,
    ) -> TokenStream {
        let types_mod = self.types_mod;
        let overrides = self.generator.type_overrides();
        let op_name = op.name();

        match overrides.get(op.method, &op.path, kind) {
            Some(TypeOverride::Rename { name, .. }) => {
                let ident = format_ident!("{}", name);
                quote! { #types_mod::#ident }
            }
            Some(TypeOverride::Replace(replacement)) => replacement.clone(),
            None => {
                let ident = format_ident!("{}{}", op_name, default_suffix);
                quote! { #types_mod::#ident }
            }
        }
    }

    /// Transform a trait method based on its operation.
    ///
    /// The `param_roles` argument contains the role for each parameter (if any explicit
    /// `#[oxapi(path)]`, `#[oxapi(query)]`, or `#[oxapi(body)]` attribute was found).
    /// If `None`, inference is used based on HTTP method and parameter position.
    pub fn transform(
        &self,
        method: &syn::TraitItemFn,
        op: &Operation,
        param_roles: &[Option<ParamRole>],
    ) -> syn::Result<TokenStream> {
        let suffixes = self.generator.response_suffixes();

        // Get Ok type (may be renamed or replaced)
        let ok_type = self.resolve_response_type(op, GeneratedTypeKind::Ok, &suffixes.ok_suffix);

        // Transform each parameter, filling in type elisions
        let transformed_params = self.transform_params(method, op, param_roles)?;

        // Determine if async and build return type
        let is_async = method.sig.asyncness.is_some();

        // Only wrap in Result if the operation has error responses defined
        let return_type = if op.has_error_responses() {
            let err_type =
                self.resolve_response_type(op, GeneratedTypeKind::Err, &suffixes.err_suffix);
            quote! { ::core::result::Result<#ok_type, #err_type> }
        } else {
            // No errors in spec, return Ok type directly
            ok_type.clone()
        };

        let method_name = &method.sig.ident;

        // Build the signature
        if is_async {
            // Rewrite async fn to fn -> impl Send + Future<Output = ...>
            Ok(quote! {
                fn #method_name(#transformed_params) -> impl ::core::marker::Send + ::core::future::Future<Output = #return_type>;
            })
        } else {
            // Sync function
            Ok(quote! {
                fn #method_name(#transformed_params) -> #return_type;
            })
        }
    }

    /// Transform the parameters, filling in type elisions.
    ///
    /// The `param_roles` argument contains the role for each parameter that has an
    /// explicit `#[oxapi(...)]` attribute.
    ///
    /// **Important**: If ANY parameter has an explicit role attribute, inference is
    /// disabled for ALL parameters. This means you must either:
    /// - Use no explicit attrs (rely entirely on type name detection), OR
    /// - Use explicit attrs on ALL parameters that need type elision
    ///
    /// This allows non-standard use cases like adding a body to a GET request.
    fn transform_params(
        &self,
        method: &syn::TraitItemFn,
        op: &Operation,
        param_roles: &[Option<ParamRole>],
    ) -> syn::Result<TokenStream> {
        let type_gen = self.generator.type_generator();

        let mut all_params: Vec<&syn::PatType> = Vec::new();

        for arg in &method.sig.inputs {
            match arg {
                FnArg::Receiver(_) => {
                    return Err(syn::Error::new_spanned(
                        arg,
                        "oxapi trait methods must be static (no self)",
                    ));
                }
                FnArg::Typed(pat_type) => {
                    all_params.push(pat_type);
                }
            }
        }

        // Check if any parameter has an explicit role attribute.
        // If so, inference is disabled for ALL parameters - only explicit attrs are used.
        let has_any_explicit = param_roles.iter().any(|r| r.is_some());

        // Compute roles for all parameters
        let roles: Vec<ParamRole> = all_params
            .iter()
            .enumerate()
            .map(|(idx, pat_type)| {
                // Explicit attr always applies
                if let Some(Some(role)) = param_roles.get(idx) {
                    return *role;
                }
                // If any explicit attr is present, don't infer - use Other
                if has_any_explicit {
                    return ParamRole::Other;
                }
                // Otherwise detect from type name
                detect_role_from_type(&pat_type.ty)
            })
            .collect();

        // Transform each parameter
        let mut transformed = Vec::new();
        for (idx, pat_type) in all_params.iter().enumerate() {
            let pat = &pat_type.pat;
            let ty = &pat_type.ty;
            let role = roles.get(idx).copied().unwrap_or(ParamRole::Other);

            let transformed_ty = self.transform_type_with_role(ty, op, type_gen, role)?;
            transformed.push(quote! { #pat: #transformed_ty });
        }

        Ok(quote! { #(#transformed),* })
    }

    /// Transform a type based on its role, preserving the user's extractor type.
    ///
    /// This method only replaces the `_` in a type like `MyExtractor<_>` with the
    /// inferred type. It does NOT replace the extractor itself with a fully-qualified path.
    fn transform_type_with_role(
        &self,
        ty: &Type,
        op: &Operation,
        type_gen: &TypeGenerator,
        role: ParamRole,
    ) -> syn::Result<TokenStream> {
        match ty {
            Type::Path(type_path) => {
                // Check if this type has elision that needs to be filled
                if !has_type_elision(ty) {
                    // No elision, pass through unchanged
                    return Ok(quote! { #ty });
                }

                // Get all segments except the last one (the prefix path)
                let prefix_segments: Vec<_> = type_path
                    .path
                    .segments
                    .iter()
                    .take(type_path.path.segments.len().saturating_sub(1))
                    .collect();

                let last_segment = type_path
                    .path
                    .segments
                    .last()
                    .ok_or_else(|| syn::Error::new_spanned(ty, "empty type path"))?;

                // Infer the inner type based on the role
                let inner_type = match role {
                    ParamRole::Path => type_gen.generate_path_type(op),
                    ParamRole::Query => self.generate_query_inner_type(op, type_gen),
                    ParamRole::Body => self.generate_body_inner_type(op, type_gen),
                    ParamRole::Other => {
                        // Other role: pass through unchanged (State, custom extractors, etc.)
                        return Ok(quote! { #ty });
                    }
                };

                // Reconstruct the type with the inferred inner type
                let type_ident = &last_segment.ident;
                let leading_colon = type_path.path.leading_colon;

                if prefix_segments.is_empty() {
                    // Simple type like `Path<_>` or `MyExtractor<_>`
                    Ok(quote! { #leading_colon #type_ident<#inner_type> })
                } else {
                    // Qualified path like `axum::extract::Path<_>`
                    Ok(quote! { #leading_colon #(#prefix_segments ::)* #type_ident<#inner_type> })
                }
            }
            _ => {
                // Non-path types pass through unchanged
                Ok(quote! { #ty })
            }
        }
    }

    /// Generate the inner type for a query parameter.
    fn generate_query_inner_type(&self, op: &Operation, type_gen: &TypeGenerator) -> TokenStream {
        let types_mod = self.types_mod;
        let overrides = self.generator.type_overrides();

        // Check for replacement first
        if let Some(TypeOverride::Replace(replacement)) =
            overrides.get(op.method, &op.path, GeneratedTypeKind::Query)
        {
            return replacement.clone();
        }

        // Generate a query struct if the operation has query params
        if let Some((name, _)) = type_gen.generate_query_struct(op, overrides) {
            quote! { #types_mod::#name }
        } else {
            // Fallback to HashMap<String, String> for operations without query params
            quote! { ::std::collections::HashMap<String, String> }
        }
    }

    /// Generate the inner type for a request body.
    fn generate_body_inner_type(&self, op: &Operation, type_gen: &TypeGenerator) -> TokenStream {
        let types_mod = self.types_mod;

        if let Some(body) = &op.request_body {
            if let Some(schema) = &body.schema {
                let op_name = op.operation_id.as_deref().unwrap_or(&op.path);
                let body_type = type_gen.request_body_type(body, op_name);

                // Check if this type needs to be prefixed with the types module.
                let needs_prefix = match schema {
                    ReferenceOr::Reference { .. } => true,
                    ReferenceOr::Item(inline) => {
                        matches!(&inline.schema_kind, SchemaKind::Type(OapiType::Object(_)))
                    }
                };

                if needs_prefix {
                    quote! { #types_mod::#body_type }
                } else {
                    body_type
                }
            } else {
                quote! { serde_json::Value }
            }
        } else {
            quote! { serde_json::Value }
        }
    }
}

/// Check if a type contains type elision (`_`).
fn has_type_elision(ty: &Type) -> bool {
    match ty {
        Type::Infer(_) => true,
        Type::Path(type_path) => {
            // Check the generic arguments of the last segment
            if let Some(last) = type_path.path.segments.last()
                && let PathArguments::AngleBracketed(args) = &last.arguments
            {
                return args
                    .args
                    .iter()
                    .any(|arg| matches!(arg, GenericArgument::Type(Type::Infer(_))));
            }
            false
        }
        _ => false,
    }
}

/// Detect the role from the extractor type name.
/// Recognizes: Path, Query, Json (as simple name or full path like axum::extract::Path).
fn detect_role_from_type(ty: &Type) -> ParamRole {
    if let Type::Path(type_path) = ty
        && let Some(last) = type_path.path.segments.last()
    {
        let name = last.ident.to_string();
        match name.as_str() {
            "Path" => return ParamRole::Path,
            "Query" => return ParamRole::Query,
            "Json" => return ParamRole::Body,
            _ => {}
        }
    }
    ParamRole::Other
}
