//! Method transformation for trait methods.

use openapiv3::{ReferenceOr, SchemaKind, Type as OapiType};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{FnArg, GenericArgument, PathArguments, Type};

use crate::openapi::Operation;
use crate::types::TypeGenerator;
use crate::{GeneratedTypeKind, Generator, TypeOverride};

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

    /// Transform a trait method based on its operation.
    pub fn transform(&self, method: &syn::TraitItemFn, op: &Operation) -> syn::Result<TokenStream> {
        let op_name = op.name();

        let types_mod = self.types_mod;
        let overrides = self.generator.type_overrides();

        let suffixes = self.generator.response_suffixes();

        // Get Ok type (may be renamed or replaced)
        let ok_type: TokenStream = match overrides.get(op.method, &op.path, GeneratedTypeKind::Ok) {
            Some(TypeOverride::Rename { name, .. }) => {
                let ident = format_ident!("{}", name);
                quote! { #types_mod::#ident }
            }
            Some(TypeOverride::Replace(replacement)) => replacement.clone(),
            None => {
                let ident = format_ident!("{}{}", op_name, suffixes.ok_suffix);
                quote! { #types_mod::#ident }
            }
        };

        // Transform each parameter, filling in type elisions
        let transformed_params = self.transform_params(method, op)?;

        // Determine if async and build return type
        let is_async = method.sig.asyncness.is_some();

        // Only wrap in Result if the operation has error responses defined
        let return_type = if op.has_error_responses() {
            // Get Err type (may be renamed or replaced)
            let err_type: TokenStream =
                match overrides.get(op.method, &op.path, GeneratedTypeKind::Err) {
                    Some(TypeOverride::Rename { name, .. }) => {
                        let ident = format_ident!("{}", name);
                        quote! { #types_mod::#ident }
                    }
                    Some(TypeOverride::Replace(replacement)) => replacement.clone(),
                    None => {
                        let ident = format_ident!("{}{}", op_name, suffixes.err_suffix);
                        quote! { #types_mod::#ident }
                    }
                };
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
    fn transform_params(
        &self,
        method: &syn::TraitItemFn,
        op: &Operation,
    ) -> syn::Result<TokenStream> {
        let type_gen = self.generator.type_generator();

        let mut transformed = Vec::new();

        for arg in &method.sig.inputs {
            match arg {
                FnArg::Receiver(_) => {
                    return Err(syn::Error::new_spanned(
                        arg,
                        "oxapi trait methods must be static (no self)",
                    ));
                }
                FnArg::Typed(pat_type) => {
                    let pat = &pat_type.pat;
                    let ty = &pat_type.ty;

                    // Check if this is an extractor with type elision
                    let transformed_ty = self.transform_type(ty, op, type_gen)?;

                    transformed.push(quote! { #pat: #transformed_ty });
                }
            }
        }

        Ok(quote! { #(#transformed),* })
    }

    /// Transform a type, filling in elisions like `Path<_>`.
    fn transform_type(
        &self,
        ty: &Type,
        op: &Operation,
        type_gen: &TypeGenerator,
    ) -> syn::Result<TokenStream> {
        let types_mod = self.types_mod;

        match ty {
            Type::Path(type_path) => {
                let last_segment = type_path
                    .path
                    .segments
                    .last()
                    .ok_or_else(|| syn::Error::new_spanned(ty, "empty type path"))?;

                let type_name = last_segment.ident.to_string();

                match type_name.as_str() {
                    "Path" => {
                        // Fill in path parameter type
                        let inner = self.get_or_infer_inner(&last_segment.arguments, || {
                            type_gen.generate_path_type(op)
                        })?;
                        Ok(quote! { ::axum::extract::Path<#inner> })
                    }
                    "Query" => {
                        // Fill in query parameter type
                        let overrides = self.generator.type_overrides();
                        let inner = self.get_or_infer_inner(&last_segment.arguments, || {
                            // Check for replacement first
                            if let Some(TypeOverride::Replace(replacement)) =
                                overrides.get(op.method, &op.path, GeneratedTypeKind::Query)
                            {
                                return replacement.clone();
                            }

                            // Generate a query struct if needed (may be renamed)
                            if let Some((name, _)) = type_gen.generate_query_struct(op, overrides) {
                                quote! { #types_mod::#name }
                            } else {
                                quote! { () }
                            }
                        })?;
                        Ok(quote! { ::axum::extract::Query<#inner> })
                    }
                    "Json" => {
                        // Fill in request body type
                        let inner = self.get_or_infer_inner(&last_segment.arguments, || {
                            if let Some(body) = &op.request_body {
                                if let Some(schema) = &body.schema {
                                    let op_name = op.operation_id.as_deref().unwrap_or(&op.path);
                                    let body_type = type_gen.request_body_type(body, op_name);

                                    // Check if this type needs to be prefixed with the types module.
                                    // References to component schemas and inline objects need prefixing
                                    // because their types are generated in the types module.
                                    let needs_prefix = match schema {
                                        ReferenceOr::Reference { .. } => true,
                                        ReferenceOr::Item(inline) => matches!(
                                            &inline.schema_kind,
                                            SchemaKind::Type(OapiType::Object(_))
                                        ),
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
                        })?;
                        Ok(quote! { ::axum::extract::Json<#inner> })
                    }
                    "State" => {
                        // State type is user-provided, don't modify
                        Ok(quote! { #ty })
                    }
                    _ => {
                        // Other types pass through unchanged
                        Ok(quote! { #ty })
                    }
                }
            }
            _ => {
                // Non-path types pass through unchanged
                Ok(quote! { #ty })
            }
        }
    }

    /// Get the inner type from generics, or infer it if it's `_`.
    fn get_or_infer_inner<F>(&self, args: &PathArguments, infer: F) -> syn::Result<TokenStream>
    where
        F: FnOnce() -> TokenStream,
    {
        match args {
            PathArguments::None => {
                // No generics, infer the type
                Ok(infer())
            }
            PathArguments::AngleBracketed(args) => {
                if let Some(GenericArgument::Type(Type::Infer(_))) = args.args.first() {
                    // Type is `_`, infer it
                    Ok(infer())
                } else if let Some(GenericArgument::Type(ty)) = args.args.first() {
                    // Type is explicit, use it
                    Ok(quote! { #ty })
                } else {
                    Err(syn::Error::new_spanned(args, "expected type argument"))
                }
            }
            PathArguments::Parenthesized(_) => Err(syn::Error::new_spanned(
                args,
                "unexpected parenthesized arguments",
            )),
        }
    }
}
