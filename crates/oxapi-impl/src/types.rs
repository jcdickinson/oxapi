//! Type generation using typify.

use std::collections::HashMap;

use heck::ToUpperCamelCase;
use openapiv3::{
    IntegerFormat, NumberFormat, ReferenceOr, Schema, SchemaKind, Type, VariantOrUnknownOrEmpty,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use typify::{TypeSpace, TypeSpaceSettings};

use crate::openapi::{Operation, OperationParam, ParamLocation, ParsedSpec, RequestBody};
use crate::{Error, GeneratedTypeKind, Result, TypeOverride, TypeOverrides};

/// Type generator that wraps typify's TypeSpace.
pub struct TypeGenerator {
    type_space: TypeSpace,
    /// Map from original schema name to renamed name (both in PascalCase)
    renames: HashMap<String, String>,
}

impl TypeGenerator {
    /// Create a new type generator from a parsed spec with default settings.
    pub fn new(spec: &ParsedSpec) -> Result<Self> {
        Self::with_settings(spec, TypeSpaceSettings::default())
    }

    /// Create a new type generator from a parsed spec with custom settings.
    pub fn with_settings(spec: &ParsedSpec, settings: TypeSpaceSettings) -> Result<Self> {
        let mut type_space = TypeSpace::new(&settings);

        // Add all component schemas to the type space
        if let Some(components) = &spec.components {
            let schemas = components
                .schemas
                .iter()
                .map(|(name, schema)| {
                    let schema = convert_to_schemars(schema)?;
                    Ok((name.clone(), schema))
                })
                .collect::<Result<Vec<_>>>()?;

            type_space
                .add_ref_types(schemas.into_iter())
                .map_err(|e| Error::TypeGenError(e.to_string()))?;
        }

        Ok(Self {
            type_space,
            renames: HashMap::new(),
        })
    }

    /// Set the rename mappings (original name -> new name, both in original case from spec).
    /// TypeSpaceSettings doesn't expose its patches for reverse lookup, so we track renames
    /// separately to resolve type references correctly.
    pub fn set_renames(&mut self, renames: HashMap<String, String>) {
        // Convert to PascalCase for lookup
        self.renames = renames
            .into_iter()
            .map(|(k, v)| (k.to_upper_camel_case(), v.to_upper_camel_case()))
            .collect();
    }

    /// Generate all types as a TokenStream.
    pub fn generate_all_types(&self) -> TokenStream {
        self.type_space.to_stream()
    }

    /// Get the type name for a schema reference, applying any renames.
    pub fn get_type_name(&self, reference: &str) -> Option<String> {
        // Extract the type name from the reference
        let name = reference.strip_prefix("#/components/schemas/")?;
        let pascal_name = name.to_upper_camel_case();

        // Check if this type has been renamed
        if let Some(renamed) = self.renames.get(&pascal_name) {
            Some(renamed.clone())
        } else {
            Some(pascal_name)
        }
    }

    /// Generate a type for an inline schema.
    pub fn type_for_schema(&self, schema: &ReferenceOr<Schema>, name_hint: &str) -> TokenStream {
        match schema {
            ReferenceOr::Reference { reference } => {
                if let Some(type_name) = self.get_type_name(reference) {
                    let ident = format_ident!("{}", type_name);
                    quote! { #ident }
                } else {
                    quote! { serde_json::Value }
                }
            }
            ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, name_hint),
        }
    }

    /// Generate a type for a boxed schema reference.
    pub fn type_for_boxed_schema(
        &self,
        schema: &ReferenceOr<Box<Schema>>,
        name_hint: &str,
    ) -> TokenStream {
        match schema {
            ReferenceOr::Reference { reference } => {
                if let Some(type_name) = self.get_type_name(reference) {
                    let ident = format_ident!("{}", type_name);
                    quote! { #ident }
                } else {
                    quote! { serde_json::Value }
                }
            }
            ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, name_hint),
        }
    }

    /// Generate a type for an inline schema.
    fn type_for_inline_schema(&self, schema: &Schema, name_hint: &str) -> TokenStream {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(_)) => quote! { String },
            SchemaKind::Type(Type::Integer(int_type)) => match &int_type.format {
                VariantOrUnknownOrEmpty::Item(IntegerFormat::Int32) => quote! { i32 },
                VariantOrUnknownOrEmpty::Item(IntegerFormat::Int64) => quote! { i64 },
                _ => quote! { i64 },
            },
            SchemaKind::Type(Type::Number(num_type)) => match &num_type.format {
                VariantOrUnknownOrEmpty::Item(NumberFormat::Float) => quote! { f32 },
                VariantOrUnknownOrEmpty::Item(NumberFormat::Double) => quote! { f64 },
                _ => quote! { f64 },
            },
            SchemaKind::Type(Type::Boolean(_)) => quote! { bool },
            SchemaKind::Type(Type::Array(arr)) => {
                if let Some(items) = &arr.items {
                    let inner = self.type_for_boxed_schema(items, &format!("{}Item", name_hint));
                    quote! { Vec<#inner> }
                } else {
                    quote! { Vec<serde_json::Value> }
                }
            }
            SchemaKind::Type(Type::Object(_)) => {
                // For inline objects, fall back to serde_json::Value
                // In a more complete impl, we'd generate a struct
                quote! { serde_json::Value }
            }
            _ => quote! { serde_json::Value },
        }
    }

    /// Get the type for a path parameter.
    pub fn path_param_type(&self, param: &OperationParam) -> TokenStream {
        if let Some(schema) = &param.schema {
            self.type_for_schema(schema, &param.name.to_upper_camel_case())
        } else {
            quote! { String }
        }
    }

    /// Get the type for a query parameter.
    pub fn query_param_type(&self, param: &OperationParam) -> TokenStream {
        if let Some(schema) = &param.schema {
            self.type_for_schema(schema, &param.name.to_upper_camel_case())
        } else {
            quote! { String }
        }
    }

    /// Get the type for a request body.
    pub fn request_body_type(&self, body: &RequestBody, op_name: &str) -> TokenStream {
        if let Some(schema) = &body.schema {
            self.type_for_schema(schema, &format!("{}Body", op_name.to_upper_camel_case()))
        } else {
            quote! { serde_json::Value }
        }
    }

    /// Get the type for a response body.
    pub fn response_type(
        &self,
        schema: &Option<ReferenceOr<Schema>>,
        op_name: &str,
        status: u16,
    ) -> TokenStream {
        if let Some(schema) = schema {
            self.type_for_schema(
                schema,
                &format!("{}Response{}", op_name.to_upper_camel_case(), status),
            )
        } else {
            quote! { () }
        }
    }

    /// Generate a query params struct for an operation.
    pub fn generate_query_struct(
        &self,
        op: &Operation,
        overrides: &TypeOverrides,
    ) -> Option<(syn::Ident, TokenStream)> {
        // Check if replaced
        if overrides.is_replaced(op.method, &op.path, GeneratedTypeKind::Query) {
            return None;
        }

        let query_params: Vec<_> = op
            .parameters
            .iter()
            .filter(|p| p.location == ParamLocation::Query)
            .collect();

        if query_params.is_empty() {
            return None;
        }

        let default_name = format!(
            "{}Query",
            op.operation_id
                .as_deref()
                .unwrap_or(&op.path)
                .to_upper_camel_case()
        );

        // Check for rename override
        let struct_name = if let Some(TypeOverride::Rename { name, .. }) =
            overrides.get(op.method, &op.path, GeneratedTypeKind::Query)
        {
            format_ident!("{}", name)
        } else {
            format_ident!("{}", default_name)
        };

        let fields = query_params.iter().map(|param| {
            let name = format_ident!("{}", heck::AsSnakeCase(&param.name).to_string());
            let ty = self.query_param_type(param);

            if param.required {
                quote! { pub #name: #ty }
            } else {
                quote! { pub #name: Option<#ty> }
            }
        });

        let definition = quote! {
            #[derive(Debug, Clone, serde::Deserialize)]
            pub struct #struct_name {
                #(#fields,)*
            }
        };

        Some((struct_name, definition))
    }

    /// Generate a path params type for an operation (usually a tuple).
    pub fn generate_path_type(&self, op: &Operation) -> TokenStream {
        let path_params: Vec<_> = op
            .parameters
            .iter()
            .filter(|p| p.location == ParamLocation::Path)
            .collect();

        if path_params.is_empty() {
            return quote! { () };
        }

        if path_params.len() == 1 {
            return self.path_param_type(path_params[0]);
        }

        let types = path_params.iter().map(|p| self.path_param_type(p));
        quote! { (#(#types),*) }
    }
}

/// Convert an OpenAPI schema to a schemars schema for typify.
fn convert_to_schemars(schema: &ReferenceOr<Schema>) -> Result<schemars::schema::Schema> {
    // Serialize to JSON and deserialize as schemars schema
    let json = serde_json::to_value(schema)
        .map_err(|e| Error::TypeGenError(format!("failed to serialize schema: {}", e)))?;

    serde_json::from_value(json)
        .map_err(|e| Error::TypeGenError(format!("failed to convert schema: {}", e)))
}
