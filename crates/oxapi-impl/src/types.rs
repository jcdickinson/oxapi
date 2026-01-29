//! Type generation using typify.

use std::collections::HashMap;

use heck::{AsSnakeCase, ToUpperCamelCase};
use openapiv3::{
    IntegerFormat, NumberFormat, ReferenceOr, Schema, SchemaKind, Type, VariantOrUnknownOrEmpty,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use typify::{TypeSpace, TypeSpaceSettings};

/// Result of generating a type for a schema, including any inline struct definitions.
pub struct GeneratedType {
    /// The type expression (e.g., `MyStruct`, `Vec<String>`, `serde_json::Value`)
    pub type_ref: TokenStream,
    /// Any struct definitions that need to be emitted (for inline object schemas)
    pub definitions: Vec<TokenStream>,
}

use crate::openapi::{Operation, OperationParam, ParamLocation, ParsedSpec, RequestBody};
use crate::{Error, GeneratedTypeKind, Result, TypeOverride, TypeOverrides};

/// Type generator that wraps typify's TypeSpace.
///
/// Uses a two-pass approach:
/// 1. Pass 1: Generate all types (component schemas + auto-generated types)
/// 2. Pass 2: Generate code that references types using the registry built in Pass 1
pub struct TypeGenerator {
    type_space: TypeSpace,
    /// Map from schema name (as in OpenAPI spec) to final type name (after typify processing)
    schema_to_type: HashMap<String, String>,
}

impl TypeGenerator {
    /// Create a new type generator from a parsed spec with default settings.
    pub fn new(spec: &ParsedSpec) -> Result<Self> {
        Self::with_settings(spec, TypeSpaceSettings::default(), HashMap::new())
    }

    /// Create a new type generator from a parsed spec with custom settings and renames.
    ///
    /// The renames map should contain original schema name -> new name mappings.
    /// These must match the patches configured in TypeSpaceSettings.
    pub fn with_settings(
        spec: &ParsedSpec,
        settings: TypeSpaceSettings,
        renames: HashMap<String, String>,
    ) -> Result<Self> {
        let mut type_space = TypeSpace::new(&settings);

        // Collect schema names before adding to type space
        let schema_names: Vec<String> = spec
            .components
            .as_ref()
            .map(|c| c.schemas.keys().cloned().collect())
            .unwrap_or_default();

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

        // First validate that all renames reference schemas that exist in the spec
        for original_name in renames.keys() {
            if !schema_names.contains(original_name) {
                return Err(Error::UnknownSchema {
                    name: original_name.clone(),
                    available: schema_names.join(", "),
                });
            }
        }

        // Build schema_to_type map by iterating TypeSpace to see what names typify generated
        let generated_names: std::collections::HashSet<String> =
            type_space.iter_types().map(|t| t.name()).collect();

        let mut schema_to_type = HashMap::new();
        for schema_name in &schema_names {
            // Check if there's a rename for this schema
            let expected_name = if let Some(renamed) = renames.get(schema_name) {
                renamed.to_upper_camel_case()
            } else {
                schema_name.to_upper_camel_case()
            };

            // Verify the type exists in TypeSpace
            if generated_names.contains(&expected_name) {
                schema_to_type.insert(schema_name.clone(), expected_name);
            } else {
                // Type doesn't exist - this shouldn't happen if typify is working correctly
                return Err(Error::TypeGenError(format!(
                    "typify did not generate expected type '{}' for schema '{}'",
                    expected_name, schema_name
                )));
            }
        }

        Ok(Self {
            type_space,
            schema_to_type,
        })
    }

    /// Generate all types as a TokenStream.
    pub fn generate_all_types(&self) -> TokenStream {
        self.type_space.to_stream()
    }

    /// Get the type name for a schema reference.
    /// Returns the actual name from TypeSpace (after any renames applied by typify).
    pub fn get_type_name(&self, reference: &str) -> Option<String> {
        // Extract the schema name from the reference
        let schema_name = reference.strip_prefix("#/components/schemas/")?;

        // Look up in our map (built from TypeSpace)
        self.schema_to_type.get(schema_name).cloned()
    }

    /// Check if a schema is inline (not a reference).
    pub fn is_inline_schema(schema: &ReferenceOr<Schema>) -> bool {
        matches!(schema, ReferenceOr::Item(_))
    }

    /// Generate a type for an inline schema (returns only the type reference, discarding definitions).
    /// For inline object types that need struct definitions, use `type_for_schema_with_definitions`.
    pub fn type_for_schema(&self, schema: &ReferenceOr<Schema>, name_hint: &str) -> TokenStream {
        self.type_for_schema_with_definitions(schema, name_hint)
            .type_ref
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
            ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, name_hint).type_ref,
        }
    }

    /// Generate a type for a schema, returning both the type reference and any generated definitions.
    /// Use this when you need to collect inline struct definitions.
    pub fn type_for_schema_with_definitions(
        &self,
        schema: &ReferenceOr<Schema>,
        name_hint: &str,
    ) -> GeneratedType {
        match schema {
            ReferenceOr::Reference { reference } => {
                let type_ref = if let Some(type_name) = self.get_type_name(reference) {
                    let ident = format_ident!("{}", type_name);
                    quote! { #ident }
                } else {
                    quote! { serde_json::Value }
                };
                GeneratedType {
                    type_ref,
                    definitions: vec![],
                }
            }
            ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, name_hint),
        }
    }

    /// Generate a type for an inline schema, returning both the type reference and any definitions.
    fn type_for_inline_schema(&self, schema: &Schema, name_hint: &str) -> GeneratedType {
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(_)) => GeneratedType {
                type_ref: quote! { String },
                definitions: vec![],
            },
            SchemaKind::Type(Type::Integer(int_type)) => {
                let type_ref = match &int_type.format {
                    VariantOrUnknownOrEmpty::Item(IntegerFormat::Int32) => quote! { i32 },
                    VariantOrUnknownOrEmpty::Item(IntegerFormat::Int64) => quote! { i64 },
                    _ => quote! { i64 },
                };
                GeneratedType {
                    type_ref,
                    definitions: vec![],
                }
            }
            SchemaKind::Type(Type::Number(num_type)) => {
                let type_ref = match &num_type.format {
                    VariantOrUnknownOrEmpty::Item(NumberFormat::Float) => quote! { f32 },
                    VariantOrUnknownOrEmpty::Item(NumberFormat::Double) => quote! { f64 },
                    _ => quote! { f64 },
                };
                GeneratedType {
                    type_ref,
                    definitions: vec![],
                }
            }
            SchemaKind::Type(Type::Boolean(_)) => GeneratedType {
                type_ref: quote! { bool },
                definitions: vec![],
            },
            SchemaKind::Type(Type::Array(arr)) => {
                if let Some(items) = &arr.items {
                    let inner = self.type_for_boxed_schema(items, &format!("{}Item", name_hint));
                    GeneratedType {
                        type_ref: quote! { Vec<#inner> },
                        definitions: vec![],
                    }
                } else {
                    GeneratedType {
                        type_ref: quote! { Vec<serde_json::Value> },
                        definitions: vec![],
                    }
                }
            }
            SchemaKind::Type(Type::Object(obj)) => {
                self.generate_inline_struct(obj, &schema.schema_data, name_hint)
            }
            _ => GeneratedType {
                type_ref: quote! { serde_json::Value },
                definitions: vec![],
            },
        }
    }

    /// Generate an inline struct for an object schema.
    fn generate_inline_struct(
        &self,
        obj: &openapiv3::ObjectType,
        _schema_data: &openapiv3::SchemaData,
        name_hint: &str,
    ) -> GeneratedType {
        let struct_name = format_ident!("{}", name_hint.to_upper_camel_case());
        let mut definitions = Vec::new();

        // Generate fields for each property
        let fields: Vec<_> = obj
            .properties
            .iter()
            .map(|(prop_name, prop_schema)| {
                let field_name = format_ident!("{}", AsSnakeCase(prop_name).to_string());
                let is_required = obj.required.contains(prop_name);

                // Generate the inner type, collecting any nested definitions
                let inner_hint = format!("{}{}", name_hint, prop_name.to_upper_camel_case());
                let generated = match prop_schema {
                    ReferenceOr::Reference { reference } => {
                        let type_ref = if let Some(type_name) = self.get_type_name(reference) {
                            let ident = format_ident!("{}", type_name);
                            quote! { #ident }
                        } else {
                            quote! { serde_json::Value }
                        };
                        GeneratedType {
                            type_ref,
                            definitions: vec![],
                        }
                    }
                    ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, &inner_hint),
                };

                // Collect nested definitions
                definitions.extend(generated.definitions);

                let field_type = if is_required {
                    generated.type_ref
                } else {
                    let inner = generated.type_ref;
                    quote! { Option<#inner> }
                };

                // Add serde rename if field name differs from property name
                let snake_name = AsSnakeCase(prop_name).to_string();
                if snake_name != *prop_name {
                    quote! {
                        #[serde(rename = #prop_name)]
                        pub #field_name: #field_type
                    }
                } else {
                    quote! { pub #field_name: #field_type }
                }
            })
            .collect();

        // Generate the struct definition
        let struct_def = quote! {
            #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
            pub struct #struct_name {
                #(#fields,)*
            }
        };

        definitions.push(struct_def);

        GeneratedType {
            type_ref: quote! { #struct_name },
            definitions,
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
            name.clone()
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

        // Use struct name span so errors point to user's override if provided
        let definition = quote_spanned! { struct_name.span() =>
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
    serde_value::to_value(schema)
        .map_err(|e| Error::TypeGenError(format!("failed to serialize schema: {}", e)))?
        .deserialize_into::<schemars::schema::Schema>()
        .map_err(|e| Error::TypeGenError(format!("failed to deserialize schema: {}", e)))
}
