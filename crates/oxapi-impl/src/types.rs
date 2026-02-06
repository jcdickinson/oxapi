//! Type generation using typify.

use std::collections::HashMap;

use heck::{AsSnakeCase, ToUpperCamelCase};
use openapiv3::{ReferenceOr, Schema, SchemaKind, Type};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use typify::{TypeId, TypeSpace, TypeSpaceSettings};

/// Result of generating a type for a schema, including any inline struct definitions.
pub struct GeneratedType {
    /// The type expression (e.g., `MyStruct`, `Vec<String>`, `serde_json::Value`)
    pub type_ref: TokenStream,
    /// Any struct definitions that need to be emitted (for inline object schemas)
    pub definitions: Vec<TokenStream>,
}

impl GeneratedType {
    /// Create a GeneratedType with just a type reference (no inline definitions).
    pub fn simple(type_ref: TokenStream) -> Self {
        Self {
            type_ref,
            definitions: vec![],
        }
    }
}

use crate::openapi::{Operation, OperationParam, ParamLocation, ParsedSpec, RequestBody};
use crate::{Error, GeneratedTypeKind, Result, TypeOverride, TypeOverrides};

/// Type generator that wraps typify's TypeSpace.
///
/// Uses a two-pass approach:
/// 1. Pass 1: Generate all types (component schemas + inline param schemas)
/// 2. Pass 2: Generate code that references types using the registry built in Pass 1
pub struct TypeGenerator {
    type_space: TypeSpace,
    /// Map from schema name (as in OpenAPI spec) to final type name (after typify processing)
    schema_to_type: HashMap<String, String>,
    /// Map from inline schema name hints to typify TypeIds for lookup during generation
    inline_types: HashMap<String, TypeId>,
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
                    let schema = to_schemars(schema)?;
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

        // Pre-populate inline schemas for all operation parameters
        let mut inline_types = HashMap::new();
        for op in spec.operations() {
            let op_name = op.raw_name();

            // Add path and query parameter schemas
            for param in &op.parameters {
                if let Some(ReferenceOr::Item(schema)) = &param.schema {
                    let name_hint = param.name.to_upper_camel_case();
                    register_inline_schema(&mut type_space, &mut inline_types, schema, name_hint);
                }
            }

            // Add request body schemas
            if let Some(body) = &op.request_body
                && let Some(ReferenceOr::Item(schema)) = &body.schema
            {
                let name_hint = format!("{}Body", op_name.to_upper_camel_case());
                register_inline_schema(&mut type_space, &mut inline_types, schema, name_hint);
            }

            // Add response body schemas
            for resp in &op.responses {
                if let Some(ReferenceOr::Item(schema)) = &resp.schema {
                    let status_suffix = match resp.status_code {
                        crate::openapi::ResponseStatus::Code(code) => code.to_string(),
                        crate::openapi::ResponseStatus::Default => "Default".to_string(),
                    };
                    let name_hint =
                        format!("{}Response{}", op_name.to_upper_camel_case(), status_suffix);
                    register_inline_schema(&mut type_space, &mut inline_types, schema, name_hint);
                }
            }
        }

        Ok(Self {
            type_space,
            schema_to_type,
            inline_types,
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

    /// Resolve a schema reference to a type TokenStream.
    /// Returns the type ident if found, or `serde_json::Value` as fallback.
    fn resolve_reference(&self, reference: &str) -> TokenStream {
        if let Some(type_name) = self.get_type_name(reference) {
            let ident = format_ident!("{}", type_name);
            quote! { #ident }
        } else {
            quote! { serde_json::Value }
        }
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
            ReferenceOr::Reference { reference } => self.resolve_reference(reference),
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
                GeneratedType::simple(self.resolve_reference(reference))
            }
            ReferenceOr::Item(schema) => self.type_for_inline_schema(schema, name_hint),
        }
    }

    /// Generate a type for an inline schema, returning both the type reference and any definitions.
    ///
    /// First checks if the schema was pre-registered with typify during initialization.
    /// If found, uses typify's generated ident. Otherwise falls back to manual handling.
    fn type_for_inline_schema(&self, schema: &Schema, name_hint: &str) -> GeneratedType {
        // Check if this schema was pre-registered with typify
        if let Some(type_id) = self.inline_types.get(name_hint)
            && let Ok(typ) = self.type_space.get_type(type_id)
        {
            return GeneratedType::simple(typ.ident());
        }

        // Fall back to manual handling for schemas not pre-registered
        // (e.g., nested schemas within arrays/objects that weren't traversed during init)
        match &schema.schema_kind {
            SchemaKind::Type(Type::String(_)) => GeneratedType::simple(quote! { String }),
            SchemaKind::Type(Type::Integer(_)) => GeneratedType::simple(quote! { i64 }),
            SchemaKind::Type(Type::Number(_)) => GeneratedType::simple(quote! { f64 }),
            SchemaKind::Type(Type::Boolean(_)) => GeneratedType::simple(quote! { bool }),
            SchemaKind::Type(Type::Array(arr)) => {
                if let Some(items) = &arr.items {
                    let inner = self.type_for_boxed_schema(items, &format!("{}Item", name_hint));
                    GeneratedType::simple(quote! { Vec<#inner> })
                } else {
                    GeneratedType::simple(quote! { Vec<serde_json::Value> })
                }
            }
            SchemaKind::Type(Type::Object(obj)) => self.generate_inline_struct(obj, name_hint),
            _ => GeneratedType::simple(quote! { serde_json::Value }),
        }
    }

    /// Generate an inline struct for an object schema.
    fn generate_inline_struct(
        &self,
        obj: &openapiv3::ObjectType,
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
                        GeneratedType::simple(self.resolve_reference(reference))
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

    /// Get the type for a path or query parameter.
    pub fn param_type(&self, param: &OperationParam) -> TokenStream {
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

    /// Generate a query params struct for an operation.
    ///
    /// If `unknown_field` is provided, an additional `#[serde(flatten)]` field with that name
    /// will be added to capture unknown query parameters as `HashMap<String, String>`.
    pub fn generate_query_struct(
        &self,
        op: &Operation,
        overrides: &TypeOverrides,
        unknown_field: Option<&syn::Ident>,
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

        // Allow struct generation with unknown_field even if no query params
        if query_params.is_empty() && unknown_field.is_none() {
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
            let ty = self.param_type(param);

            if param.required {
                quote! { pub #name: #ty }
            } else {
                quote! { pub #name: Option<#ty> }
            }
        });

        // Generate unknown field with serde(flatten) if requested
        let unknown_field_def = unknown_field.map(|name| {
            quote! {
                #[serde(flatten)]
                pub #name: ::std::collections::HashMap<String, String>
            }
        });

        // Use struct name span so errors point to user's override if provided
        let definition = quote_spanned! { struct_name.span() =>
            #[derive(Debug, Clone, serde::Deserialize)]
            pub struct #struct_name {
                #(#fields,)*
                #unknown_field_def
            }
        };

        Some((struct_name, definition))
    }

    /// Generate a path params struct for an operation.
    ///
    /// Returns `Some((struct_name, struct_definition))` if the operation has path params,
    /// or `None` if there are no path params or the struct is replaced by an override.
    ///
    /// Path parameters are sorted by their position in the URL path to ensure correct
    /// serde deserialization order.
    pub fn generate_path_struct(
        &self,
        op: &Operation,
        overrides: &TypeOverrides,
    ) -> Option<(syn::Ident, TokenStream)> {
        // Check if replaced
        if overrides.is_replaced(op.method, &op.path, GeneratedTypeKind::Path) {
            return None;
        }

        // Get path params and sort by position in URL
        let mut path_params: Vec<_> = op
            .parameters
            .iter()
            .filter(|p| p.location == ParamLocation::Path)
            .collect();

        if path_params.is_empty() {
            return None;
        }

        // Sort by position in the path string
        path_params.sort_by_key(|p| {
            let placeholder = format!("{{{}}}", p.name);
            op.path.find(&placeholder).unwrap_or(usize::MAX)
        });

        let default_name = format!(
            "{}Path",
            op.operation_id
                .as_deref()
                .unwrap_or(&op.path)
                .to_upper_camel_case()
        );

        // Check for rename override
        let struct_name = if let Some(TypeOverride::Rename { name, .. }) =
            overrides.get(op.method, &op.path, GeneratedTypeKind::Path)
        {
            name.clone()
        } else {
            format_ident!("{}", default_name)
        };

        let fields = path_params.iter().map(|param| {
            let snake_name = heck::AsSnakeCase(&param.name).to_string();
            let field_name = format_ident!("{}", snake_name);
            let ty = self.param_type(param);
            let param_name = &param.name;

            // Add serde rename if field name differs from param name (for path deserialization)
            if snake_name != param.name {
                quote! {
                    #[serde(rename = #param_name)]
                    pub #field_name: #ty
                }
            } else {
                quote! { pub #field_name: #ty }
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
}

/// Convert any serializable schema type to a schemars schema for typify.
fn to_schemars<T: serde::Serialize>(schema: &T) -> Result<schemars::schema::Schema> {
    serde_value::to_value(schema)
        .map_err(|e| Error::TypeGenError(format!("failed to serialize schema: {}", e)))?
        .deserialize_into::<schemars::schema::Schema>()
        .map_err(|e| Error::TypeGenError(format!("failed to deserialize schema: {}", e)))
}

/// Register an inline schema with typify, returning the TypeId if successful.
fn register_inline_schema(
    type_space: &mut TypeSpace,
    inline_types: &mut HashMap<String, TypeId>,
    schema: &Schema,
    name_hint: String,
) {
    if let Ok(schemars_schema) = to_schemars(schema)
        && let Ok(type_id) =
            type_space.add_type_with_name(&schemars_schema, Some(name_hint.clone()))
    {
        inline_types.insert(name_hint, type_id);
    }
}
