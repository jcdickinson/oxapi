//! Core implementation for the oxapi OpenAPI server stub generator.

use std::collections::{HashMap, HashSet};

use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use thiserror::Error;

mod method;
mod openapi;
mod responses;
mod router;
mod types;

pub use method::{MethodTransformer, ParamRole};
pub use openapi::{HttpMethod, Operation, OperationParam, ParamLocation, ParsedSpec};
pub use responses::ResponseGenerator;
pub use router::RouterGenerator;
pub use types::TypeGenerator;

// Re-export typify types for use in the macro crate
pub use typify::{TypeSpacePatch, TypeSpaceSettings};

/// Configuration for response type suffixes and derives.
#[derive(Debug, Clone)]
pub struct ResponseSuffixes {
    /// Suffix for success response types (e.g., "Response" → `GetPetResponse`).
    pub ok_suffix: String,
    /// Suffix for error response types (e.g., "Error" → `GetPetError`).
    pub err_suffix: String,
    /// Default derive attributes for generated enums (used when no derive is specified).
    pub default_derives: TokenStream,
}

impl Default for ResponseSuffixes {
    fn default() -> Self {
        Self {
            ok_suffix: "Response".to_string(),
            err_suffix: "Error".to_string(),
            default_derives: quote::quote! { #[derive(Debug)] },
        }
    }
}

/// Kind of generated type for an operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GeneratedTypeKind {
    /// Success response enum ({Op}{ok_suffix}, default: {Op}Response)
    Ok,
    /// Error response enum ({Op}{err_suffix}, default: {Op}Error)
    Err,
    /// Query parameters struct ({Op}Query)
    Query,
    /// Path parameters struct ({Op}Path)
    Path,
}

/// Key for looking up type overrides.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeOverrideKey {
    pub method: HttpMethod,
    pub path: String,
    pub kind: GeneratedTypeKind,
}

/// Variant override info: name, inner type name, and attributes to pass through.
#[derive(Debug, Clone)]
pub struct VariantOverride {
    /// The variant name (as an Ident for better error spans).
    pub name: proc_macro2::Ident,
    /// Optional inner type name override for inline schemas.
    /// If specified, inline schemas for this variant will use this name instead of the default.
    pub inner_type_name: Option<proc_macro2::Ident>,
    /// Attributes to apply to the variant (excluding the consumed `#[status()]`)
    pub attrs: Vec<TokenStream>,
}

/// Override for a generated type - either rename or replace.
#[derive(Debug, Clone)]
pub enum TypeOverride {
    /// Rename the type to a new name, optionally with variant renames
    Rename {
        /// The new name (as an Ident for better error spans).
        name: proc_macro2::Ident,
        /// Attributes to apply to the enum (excluding consumed `#[rename(...)]`)
        /// If this contains a `#[derive(...)]`, it overrides the default.
        attrs: Vec<TokenStream>,
        /// Status code → variant override (name + attributes)
        variant_overrides: HashMap<u16, VariantOverride>,
    },
    /// Replace the type with an existing type (as TokenStream)
    Replace(TokenStream),
}

/// Key for query unknown field storage.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct QueryUnknownFieldKey {
    method: HttpMethod,
    path: String,
}

/// Collection of type overrides for generated types.
#[derive(Debug, Clone, Default)]
pub struct TypeOverrides {
    overrides: HashMap<TypeOverrideKey, TypeOverride>,
    /// Unknown field names for query structs, set via `#[oxapi(query, field_name)]`.
    query_unknown_fields: HashMap<QueryUnknownFieldKey, proc_macro2::Ident>,
}

impl TypeOverrides {
    /// Create a new empty TypeOverrides.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a rename override.
    pub fn add_rename(
        &mut self,
        method: HttpMethod,
        path: impl Into<String>,
        kind: GeneratedTypeKind,
        new_name: proc_macro2::Ident,
    ) {
        self.overrides.insert(
            TypeOverrideKey {
                method,
                path: path.into(),
                kind,
            },
            TypeOverride::Rename {
                name: new_name,
                attrs: Vec::new(),
                variant_overrides: HashMap::new(),
            },
        );
    }

    /// Add a rename override with attributes and variant overrides.
    pub fn add_rename_with_overrides(
        &mut self,
        method: HttpMethod,
        path: impl Into<String>,
        kind: GeneratedTypeKind,
        new_name: proc_macro2::Ident,
        attrs: Vec<TokenStream>,
        variant_overrides: HashMap<u16, VariantOverride>,
    ) {
        self.overrides.insert(
            TypeOverrideKey {
                method,
                path: path.into(),
                kind,
            },
            TypeOverride::Rename {
                name: new_name,
                attrs,
                variant_overrides,
            },
        );
    }

    /// Add a replace override.
    pub fn add_replace(
        &mut self,
        method: HttpMethod,
        path: impl Into<String>,
        kind: GeneratedTypeKind,
        replacement: TokenStream,
    ) {
        self.overrides.insert(
            TypeOverrideKey {
                method,
                path: path.into(),
                kind,
            },
            TypeOverride::Replace(replacement),
        );
    }

    /// Get an override for a specific operation and kind.
    pub fn get(
        &self,
        method: HttpMethod,
        path: &str,
        kind: GeneratedTypeKind,
    ) -> Option<&TypeOverride> {
        self.overrides.get(&TypeOverrideKey {
            method,
            path: path.to_string(),
            kind,
        })
    }

    /// Check if there's a replacement for this operation/kind.
    pub fn is_replaced(&self, method: HttpMethod, path: &str, kind: GeneratedTypeKind) -> bool {
        matches!(self.get(method, path, kind), Some(TypeOverride::Replace(_)))
    }

    /// Set the unknown field name for a query struct.
    ///
    /// This is used when `#[oxapi(query, field_name)]` is specified on a parameter.
    pub fn set_query_unknown_field(
        &mut self,
        method: HttpMethod,
        path: impl Into<String>,
        field_name: proc_macro2::Ident,
    ) {
        self.query_unknown_fields.insert(
            QueryUnknownFieldKey {
                method,
                path: path.into(),
            },
            field_name,
        );
    }

    /// Get the unknown field name for a query struct, if set.
    pub fn get_query_unknown_field(
        &self,
        method: HttpMethod,
        path: &str,
    ) -> Option<&proc_macro2::Ident> {
        self.query_unknown_fields.get(&QueryUnknownFieldKey {
            method,
            path: path.to_string(),
        })
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("failed to parse OpenAPI spec: {0}")]
    ParseError(String),

    #[error("operation not found: {method} {path}")]
    OperationNotFound { method: String, path: String },

    #[error("missing operations in trait: {0:?}")]
    MissingOperations(Vec<String>),

    #[error("type generation error: {0}")]
    TypeGenError(String),

    #[error("invalid attribute: {0}")]
    InvalidAttribute(String),

    #[error("unsupported feature: {0}")]
    Unsupported(String),

    #[error("unknown schema '{name}'. Available schemas: {available}")]
    UnknownSchema { name: String, available: String },
}

pub type Result<T> = std::result::Result<T, Error>;

/// Builder for configuring and creating a Generator.
pub struct GeneratorBuilder {
    spec: OpenAPI,
    settings: TypeSpaceSettings,
    type_overrides: TypeOverrides,
    response_suffixes: ResponseSuffixes,
    schema_renames: HashMap<String, String>,
}

impl GeneratorBuilder {
    /// Create a builder from an OpenAPI spec.
    pub fn new(spec: OpenAPI) -> Self {
        Self {
            spec,
            settings: TypeSpaceSettings::default(),
            type_overrides: TypeOverrides::default(),
            response_suffixes: ResponseSuffixes::default(),
            schema_renames: HashMap::new(),
        }
    }

    /// Create a builder by loading an OpenAPI spec from a file.
    pub fn from_file(path: &std::path::Path) -> Result<Self> {
        let spec = openapi::load_spec(path)?;
        Ok(Self::new(spec))
    }

    /// Set custom type space settings.
    pub fn settings(mut self, settings: TypeSpaceSettings) -> Self {
        self.settings = settings;
        self
    }

    /// Set type overrides for generated types.
    pub fn type_overrides(mut self, overrides: TypeOverrides) -> Self {
        self.type_overrides = overrides;
        self
    }

    /// Set response suffixes configuration.
    pub fn response_suffixes(mut self, suffixes: ResponseSuffixes) -> Self {
        self.response_suffixes = suffixes;
        self
    }

    /// Set schema renames (original name → new name).
    pub fn schema_renames(mut self, renames: HashMap<String, String>) -> Self {
        self.schema_renames = renames;
        self
    }

    /// Build the Generator.
    pub fn build(self) -> Result<Generator> {
        let parsed = ParsedSpec::from_openapi(self.spec)?;
        let type_gen = TypeGenerator::with_settings(&parsed, self.settings, self.schema_renames)?;

        Ok(Generator {
            spec: parsed,
            type_gen,
            type_overrides: self.type_overrides,
            response_suffixes: self.response_suffixes,
        })
    }
}

/// Main generator that coordinates all the pieces.
pub struct Generator {
    spec: ParsedSpec,
    type_gen: TypeGenerator,
    type_overrides: TypeOverrides,
    response_suffixes: ResponseSuffixes,
}

impl Generator {
    /// Create a builder from an OpenAPI spec.
    pub fn builder(spec: OpenAPI) -> GeneratorBuilder {
        GeneratorBuilder::new(spec)
    }

    /// Create a builder by loading an OpenAPI spec from a file.
    pub fn builder_from_file(path: &std::path::Path) -> Result<GeneratorBuilder> {
        GeneratorBuilder::from_file(path)
    }

    /// Create a new generator from an OpenAPI spec with default settings.
    pub fn new(spec: OpenAPI) -> Result<Self> {
        GeneratorBuilder::new(spec).build()
    }

    /// Create a new generator from an OpenAPI spec with custom type settings.
    pub fn with_settings(spec: OpenAPI, settings: TypeSpaceSettings) -> Result<Self> {
        GeneratorBuilder::new(spec).settings(settings).build()
    }

    /// Load and parse an OpenAPI spec from a file path.
    pub fn from_file(path: &std::path::Path) -> Result<Self> {
        GeneratorBuilder::from_file(path)?.build()
    }

    /// Get the parsed spec.
    pub fn spec(&self) -> &ParsedSpec {
        &self.spec
    }

    /// Get the type generator.
    pub fn type_generator(&self) -> &TypeGenerator {
        &self.type_gen
    }

    /// Generate all types as a TokenStream.
    pub fn generate_types(&self) -> TokenStream {
        self.type_gen.generate_all_types()
    }

    /// Generate response enums for all operations.
    pub fn generate_responses(&self) -> TokenStream {
        ResponseGenerator::new(
            &self.spec,
            &self.type_gen,
            &self.type_overrides,
            &self.response_suffixes,
        )
        .generate_all()
    }

    /// Generate query parameter structs for all operations.
    pub fn generate_query_structs(&self) -> TokenStream {
        let mut structs = Vec::new();
        for op in self.spec.operations() {
            // Check if there's an unknown field specified for this operation's query struct
            let unknown_field = self
                .type_overrides
                .get_query_unknown_field(op.method, &op.path);
            if let Some((_, definition)) =
                self.type_gen
                    .generate_query_struct(op, &self.type_overrides, unknown_field)
            {
                structs.push(definition);
            }
        }
        quote::quote! { #(#structs)* }
    }

    /// Generate path parameter structs for all operations.
    pub fn generate_path_structs(&self) -> TokenStream {
        let mut structs = Vec::new();
        for op in self.spec.operations() {
            if let Some((_, definition)) =
                self.type_gen.generate_path_struct(op, &self.type_overrides)
            {
                structs.push(definition);
            }
        }
        quote::quote! { #(#structs)* }
    }

    /// Get the type overrides.
    pub fn type_overrides(&self) -> &TypeOverrides {
        &self.type_overrides
    }

    /// Get the response suffixes.
    pub fn response_suffixes(&self) -> &ResponseSuffixes {
        &self.response_suffixes
    }

    /// Look up an operation by HTTP method and path.
    pub fn get_operation(&self, method: HttpMethod, path: &str) -> Option<&Operation> {
        self.spec.get_operation(method, path)
    }

    /// Get all operations.
    pub fn operations(&self) -> impl Iterator<Item = &Operation> {
        self.spec.operations()
    }

    /// Validate that all operations are covered by trait methods.
    pub fn validate_coverage(&self, covered: &HashSet<(HttpMethod, String)>) -> Result<()> {
        let mut missing = Vec::new();

        for op in self.spec.operations() {
            let key = (op.method, op.path.clone());
            if !covered.contains(&key) {
                missing.push(format!("{} {}", op.method, op.path));
            }
        }

        if missing.is_empty() {
            Ok(())
        } else {
            Err(Error::MissingOperations(missing))
        }
    }
}
