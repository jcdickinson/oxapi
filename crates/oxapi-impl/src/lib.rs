//! Core implementation for the oxapi OpenAPI server stub generator.

use std::collections::HashMap;

use openapiv3::OpenAPI;
use proc_macro2::TokenStream;
use thiserror::Error;

mod method;
mod openapi;
mod responses;
mod router;
mod types;

pub use method::MethodTransformer;
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
}

/// Key for looking up type overrides.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeOverrideKey {
    pub method: HttpMethod,
    pub path: String,
    pub kind: GeneratedTypeKind,
}

/// Variant override info: name and attributes to pass through.
#[derive(Debug, Clone)]
pub struct VariantOverride {
    pub name: String,
    /// Attributes to apply to the variant (excluding the consumed #[status()])
    pub attrs: Vec<TokenStream>,
}

/// Override for a generated type - either rename or replace.
#[derive(Debug, Clone)]
pub enum TypeOverride {
    /// Rename the type to a new name, optionally with variant renames
    Rename {
        name: String,
        /// Attributes to apply to the enum (excluding consumed #[rename(...)])
        /// If this contains a #[derive(...)], it overrides the default.
        attrs: Vec<TokenStream>,
        /// Status code → variant override (name + attributes)
        variant_overrides: HashMap<u16, VariantOverride>,
    },
    /// Replace the type with an existing type (as TokenStream)
    Replace(TokenStream),
}

/// Collection of type overrides for generated types.
#[derive(Debug, Clone, Default)]
pub struct TypeOverrides {
    overrides: HashMap<TypeOverrideKey, TypeOverride>,
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
        new_name: impl Into<String>,
    ) {
        self.overrides.insert(
            TypeOverrideKey {
                method,
                path: path.into(),
                kind,
            },
            TypeOverride::Rename {
                name: new_name.into(),
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
        new_name: impl Into<String>,
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
                name: new_name.into(),
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
    pub fn get(&self, method: HttpMethod, path: &str, kind: GeneratedTypeKind) -> Option<&TypeOverride> {
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
}

pub type Result<T> = std::result::Result<T, Error>;

/// Main generator that coordinates all the pieces.
pub struct Generator {
    spec: ParsedSpec,
    type_gen: TypeGenerator,
    type_overrides: TypeOverrides,
    response_suffixes: ResponseSuffixes,
}

impl Generator {
    /// Create a new generator from an OpenAPI spec with default settings.
    pub fn new(spec: OpenAPI) -> Result<Self> {
        Self::with_settings(spec, TypeSpaceSettings::default())
    }

    /// Create a new generator from an OpenAPI spec with custom type settings.
    pub fn with_settings(spec: OpenAPI, settings: TypeSpaceSettings) -> Result<Self> {
        Self::with_all_settings(
            spec,
            settings,
            TypeOverrides::default(),
            ResponseSuffixes::default(),
        )
    }

    /// Create a new generator with all settings.
    pub fn with_all_settings(
        spec: OpenAPI,
        settings: TypeSpaceSettings,
        type_overrides: TypeOverrides,
        response_suffixes: ResponseSuffixes,
    ) -> Result<Self> {
        let parsed = ParsedSpec::from_openapi(spec)?;
        let type_gen = TypeGenerator::with_settings(&parsed, settings)?;

        Ok(Self {
            spec: parsed,
            type_gen,
            type_overrides,
            response_suffixes,
        })
    }

    /// Load and parse an OpenAPI spec from a file path.
    pub fn from_file(path: &std::path::Path) -> Result<Self> {
        let spec = openapi::load_spec(path)?;
        Self::new(spec)
    }

    /// Load and parse an OpenAPI spec from a file path with custom type settings.
    pub fn from_file_with_settings(
        path: &std::path::Path,
        settings: TypeSpaceSettings,
    ) -> Result<Self> {
        let spec = openapi::load_spec(path)?;
        Self::with_settings(spec, settings)
    }

    /// Load and parse an OpenAPI spec from a file path with all settings.
    pub fn from_file_with_all_settings(
        path: &std::path::Path,
        settings: TypeSpaceSettings,
        type_overrides: TypeOverrides,
        response_suffixes: ResponseSuffixes,
    ) -> Result<Self> {
        let spec = openapi::load_spec(path)?;
        Self::with_all_settings(spec, settings, type_overrides, response_suffixes)
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
            if let Some((_, definition)) =
                self.type_gen
                    .generate_query_struct(op, &self.type_overrides)
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
    pub fn validate_coverage(&self, covered: &HashMap<(HttpMethod, String), ()>) -> Result<()> {
        let mut missing = Vec::new();

        for op in self.spec.operations() {
            let key = (op.method, op.path.clone());
            if !covered.contains_key(&key) {
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
