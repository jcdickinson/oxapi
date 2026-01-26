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
}

impl Generator {
    /// Create a new generator from an OpenAPI spec.
    pub fn new(spec: OpenAPI) -> Result<Self> {
        let parsed = ParsedSpec::from_openapi(spec)?;
        let type_gen = TypeGenerator::new(&parsed)?;

        Ok(Self {
            spec: parsed,
            type_gen,
        })
    }

    /// Load and parse an OpenAPI spec from a file path.
    pub fn from_file(path: &std::path::Path) -> Result<Self> {
        let spec = openapi::load_spec(path)?;
        Self::new(spec)
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
        ResponseGenerator::new(&self.spec, &self.type_gen).generate_all()
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
