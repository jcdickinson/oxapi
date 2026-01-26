//! OpenAPI spec parsing and operation extraction.

use std::collections::HashMap;
use std::fs::File;
use std::path::Path;

use openapiv3::{OpenAPI, ReferenceOr, Schema, StatusCode};

use crate::{Error, Result};

/// HTTP methods supported by OpenAPI.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Head,
    Options,
}

impl std::fmt::Display for HttpMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HttpMethod::Get => write!(f, "GET"),
            HttpMethod::Post => write!(f, "POST"),
            HttpMethod::Put => write!(f, "PUT"),
            HttpMethod::Delete => write!(f, "DELETE"),
            HttpMethod::Patch => write!(f, "PATCH"),
            HttpMethod::Head => write!(f, "HEAD"),
            HttpMethod::Options => write!(f, "OPTIONS"),
        }
    }
}

impl HttpMethod {
    pub fn as_str(&self) -> &'static str {
        match self {
            HttpMethod::Get => "get",
            HttpMethod::Post => "post",
            HttpMethod::Put => "put",
            HttpMethod::Delete => "delete",
            HttpMethod::Patch => "patch",
            HttpMethod::Head => "head",
            HttpMethod::Options => "options",
        }
    }
}

/// Location of a parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamLocation {
    Path,
    Query,
    Header,
    Cookie,
}

/// A parsed operation parameter.
#[derive(Debug, Clone)]
pub struct OperationParam {
    pub name: String,
    pub location: ParamLocation,
    pub required: bool,
    pub schema: Option<ReferenceOr<Schema>>,
    pub description: Option<String>,
}

/// A parsed response.
#[derive(Debug, Clone)]
pub struct OperationResponse {
    pub status_code: ResponseStatus,
    pub description: String,
    pub schema: Option<ReferenceOr<Schema>>,
    pub content_type: Option<String>,
}

/// Response status code or range.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ResponseStatus {
    Code(u16),
    Default,
}

impl ResponseStatus {
    pub fn is_success(&self) -> bool {
        match self {
            ResponseStatus::Code(code) => (200..300).contains(code),
            ResponseStatus::Default => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            ResponseStatus::Code(code) => *code >= 400,
            ResponseStatus::Default => true,
        }
    }
}

/// A parsed OpenAPI operation.
#[derive(Debug, Clone)]
pub struct Operation {
    pub operation_id: Option<String>,
    pub method: HttpMethod,
    pub path: String,
    pub summary: Option<String>,
    pub description: Option<String>,
    pub parameters: Vec<OperationParam>,
    pub request_body: Option<RequestBody>,
    pub responses: Vec<OperationResponse>,
    pub tags: Vec<String>,
}

/// Request body information.
#[derive(Debug, Clone)]
pub struct RequestBody {
    pub required: bool,
    pub description: Option<String>,
    pub content_type: String,
    pub schema: Option<ReferenceOr<Schema>>,
}

/// Parsed OpenAPI specification.
pub struct ParsedSpec {
    pub info: SpecInfo,
    operations: Vec<Operation>,
    operation_map: HashMap<(HttpMethod, String), usize>,
    pub components: Option<openapiv3::Components>,
}

/// Basic spec info.
#[derive(Debug, Clone)]
pub struct SpecInfo {
    pub title: String,
    pub version: String,
    pub description: Option<String>,
}

impl ParsedSpec {
    /// Parse an OpenAPI spec into our internal representation.
    pub fn from_openapi(spec: OpenAPI) -> Result<Self> {
        let info = SpecInfo {
            title: spec.info.title.clone(),
            version: spec.info.version.clone(),
            description: spec.info.description.clone(),
        };

        let mut operations = Vec::new();
        let mut operation_map = HashMap::new();

        for (path, path_item) in &spec.paths.paths {
            let item = match path_item {
                ReferenceOr::Reference { .. } => {
                    return Err(Error::Unsupported(
                        "external path references not supported".to_string(),
                    ));
                }
                ReferenceOr::Item(item) => item,
            };

            // Extract operations for each HTTP method
            let method_ops = [
                (HttpMethod::Get, &item.get),
                (HttpMethod::Post, &item.post),
                (HttpMethod::Put, &item.put),
                (HttpMethod::Delete, &item.delete),
                (HttpMethod::Patch, &item.patch),
                (HttpMethod::Head, &item.head),
                (HttpMethod::Options, &item.options),
            ];

            for (method, op_opt) in method_ops {
                if let Some(op) = op_opt {
                    let operation = parse_operation(method, path, op, &item.parameters, &spec)?;
                    let idx = operations.len();
                    operation_map.insert((method, path.clone()), idx);
                    operations.push(operation);
                }
            }
        }

        Ok(Self {
            info,
            operations,
            operation_map,
            components: spec.components,
        })
    }

    /// Get an operation by method and path.
    pub fn get_operation(&self, method: HttpMethod, path: &str) -> Option<&Operation> {
        self.operation_map
            .get(&(method, path.to_string()))
            .map(|&idx| &self.operations[idx])
    }

    /// Iterate over all operations.
    pub fn operations(&self) -> impl Iterator<Item = &Operation> {
        self.operations.iter()
    }
}

/// Load an OpenAPI spec from a file.
pub fn load_spec(path: &Path) -> Result<OpenAPI> {
    let file =
        File::open(path).map_err(|e| Error::ParseError(format!("failed to open file: {}", e)))?;

    // Try JSON first, then YAML
    if let Ok(spec) = serde_json::from_reader::<_, OpenAPI>(&file) {
        return Ok(spec);
    }

    let file =
        File::open(path).map_err(|e| Error::ParseError(format!("failed to open file: {}", e)))?;

    yaml_serde::from_reader(file)
        .map_err(|e| Error::ParseError(format!("failed to parse spec: {}", e)))
}

fn parse_operation(
    method: HttpMethod,
    path: &str,
    op: &openapiv3::Operation,
    path_params: &[ReferenceOr<openapiv3::Parameter>],
    spec: &OpenAPI,
) -> Result<Operation> {
    let mut parameters = Vec::new();

    // First add path-level parameters
    for param_ref in path_params {
        if let Some(param) = resolve_parameter(param_ref, spec)? {
            parameters.push(param);
        }
    }

    // Then add operation-level parameters (which may override path-level)
    for param_ref in &op.parameters {
        if let Some(param) = resolve_parameter(param_ref, spec)? {
            // Remove any existing param with same name/location
            parameters.retain(|p| !(p.name == param.name && p.location == param.location));
            parameters.push(param);
        }
    }

    // Parse request body
    let request_body = if let Some(body_ref) = &op.request_body {
        parse_request_body(body_ref, spec)?
    } else {
        None
    };

    // Parse responses
    let mut responses = Vec::new();

    if let Some(default) = &op.responses.default
        && let Some(resp) = parse_response(ResponseStatus::Default, default, spec)?
    {
        responses.push(resp);
    }

    for (code, resp_ref) in &op.responses.responses {
        let status = match code {
            StatusCode::Code(c) => ResponseStatus::Code(*c),
            StatusCode::Range(_) => continue, // Skip ranges for now
        };
        if let Some(resp) = parse_response(status, resp_ref, spec)? {
            responses.push(resp);
        }
    }

    Ok(Operation {
        operation_id: op.operation_id.clone(),
        method,
        path: path.to_string(),
        summary: op.summary.clone(),
        description: op.description.clone(),
        parameters,
        request_body,
        responses,
        tags: op.tags.clone(),
    })
}

fn resolve_parameter(
    param_ref: &ReferenceOr<openapiv3::Parameter>,
    spec: &OpenAPI,
) -> Result<Option<OperationParam>> {
    let param = match param_ref {
        ReferenceOr::Reference { reference } => {
            // Resolve reference
            let name = reference
                .strip_prefix("#/components/parameters/")
                .ok_or_else(|| {
                    Error::ParseError(format!("invalid parameter reference: {}", reference))
                })?;
            spec.components
                .as_ref()
                .and_then(|c| c.parameters.get(name))
                .and_then(|p| match p {
                    ReferenceOr::Item(item) => Some(item),
                    _ => None,
                })
                .ok_or_else(|| Error::ParseError(format!("parameter not found: {}", name)))?
        }
        ReferenceOr::Item(item) => item,
    };

    let (location, data) = match param {
        openapiv3::Parameter::Path { parameter_data, .. } => (ParamLocation::Path, parameter_data),
        openapiv3::Parameter::Query { parameter_data, .. } => {
            (ParamLocation::Query, parameter_data)
        }
        openapiv3::Parameter::Header { parameter_data, .. } => {
            (ParamLocation::Header, parameter_data)
        }
        openapiv3::Parameter::Cookie { parameter_data, .. } => {
            (ParamLocation::Cookie, parameter_data)
        }
    };

    let schema = match &data.format {
        openapiv3::ParameterSchemaOrContent::Schema(s) => Some(s.clone()),
        openapiv3::ParameterSchemaOrContent::Content(_) => None,
    };

    Ok(Some(OperationParam {
        name: data.name.clone(),
        location,
        required: data.required,
        schema,
        description: data.description.clone(),
    }))
}

fn parse_request_body(
    body_ref: &ReferenceOr<openapiv3::RequestBody>,
    spec: &OpenAPI,
) -> Result<Option<RequestBody>> {
    let body = match body_ref {
        ReferenceOr::Reference { reference } => {
            let name = reference
                .strip_prefix("#/components/requestBodies/")
                .ok_or_else(|| {
                    Error::ParseError(format!("invalid request body reference: {}", reference))
                })?;
            spec.components
                .as_ref()
                .and_then(|c| c.request_bodies.get(name))
                .and_then(|b| match b {
                    ReferenceOr::Item(item) => Some(item),
                    _ => None,
                })
                .ok_or_else(|| Error::ParseError(format!("request body not found: {}", name)))?
        }
        ReferenceOr::Item(item) => item,
    };

    // Prefer application/json
    let (content_type, media) = body
        .content
        .iter()
        .find(|(ct, _)| ct.starts_with("application/json"))
        .or_else(|| body.content.first())
        .ok_or_else(|| Error::ParseError("request body has no content".to_string()))?;

    Ok(Some(RequestBody {
        required: body.required,
        description: body.description.clone(),
        content_type: content_type.clone(),
        schema: media.schema.clone(),
    }))
}

fn parse_response(
    status: ResponseStatus,
    resp_ref: &ReferenceOr<openapiv3::Response>,
    spec: &OpenAPI,
) -> Result<Option<OperationResponse>> {
    let resp = match resp_ref {
        ReferenceOr::Reference { reference } => {
            let name = reference
                .strip_prefix("#/components/responses/")
                .ok_or_else(|| {
                    Error::ParseError(format!("invalid response reference: {}", reference))
                })?;
            spec.components
                .as_ref()
                .and_then(|c| c.responses.get(name))
                .and_then(|r| match r {
                    ReferenceOr::Item(item) => Some(item),
                    _ => None,
                })
                .ok_or_else(|| Error::ParseError(format!("response not found: {}", name)))?
        }
        ReferenceOr::Item(item) => item,
    };

    // Get schema from content (prefer application/json)
    let (content_type, schema) = if let Some((ct, media)) = resp
        .content
        .iter()
        .find(|(ct, _)| ct.starts_with("application/json"))
        .or_else(|| resp.content.first())
    {
        (Some(ct.clone()), media.schema.clone())
    } else {
        (None, None)
    };

    Ok(Some(OperationResponse {
        status_code: status,
        description: resp.description.clone(),
        schema,
        content_type,
    }))
}
