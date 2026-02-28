//! Procedural macros for the oxapi OpenAPI server stub generator.
//!
//! This crate provides the `#[oxapi]` attribute macro for generating type-safe
//! server stubs from OpenAPI specifications. See [`oxapi`] for full documentation.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, quote, quote_spanned};
use schemars::schema::{InstanceType, SchemaObject, SingleOrVec};
use syn::{Ident, ItemMod, ItemTrait, LitStr, Token};

/// Generate type-safe server stubs from OpenAPI specifications.
///
/// # Syntax
///
/// ```text
/// #[oxapi(framework, "spec_path")]
/// #[oxapi(framework, "spec_path", unwrap)]
/// #[oxapi(framework, "spec_path", ok_suffix = "Response", err_suffix = "Error")]
/// #[oxapi(framework, "spec_path", derive = (Debug, Clone))]
/// #[oxapi(framework, "spec_path", unwrap, ok_suffix = "Ok", err_suffix = "Err")]
/// ```
///
/// ## Arguments
///
/// - `framework`: The web framework to generate code for. Currently only `axum` is supported.
/// - `spec_path`: Path to the OpenAPI specification file (JSON or YAML), relative to `Cargo.toml`.
/// - `unwrap` (optional): Emit contents directly without wrapping in a module.
/// - `ok_suffix` (optional): Suffix for success response types. Defaults to `"Response"`.
/// - `err_suffix` (optional): Suffix for error response types. Defaults to `"Error"`.
/// - `derive` (optional): Default derives for generated response enums. Defaults to `(Debug)`.
///
/// All options after the spec path can be specified in any order.
///
/// # Usage Modes
///
/// ## Single Trait Mode
///
/// Apply directly to a trait to generate types in a sibling `{trait_name}_types` module:
///
/// ```ignore
/// #[oxapi::oxapi(axum, "spec.json")]
/// trait PetService<S> {
///     #[oxapi(map)]
///     fn map_routes(router: Router<S>) -> Router<S>;
///
///     #[oxapi(get, "/pet/{petId}")]
///     async fn get_pet(state: State<S>, pet_id: Path<_>) -> Result<GetPetResponse, GetPetError>;
/// }
/// ```
///
/// ## Module Mode
///
/// Apply to a module containing one or more traits. Types are generated in a shared `types` submodule:
///
/// ```ignore
/// #[oxapi::oxapi(axum, "spec.json")]
/// mod api {
///     trait PetService<S> {
///         #[oxapi(map)]
///         fn map_routes(router: Router<S>) -> Router<S>;
///
///         #[oxapi(get, "/pet/{petId}")]
///         async fn get_pet(state: State<S>, pet_id: Path<_>);
///     }
///
///     trait StoreService<S> {
///         #[oxapi(map)]
///         fn map_routes(router: Router<S>) -> Router<S>;
///
///         #[oxapi(get, "/store/inventory")]
///         async fn get_inventory(state: State<S>);
///     }
/// }
/// ```
///
/// ## Unwrap Mode
///
/// Use `unwrap` to emit contents directly without a module wrapper:
///
/// ```ignore
/// #[oxapi::oxapi(axum, "spec.json", unwrap)]
/// mod api {
///     trait PetService<S> { /* ... */ }
/// }
/// // Generates: pub mod types { ... } pub trait PetService<S> { ... }
/// // Instead of: mod api { pub mod types { ... } pub trait PetService<S> { ... } }
/// ```
///
/// # Method Attributes
///
/// ## `#[oxapi(map)]`
///
/// Marks a method as the route mapper. Must have signature `fn map_routes(router: Router<S>) -> Router<S>`.
/// The macro fills in the body to register all routes:
///
/// ```ignore
/// #[oxapi(map)]
/// fn map_routes(router: Router<S>) -> Router<S>;
/// // Generates body: router.route("/pet/{petId}", get(Self::get_pet)).route(...)
/// ```
///
/// ## `#[oxapi(method, "path")]`
///
/// Maps a trait method to an OpenAPI operation. Supported methods: `get`, `post`, `put`, `delete`, `patch`, `head`, `options`.
///
/// ```ignore
/// #[oxapi(get, "/pet/{petId}")]
/// async fn get_pet(state: State<S>, pet_id: Path<_>);
///
/// #[oxapi(post, "/pet")]
/// async fn add_pet(state: State<S>, body: Json<_>);
/// ```
///
/// ## `#[oxapi(spec, "endpoint_path")]`
///
/// Serves the embedded OpenAPI spec at the given endpoint path. The method must be completely bare
/// (no parameters, no return type, not async, no generics). The macro generates:
/// - A method that returns `&'static str` containing the spec contents via `include_str!`
/// - A GET route at the specified path (added to `map_routes`)
///
/// This endpoint does not appear in the OpenAPI spec itself but can be used for validation purposes.
///
/// ```ignore
/// #[oxapi(spec, "/openapi.yaml")]
/// fn spec();
/// // Generates: fn spec() -> &'static str { include_str!("path/to/spec.yaml") }
/// // And adds: .route("/openapi.yaml", get(|| async { Self::spec() }))
/// ```
///
/// # Type Elision
///
/// Use `_` as a type parameter to have the macro infer the correct type from the OpenAPI spec:
///
/// - `Path<_>` → Inferred from path parameters (single type or tuple for multiple)
/// - `Query<_>` → Generated query struct `{OperationId}Query`
/// - `Json<_>` → Inferred from request body schema
/// - `State<S>` → Passed through unchanged (user-provided)
///
/// ```ignore
/// #[oxapi(get, "/pet/{petId}")]
/// async fn get_pet(
///     state: State<S>,       // User state, unchanged
///     pet_id: Path<_>,       // Becomes Path<i64> based on spec
///     query: Query<_>,       // Becomes Query<GetPetQuery>
/// );
///
/// #[oxapi(post, "/pet")]
/// async fn add_pet(
///     state: State<S>,
///     body: Json<_>,         // Becomes Json<Pet> based on spec
/// );
/// ```
///
/// # Custom Extractors
///
/// Parameters with explicit types (no `_` elision) are passed through unchanged.
/// This allows adding authentication, state, or other custom extractors:
///
/// ```ignore
/// #[oxapi(post, "/items")]
/// async fn create_item(
///     state: State<S>,
///     claims: Jwt<AppClaims>,    // Custom extractor - passed through unchanged
///     body: Json<_>,              // Type elision - inferred from spec
/// );
/// ```
///
/// ## Parameter Role Attributes
///
/// Use `#[oxapi(path)]`, `#[oxapi(query)]`, or `#[oxapi(body)]` on parameters when
/// the extractor name isn't recognized (`Path`, `Query`, `Json`):
///
/// ```ignore
/// #[oxapi(get, "/items/{id}")]
/// async fn get_item(
///     state: State<S>,
///     #[oxapi(path)] id: MyPathExtractor<_>,   // Infers type from path params
///     #[oxapi(query)] q: MyQueryExtractor<_>,  // Infers type as query struct
/// );
/// ```
///
/// **Note**: When ANY parameter has an explicit role attribute, inference is disabled
/// for ALL parameters. Use explicit attrs on all params that need type elision.
///
/// ## Capturing Unknown Query Parameters
///
/// Use `#[oxapi(query, field_name)]` to capture query parameters not in the spec:
///
/// ```ignore
/// #[oxapi(get, "/search")]
/// async fn search(
///     state: State<S>,
///     #[oxapi(query, extras)] q: Query<_>,
/// );
/// // Generates SearchQuery with: #[serde(flatten)] pub extras: HashMap<String, String>
/// ```
///
/// # Generated Types
///
/// For each operation, the macro generates:
///
/// - `{OperationId}{ok_suffix}` - Enum with success response variants (2xx status codes), default suffix is `Response`
/// - `{OperationId}{err_suffix}` - Enum with error response variants (4xx, 5xx, default), default suffix is `Error`
/// - `{OperationId}Query` - Struct for query parameters (if operation has query params)
/// - `{OperationId}Path` - Struct for path parameters (if operation has path params)
///
/// ## Flag Query Parameters
///
/// When a query parameter has `allowEmptyValue: true` and a `boolean` schema, the generated
/// field uses [`oxapi::Flag`] instead of `Option<bool>`. This handles presence-only query
/// parameters like `?verbose` (without a value):
///
/// ```yaml
/// parameters:
///   - name: verbose
///     in: query
///     allowEmptyValue: true
///     schema:
///       type: boolean
/// ```
///
/// Generates:
///
/// ```ignore
/// pub struct ListItemsQuery {
///     #[serde(default)]
///     pub verbose: oxapi::Flag,
/// }
/// ```
///
/// All response enums implement `axum::response::IntoResponse`.
///
/// ## Response Headers
///
/// When an OpenAPI response defines `headers`, the corresponding enum variant becomes a struct
/// variant with `headers` and `body` fields (instead of a tuple variant):
///
/// ```ignore
/// // Spec defines X-Rate-Limit and X-Request-Id headers on the 200 response
/// // Generated:
/// #[derive(Debug, Default)]
/// pub struct GetPetResponseStatus200Headers {
///     pub x_rate_limit: i64,            // required header
///     pub x_request_id: Option<String>,  // optional header
/// }
///
/// pub enum GetPetResponse {
///     Status200 { headers: GetPetResponseStatus200Headers, body: Pet },
///     Status404, // no headers → unit variant unchanged
/// }
/// ```
///
/// Header struct naming follows `{EnumName}{VariantName}Headers`. If the enum or variant is
/// renamed via overrides, the header struct name updates accordingly (e.g., `PetResponseSuccessHeaders`).
///
/// Header field types are derived from the OpenAPI schema (integer → `i64`, string → `String`, etc.).
/// Responses without headers are unchanged (tuple or unit variants).
///
/// # Type Customization
///
/// ## Schema Type Conversion
///
/// Replace JSON schema constructs with custom types using `#[convert(...)]` on the module:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// #[convert(into = uuid::Uuid, type = "string", format = "uuid")]
/// #[convert(into = rust_decimal::Decimal, type = "number")]
/// mod api {
///     // All schemas with type="string", format="uuid" use uuid::Uuid
///     // All schemas with type="number" use rust_decimal::Decimal
/// }
/// ```
///
/// ### Convert Attribute Fields
///
/// - `into`: The replacement Rust type (required)
/// - `type`: JSON schema type (`"string"`, `"number"`, `"integer"`, `"boolean"`, `"array"`, `"object"`)
/// - `format`: JSON schema format (e.g., `"uuid"`, `"date-time"`, `"uri"`)
///
/// ## Schema Type Patching (Rename/Derive)
///
/// Rename schema types or add derives using struct declarations:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     // Rename "Veggie" from the schema to "Vegetable" in generated code
///     #[oxapi(Veggie)]
///     struct Vegetable;
///
///     // Add extra derives to a schema type
///     #[oxapi(Veggie)]
///     #[derive(schemars::JsonSchema, PartialEq, Eq, Hash)]
///     struct Vegetable;
/// }
/// ```
///
/// ## Schema Type Replacement
///
/// Replace a schema type entirely with an existing Rust type:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     // Use my_networking::Ipv6Cidr instead of generating Ipv6Cidr
///     #[oxapi]
///     type Ipv6Cidr = my_networking::Ipv6Cidr;
/// }
/// ```
///
/// ## Generated Type Customization
///
/// Rename or replace the auto-generated `Response`/`Error`/`Query` types using operation coordinates:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     // Rename GetPetByIdResponse to PetResponse
///     #[oxapi(get, "/pet/{petId}", ok)]
///     struct PetResponse;
///
///     // Rename GetPetByIdError to PetError
///     #[oxapi(get, "/pet/{petId}", err)]
///     struct PetError;
///
///     // Rename FindPetsByStatusQuery to PetSearchParams
///     #[oxapi(get, "/pet/findByStatus", query)]
///     struct PetSearchParams;
///
///     // Replace GetPetByIdResponse with a custom type (skips generation)
///     #[oxapi(get, "/pet/{petId}", ok)]
///     type _ = my_types::PetResponse;
///
///     // Replace GetPetByIdError with a custom type
///     #[oxapi(get, "/pet/{petId}", err)]
///     type _ = my_types::PetError;
/// }
/// ```
///
/// ## Enum Variant Renaming
///
/// Rename individual status code variants within response enums using the enum syntax:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     // Rename the enum to PetError and customize specific variant names
///     #[oxapi(get, "/pet/{petId}", err)]
///     enum PetError {
///         #[oxapi(status = 401)]
///         Unauthorized,
///         #[oxapi(status = 404)]
///         NotFound,
///     }
///     // Generates: enum PetError { Unauthorized(...), NotFound(...), Status500(...), ... }
///     // instead of: enum GetPetByIdError { Status401(...), Status404(...), Status500(...), ... }
/// }
/// ```
///
/// Only status codes you specify will be renamed; others retain their default `Status{code}` names.
///
/// ### Inline Type Naming
///
/// When a response has an inline schema (not a `$ref`), oxapi generates a struct for it.
/// The default name is `{EnumName}{VariantName}`. You can override this by specifying
/// a type name in the variant:
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     #[oxapi(get, "/store/inventory", ok)]
///     enum InventoryResponse {
///         #[oxapi(status = 200)]
///         Success(Inventory),  // The inline schema struct will be named "Inventory"
///     }
/// }
/// ```
///
/// Note: This only works for inline schemas. Using a type name with a `$ref` schema
/// will result in a compile error.
///
/// ### Attribute Pass-Through
///
/// All attributes on the enum (except `#[oxapi(...)]`) and on variants (except `#[oxapi(...)]`)
/// are passed through to the generated code. If you specify a `#[derive(...)]` on the enum,
/// it completely overrides the default derives.
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     #[oxapi(get, "/pet/{petId}", err)]
///     #[derive(Debug, thiserror::Error)]  // Overrides default, must include Debug if needed
///     enum PetError {
///         #[oxapi(status = 401)]
///         #[error("Unauthorized access")]
///         Unauthorized,
///
///         #[oxapi(status = 404)]
///         #[error("Pet not found: {0}")]
///         NotFound(String),
///     }
/// }
/// ```
///
/// ### Kind Values
///
/// - `ok` - Success response enum (`{OperationId}{ok_suffix}`, default: `{OperationId}Response`). Supports variant renames with enum syntax.
/// - `err` - Error response enum (`{OperationId}{err_suffix}`, default: `{OperationId}Error`). Supports variant renames with enum syntax.
/// - `query` - Query parameters struct (`{OperationId}Query`)
/// - `path` - Path parameters struct (`{OperationId}Path`)
///
/// # Complete Example
///
/// ```ignore
/// use axum::{Router, extract::{State, Path, Query, Json}};
///
/// #[oxapi::oxapi(axum, "petstore.json")]
/// #[convert(into = uuid::Uuid, type = "string", format = "uuid")]
/// mod api {
///     // Rename a schema type (Pet from OpenAPI becomes Animal in Rust)
///     #[oxapi(Pet)]
///     #[derive(Clone)]
///     struct Animal;
///
///     // Replace a generated response type
///     #[oxapi(get, "/pet/{petId}", err)]
///     type _ = crate::errors::PetNotFound;
///
///     trait PetService<S: Clone + Send + Sync + 'static> {
///         #[oxapi(map)]
///         fn map_routes(router: Router<S>) -> Router<S>;
///
///         #[oxapi(get, "/pet/{petId}")]
///         async fn get_pet(state: State<S>, pet_id: Path<_>);
///
///         #[oxapi(post, "/pet")]
///         async fn add_pet(state: State<S>, body: Json<_>);
///
///         #[oxapi(get, "/pet/findByStatus")]
///         async fn find_by_status(state: State<S>, query: Query<_>);
///     }
/// }
///
/// // Implement the trait
/// struct MyPetService;
///
/// impl api::PetService<AppState> for MyPetService {
///     fn map_routes(router: Router<AppState>) -> Router<AppState> {
///         <Self as api::PetService<AppState>>::map_routes(router)
///     }
///
///     async fn get_pet(
///         State(state): State<AppState>,
///         Path(pet_id): Path<i64>,
///     ) -> Result<api::types::GetPetByIdResponse, crate::errors::PetNotFound> {
///         // Implementation
///     }
///     // ...
/// }
/// ```
#[proc_macro_attribute]
pub fn oxapi(attr: TokenStream, item: TokenStream) -> TokenStream {
    match do_oxapi(attr.into(), item.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn do_oxapi(attr: TokenStream2, item: TokenStream2) -> syn::Result<TokenStream2> {
    // Parse the attribute arguments: (framework, "spec_path")
    let args = syn::parse2::<MacroArgs>(attr)?;

    // Build response suffixes from macro args
    let response_suffixes = oxapi_impl::ResponseSuffixes {
        ok_suffix: args.ok_suffix.clone(),
        err_suffix: args.err_suffix.clone(),
        default_derives: args
            .default_derives
            .clone()
            .unwrap_or_else(|| quote! { #[derive(Debug)] }),
    };

    // Try to parse as trait first, then as module
    if let Ok(trait_item) = syn::parse2::<ItemTrait>(item.clone()) {
        // Collect query unknown fields from the trait before creating Generator
        let query_unknown_fields = collect_query_unknown_fields_from_trait(&trait_item)?;
        let mut overrides = oxapi_impl::TypeOverrides::new();
        for (method, path, field_name) in query_unknown_fields {
            overrides.set_query_unknown_field(method, path, field_name);
        }

        // Load the OpenAPI spec with default settings for traits
        let spec_path = resolve_spec_path(&args.spec_path)?;
        let generator = oxapi_impl::Generator::builder_from_file(&spec_path)
            .map_err(|e| {
                syn::Error::new(args.spec_path.span(), format!("failed to load spec: {}", e))
            })?
            .type_overrides(overrides)
            .response_suffixes(response_suffixes)
            .build()
            .map_err(|e| {
                syn::Error::new(args.spec_path.span(), format!("failed to load spec: {}", e))
            })?;
        let processor = TraitProcessor::new(generator, trait_item, spec_path)?;
        processor.generate()
    } else if let Ok(mod_item) = syn::parse2::<ItemMod>(item) {
        // Parse convert attributes from module and build settings
        let (settings, mut overrides, schema_renames) = build_type_settings(&mod_item)?;

        // Collect query unknown fields from all traits in the module
        let content = mod_item
            .content
            .as_ref()
            .map(|(_, items)| items.as_slice())
            .unwrap_or(&[]);
        let query_unknown_fields = collect_query_unknown_fields_from_module(content)?;
        for (method, path, field_name) in query_unknown_fields {
            overrides.set_query_unknown_field(method, path, field_name);
        }

        // Convert to HashMap<String, String> for the generator (original_name -> new_name)
        let schema_renames_for_gen: std::collections::HashMap<String, String> = schema_renames
            .iter()
            .map(|(k, rename)| (k.clone(), rename.new.to_string()))
            .collect();

        // Load the OpenAPI spec with custom settings
        // Validation of schema renames happens during TypeGenerator construction
        let spec_path = resolve_spec_path(&args.spec_path)?;
        let generator = oxapi_impl::Generator::builder_from_file(&spec_path)
            .map_err(|e| syn::Error::new(args.spec_path.span(), format!("{}", e)))?
            .settings(settings)
            .type_overrides(overrides)
            .response_suffixes(response_suffixes)
            .schema_renames(schema_renames_for_gen)
            .build()
            .map_err(|e| {
                // Check if this is an UnknownSchema error and use the span from the original ident
                if let oxapi_impl::Error::UnknownSchema { ref name, .. } = e
                    && let Some(rename) = schema_renames.get(name)
                {
                    return syn::Error::new(rename.original.span(), format!("{}", e));
                }
                syn::Error::new(args.spec_path.span(), format!("{}", e))
            })?;

        let processor = ModuleProcessor::new(generator, mod_item, args.unwrap, spec_path)?;
        processor.generate()
    } else {
        Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "expected trait or mod item",
        ))
    }
}

/// Resolve the spec path relative to CARGO_MANIFEST_DIR.
fn resolve_spec_path(lit: &LitStr) -> syn::Result<std::path::PathBuf> {
    let dir = std::env::var("CARGO_MANIFEST_DIR")
        .map_err(|_| syn::Error::new(lit.span(), "CARGO_MANIFEST_DIR not set"))?;

    let path = std::path::Path::new(&dir).join(lit.value());
    Ok(path)
}

/// Rename tracking original and new tokens for name resolution and error reporting.
struct Rename<T> {
    original: T,
    new: T,
}

/// Process a struct item for schema renames or operation type renames.
fn process_struct_item(
    s: &syn::ItemStruct,
    settings: &mut oxapi_impl::TypeSpaceSettings,
    overrides: &mut oxapi_impl::TypeOverrides,
    schema_renames: &mut std::collections::HashMap<String, Rename<Ident>>,
) -> syn::Result<()> {
    if let Some(oxapi_attr) = find_struct_oxapi_attr(&s.attrs)? {
        match oxapi_attr {
            StructOxapiAttr::Schema(original_ident) => {
                // Schema type rename: #[oxapi(OriginalName)] struct NewName;
                let original_name = original_ident.to_string();
                let new_name = s.ident.to_string();
                let mut patch = oxapi_impl::TypeSpacePatch::default();
                patch.with_rename(&new_name);

                for derive_path in find_derives(&s.attrs)? {
                    patch.with_derive(derive_path.to_token_stream().to_string());
                }

                settings.with_patch(&original_name, &patch);
                schema_renames.insert(
                    original_name,
                    Rename {
                        original: original_ident,
                        new: s.ident.clone(),
                    },
                );
            }
            StructOxapiAttr::Operation(op) => {
                // Generated type rename: #[oxapi(get, "/path", ok)] struct NewName;
                overrides.add_rename(op.method, op.path.value(), op.kind, s.ident.clone());
            }
        }
    }
    Ok(())
}

/// Process an enum item for variant renames.
fn process_enum_item(
    e: &syn::ItemEnum,
    overrides: &mut oxapi_impl::TypeOverrides,
) -> syn::Result<()> {
    if let Some(op) = find_enum_oxapi_attr(&e.attrs)? {
        let attrs = extract_passthrough_attrs(&e.attrs);
        let variant_overrides = parse_variant_overrides(&e.variants)?;
        overrides.add_rename_with_overrides(
            op.method,
            op.path.value(),
            op.kind,
            e.ident.clone(),
            attrs,
            variant_overrides,
        );
    }
    Ok(())
}

/// Process a type alias item for schema or operation type replacements.
fn process_type_alias_item(
    t: &syn::ItemType,
    settings: &mut oxapi_impl::TypeSpaceSettings,
    overrides: &mut oxapi_impl::TypeOverrides,
) -> syn::Result<()> {
    if let Some(oxapi_attr) = find_type_alias_oxapi_attr(&t.attrs)? {
        match oxapi_attr {
            TypeAliasOxapiAttr::Operation(op) => {
                // Generated type replacement: #[oxapi(get, "/path", ok)] type _ = T;
                let replacement = t.ty.to_token_stream();
                overrides.add_replace(op.method, op.path.value(), op.kind, replacement);
            }
            TypeAliasOxapiAttr::Schema => {
                // Schema type replacement: #[oxapi] type Name = T;
                let type_name = t.ident.to_string();
                let replace_type = t.ty.to_token_stream().to_string();
                settings.with_replacement(&type_name, &replace_type, std::iter::empty());
            }
        }
    }
    Ok(())
}

/// Build TypeSpaceSettings, TypeOverrides, and schema renames from module attributes and items.
fn build_type_settings(
    mod_item: &ItemMod,
) -> syn::Result<(
    oxapi_impl::TypeSpaceSettings,
    oxapi_impl::TypeOverrides,
    std::collections::HashMap<String, Rename<Ident>>,
)> {
    let mut settings = oxapi_impl::TypeSpaceSettings::default();
    let mut overrides = oxapi_impl::TypeOverrides::new();
    let mut schema_renames = std::collections::HashMap::new();

    // Parse #[convert(...)] attributes on the module
    for attr in find_convert_attrs(&mod_item.attrs)? {
        let schema = attr.to_schema();
        let type_name = attr.into.to_token_stream().to_string();
        settings.with_conversion(schema, type_name, std::iter::empty());
    }

    // Parse items inside the module for patches and replacements
    if let Some((_, content)) = &mod_item.content {
        for item in content {
            match item {
                syn::Item::Struct(s) => {
                    process_struct_item(s, &mut settings, &mut overrides, &mut schema_renames)?;
                }
                syn::Item::Enum(e) => {
                    process_enum_item(e, &mut overrides)?;
                }
                syn::Item::Type(t) => {
                    process_type_alias_item(t, &mut settings, &mut overrides)?;
                }
                _ => {}
            }
        }
    }

    Ok((settings, overrides, schema_renames))
}

/// Parsed macro arguments.
struct MacroArgs {
    spec_path: LitStr,
    unwrap: bool,
    ok_suffix: String,
    err_suffix: String,
    /// Default derives for generated enums (None means use Debug)
    default_derives: Option<TokenStream2>,
}

impl syn::parse::Parse for MacroArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let framework: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let spec_path: LitStr = input.parse()?;

        // Parse optional arguments (unordered)
        let mut unwrap = false;
        let mut ok_suffix: Option<String> = None;
        let mut err_suffix: Option<String> = None;
        let mut default_derives: Option<TokenStream2> = None;

        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;

            // Check if this is a key = value pair or a bare identifier
            let ident: Ident = input.parse()?;

            if input.peek(Token![=]) {
                // key = value syntax
                input.parse::<Token![=]>()?;

                match ident.to_string().as_str() {
                    "ok_suffix" => {
                        let value: LitStr = input.parse()?;
                        if ok_suffix.is_some() {
                            return Err(syn::Error::new(
                                ident.span(),
                                "ok_suffix specified more than once",
                            ));
                        }
                        ok_suffix = Some(value.value());
                    }
                    "err_suffix" => {
                        let value: LitStr = input.parse()?;
                        if err_suffix.is_some() {
                            return Err(syn::Error::new(
                                ident.span(),
                                "err_suffix specified more than once",
                            ));
                        }
                        err_suffix = Some(value.value());
                    }
                    "derive" => {
                        // Parse derive = (Trait1, Trait2, ...)
                        if default_derives.is_some() {
                            return Err(syn::Error::new(
                                ident.span(),
                                "derive specified more than once",
                            ));
                        }
                        let content;
                        syn::parenthesized!(content in input);
                        let derives: syn::punctuated::Punctuated<syn::Path, Token![,]> =
                            content.parse_terminated(syn::Path::parse, Token![,])?;
                        default_derives = Some(quote! { #[derive(#derives)] });
                    }
                    other => {
                        return Err(syn::Error::new(
                            ident.span(),
                            format!("unknown option: {}", other),
                        ));
                    }
                }
            } else {
                // Bare identifier (flag)
                match ident.to_string().as_str() {
                    "unwrap" => {
                        if unwrap {
                            return Err(syn::Error::new(
                                ident.span(),
                                "unwrap specified more than once",
                            ));
                        }
                        unwrap = true;
                    }
                    other => {
                        return Err(syn::Error::new(
                            ident.span(),
                            format!(
                                "unknown option: {} (expected 'unwrap', 'ok_suffix = \"...\"', 'err_suffix = \"...\"', or 'derive = (...)')",
                                other
                            ),
                        ));
                    }
                }
            }
        }

        // Only axum is supported for now
        if framework.to_string().as_str() != "axum" {
            return Err(syn::Error::new(
                framework.span(),
                format!("unsupported framework: {}", framework),
            ));
        }

        Ok(MacroArgs {
            spec_path,
            unwrap,
            ok_suffix: ok_suffix.unwrap_or_else(|| "Response".to_string()),
            err_suffix: err_suffix.unwrap_or_else(|| "Error".to_string()),
            default_derives,
        })
    }
}

/// Set tracking which operations are covered by a trait.
type CoverageSet = std::collections::HashSet<(oxapi_impl::HttpMethod, String)>;

/// Information extracted from a trait for code generation.
struct TraitInfo<'a> {
    trait_item: &'a ItemTrait,
    map_method: Option<&'a syn::TraitItemFn>,
    spec_method: Option<(&'a syn::TraitItemFn, String)>,
    handler_methods: Vec<(&'a syn::TraitItemFn, oxapi_impl::HttpMethod, String)>,
}

/// Extract trait info from a trait, parsing all method attributes.
/// Returns the TraitInfo and a set of covered operations.
fn extract_trait_info<'a>(trait_item: &'a ItemTrait) -> syn::Result<(TraitInfo<'a>, CoverageSet)> {
    let mut covered = CoverageSet::new();
    let mut map_method: Option<&syn::TraitItemFn> = None;
    let mut spec_method: Option<(&syn::TraitItemFn, String)> = None;
    let mut handler_methods: Vec<(&syn::TraitItemFn, oxapi_impl::HttpMethod, String)> = Vec::new();

    for item in &trait_item.items {
        if let syn::TraitItem::Fn(method) = item {
            if let Some(attr) = find_oxapi_attr(&method.attrs)? {
                match attr {
                    OxapiAttr::Map => {
                        map_method = Some(method);
                    }
                    OxapiAttr::Route {
                        method: http_method,
                        path,
                    } => {
                        covered.insert((http_method, path.clone()));
                        handler_methods.push((method, http_method, path));
                    }
                    OxapiAttr::Spec { path } => {
                        validate_bare_spec_method(method)?;
                        covered.insert((oxapi_impl::HttpMethod::Get, path.clone()));
                        spec_method = Some((method, path));
                    }
                }
            } else {
                return Err(syn::Error::new_spanned(
                    method,
                    "all trait methods must have #[oxapi(...)] attribute",
                ));
            }
        }
    }

    let info = TraitInfo {
        trait_item,
        map_method,
        spec_method,
        handler_methods,
    };

    Ok((info, covered))
}

/// Generate transformed methods for a trait.
fn generate_transformed_methods(
    info: &TraitInfo<'_>,
    generator: &oxapi_impl::Generator,
    types_mod_name: &syn::Ident,
    spec_path: &std::path::Path,
) -> syn::Result<Vec<TokenStream2>> {
    let mut transformed_methods = Vec::new();
    let method_transformer = oxapi_impl::MethodTransformer::new(generator, types_mod_name);

    // Generate map_routes if present
    if let Some(map_fn) = info.map_method {
        let map_body = oxapi_impl::RouterGenerator.generate_map_routes(
            &info
                .handler_methods
                .iter()
                .map(|(m, method, path)| (m.sig.ident.clone(), *method, path.clone()))
                .collect::<Vec<_>>(),
            info.spec_method
                .as_ref()
                .map(|(m, path)| (m.sig.ident.clone(), path.clone())),
        );

        let sig = &map_fn.sig;
        transformed_methods.push(quote! {
            #sig {
                #map_body
            }
        });
    }

    // Generate handler methods
    for (method, http_method, path) in &info.handler_methods {
        let op = generator.get_operation(*http_method, path).ok_or_else(|| {
            syn::Error::new_spanned(
                method,
                format!("operation not found: {} {}", http_method, path),
            )
        })?;

        let param_roles = collect_param_roles(method)?;
        let transformed = method_transformer.transform(method, op, &param_roles)?;
        transformed_methods.push(transformed);
    }

    // Generate spec method if present
    if let Some((method, _endpoint_path)) = &info.spec_method {
        let method_name = &method.sig.ident;
        let spec_file_path = spec_path.to_string_lossy();
        transformed_methods.push(quote! {
            fn #method_name() -> &'static str {
                include_str!(#spec_file_path)
            }
        });
    }

    Ok(transformed_methods)
}

/// Processes a trait and generates the output.
struct TraitProcessor {
    generator: oxapi_impl::Generator,
    trait_item: ItemTrait,
    /// Resolved path to the spec file for include_str!
    spec_path: std::path::PathBuf,
}

impl TraitProcessor {
    fn new(
        generator: oxapi_impl::Generator,
        trait_item: ItemTrait,
        spec_path: std::path::PathBuf,
    ) -> syn::Result<Self> {
        Ok(Self {
            generator,
            trait_item,
            spec_path,
        })
    }

    fn generate(self) -> syn::Result<TokenStream2> {
        let trait_name = &self.trait_item.ident;
        let types_mod_name = syn::Ident::new(
            &format!("{}_types", heck::AsSnakeCase(trait_name.to_string())),
            trait_name.span(),
        );

        // Extract trait info and validate coverage
        let (info, covered) = extract_trait_info(&self.trait_item)?;
        self.generator
            .validate_coverage(&covered)
            .map_err(|e| syn::Error::new_spanned(&self.trait_item, e.to_string()))?;

        // Generate types module
        let types = self.generator.generate_types();
        let responses = self.generator.generate_responses();
        let query_structs = self.generator.generate_query_structs();
        let path_structs = self.generator.generate_path_structs();

        // Generate transformed methods using shared helper
        let transformed_methods =
            generate_transformed_methods(&info, &self.generator, &types_mod_name, &self.spec_path)?;

        // Generate the full output
        let vis = &self.trait_item.vis;
        let trait_attrs: Vec<_> = self
            .trait_item
            .attrs
            .iter()
            .filter(|a| !is_oxapi_attr(a))
            .collect();

        // Preserve generic parameters from the original trait
        let generics = &self.trait_item.generics;
        let where_clause = &generics.where_clause;

        // Use trait name span so errors point to user's trait definition
        let trait_def = quote_spanned! { trait_name.span() =>
            #(#trait_attrs)*
            #vis trait #trait_name #generics: 'static #where_clause {
                #(#transformed_methods)*
            }
        };

        let output = quote! {
            #vis mod #types_mod_name {
                use super::*;

                #types
                #responses
                #query_structs
                #path_structs
            }

            #trait_def
        };

        Ok(output)
    }
}

/// Processes a module containing multiple traits.
struct ModuleProcessor {
    generator: oxapi_impl::Generator,
    /// Module wrapper (visibility, name) - None if unwrap mode
    mod_wrapper: Option<(syn::Visibility, Ident)>,
    /// Module content items
    content: Vec<syn::Item>,
    /// Module span for error reporting
    mod_span: proc_macro2::Span,
    /// Resolved path to the spec file for include_str!
    spec_path: std::path::PathBuf,
}

impl ModuleProcessor {
    fn new(
        generator: oxapi_impl::Generator,
        mod_item: ItemMod,
        unwrap: bool,
        spec_path: std::path::PathBuf,
    ) -> syn::Result<Self> {
        // Module must have content (not just a declaration)
        let mod_span = mod_item.ident.span();
        let (_, content) = mod_item.content.ok_or_else(|| {
            syn::Error::new(
                mod_span,
                "module must have inline content, not just a declaration",
            )
        })?;

        let mod_wrapper = if unwrap {
            None
        } else {
            Some((mod_item.vis, mod_item.ident))
        };

        Ok(Self {
            generator,
            mod_wrapper,
            content,
            mod_span,
            spec_path,
        })
    }

    fn generate(self) -> syn::Result<TokenStream2> {
        // Find all traits in the module, filtering out patch/replace items
        let mut traits: Vec<&ItemTrait> = Vec::new();
        let mut other_items: Vec<&syn::Item> = Vec::new();

        for item in &self.content {
            match item {
                syn::Item::Trait(t) => traits.push(t),
                // Skip structs with #[oxapi(...)] (patch/override items)
                syn::Item::Struct(s) if find_struct_oxapi_attr(&s.attrs)?.is_some() => {}
                // Skip enums with #[oxapi(...)] (variant rename items)
                syn::Item::Enum(e) if find_enum_oxapi_attr(&e.attrs)?.is_some() => {}
                // Skip type aliases with #[oxapi] or #[oxapi(...)] (replacement items)
                syn::Item::Type(t) if has_oxapi_attr(&t.attrs) => {}
                other => other_items.push(other),
            }
        }

        if traits.is_empty() {
            return Err(syn::Error::new(
                self.mod_span,
                "module must contain at least one trait",
            ));
        }

        // Extract info from each trait and collect coverage
        let mut all_covered = CoverageSet::new();
        let mut trait_infos: Vec<TraitInfo> = Vec::new();

        for trait_item in &traits {
            let (info, covered) = extract_trait_info(trait_item)?;

            // Check for duplicates across traits
            for key in &covered {
                if all_covered.contains(key) {
                    return Err(syn::Error::new_spanned(
                        trait_item,
                        format!(
                            "operation {} {} is already defined in another trait",
                            key.0, key.1
                        ),
                    ));
                }
            }
            all_covered.extend(covered);
            trait_infos.push(info);
        }

        // Validate coverage against spec
        self.generator
            .validate_coverage(&all_covered)
            .map_err(|e| syn::Error::new(self.mod_span, e.to_string()))?;

        // Generate shared types module
        let types = self.generator.generate_types();
        let responses = self.generator.generate_responses();
        let query_structs = self.generator.generate_query_structs();
        let path_structs = self.generator.generate_path_structs();

        // Generate each trait using the shared helper
        let types_mod_name = syn::Ident::new("types", proc_macro2::Span::call_site());
        let mut generated_traits = Vec::new();

        for info in &trait_infos {
            let trait_name = &info.trait_item.ident;
            let trait_attrs: Vec<_> = info
                .trait_item
                .attrs
                .iter()
                .filter(|a| !is_oxapi_attr(a))
                .collect();

            // Generate transformed methods using shared helper
            let transformed_methods = generate_transformed_methods(
                info,
                &self.generator,
                &types_mod_name,
                &self.spec_path,
            )?;

            // Traits in module are always pub (for external use)
            // Preserve generic parameters from the original trait
            let generics = &info.trait_item.generics;
            let where_clause = &generics.where_clause;
            // Use trait name span so errors point to user's trait definition
            generated_traits.push(quote_spanned! { trait_name.span() =>
                #(#trait_attrs)*
                pub trait #trait_name #generics: 'static #where_clause {
                    #(#transformed_methods)*
                }
            });
        }

        // Generate the output
        let inner = quote! {
            #(#other_items)*

            pub mod #types_mod_name {
                use super::*;

                #types
                #responses
                #query_structs
                #path_structs
            }

            #(#generated_traits)*
        };

        let output = if let Some((vis, name)) = &self.mod_wrapper {
            quote! {
                #vis mod #name {
                    use super::*;
                    #inner
                }
            }
        } else {
            inner
        };

        Ok(output)
    }
}

/// Find and parse the #[oxapi(...)] attribute on a method.
fn find_oxapi_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<OxapiAttr>> {
    for attr in attrs {
        if attr.path().is_ident("oxapi") {
            return attr.parse_args::<OxapiAttr>().map(Some);
        }
    }
    Ok(None)
}

/// Parsed #[oxapi(...)] attribute.
enum OxapiAttr {
    Map,
    Route {
        method: oxapi_impl::HttpMethod,
        path: String,
    },
    /// Spec endpoint: `#[oxapi(spec, "/openapi.yaml")]`
    /// Returns the embedded spec contents at the given path.
    Spec {
        path: String,
    },
}

impl syn::parse::Parse for OxapiAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        match ident.to_string().as_str() {
            "map" => Ok(OxapiAttr::Map),
            "spec" => {
                input.parse::<Token![,]>()?;
                let path: LitStr = input.parse()?;
                Ok(OxapiAttr::Spec { path: path.value() })
            }
            _ => {
                let method = parse_http_method(&ident)?;
                input.parse::<Token![,]>()?;
                let path: LitStr = input.parse()?;
                Ok(OxapiAttr::Route {
                    method,
                    path: path.value(),
                })
            }
        }
    }
}

fn parse_http_method(ident: &Ident) -> syn::Result<oxapi_impl::HttpMethod> {
    match ident.to_string().as_str() {
        "get" => Ok(oxapi_impl::HttpMethod::Get),
        "post" => Ok(oxapi_impl::HttpMethod::Post),
        "put" => Ok(oxapi_impl::HttpMethod::Put),
        "delete" => Ok(oxapi_impl::HttpMethod::Delete),
        "patch" => Ok(oxapi_impl::HttpMethod::Patch),
        "head" => Ok(oxapi_impl::HttpMethod::Head),
        "options" => Ok(oxapi_impl::HttpMethod::Options),
        other => Err(syn::Error::new(
            ident.span(),
            format!("unknown HTTP method: {}", other),
        )),
    }
}

/// Validate that a spec method is completely bare: no parameters, no return type, not async.
fn validate_bare_spec_method(method: &syn::TraitItemFn) -> syn::Result<()> {
    // Check for async
    if method.sig.asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            method.sig.asyncness,
            "spec method must not be async",
        ));
    }

    // Check for parameters
    if !method.sig.inputs.is_empty() {
        return Err(syn::Error::new_spanned(
            &method.sig.inputs,
            "spec method must have no parameters",
        ));
    }

    // Check for return type
    if !matches!(method.sig.output, syn::ReturnType::Default) {
        return Err(syn::Error::new_spanned(
            &method.sig.output,
            "spec method must have no return type (it will be generated)",
        ));
    }

    // Check for generics
    if !method.sig.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            &method.sig.generics,
            "spec method must not have generics",
        ));
    }

    Ok(())
}

/// Parsed #[convert(into = Type, type = "...", format = "...")] attribute.
struct ConvertAttr {
    into: syn::Type,
    type_field: Option<LitStr>,
    format: Option<LitStr>,
}

impl syn::parse::Parse for ConvertAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut into: Option<syn::Type> = None;
        let mut type_field: Option<LitStr> = None;
        let mut format: Option<LitStr> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![=]>()?;

            match key.to_string().as_str() {
                "into" => {
                    into = Some(input.parse()?);
                }
                "type" => {
                    type_field = Some(input.parse()?);
                }
                "format" => {
                    format = Some(input.parse()?);
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown convert attribute: {}", other),
                    ));
                }
            }

            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        let into =
            into.ok_or_else(|| syn::Error::new(input.span(), "convert requires 'into' field"))?;

        Ok(ConvertAttr {
            into,
            type_field,
            format,
        })
    }
}

impl ConvertAttr {
    /// Build a schemars SchemaObject from the type/format fields.
    fn to_schema(&self) -> SchemaObject {
        let instance_type = self.type_field.as_ref().map(|t| {
            let ty = match t.value().as_str() {
                "string" => InstanceType::String,
                "number" => InstanceType::Number,
                "integer" => InstanceType::Integer,
                "boolean" => InstanceType::Boolean,
                "array" => InstanceType::Array,
                "object" => InstanceType::Object,
                "null" => InstanceType::Null,
                _ => InstanceType::String, // fallback
            };
            SingleOrVec::Single(Box::new(ty))
        });

        SchemaObject {
            instance_type,
            format: self.format.as_ref().map(|f| f.value()),
            ..Default::default()
        }
    }
}

/// Find all #[convert(...)] attributes on an item.
fn find_convert_attrs(attrs: &[syn::Attribute]) -> syn::Result<Vec<ConvertAttr>> {
    let mut result = Vec::new();
    for attr in attrs {
        if attr.path().is_ident("convert") {
            result.push(attr.parse_args::<ConvertAttr>()?);
        }
    }
    Ok(result)
}

/// Parsed `#[oxapi(SchemaName)]` attribute for schema type renames.
struct SchemaRenameAttr(Ident);

impl syn::parse::Parse for SchemaRenameAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(SchemaRenameAttr(ident))
    }
}

/// Parsed `#[oxapi(method, "path", kind)]` attribute for generated type renames/replaces.
struct OpAttr {
    method: oxapi_impl::HttpMethod,
    path: LitStr,
    kind: oxapi_impl::GeneratedTypeKind,
}

impl syn::parse::Parse for OpAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method_ident: Ident = input.parse()?;
        let method = parse_http_method(&method_ident)?;
        input.parse::<Token![,]>()?;
        let path: LitStr = input.parse()?;
        input.parse::<Token![,]>()?;
        let kind_ident: Ident = input.parse()?;
        let kind = parse_type_kind(&kind_ident)?;
        Ok(OpAttr { method, path, kind })
    }
}

fn parse_type_kind(ident: &Ident) -> syn::Result<oxapi_impl::GeneratedTypeKind> {
    match ident.to_string().as_str() {
        "ok" => Ok(oxapi_impl::GeneratedTypeKind::Ok),
        "err" => Ok(oxapi_impl::GeneratedTypeKind::Err),
        "query" => Ok(oxapi_impl::GeneratedTypeKind::Query),
        "path" => Ok(oxapi_impl::GeneratedTypeKind::Path),
        other => Err(syn::Error::new(
            ident.span(),
            format!(
                "unknown type kind: {} (expected ok, err, query, or path)",
                other
            ),
        )),
    }
}

/// Parsed `#[oxapi(status = code)]` attribute on enum variant.
struct VariantStatusAttr(u16);

impl syn::parse::Parse for VariantStatusAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        if ident != "status" {
            return Err(syn::Error::new(
                ident.span(),
                format!("expected 'status', found '{}'", ident),
            ));
        }
        input.parse::<Token![=]>()?;
        let lit: syn::LitInt = input.parse()?;
        let code: u16 = lit.base10_parse()?;
        Ok(VariantStatusAttr(code))
    }
}

/// Find `#[oxapi(status = code)]` attribute and return remaining attributes.
/// Returns (status_code, pass_through_attrs) where pass_through_attrs excludes the `#[oxapi]`.
/// Returns an error if more than one `#[oxapi]` attribute is found.
fn extract_variant_status_and_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<Option<(u16, Vec<proc_macro2::TokenStream>)>> {
    let mut found_status: Option<(u16, proc_macro2::Span)> = None;
    let mut pass_through = Vec::new();

    for attr in attrs {
        if attr.path().is_ident("oxapi") {
            if found_status.is_some() {
                return Err(syn::Error::new_spanned(
                    attr,
                    "duplicate #[oxapi] attribute; only one #[oxapi(status = ...)] allowed per variant",
                ));
            }
            let status: VariantStatusAttr = attr.parse_args()?;
            found_status = Some((status.0, attr.path().get_ident().unwrap().span()));
        } else {
            pass_through.push(quote::quote! { #attr });
        }
    }

    Ok(found_status.map(|(s, _)| (s, pass_through)))
}

/// Parse variant overrides from an enum's variants.
/// Extracts variant name, optional inner type name (from tuple variant), and attributes.
fn parse_variant_overrides(
    variants: &syn::punctuated::Punctuated<syn::Variant, Token![,]>,
) -> syn::Result<std::collections::HashMap<u16, oxapi_impl::VariantOverride>> {
    let mut overrides = std::collections::HashMap::new();
    for variant in variants {
        if let Some((status, attrs)) = extract_variant_status_and_attrs(&variant.attrs)? {
            // Extract inner type name from tuple variant like `Success(TheResponse)`
            let inner_type_name = extract_inner_type_ident(&variant.fields)?;

            overrides.insert(
                status,
                oxapi_impl::VariantOverride {
                    name: variant.ident.clone(),
                    inner_type_name,
                    attrs,
                },
            );
        }
    }
    Ok(overrides)
}

/// Extract the inner type identifier from a tuple variant's single field.
/// For `Success(TheResponse)`, returns `Some(TheResponse)`.
/// For `Success` (unit variant), returns `None`.
fn extract_inner_type_ident(fields: &syn::Fields) -> syn::Result<Option<syn::Ident>> {
    match fields {
        syn::Fields::Unit => Ok(None),
        syn::Fields::Unnamed(unnamed) => {
            if unnamed.unnamed.len() != 1 {
                return Err(syn::Error::new_spanned(
                    unnamed,
                    "variant must have exactly one field for inline type override",
                ));
            }
            let field = unnamed.unnamed.first().unwrap();
            // The type should be a simple path like `TheResponse`
            if let syn::Type::Path(type_path) = &field.ty
                && type_path.qself.is_none()
                && type_path.path.segments.len() == 1
            {
                let segment = type_path.path.segments.first().unwrap();
                if segment.arguments.is_empty() {
                    return Ok(Some(segment.ident.clone()));
                }
            }
            Err(syn::Error::new_spanned(
                &field.ty,
                "inner type must be a simple identifier (e.g., `MyTypeName`)",
            ))
        }
        syn::Fields::Named(named) => Err(syn::Error::new_spanned(
            named,
            "named fields not supported for variant overrides; use tuple variant like `Success(TypeName)`",
        )),
    }
}

/// Check if an attribute is an oxapi attribute.
fn is_oxapi_attr(attr: &syn::Attribute) -> bool {
    attr.path().is_ident("oxapi")
}

/// Extract all attributes except `#[oxapi(...)]` as TokenStreams.
fn extract_passthrough_attrs(attrs: &[syn::Attribute]) -> Vec<proc_macro2::TokenStream> {
    attrs
        .iter()
        .filter(|attr| !is_oxapi_attr(attr))
        .map(|attr| quote::quote! { #attr })
        .collect()
}

/// Result of parsing a `#[oxapi(...)]` attribute on a struct (rename context).
enum StructOxapiAttr {
    /// Simple rename for schema types: `#[oxapi(SchemaName)]`
    Schema(Ident),
    /// Operation rename for generated types: `#[oxapi(get, "/path", ok)]`
    Operation(OpAttr),
}

/// Find a single `#[oxapi(...)]` attribute and parse it with the given parser.
/// Returns an error if more than one `#[oxapi]` attribute is found.
fn find_single_oxapi_attr<T, F>(attrs: &[syn::Attribute], parser: F) -> syn::Result<Option<T>>
where
    F: FnOnce(&syn::Attribute) -> syn::Result<T>,
{
    let mut oxapi_attr: Option<&syn::Attribute> = None;

    for attr in attrs {
        if attr.path().is_ident("oxapi") {
            if oxapi_attr.is_some() {
                return Err(syn::Error::new_spanned(
                    attr,
                    "duplicate #[oxapi] attribute; only one allowed per item",
                ));
            }
            oxapi_attr = Some(attr);
        }
    }

    oxapi_attr.map(parser).transpose()
}

/// Find `#[oxapi(...)]` attribute on a struct (for renames).
fn find_struct_oxapi_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<StructOxapiAttr>> {
    find_single_oxapi_attr(attrs, |attr| {
        // Try operation syntax first, then schema syntax (single ident)
        if let Ok(op) = attr.parse_args::<OpAttr>() {
            Ok(StructOxapiAttr::Operation(op))
        } else {
            let schema: SchemaRenameAttr = attr.parse_args()?;
            Ok(StructOxapiAttr::Schema(schema.0))
        }
    })
}

/// Result of parsing a `#[oxapi(...)]` attribute on a type alias (replace context).
enum TypeAliasOxapiAttr {
    /// Schema type replacement: `#[oxapi]` (no args)
    Schema,
    /// Operation replacement: `#[oxapi(get, "/path", ok)]`
    Operation(OpAttr),
}

/// Find `#[oxapi(...)]` attribute on a type alias (for replacements).
fn find_type_alias_oxapi_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<TypeAliasOxapiAttr>> {
    find_single_oxapi_attr(attrs, |attr| {
        // Check if it has arguments (operation style) or not (schema style)
        if let syn::Meta::Path(_) = &attr.meta {
            // No args: #[oxapi]
            Ok(TypeAliasOxapiAttr::Schema)
        } else if let Ok(op) = attr.parse_args::<OpAttr>() {
            Ok(TypeAliasOxapiAttr::Operation(op))
        } else {
            // No args but in list form: #[oxapi()]
            Ok(TypeAliasOxapiAttr::Schema)
        }
    })
}

/// Check if an item has `#[oxapi]` attribute (any style).
fn has_oxapi_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident("oxapi"))
}

/// Find `#[oxapi(...)]` attribute on an enum (for renames).
/// For enums, we only support operation syntax: `#[oxapi(get, "/path", err)]`
fn find_enum_oxapi_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<OpAttr>> {
    find_single_oxapi_attr(attrs, |attr| attr.parse_args())
}

/// Find #[derive(...)] attributes and extract the derive paths.
fn find_derives(attrs: &[syn::Attribute]) -> syn::Result<Vec<syn::Path>> {
    let mut derives = Vec::new();
    for attr in attrs {
        if attr.path().is_ident("derive") {
            let nested: syn::punctuated::Punctuated<syn::Path, Token![,]> =
                attr.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
            derives.extend(nested);
        }
    }
    Ok(derives)
}

/// Parameter role attribute: `#[oxapi(path)]`, `#[oxapi(query)]`, `#[oxapi(query, field_name)]`, or `#[oxapi(body)]`.
#[derive(Debug, Clone, PartialEq, Eq)]
enum ParamOxapiAttr {
    Path,
    /// Query with optional unknown field name for capturing extra query params.
    /// e.g., `#[oxapi(query, extras)]` adds a `#[serde(flatten)] pub extras: HashMap<String, String>` field.
    Query {
        unknown_field: Option<Ident>,
    },
    Body,
}

impl syn::parse::Parse for ParamOxapiAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;
        match ident.to_string().as_str() {
            "path" => Ok(ParamOxapiAttr::Path),
            "query" => {
                // Check for optional unknown field name: #[oxapi(query, field_name)]
                let unknown_field = if input.peek(Token![,]) {
                    input.parse::<Token![,]>()?;
                    Some(input.parse::<Ident>()?)
                } else {
                    None
                };
                Ok(ParamOxapiAttr::Query { unknown_field })
            }
            "body" => Ok(ParamOxapiAttr::Body),
            other => Err(syn::Error::new(
                ident.span(),
                format!(
                    "unknown parameter attribute: {} (expected path, query, or body)",
                    other
                ),
            )),
        }
    }
}

/// Find `#[oxapi(path)]`, `#[oxapi(query)]`, or `#[oxapi(body)]` attribute on a parameter.
fn find_param_oxapi_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<ParamOxapiAttr>> {
    find_single_oxapi_attr(attrs, |attr| attr.parse_args())
}

/// Collect all query unknown fields from traits in a module.
///
/// Returns a map of (HTTP method, path) → unknown field name.
fn collect_query_unknown_fields_from_module(
    items: &[syn::Item],
) -> syn::Result<Vec<(oxapi_impl::HttpMethod, String, Ident)>> {
    let mut fields = Vec::new();
    for item in items {
        if let syn::Item::Trait(t) = item {
            fields.extend(collect_query_unknown_fields_from_trait(t)?);
        }
    }
    Ok(fields)
}

/// Collect query unknown fields from a single trait.
fn collect_query_unknown_fields_from_trait(
    trait_item: &syn::ItemTrait,
) -> syn::Result<Vec<(oxapi_impl::HttpMethod, String, Ident)>> {
    let mut fields = Vec::new();
    for item in &trait_item.items {
        if let syn::TraitItem::Fn(method) = item
            && let Some(OxapiAttr::Route {
                method: http_method,
                path,
            }) = find_oxapi_attr(&method.attrs)?
        {
            // Check parameters for #[oxapi(query, unknown_field)]
            for arg in &method.sig.inputs {
                if let syn::FnArg::Typed(pat_type) = arg
                    && let Some(ParamOxapiAttr::Query {
                        unknown_field: Some(field_name),
                    }) = find_param_oxapi_attr(&pat_type.attrs)?
                {
                    fields.push((http_method, path.clone(), field_name));
                }
            }
        }
    }
    Ok(fields)
}

/// Extract parameter roles from a method's parameters.
/// Returns a Vec with the same length as the method's inputs, with Some(role) for
/// parameters that have an explicit `#[oxapi(path)]`, `#[oxapi(query)]`, or `#[oxapi(body)]`
/// attribute, and None for parameters without such attributes.
fn collect_param_roles(
    method: &syn::TraitItemFn,
) -> syn::Result<Vec<Option<oxapi_impl::ParamRole>>> {
    let mut roles = Vec::new();

    for arg in &method.sig.inputs {
        match arg {
            syn::FnArg::Receiver(_) => {
                // Receiver will be rejected later, just push None for now
                roles.push(None);
            }
            syn::FnArg::Typed(pat_type) => {
                // Check for #[oxapi(path)], #[oxapi(query)], or #[oxapi(body)]
                if let Some(attr) = find_param_oxapi_attr(&pat_type.attrs)? {
                    let role = match attr {
                        ParamOxapiAttr::Path => oxapi_impl::ParamRole::Path,
                        ParamOxapiAttr::Query { unknown_field } => {
                            oxapi_impl::ParamRole::Query { unknown_field }
                        }
                        ParamOxapiAttr::Body => oxapi_impl::ParamRole::Body,
                    };
                    roles.push(Some(role));
                } else {
                    roles.push(None);
                }
            }
        }
    }

    Ok(roles)
}
