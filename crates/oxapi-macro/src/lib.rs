//! Procedural macros for the oxapi OpenAPI server stub generator.
//!
//! This crate provides the `#[oxapi]` attribute macro for generating type-safe
//! server stubs from OpenAPI specifications. See [`oxapi`] for full documentation.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
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
/// # Generated Types
///
/// For each operation, the macro generates:
///
/// - `{OperationId}{ok_suffix}` - Enum with success response variants (2xx status codes), default suffix is `Response`
/// - `{OperationId}{err_suffix}` - Enum with error response variants (4xx, 5xx, default), default suffix is `Error`
/// - `{OperationId}Query` - Struct for query parameters (if operation has query params)
///
/// All response enums implement `axum::response::IntoResponse`.
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
///     #[rename("Vegetable")]
///     struct Veggie;
///
///     // Add extra derives to a schema type
///     #[rename("Vegetable")]
///     #[derive(schemars::JsonSchema, PartialEq, Eq, Hash)]
///     struct Veggie;
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
///     #[replace]
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
///     #[rename(get, "/pet/{petId}", ok)]
///     struct PetResponse;
///
///     // Rename GetPetByIdError to PetError
///     #[rename(get, "/pet/{petId}", err)]
///     struct PetError;
///
///     // Rename FindPetsByStatusQuery to PetSearchParams
///     #[rename(get, "/pet/findByStatus", query)]
///     struct PetSearchParams;
///
///     // Replace GetPetByIdResponse with a custom type (skips generation)
///     #[replace(get, "/pet/{petId}", ok)]
///     type _ = my_types::PetResponse;
///
///     // Replace GetPetByIdError with a custom type
///     #[replace(get, "/pet/{petId}", err)]
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
///     #[rename(get, "/pet/{petId}", err)]
///     enum PetError {
///         #[status(401)]
///         Unauthorized,
///         #[status(404)]
///         NotFound,
///     }
///     // Generates: enum PetError { Unauthorized(...), NotFound(...), Status500(...), ... }
///     // instead of: enum GetPetByIdError { Status401(...), Status404(...), Status500(...), ... }
/// }
/// ```
///
/// Only status codes you specify will be renamed; others retain their default `Status{code}` names.
///
/// ### Attribute Pass-Through
///
/// All attributes on the enum (except `#[rename(...)]`) and on variants (except the first
/// `#[status(...)]`) are passed through to the generated code. If you specify a `#[derive(...)]`
/// on the enum, it completely overrides the default derives.
///
/// ```ignore
/// #[oxapi(axum, "spec.json")]
/// mod api {
///     #[rename(get, "/pet/{petId}", err)]
///     #[derive(Debug, thiserror::Error)]  // Overrides default, must include Debug if needed
///     enum PetError {
///         #[status(401)]
///         #[error("Unauthorized access")]
///         Unauthorized,
///
///         #[status(404)]
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
///
/// # Complete Example
///
/// ```ignore
/// use axum::{Router, extract::{State, Path, Query, Json}};
///
/// #[oxapi::oxapi(axum, "petstore.json")]
/// #[convert(into = uuid::Uuid, type = "string", format = "uuid")]
/// mod api {
///     // Rename a schema type
///     #[rename("Animal")]
///     #[derive(Clone)]
///     struct Pet;
///
///     // Replace a generated response type
///     #[replace(get, "/pet/{petId}", err)]
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
        // Load the OpenAPI spec with default settings for traits
        let spec_path = resolve_spec_path(&args.spec_path)?;
        let generator = oxapi_impl::Generator::from_file_with_all_settings(
            &spec_path,
            oxapi_impl::TypeSpaceSettings::default(),
            oxapi_impl::TypeOverrides::new(),
            response_suffixes,
        )
        .map_err(|e| {
            syn::Error::new(args.spec_path.span(), format!("failed to load spec: {}", e))
        })?;
        let processor = TraitProcessor::new(generator, args.framework, trait_item)?;
        processor.generate()
    } else if let Ok(mod_item) = syn::parse2::<ItemMod>(item) {
        // Parse convert attributes from module and build settings
        let (settings, overrides) = build_type_settings(&mod_item)?;

        // Load the OpenAPI spec with custom settings
        let spec_path = resolve_spec_path(&args.spec_path)?;
        let generator = oxapi_impl::Generator::from_file_with_all_settings(
            &spec_path,
            settings,
            overrides,
            response_suffixes,
        )
        .map_err(|e| {
            syn::Error::new(args.spec_path.span(), format!("failed to load spec: {}", e))
        })?;

        let processor = ModuleProcessor::new(generator, args.framework, mod_item, args.unwrap)?;
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

/// Build TypeSpaceSettings and TypeOverrides from module attributes and items.
fn build_type_settings(
    mod_item: &ItemMod,
) -> syn::Result<(oxapi_impl::TypeSpaceSettings, oxapi_impl::TypeOverrides)> {
    let mut settings = oxapi_impl::TypeSpaceSettings::default();
    let mut overrides = oxapi_impl::TypeOverrides::new();

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
                // Struct with #[rename(...)]
                syn::Item::Struct(s) => {
                    if let Some(rename) = find_rename_attr(&s.attrs)? {
                        match rename {
                            RenameAttr::Schema(new_name) => {
                                // Schema type rename: #[rename("NewName")] struct OldName;
                                let original_name = s.ident.to_string();
                                let mut patch = oxapi_impl::TypeSpacePatch::default();
                                patch.with_rename(&new_name);

                                // Also add any derives
                                for derive_path in find_derives(&s.attrs)? {
                                    patch.with_derive(derive_path.to_token_stream().to_string());
                                }

                                settings.with_patch(&original_name, &patch);
                            }
                            RenameAttr::Operation(op) => {
                                // Generated type rename: #[rename(get, "/path", ok)] struct NewName;
                                let new_name = s.ident.to_string();
                                overrides.add_rename(op.method, op.path, op.kind, new_name);
                            }
                        }
                    }
                }

                // Enum with #[rename(method, "path", kind)] for variant renames
                syn::Item::Enum(e) => {
                    if let Some(RenameAttr::Operation(op)) = find_rename_attr(&e.attrs)? {
                        let new_name = e.ident.to_string();
                        let attrs = extract_passthrough_attrs(&e.attrs);
                        let variant_overrides = parse_variant_overrides(&e.variants)?;
                        overrides.add_rename_with_overrides(
                            op.method,
                            op.path,
                            op.kind,
                            new_name,
                            attrs,
                            variant_overrides,
                        );
                    }
                }

                // Type alias with #[replace] or #[replace(method, path, kind)]
                syn::Item::Type(t) => {
                    if has_replace_attr(&t.attrs) {
                        if let Some(op) = find_replace_attr(&t.attrs)? {
                            // Generated type replacement: #[replace(get, "/path", ok)] type _ = T;
                            let replacement = t.ty.to_token_stream();
                            overrides.add_replace(op.method, op.path, op.kind, replacement);
                        } else {
                            // Schema type replacement: #[replace] type Name = T;
                            let type_name = t.ident.to_string();
                            let replace_type = t.ty.to_token_stream().to_string();
                            settings
                                .with_replacement(&type_name, &replace_type, std::iter::empty());
                        }
                    }
                }

                _ => {}
            }
        }
    }

    Ok((settings, overrides))
}

/// Parsed macro arguments.
struct MacroArgs {
    framework: Framework,
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

        let framework = match framework.to_string().as_str() {
            "axum" => Framework::Axum,
            other => {
                return Err(syn::Error::new(
                    framework.span(),
                    format!("unsupported framework: {}", other),
                ));
            }
        };

        Ok(MacroArgs {
            framework,
            spec_path,
            unwrap,
            ok_suffix: ok_suffix.unwrap_or_else(|| "Response".to_string()),
            err_suffix: err_suffix.unwrap_or_else(|| "Error".to_string()),
            default_derives,
        })
    }
}

#[derive(Clone, Copy)]
enum Framework {
    Axum,
}

/// Processes a trait and generates the output.
struct TraitProcessor {
    generator: oxapi_impl::Generator,
    #[allow(dead_code)]
    framework: Framework,
    trait_item: ItemTrait,
}

impl TraitProcessor {
    fn new(
        generator: oxapi_impl::Generator,
        framework: Framework,
        trait_item: ItemTrait,
    ) -> syn::Result<Self> {
        Ok(Self {
            generator,
            framework,
            trait_item,
        })
    }

    fn generate(self) -> syn::Result<TokenStream2> {
        use std::collections::HashMap;

        let trait_name = &self.trait_item.ident;
        let types_mod_name = syn::Ident::new(
            &format!("{}_types", heck::AsSnakeCase(trait_name.to_string())),
            trait_name.span(),
        );

        // Parse all method attributes and collect coverage info
        let mut covered: HashMap<(oxapi_impl::HttpMethod, String), ()> = HashMap::new();
        let mut map_method: Option<&syn::TraitItemFn> = None;
        let mut handler_methods: Vec<(&syn::TraitItemFn, oxapi_impl::HttpMethod, String)> =
            Vec::new();

        for item in &self.trait_item.items {
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
                            covered.insert((http_method, path.clone()), ());
                            handler_methods.push((method, http_method, path));
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

        // Validate coverage
        self.generator
            .validate_coverage(&covered)
            .map_err(|e| syn::Error::new_spanned(&self.trait_item, e.to_string()))?;

        // Generate types module
        let types = self.generator.generate_types();
        let responses = self.generator.generate_responses();
        let query_structs = self.generator.generate_query_structs();

        // Generate transformed methods
        let mut transformed_methods = Vec::new();

        // Generate map_routes if present
        if let Some(map_fn) = map_method {
            let router_gen = oxapi_impl::RouterGenerator::new(&self.generator);
            let map_body = router_gen.generate_map_routes(
                &handler_methods
                    .iter()
                    .map(|(m, method, path)| (m.sig.ident.clone(), *method, path.clone()))
                    .collect::<Vec<_>>(),
            );

            let sig = &map_fn.sig;
            transformed_methods.push(quote! {
                #sig {
                    #map_body
                }
            });
        }

        // Generate handler methods
        let method_transformer =
            oxapi_impl::MethodTransformer::new(&self.generator, &types_mod_name);
        for (method, http_method, path) in &handler_methods {
            let op = self
                .generator
                .get_operation(*http_method, path)
                .ok_or_else(|| {
                    syn::Error::new_spanned(
                        method,
                        format!("operation not found: {} {}", http_method, path),
                    )
                })?;

            let transformed = method_transformer.transform(method, op)?;
            transformed_methods.push(transformed);
        }

        // Generate the full output
        let vis = &self.trait_item.vis;
        let trait_attrs: Vec<_> = self
            .trait_item
            .attrs
            .iter()
            .filter(|a| !a.path().is_ident("oxapi"))
            .collect();

        // Preserve generic parameters from the original trait
        let generics = &self.trait_item.generics;
        let where_clause = &generics.where_clause;

        let output = quote! {
            #vis mod #types_mod_name {
                use super::*;

                #types
                #responses
                #query_structs
            }

            #(#trait_attrs)*
            #vis trait #trait_name #generics: 'static #where_clause {
                #(#transformed_methods)*
            }
        };

        Ok(output)
    }
}

/// Processes a module containing multiple traits.
struct ModuleProcessor {
    generator: oxapi_impl::Generator,
    #[allow(dead_code)]
    framework: Framework,
    /// Module wrapper (visibility, name) - None if unwrap mode
    mod_wrapper: Option<(syn::Visibility, Ident)>,
    /// Module content items
    content: Vec<syn::Item>,
}

impl ModuleProcessor {
    fn new(
        generator: oxapi_impl::Generator,
        framework: Framework,
        mod_item: ItemMod,
        unwrap: bool,
    ) -> syn::Result<Self> {
        // Module must have content (not just a declaration)
        let (_, content) = mod_item.content.ok_or_else(|| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
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
            framework,
            mod_wrapper,
            content,
        })
    }

    fn generate(self) -> syn::Result<TokenStream2> {
        use std::collections::HashMap;

        // Find all traits in the module, filtering out patch/replace items
        let mut traits: Vec<&ItemTrait> = Vec::new();
        let mut other_items: Vec<&syn::Item> = Vec::new();

        for item in &self.content {
            match item {
                syn::Item::Trait(t) => traits.push(t),
                // Skip structs with #[rename(...)] (patch/override items)
                syn::Item::Struct(s) if find_rename_attr(&s.attrs)?.is_some() => {}
                // Skip enums with #[rename(...)] (variant rename items)
                syn::Item::Enum(e) if find_rename_attr(&e.attrs)?.is_some() => {}
                // Skip type aliases with #[replace] or #[replace(...)]
                syn::Item::Type(t) if has_replace_attr(&t.attrs) => {}
                other => other_items.push(other),
            }
        }

        if traits.is_empty() {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "module must contain at least one trait",
            ));
        }

        // Collect all operations across all traits for coverage validation
        let mut all_covered: HashMap<(oxapi_impl::HttpMethod, String), ()> = HashMap::new();

        // Process each trait and collect handler info
        struct TraitInfo<'a> {
            trait_item: &'a ItemTrait,
            map_method: Option<&'a syn::TraitItemFn>,
            handler_methods: Vec<(&'a syn::TraitItemFn, oxapi_impl::HttpMethod, String)>,
        }

        let mut trait_infos: Vec<TraitInfo> = Vec::new();

        for trait_item in &traits {
            let mut map_method: Option<&syn::TraitItemFn> = None;
            let mut handler_methods: Vec<(&syn::TraitItemFn, oxapi_impl::HttpMethod, String)> =
                Vec::new();

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
                                // Check for duplicates across traits
                                let key = (http_method, path.clone());
                                if all_covered.contains_key(&key) {
                                    return Err(syn::Error::new_spanned(
                                        method,
                                        format!(
                                            "operation {} {} is already defined in another trait",
                                            http_method, path
                                        ),
                                    ));
                                }
                                all_covered.insert(key, ());
                                handler_methods.push((method, http_method, path));
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

            trait_infos.push(TraitInfo {
                trait_item,
                map_method,
                handler_methods,
            });
        }

        // Validate coverage against spec
        self.generator
            .validate_coverage(&all_covered)
            .map_err(|e| syn::Error::new(proc_macro2::Span::call_site(), e.to_string()))?;

        // Generate shared types module
        let types = self.generator.generate_types();
        let responses = self.generator.generate_responses();
        let query_structs = self.generator.generate_query_structs();

        // Generate each trait
        let types_mod_name = syn::Ident::new("types", proc_macro2::Span::call_site());
        let method_transformer =
            oxapi_impl::MethodTransformer::new(&self.generator, &types_mod_name);

        let mut generated_traits = Vec::new();

        for info in &trait_infos {
            let trait_name = &info.trait_item.ident;
            let _trait_vis = &info.trait_item.vis;
            let trait_attrs: Vec<_> = info
                .trait_item
                .attrs
                .iter()
                .filter(|a| !a.path().is_ident("oxapi"))
                .collect();

            let mut transformed_methods = Vec::new();

            // Generate map_routes if present
            if let Some(map_fn) = info.map_method {
                let router_gen = oxapi_impl::RouterGenerator::new(&self.generator);
                let map_body = router_gen.generate_map_routes(
                    &info
                        .handler_methods
                        .iter()
                        .map(|(m, method, path)| (m.sig.ident.clone(), *method, path.clone()))
                        .collect::<Vec<_>>(),
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
                let op = self
                    .generator
                    .get_operation(*http_method, path)
                    .ok_or_else(|| {
                        syn::Error::new_spanned(
                            method,
                            format!("operation not found: {} {}", http_method, path),
                        )
                    })?;

                let transformed = method_transformer.transform(method, op)?;
                transformed_methods.push(transformed);
            }

            // Traits in module are always pub (for external use)
            // Preserve generic parameters from the original trait
            let generics = &info.trait_item.generics;
            let where_clause = &generics.where_clause;
            generated_traits.push(quote! {
                #(#trait_attrs)*
                pub trait #trait_name #generics: 'static #where_clause {
                    #(#transformed_methods)*
                }
            });
        }

        // Generate the output
        let inner = quote! {
            pub mod #types_mod_name {
                use super::*;

                #types
                #responses
                #query_structs
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
}

impl syn::parse::Parse for OxapiAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        match ident.to_string().as_str() {
            "map" => Ok(OxapiAttr::Map),
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

/// Parsed #[convert(into = Type, type = "...", format = "...")] attribute.
struct ConvertAttr {
    into: syn::Type,
    type_field: Option<String>,
    format: Option<String>,
}

impl syn::parse::Parse for ConvertAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut into: Option<syn::Type> = None;
        let mut type_field: Option<String> = None;
        let mut format: Option<String> = None;

        while !input.is_empty() {
            let key: Ident = input.parse()?;
            input.parse::<Token![=]>()?;

            match key.to_string().as_str() {
                "into" => {
                    into = Some(input.parse()?);
                }
                "type" => {
                    let lit: LitStr = input.parse()?;
                    type_field = Some(lit.value());
                }
                "format" => {
                    let lit: LitStr = input.parse()?;
                    format = Some(lit.value());
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
            let ty = match t.as_str() {
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
            format: self.format.clone(),
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

/// Parsed #[rename("...")] attribute for schema types.
struct SchemaRenameAttr(String);

impl syn::parse::Parse for SchemaRenameAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lit: LitStr = input.parse()?;
        Ok(SchemaRenameAttr(lit.value()))
    }
}

/// Parsed #[rename(method, "path", kind)] attribute for generated types.
struct OpRenameAttr {
    method: oxapi_impl::HttpMethod,
    path: String,
    kind: oxapi_impl::GeneratedTypeKind,
}

impl syn::parse::Parse for OpRenameAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method_ident: Ident = input.parse()?;
        let method = parse_http_method(&method_ident)?;
        input.parse::<Token![,]>()?;
        let path_lit: LitStr = input.parse()?;
        let path = path_lit.value();
        input.parse::<Token![,]>()?;
        let kind_ident: Ident = input.parse()?;
        let kind = parse_type_kind(&kind_ident)?;
        Ok(OpRenameAttr { method, path, kind })
    }
}

/// Parsed #[replace(method, "path", kind)] attribute for generated types.
struct OpReplaceAttr {
    method: oxapi_impl::HttpMethod,
    path: String,
    kind: oxapi_impl::GeneratedTypeKind,
}

impl syn::parse::Parse for OpReplaceAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method_ident: Ident = input.parse()?;
        let method = parse_http_method(&method_ident)?;
        input.parse::<Token![,]>()?;
        let path_lit: LitStr = input.parse()?;
        let path = path_lit.value();
        input.parse::<Token![,]>()?;
        let kind_ident: Ident = input.parse()?;
        let kind = parse_type_kind(&kind_ident)?;
        Ok(OpReplaceAttr { method, path, kind })
    }
}

fn parse_type_kind(ident: &Ident) -> syn::Result<oxapi_impl::GeneratedTypeKind> {
    match ident.to_string().as_str() {
        "ok" => Ok(oxapi_impl::GeneratedTypeKind::Ok),
        "err" => Ok(oxapi_impl::GeneratedTypeKind::Err),
        "query" => Ok(oxapi_impl::GeneratedTypeKind::Query),
        other => Err(syn::Error::new(
            ident.span(),
            format!("unknown type kind: {} (expected ok, err, or query)", other),
        )),
    }
}

/// Parsed #[status(code)] attribute on enum variant.
struct StatusAttr(u16);

impl syn::parse::Parse for StatusAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lit: syn::LitInt = input.parse()?;
        let code: u16 = lit.base10_parse()?;
        Ok(StatusAttr(code))
    }
}

/// Find the first #[status(code)] attribute and return remaining attributes.
/// Returns (status_code, pass_through_attrs) where pass_through_attrs includes
/// all attributes except the first #[status(...)].
fn extract_status_and_attrs(
    attrs: &[syn::Attribute],
) -> syn::Result<Option<(u16, Vec<proc_macro2::TokenStream>)>> {
    let mut found_status: Option<u16> = None;
    let mut pass_through = Vec::new();
    let mut first_status_consumed = false;

    for attr in attrs {
        if attr.path().is_ident("status") && !first_status_consumed {
            let status: StatusAttr = attr.parse_args()?;
            found_status = Some(status.0);
            first_status_consumed = true;
        } else {
            // Pass through all other attributes (including subsequent #[status(...)])
            pass_through.push(quote::quote! { #attr });
        }
    }

    Ok(found_status.map(|s| (s, pass_through)))
}

/// Parse variant overrides from an enum's variants.
fn parse_variant_overrides(
    variants: &syn::punctuated::Punctuated<syn::Variant, Token![,]>,
) -> syn::Result<std::collections::HashMap<u16, oxapi_impl::VariantOverride>> {
    let mut overrides = std::collections::HashMap::new();
    for variant in variants {
        if let Some((status, attrs)) = extract_status_and_attrs(&variant.attrs)? {
            overrides.insert(
                status,
                oxapi_impl::VariantOverride {
                    name: variant.ident.to_string(),
                    attrs,
                },
            );
        }
    }
    Ok(overrides)
}

/// Extract all attributes except #[rename(...)] as TokenStreams.
fn extract_passthrough_attrs(attrs: &[syn::Attribute]) -> Vec<proc_macro2::TokenStream> {
    attrs
        .iter()
        .filter(|attr| !attr.path().is_ident("rename"))
        .map(|attr| quote::quote! { #attr })
        .collect()
}

/// Result of parsing a #[rename(...)] attribute.
enum RenameAttr {
    /// Simple rename for schema types: #[rename("NewName")]
    Schema(String),
    /// Operation rename for generated types: #[rename(get, "/path", ok)]
    Operation(OpRenameAttr),
}

/// Find #[rename(...)] attribute on an item.
fn find_rename_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<RenameAttr>> {
    for attr in attrs {
        if attr.path().is_ident("rename") {
            // Try operation syntax first, then schema syntax
            if let Ok(op) = attr.parse_args::<OpRenameAttr>() {
                return Ok(Some(RenameAttr::Operation(op)));
            }
            let schema: SchemaRenameAttr = attr.parse_args()?;
            return Ok(Some(RenameAttr::Schema(schema.0)));
        }
    }
    Ok(None)
}

/// Find #[replace(...)] attribute on an item.
fn find_replace_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<OpReplaceAttr>> {
    for attr in attrs {
        if attr.path().is_ident("replace") {
            // Check if it has arguments (operation style) or not (schema style)
            if attr.meta.require_list().is_ok() {
                return Ok(Some(attr.parse_args::<OpReplaceAttr>()?));
            }
            // No args means schema-style #[replace]
            return Ok(None);
        }
    }
    Ok(None)
}

/// Check if an item has #[replace] attribute (either style).
fn has_replace_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident("replace"))
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
