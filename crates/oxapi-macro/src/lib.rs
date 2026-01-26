//! Procedural macros for the oxapi OpenAPI server stub generator.

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Ident, ItemMod, ItemTrait, LitStr, Token};

/// Main attribute macro for generating server stubs from OpenAPI specs.
///
/// # Example (single trait)
///
/// ```ignore
/// #[oxapi::oxapi(axum, "spec.json")]
/// trait MyServer {
///     #[oxapi(map)]
///     fn map_routes(router: Router) -> Router;
///
///     #[oxapi(get, "/users")]
///     async fn get_users(state: State<AppState>, query: Query<_>);
/// }
/// ```
///
/// # Example (module with multiple traits)
///
/// ```ignore
/// #[oxapi::oxapi(axum, "spec.json")]
/// mod my_api {
///     trait PetService {
///         #[oxapi(map)]
///         fn map_routes(router: Router<PetState>) -> Router<PetState>;
///
///         #[oxapi(get, "/pet/{petId}")]
///         async fn get_pet(state: State<PetState>, pet_id: Path<_>);
///     }
///
///     trait StoreService {
///         #[oxapi(map)]
///         fn map_routes(router: Router<StoreState>) -> Router<StoreState>;
///
///         #[oxapi(get, "/store/inventory")]
///         async fn get_inventory(state: State<StoreState>);
///     }
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

    // Load the OpenAPI spec
    let spec_path = resolve_spec_path(&args.spec_path)?;
    let generator = oxapi_impl::Generator::from_file(&spec_path).map_err(|e| {
        syn::Error::new(args.spec_path.span(), format!("failed to load spec: {}", e))
    })?;

    // Try to parse as trait first, then as module
    if let Ok(trait_item) = syn::parse2::<ItemTrait>(item.clone()) {
        let processor = TraitProcessor::new(generator, args.framework, trait_item)?;
        processor.generate()
    } else if let Ok(mod_item) = syn::parse2::<ItemMod>(item) {
        let processor = ModuleProcessor::new(generator, args.framework, mod_item)?;
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

/// Parsed macro arguments.
struct MacroArgs {
    framework: Framework,
    spec_path: LitStr,
}

impl syn::parse::Parse for MacroArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let framework: Ident = input.parse()?;
        input.parse::<Token![,]>()?;
        let spec_path: LitStr = input.parse()?;

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
    mod_item: ItemMod,
}

impl ModuleProcessor {
    fn new(
        generator: oxapi_impl::Generator,
        framework: Framework,
        mod_item: ItemMod,
    ) -> syn::Result<Self> {
        // Module must have content (not just a declaration)
        if mod_item.content.is_none() {
            return Err(syn::Error::new_spanned(
                &mod_item,
                "module must have inline content, not just a declaration",
            ));
        }
        Ok(Self {
            generator,
            framework,
            mod_item,
        })
    }

    fn generate(self) -> syn::Result<TokenStream2> {
        use std::collections::HashMap;

        let mod_name = &self.mod_item.ident;
        let mod_vis = &self.mod_item.vis;
        let (_, content) = self.mod_item.content.as_ref().unwrap();

        // Find all traits in the module
        let mut traits: Vec<&ItemTrait> = Vec::new();
        let mut other_items: Vec<&syn::Item> = Vec::new();

        for item in content {
            match item {
                syn::Item::Trait(t) => traits.push(t),
                other => other_items.push(other),
            }
        }

        if traits.is_empty() {
            return Err(syn::Error::new_spanned(
                &self.mod_item,
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
            .map_err(|e| syn::Error::new_spanned(&self.mod_item, e.to_string()))?;

        // Generate shared types module
        let types = self.generator.generate_types();
        let responses = self.generator.generate_responses();

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

        // Generate the module
        let output = quote! {
            #mod_vis mod #mod_name {
                use super::*;

                pub mod #types_mod_name {
                    use super::*;

                    #types
                    #responses
                }

                #(#generated_traits)*
            }
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
