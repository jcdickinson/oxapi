//! Integration test: Additional user-defined extractors and parameter attributes
//!
//! This test demonstrates:
//! 1. Additional extractors in handler signatures beyond what's inferred from the spec
//! 2. The new `#[oxapi(path)]`, `#[oxapi(query)]`, and `#[oxapi(body)]` parameter attributes
//!
//! Custom extractors with explicit types (no `_` elision) are passed through
//! unchanged into the generated trait. Only extractors with `_` have their
//! types inferred from the spec.
//!
//! The parameter attributes allow explicit role assignment for custom extractors.

mod common;

use axum::Router;
use axum::extract::FromRequestParts;
use axum::http::request::Parts;
use common::StateProvider;
use std::convert::Infallible;

/// Mock JWT claims extractor for authentication.
#[derive(Debug, Clone)]
pub struct Jwt<T>(pub T);

/// Mock claims type.
#[derive(Debug, Clone)]
pub struct AppClaims {
    pub user_id: String,
}

impl<S: StateProvider> FromRequestParts<S> for Jwt<AppClaims> {
    type Rejection = Infallible;

    async fn from_request_parts(_parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        Ok(Jwt(AppClaims {
            user_id: "test".to_string(),
        }))
    }
}

/// Custom path extractor (to test explicit `#[oxapi(path)]` attribute)
#[derive(Debug)]
pub struct MyPath<T>(pub T);

impl<S, T> FromRequestParts<S> for MyPath<T>
where
    S: Send + Sync,
    T: Send + Sync + 'static,
{
    type Rejection = Infallible;

    async fn from_request_parts(_parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        unimplemented!()
    }
}

/// Custom query extractor (to test explicit `#[oxapi(query)]` attribute)
#[derive(Debug)]
pub struct MyQuery<T>(pub T);

impl<S, T> FromRequestParts<S> for MyQuery<T>
where
    S: Send + Sync,
    T: Send + Sync + 'static,
{
    type Rejection = Infallible;

    async fn from_request_parts(_parts: &mut Parts, _state: &S) -> Result<Self, Self::Rejection> {
        unimplemented!()
    }
}

#[oxapi::oxapi(axum, "tests/additional_extractors.yaml")]
pub mod api {
    use axum::Json;
    use axum::extract::{Path, Query, State};

    /// Items API with authentication requirement.
    ///
    /// Custom extractors like `Jwt<AppClaims>` can be added to method signatures.
    /// Extractors with explicit types (no type elision) are passed through unchanged.
    /// Only extractors with `_` (like `Json<_>`) have their types inferred from the spec.
    pub trait ItemsApi<S: StateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        // Custom auth extractor is copied unchanged into the generated trait
        #[oxapi(post, "/items")]
        async fn create_item(state: State<S>, claims: Jwt<AppClaims>, body: Json<_>);

        #[oxapi(get, "/items")]
        async fn list_items(state: State<S>, claims: Jwt<AppClaims>);

        // Standard extractors infer types from the spec
        #[oxapi(get, "/items/{id}")]
        async fn get_item(state: State<S>, path: Path<_>);

        // Path and body together (PUT has both)
        #[oxapi(put, "/items/{id}")]
        async fn update_item(state: State<S>, path: Path<_>, body: Json<_>);

        // Query extractor gets inferred type from spec
        #[oxapi(get, "/items/search")]
        async fn search_items(state: State<S>, query: Query<_>);
    }
}

/// Separate module to test explicit parameter attributes with custom extractors.
/// Note: This module uses only Path and Query extractors because body extractors
/// (like Json) require FromRequest implementation which is more complex.
#[oxapi::oxapi(axum, "tests/additional_extractors.yaml")]
pub mod api_with_attrs {
    use axum::extract::{Json, State};

    /// Items API demonstrating explicit parameter attributes.
    ///
    /// Use `#[oxapi(path)]`, `#[oxapi(query)]`, and `#[oxapi(body)]` to explicitly
    /// mark the role of custom extractors that we can't detect by name.
    pub trait ItemsApiWithAttrs<S: StateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        // Custom extractors with explicit role attributes
        #[oxapi(get, "/items/{id}")]
        async fn get_item(state: State<S>, #[oxapi(path)] path: MyPath<_>);

        // When ANY explicit role attr is present, ALL params need explicit attrs
        #[oxapi(put, "/items/{id}")]
        async fn update_item(
            state: State<S>,
            #[oxapi(path)] path: MyPath<_>,
            #[oxapi(body)] body: Json<_>,
        );

        #[oxapi(get, "/items/search")]
        async fn search_items(state: State<S>, #[oxapi(query)] query: MyQuery<_>);

        // Remaining operations to satisfy coverage
        #[oxapi(post, "/items")]
        async fn create_item(state: State<S>, body: Json<_>);

        #[oxapi(get, "/items")]
        async fn list_items(state: State<S>);
    }
}

/// Handler implementation using auth extractors.
pub struct ItemsHandler;

impl<S: StateProvider> api::ItemsApi<S> for ItemsHandler {
    async fn create_item(
        axum::extract::State(_state): axum::extract::State<S>,
        _claims: Jwt<AppClaims>,
        axum::Json(_body): axum::Json<api::types::CreateItemRequest>,
    ) -> Result<api::types::CreateItemResponse, api::types::CreateItemError> {
        todo!()
    }

    async fn list_items(
        axum::extract::State(_state): axum::extract::State<S>,
        _claims: Jwt<AppClaims>,
    ) -> Result<api::types::ListItemsResponse, api::types::ListItemsError> {
        todo!()
    }

    async fn get_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_id): axum::extract::Path<String>,
    ) -> Result<api::types::GetItemResponse, api::types::GetItemError> {
        todo!()
    }

    async fn update_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_id): axum::extract::Path<String>,
        axum::Json(_body): axum::Json<api::types::CreateItemRequest>,
    ) -> Result<api::types::UpdateItemResponse, api::types::UpdateItemError> {
        todo!()
    }

    async fn search_items(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Query(_query): axum::extract::Query<api::types::SearchItemsQuery>,
    ) -> api::types::SearchItemsResponse {
        todo!()
    }
}

/// Handler implementation using explicit parameter attributes with custom extractors.
pub struct ItemsHandlerWithAttrs;

impl<S: StateProvider> api_with_attrs::ItemsApiWithAttrs<S> for ItemsHandlerWithAttrs {
    async fn get_item(
        axum::extract::State(_state): axum::extract::State<S>,
        // The trait uses MyPath<_> which got inferred to MyPath<String>
        MyPath(_id): MyPath<String>,
    ) -> Result<api_with_attrs::types::GetItemResponse, api_with_attrs::types::GetItemError> {
        todo!()
    }

    async fn update_item(
        axum::extract::State(_state): axum::extract::State<S>,
        MyPath(_id): MyPath<String>,
        axum::Json(_body): axum::Json<api_with_attrs::types::CreateItemRequest>,
    ) -> Result<api_with_attrs::types::UpdateItemResponse, api_with_attrs::types::UpdateItemError>
    {
        todo!()
    }

    async fn search_items(
        axum::extract::State(_state): axum::extract::State<S>,
        MyQuery(_query): MyQuery<api_with_attrs::types::SearchItemsQuery>,
    ) -> api_with_attrs::types::SearchItemsResponse {
        todo!()
    }

    async fn create_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::Json(_body): axum::Json<api_with_attrs::types::CreateItemRequest>,
    ) -> Result<api_with_attrs::types::CreateItemResponse, api_with_attrs::types::CreateItemError>
    {
        todo!()
    }

    async fn list_items(
        axum::extract::State(_state): axum::extract::State<S>,
    ) -> Result<api_with_attrs::types::ListItemsResponse, api_with_attrs::types::ListItemsError>
    {
        todo!()
    }
}

#[test]
fn additional_extractors_compile() {
    // This test verifies that:
    // 1. User-defined extractors can be added to trait method signatures
    // 2. Standard extractors (Path, Query, Json) have their types inferred
    // 3. Custom extractors with explicit `#[oxapi(path/query/body)]` attributes work
}
