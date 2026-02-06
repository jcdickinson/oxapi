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

        // Typed path and query params (uuid, i64, i32, bool, f32)
        #[oxapi(get, "/typed/{user_id}/items/{item_id}")]
        async fn get_typed_item(state: State<S>, path: Path<_>, query: Query<_>);
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

        #[oxapi(get, "/typed/{user_id}/items/{item_id}")]
        async fn get_typed_item(
            state: State<S>,
            #[oxapi(path)] path: MyPath<_>,
            #[oxapi(query)] query: MyQuery<_>,
        );
    }
}

/// Module to test query unknown field feature.
/// Use `#[oxapi(query, field_name)]` to add a HashMap field for unknown query params.
#[oxapi::oxapi(axum, "tests/additional_extractors.yaml")]
pub mod api_with_unknown_query_field {
    use axum::extract::{Json, Query, State};

    pub trait ItemsApiUnknownField<S: StateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        #[oxapi(post, "/items")]
        async fn create_item(state: State<S>, body: Json<_>);

        #[oxapi(get, "/items")]
        async fn list_items(state: State<S>);

        #[oxapi(get, "/items/{id}")]
        async fn get_item(state: State<S>, path: axum::extract::Path<_>);

        #[oxapi(put, "/items/{id}")]
        async fn update_item(state: State<S>, path: axum::extract::Path<_>, body: Json<_>);

        // Use query with unknown field to capture extra query params
        #[oxapi(get, "/items/search")]
        async fn search_items(state: State<S>, #[oxapi(query, extras)] query: Query<_>);

        #[oxapi(get, "/typed/{user_id}/items/{item_id}")]
        async fn get_typed_item(state: State<S>, path: axum::extract::Path<_>, query: Query<_>);
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
        // Path now uses the generated GetItemPath struct instead of raw String
        axum::extract::Path(_path): axum::extract::Path<api::types::GetItemPath>,
    ) -> Result<api::types::GetItemResponse, api::types::GetItemError> {
        todo!()
    }

    async fn update_item(
        axum::extract::State(_state): axum::extract::State<S>,
        // Path now uses the generated UpdateItemPath struct
        axum::extract::Path(_path): axum::extract::Path<api::types::UpdateItemPath>,
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

    async fn get_typed_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_path): axum::extract::Path<api::types::GetTypedItemPath>,
        axum::extract::Query(_query): axum::extract::Query<api::types::GetTypedItemQuery>,
    ) -> Result<api::types::GetTypedItemResponse, api::types::GetTypedItemError> {
        todo!()
    }
}

/// Handler implementation using explicit parameter attributes with custom extractors.
pub struct ItemsHandlerWithAttrs;

impl<S: StateProvider> api_with_attrs::ItemsApiWithAttrs<S> for ItemsHandlerWithAttrs {
    async fn get_item(
        axum::extract::State(_state): axum::extract::State<S>,
        // The trait uses MyPath<_> which got inferred to MyPath<GetItemPath>
        MyPath(_path): MyPath<api_with_attrs::types::GetItemPath>,
    ) -> Result<api_with_attrs::types::GetItemResponse, api_with_attrs::types::GetItemError> {
        todo!()
    }

    async fn update_item(
        axum::extract::State(_state): axum::extract::State<S>,
        MyPath(_path): MyPath<api_with_attrs::types::UpdateItemPath>,
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

    async fn get_typed_item(
        axum::extract::State(_state): axum::extract::State<S>,
        MyPath(_path): MyPath<api_with_attrs::types::GetTypedItemPath>,
        MyQuery(_query): MyQuery<api_with_attrs::types::GetTypedItemQuery>,
    ) -> Result<api_with_attrs::types::GetTypedItemResponse, api_with_attrs::types::GetTypedItemError>
    {
        todo!()
    }
}

/// Handler implementation for unknown query field test.
pub struct ItemsHandlerUnknownField;

impl<S: StateProvider> api_with_unknown_query_field::ItemsApiUnknownField<S>
    for ItemsHandlerUnknownField
{
    async fn create_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::Json(_body): axum::Json<api_with_unknown_query_field::types::CreateItemRequest>,
    ) -> Result<
        api_with_unknown_query_field::types::CreateItemResponse,
        api_with_unknown_query_field::types::CreateItemError,
    > {
        todo!()
    }

    async fn list_items(
        axum::extract::State(_state): axum::extract::State<S>,
    ) -> Result<
        api_with_unknown_query_field::types::ListItemsResponse,
        api_with_unknown_query_field::types::ListItemsError,
    > {
        todo!()
    }

    async fn get_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_path): axum::extract::Path<
            api_with_unknown_query_field::types::GetItemPath,
        >,
    ) -> Result<
        api_with_unknown_query_field::types::GetItemResponse,
        api_with_unknown_query_field::types::GetItemError,
    > {
        todo!()
    }

    async fn update_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_path): axum::extract::Path<
            api_with_unknown_query_field::types::UpdateItemPath,
        >,
        axum::Json(_body): axum::Json<api_with_unknown_query_field::types::CreateItemRequest>,
    ) -> Result<
        api_with_unknown_query_field::types::UpdateItemResponse,
        api_with_unknown_query_field::types::UpdateItemError,
    > {
        todo!()
    }

    async fn search_items(
        axum::extract::State(_state): axum::extract::State<S>,
        // The query struct now has an `extras` field for unknown query params
        axum::extract::Query(query): axum::extract::Query<
            api_with_unknown_query_field::types::SearchItemsQuery,
        >,
    ) -> api_with_unknown_query_field::types::SearchItemsResponse {
        // Verify that the extras field exists
        let _extras: &std::collections::HashMap<String, String> = &query.extras;
        todo!()
    }

    async fn get_typed_item(
        axum::extract::State(_state): axum::extract::State<S>,
        axum::extract::Path(_path): axum::extract::Path<
            api_with_unknown_query_field::types::GetTypedItemPath,
        >,
        axum::extract::Query(_query): axum::extract::Query<
            api_with_unknown_query_field::types::GetTypedItemQuery,
        >,
    ) -> Result<
        api_with_unknown_query_field::types::GetTypedItemResponse,
        api_with_unknown_query_field::types::GetTypedItemError,
    > {
        todo!()
    }
}

#[test]
fn additional_extractors_compile() {
    // This test verifies that:
    // 1. User-defined extractors can be added to trait method signatures
    // 2. Standard extractors (Path, Query, Json) have their types inferred
    // 3. Custom extractors with explicit `#[oxapi(path/query/body)]` attributes work
    // 4. Path parameters now use generated struct types (e.g., GetItemPath)
    // 5. Query with unknown field (#[oxapi(query, extras)]) adds an extras HashMap field
}

/// Test that path and query structs use correct types for various formats
#[test]
fn path_and_query_struct_types() {
    // Verify GetItemPath has uuid::Uuid type for id field
    let get_item_path = api::types::GetItemPath {
        id: uuid::Uuid::nil(),
    };
    let _: uuid::Uuid = get_item_path.id;

    // Verify UpdateItemPath has uuid::Uuid type for id field
    let update_item_path = api::types::UpdateItemPath {
        id: uuid::Uuid::nil(),
    };
    let _: uuid::Uuid = update_item_path.id;

    // Verify GetTypedItemPath has correct types:
    // - user_id: uuid::Uuid (format: uuid)
    // - item_id: i64 (format: int64)
    let typed_path = api::types::GetTypedItemPath {
        user_id: uuid::Uuid::nil(),
        item_id: 42i64,
    };
    let _: uuid::Uuid = typed_path.user_id;
    let _: i64 = typed_path.item_id;

    // Verify GetTypedItemQuery has correct types:
    // - version: Option<i32> (format: int32)
    // - active: Option<bool>
    // - score: Option<f32> (format: float)
    let typed_query = api::types::GetTypedItemQuery {
        version: Some(1i32),
        active: Some(true),
        score: Some(3.14f32),
    };
    let _: Option<i32> = typed_query.version;
    let _: Option<bool> = typed_query.active;
    let _: Option<f32> = typed_query.score;

    // Verify SearchItemsQuery has correct types:
    // - q: String (required)
    // - limit: Option<i64> (integer without format defaults to i64)
    let search_query = api::types::SearchItemsQuery {
        q: "test".to_string(),
        limit: Some(10i64),
    };
    let _: String = search_query.q;
    let _: Option<i64> = search_query.limit;
}
