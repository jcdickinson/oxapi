//! Integration test: Module mode with $ref request bodies
//!
//! This test verifies that request body types referenced via `$ref` are properly
//! qualified with the `types::` module path when using module mode.
//!
//! The test passes if this file compiles successfully. Previously, the generated
//! code would reference types like `CreateItemRequest` without qualification,
//! causing "cannot find type" errors since the types are defined in the `types`
//! submodule.

mod common;

use axum::Router;
use common::StateProvider;

#[oxapi::oxapi(axum, "tests/module_mode_request_body.yaml")]
pub mod api {
    pub trait ItemsApi<S: StateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        #[oxapi(post, "/items")]
        async fn create_item(state: axum::extract::State<S>, body: axum::Json<_>);

        #[oxapi(get, "/items")]
        async fn list_items(state: axum::extract::State<S>);

        #[oxapi(get, "/items/{id}")]
        async fn get_item(state: axum::extract::State<S>, id: axum::extract::Path<_>);

        #[oxapi(patch, "/items/{id}")]
        async fn update_item(
            state: axum::extract::State<S>,
            id: axum::extract::Path<_>,
            body: axum::Json<_>,
        );
    }
}

#[test]
fn module_mode_request_body_types_compile() {
    // This test verifies compilation success only.
    // If the `$ref` request body types are not properly qualified with `types::`,
    // this test will fail to compile with errors like:
    //   - "cannot find type `CreateItemRequest` in this scope"
    //   - "cannot find type `UpdateItemRequest` in this scope"
}
