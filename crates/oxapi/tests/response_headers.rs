//! Integration test: Response headers support
//!
//! Verifies that OpenAPI response headers are parsed and generate:
//! - Per-variant header structs with typed fields
//! - Struct variants with `.headers` and `.body` fields
//! - Unchanged unit/tuple variants for responses without headers

mod common;

use axum::Router;
use common::StateProvider;

#[oxapi::oxapi(axum, "tests/response_headers.yaml")]
pub mod api {
    pub trait PetApi<S: StateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        #[oxapi(get, "/pets/{petId}")]
        async fn get_pet(state: axum::extract::State<S>, pet_id: axum::extract::Path<_>);
    }
}

use api::types::*;

#[test]
fn struct_variant_with_headers_and_body() {
    // 200 has headers + body → struct variant
    let _resp = GetPetResponse::Status200 {
        headers: GetPetResponseStatus200Headers {
            x_rate_limit: 100,
            x_request_id: Some("abc-123".to_string()),
        },
        body: Pet {
            id: 1,
            name: "Fido".to_string(),
        },
    };
}

#[test]
fn unit_variant_without_headers() {
    // 404 has no headers and no body → unit variant (on the error enum)
    let _resp = GetPetError::Status404;
}

#[test]
fn struct_variant_with_headers_no_body() {
    // 429 has headers but no body → struct variant with only headers
    let _resp = GetPetError::Status429 {
        headers: GetPetErrorStatus429Headers { retry_after: 30 },
    };
}

#[test]
fn header_struct_default() {
    // Header structs implement Default
    let headers = GetPetResponseStatus200Headers::default();
    assert_eq!(headers.x_rate_limit, 0);
    assert!(headers.x_request_id.is_none());
}

#[test]
fn into_response_compiles() {
    use axum::response::IntoResponse;

    // Verify IntoResponse works for struct variants with headers
    let resp = GetPetResponse::Status200 {
        headers: GetPetResponseStatus200Headers {
            x_rate_limit: 42,
            x_request_id: Some("req-1".to_string()),
        },
        body: Pet {
            id: 1,
            name: "Rex".to_string(),
        },
    };
    let response = resp.into_response();
    assert_eq!(response.status(), 200);
    assert_eq!(
        response
            .headers()
            .get("x-rate-limit")
            .unwrap()
            .to_str()
            .unwrap(),
        "42"
    );
    assert_eq!(
        response
            .headers()
            .get("x-request-id")
            .unwrap()
            .to_str()
            .unwrap(),
        "req-1"
    );

    // Verify unit variant still works (404 is on the error enum)
    let resp = GetPetError::Status404;
    let response = resp.into_response();
    assert_eq!(response.status(), 404);

    // Verify headers-only struct variant
    let resp = GetPetError::Status429 {
        headers: GetPetErrorStatus429Headers { retry_after: 60 },
    };
    let response = resp.into_response();
    assert_eq!(response.status(), 429);
    assert_eq!(
        response
            .headers()
            .get("retry-after")
            .unwrap()
            .to_str()
            .unwrap(),
        "60"
    );
}
