//! OxAPI - Generate server stubs from OpenAPI specifications.
//!
//! This crate provides a procedural macro for generating server trait stubs
//! from OpenAPI specifications.
//!
//! # Example
//!
//! ```ignore
//! use oxapi::oxapi;
//!
//! #[oxapi(axum, "api.json")]
//! trait MyServer {
//!     #[oxapi(map)]
//!     fn map_routes(router: Router) -> Router;
//!
//!     #[oxapi(get, "/users")]
//!     async fn get_users(state: State<AppState>, query: Query<_>);
//!
//!     #[oxapi(post, "/users/{id}")]
//!     fn create_user(state: State<AppState>, id: Path<_>, body: Json<_>);
//! }
//! ```

pub use oxapi_macro::oxapi;
