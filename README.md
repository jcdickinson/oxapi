# oxapi

Generate type-safe Rust server stubs from OpenAPI specs.

> **Note**: This crate was initially vibe-coded because it's an uninteresting problem. Use at your own risk. Further
> updates will likely be by hand.

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
oxapi = { git = "..." }
axum = "0.8"
tokio = { version = "1", features = ["full"] }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

## Example

Given an OpenAPI spec, define a trait with your handlers:

```rust
use axum::{Router, extract::{State, Path, Json}};

#[oxapi::oxapi(axum, "api.json")]
trait MyApi {
    #[oxapi(map)]
    fn map_routes(router: Router<AppState>) -> Router<AppState>;

    #[oxapi(get, "/users/{id}")]
    async fn get_user(state: State<AppState>, id: Path<_>);

    #[oxapi(post, "/users")]
    async fn create_user(state: State<AppState>, body: Json<_>);
}
```

The macro generates:
- A `my_api_types` module with all types from the spec
- Response enums (`GetUserOk`, `GetUserErr`) that implement `IntoResponse`
- Filled-in type parameters for `Path<_>`, `Query<_>`, `Json<_>`
- Return types as `Result<{Op}Ok, {Op}Err>`

Implement the trait:

```rust
use my_api_types::*;

struct MyApiImpl;

impl MyApi for MyApiImpl {
    async fn get_user(
        State(state): State<AppState>,
        Path(id): Path<i64>,
    ) -> Result<GetUserOk, GetUserErr> {
        match state.users.get(&id) {
            Some(user) => Ok(GetUserOk::Status200(user.clone())),
            None => Err(GetUserErr::Status404),
        }
    }

    async fn create_user(
        State(state): State<AppState>,
        Json(user): Json<User>,
    ) -> Result<CreateUserOk, CreateUserErr> {
        // ...
    }
}

#[tokio::main]
async fn main() {
    let state = AppState::new();
    let app = MyApiImpl::map_routes(Router::new()).with_state(state);
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}
```

## Splitting by Responsibility

For larger APIs, use a module to split operations across multiple traits:

```rust
#[oxapi::oxapi(axum, "api.json")]
mod api {
    trait UserService {
        #[oxapi(map)]
        fn map_routes(router: Router<UserState>) -> Router<UserState>;

        #[oxapi(get, "/users/{id}")]
        async fn get_user(state: State<UserState>, id: Path<_>);
    }

    trait OrderService {
        #[oxapi(map)]
        fn map_routes(router: Router<OrderState>) -> Router<OrderState>;

        #[oxapi(get, "/orders/{id}")]
        async fn get_order(state: State<OrderState>, id: Path<_>);
    }
}

use api::{types::*, UserService, OrderService};
```

Each trait can have its own state type. The macro validates that all spec operations are covered exactly once across all traits.

Compose the routers:

```rust
let app = Router::new()
    .merge(UserServiceImpl::map_routes(Router::new()).with_state(user_state))
    .merge(OrderServiceImpl::map_routes(Router::new()).with_state(order_state));
```

## Attributes

- `#[oxapi(map)]` - Marks the route mapping function (body auto-generated)
- `#[oxapi(get, "/path")]` - GET handler
- `#[oxapi(post, "/path")]` - POST handler
- `#[oxapi(put, "/path")]` - PUT handler
- `#[oxapi(delete, "/path")]` - DELETE handler
- `#[oxapi(patch, "/path")]` - PATCH handler

## Type Elision

Use `_` for types the macro should fill from the spec:

```rust
async fn get_user(id: Path<_>, body: Json<_>);
//                      ^ i64        ^ User (from spec)
```

## For AI Agents

See [llms.txt](./llms.txt) for comprehensive documentation optimized for LLMs.

## License

LGPL-3.0
