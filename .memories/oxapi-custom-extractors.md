---
title: Custom Extractors in oxapi Trait Definitions
description: How custom extractors work in oxapi trait method signatures and the bug fix for preserving use statements
tags:
  bug-fix: true
  project: oxapi
  topic: extractors
created: 2026-02-05T20:36:23.240561559-08:00
modified: 2026-02-05T20:39:55.57488631-08:00
---

# Custom Extractors in oxapi

## How It Works

When defining trait methods with `#[oxapi(...)]` attributes:

1. **Type elision extractors** (e.g., `Path<_>`, `Json<_>`, `Query<_>`) have their inner types inferred from the OpenAPI spec
2. **Explicit type extractors** (e.g., `Jwt<AppClaims>`, `State<S>`) are passed through unchanged

## Bug Fix: Preserving User Imports

The macro was collecting user's `use` statements but not including them in the output.

**Fix in `lib.rs:1087`**: Added `#(#other_items)*` to preserve user imports:
```rust
let inner = quote! {
    #(#other_items)*  // <-- Added this line

    pub mod #types_mod_name { ... }
    #(#generated_traits)*
};
```

## Usage Example

```rust
#[oxapi::oxapi(axum, "api.yaml")]
pub mod api {
    use super::*;
    use axum::extract::State;
    use crate::auth::Jwt;

    pub trait ItemsApi<S: StateProvider> {
        #[oxapi(post, "/items")]
        async fn create_item(
            state: State<S>,       // Works because import is preserved
            claims: Jwt<AppClaims>, // Custom extractor - passed through
            body: Json<_>,          // Type elision - inferred from spec
        );
    }
}
```

## Key Files
- `crates/oxapi-macro/src/lib.rs:1087` - Output generation (preserves other_items)
- `crates/oxapi-impl/src/method.rs:208-211` - Unknown types pass through unchanged
- `crates/oxapi/tests/additional_extractors.rs` - Test demonstrating custom extractors