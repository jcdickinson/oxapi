---
title: Custom Extractors and Parameter Role Attributes in oxapi
description: 'How custom extractors and #[oxapi(path/query/body)] parameter attributes work in oxapi trait method signatures'
tags:
  project: oxapi
  topic: extractors
  topic-2: documentation
created: 2026-02-05T20:36:23.240561559-08:00
modified: 2026-02-05T22:00:12.541022881-08:00
---

# Custom Extractors and Parameter Role Attributes in oxapi

## Parameter Role Attributes

Use `#[oxapi(path)]`, `#[oxapi(query)]`, or `#[oxapi(body)]` on method parameters to explicitly specify their role.

### All-or-Nothing Inference

**Important**: When ANY parameter has an explicit role attribute, type name inference is disabled for ALL parameters. You must either:
- Use no explicit attrs (rely entirely on type name detection), OR
- Use explicit attrs on ALL parameters that need type elision

```rust
// GOOD: No explicit attrs - all types detected by name
#[oxapi(put, "/items/{id}")]
async fn update(state: State<S>, path: Path<_>, body: Json<_>);

// GOOD: Explicit attrs on all params with type elision
#[oxapi(put, "/items/{id}")]
async fn update(
    state: State<S>,
    #[oxapi(path)] path: MyPath<_>,
    #[oxapi(body)] body: Json<_>,
);

// BAD: Mixed - Json<_> won't be inferred because path has explicit attr
#[oxapi(put, "/items/{id}")]
async fn update(
    state: State<S>,
    #[oxapi(path)] path: MyPath<_>,
    body: Json<_>,  // ERROR: `_` not allowed without inference
);
```

### Use Cases

- **Custom extractors**: When extractor name isn't `Path`, `Query`, or `Json`
- **Non-standard requests**: E.g., adding a body to a GET request

```rust
#[oxapi(get, "/search")]
async fn search(
    state: State<S>,
    #[oxapi(body)] body: Json<_>,  // Force body role on a GET request
);
```

## Custom Extractors (Pass-Through)

Extractors with explicit types (no `_` elision) are passed through unchanged:

```rust
#[oxapi(post, "/items")]
async fn create_item(
    state: State<S>,
    claims: Jwt<AppClaims>,    // Custom extractor - passed through unchanged
    body: Json<_>,              // Type elision - inferred from spec
);
```

## Implementation

### Role Detection Priority (when no explicit attrs)
1. Type name detection (`Path`, `Query`, `Json`)
2. `ParamRole::Other` - passed through unchanged

### When explicit attrs present
- Only explicitly marked params get roles
- All other params get `ParamRole::Other`

### Key Files
- `crates/oxapi-macro/src/lib.rs` - Documentation and attr parsing
- `crates/oxapi-impl/src/method.rs:140-158` - Inference disable logic
- `crates/oxapi/tests/additional_extractors.rs` - Test demonstrating all features