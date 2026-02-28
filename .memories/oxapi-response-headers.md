---
title: Response Headers Support in oxapi
description: How response headers are parsed and generate typed header structs with struct variants
tags:
  project: oxapi
  topic: feature-implementation
created: 2026-02-26T23:01:28.799936457-08:00
modified: 2026-02-26T23:01:28.799936457-08:00
---

# Response Headers Support in oxapi

## Summary

Implemented parsing and code generation for OpenAPI response headers. When a response defines headers, the enum variant becomes a struct variant with `.headers` and `.body` fields.

## How It Works

1. **openapi.rs**: `ResponseHeader` struct + `RefResolvable` for `openapiv3::Header` + `headers: Vec<ResponseHeader>` on `OperationResponse`. `parse_response()` resolves header refs and extracts schema.

2. **responses.rs**: `VariantInfo` gets `headers` field. Variants with headers become struct variants (`Status200 { headers: ..., body: ... }`). Header structs named `{EnumName}{VariantName}Headers` with `#[derive(Debug, Default)]`. `IntoResponse` impl inserts headers via `response.headers_mut().insert(...)`.

3. **types.rs**: Inline header schemas registered with typify during init.

## Key Details

- Header structs don't derive Serialize/Deserialize (no serde attrs needed)
- `HeaderName::from_static` with lowercase header name string literal
- `HeaderValue::from_str(&value.to_string()).unwrap()` for all types
- Optional headers wrapped in `if let Some(ref v) = headers.field { ... }`
- Header struct naming follows rename system: if enum renamed to `PetResponse` and variant to `Success`, struct is `PetResponseSuccessHeaders`

## Impact on Existing Code

- Petstore example's `LoginUserResponse::Status200` changed from tuple to struct variant (has X-Rate-Limit and X-Expires-After headers in spec)
