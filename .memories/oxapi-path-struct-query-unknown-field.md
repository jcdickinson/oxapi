---
title: Path Struct Generation + Query Unknown Field Support
description: Implementation details for oxapi path struct generation and query unknown field feature
tags:
  project: oxapi
  topic: feature-implementation
created: 2026-02-05T23:17:26.166909321-08:00
modified: 2026-02-05T23:17:26.166909321-08:00
---

# Path Struct Generation + Query Unknown Field Support

## Summary

Implemented two features for oxapi:

1. **Path parameters as struct**: Generate a deserializable struct `{OperationId}Path` instead of tuples
2. **Query unknown field**: Support `#[oxapi(query, field_name)]` to generate an extra HashMap field for unknown params

## Part 1: Path Parameters as Struct

### Changes Made

1. **lib.rs**: Added `GeneratedTypeKind::Path` variant to the enum

2. **types.rs**: Added `generate_path_struct()` method:
   - Filters params by `ParamLocation::Path`
   - Sorts by position in path string for serde deserialization order
   - Generates struct `{OperationId}Path` with `serde::Deserialize`
   - Uses `String` as fallback type when schema is missing
   - Adds `#[serde(rename = "...")]` when field name differs from param name

3. **lib.rs Generator**: Added `generate_path_structs()` method

4. **method.rs**: Added `generate_path_inner_type()` method:
   - Checks for `TypeOverride::Replace` → returns replacement
   - Checks for `TypeOverride::Rename` → uses renamed struct
   - Otherwise uses default `{OperationId}Path`
   - Returns `()` if no path params

5. **responses.rs**: Updated all match statements to handle `GeneratedTypeKind::Path`

6. **oxapi-macro/lib.rs**: Added `"path"` case to `parse_type_kind`

## Part 2: Query Unknown Field Support

### Changes Made

1. **ParamOxapiAttr** (macro): Extended to capture optional field name:
   ```rust
   enum ParamOxapiAttr {
       Path,
       Query { unknown_field: Option<Ident> },
       Body,
   }
   ```

2. **ParamRole** (method.rs): Extended similarly:
   ```rust
   pub enum ParamRole {
       Path,
       Query { unknown_field: Option<proc_macro2::Ident> },
       Body,
       Other,
   }
   ```

3. **TypeOverrides** (lib.rs): Added storage for query unknown fields:
   ```rust
   query_unknown_fields: HashMap<QueryUnknownFieldKey, proc_macro2::Ident>,
   ```
   With methods `set_query_unknown_field()` and `get_query_unknown_field()`

4. **generate_query_struct** (types.rs): Updated to accept and use unknown_field:
   - Generates extra `#[serde(flatten)] pub <name>: HashMap<String, String>` field

5. **Macro do_oxapi**: Pre-collects query unknown fields from traits before creating Generator

## Generated Code Examples

### Path Struct

For `GET /items/{itemId}/details/{detailId}`:

```rust
#[derive(Debug, Clone, serde::Deserialize)]
pub struct GetItemDetailsPath {
    #[serde(rename = "itemId")]
    pub item_id: String,
    #[serde(rename = "detailId")]
    pub detail_id: i64,
}
```

### Query Struct with Unknown Field

For `#[oxapi(query, extras)] query: Query<_>`:

```rust
#[derive(Debug, Clone, serde::Deserialize)]
pub struct SearchQuery {
    pub q: String,
    pub limit: Option<i32>,
    #[serde(flatten)]
    pub extras: ::std::collections::HashMap<String, String>,
}
```

## Files Modified

- `crates/oxapi-impl/src/lib.rs`
- `crates/oxapi-impl/src/types.rs`
- `crates/oxapi-impl/src/method.rs`
- `crates/oxapi-impl/src/responses.rs`
- `crates/oxapi-macro/src/lib.rs`
- `crates/oxapi/tests/additional_extractors.rs`
- `examples/petstore/src/main.rs`
