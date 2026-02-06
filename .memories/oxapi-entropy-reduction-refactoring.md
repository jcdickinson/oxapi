---
title: Entropy Reduction Refactoring in oxapi
description: Summary of code quality improvements to reduce duplication in types.rs, openapi.rs, and responses.rs
tags:
  project: oxapi
  topic: refactoring
created: 2026-02-06T00:13:48.960634133-08:00
modified: 2026-02-06T00:13:48.960634133-08:00
---

# Entropy Reduction Refactoring in oxapi

## Summary

Implemented a 5-phase refactoring to reduce code duplication and improve readability.

## Changes Made

### Phase 1: Extract `resolve_reference` helper (types.rs)
- Added `TypeGenerator::resolve_reference(&self, reference: &str) -> TokenStream`
- Eliminated 3 duplicated 7-line blocks in:
  - `type_for_boxed_schema`
  - `type_for_schema_with_definitions`
  - `generate_inline_struct`

### Phase 2: Add `raw_name()` to Operation (openapi.rs)
- Added `Operation::raw_name(&self) -> &str` for raw operation name
- Refactored `Operation::name()` to use `raw_name().to_upper_camel_case()`
- Updated types.rs:113 to use `op.raw_name()`

### Phase 3: Add `GeneratedType::simple()` constructor (types.rs)
- Added `GeneratedType::simple(type_ref: TokenStream) -> Self`
- Reduces verbose `GeneratedType { type_ref, definitions: vec![] }` patterns

### Phase 4: Simplify match arms (types.rs)
- Simplified `type_for_inline_schema` match arms using `GeneratedType::simple()`
- Reduced ~20 lines of repetitive code

### Phase 5: Extract `collect_variants` (responses.rs)
- Added `CollectedVariants` struct to hold variants, errors, and inline definitions
- Extracted `ResponseGenerator::collect_variants()` method
- Separated collection logic from generation logic in `generate_response_enum`

## Files Modified
- `crates/oxapi-impl/src/types.rs`
- `crates/oxapi-impl/src/openapi.rs`
- `crates/oxapi-impl/src/responses.rs`

## Verification
- `cargo check` passes
- `cargo test` passes (all tests pass)
- `cargo build -p petstore-example` succeeds
