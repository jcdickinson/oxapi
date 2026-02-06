//! Shared test helpers for oxapi integration tests.

/// Trait for types that can provide state dependencies.
///
/// This is used in test traits to constrain the generic state type parameter.
pub trait StateProvider: Clone + Send + Sync + 'static {}
