// Copyright 2025-2026 Neil Henderson
//
//! The `source_ident` module defines the [SourceIdentifier] type.

use super::SourceLocation;

/// An identifier string and its source code location.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SourceIdentifier<'a>(pub &'a str, pub SourceLocation);
