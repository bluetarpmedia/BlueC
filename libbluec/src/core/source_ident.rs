// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `source_ident` module defines the [SourceIdentifier] type.

use super::SourceLocation;

/// An identifier string and its source code location.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SourceIdentifier<'a>(pub &'a str, pub SourceLocation);
