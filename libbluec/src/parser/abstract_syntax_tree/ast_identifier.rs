// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_identifier` module defines the `AstIdentifier` type.

use crate::lexer::SourceLocation;
use crate::compiler_driver::diagnostics::SourceIdentifier;

use std::fmt;

/// An identifier is a user-defined name for variables, functions, enums, etc.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstIdentifier {
    pub name: String,
    pub loc: SourceLocation,
}

impl AstIdentifier {
    /// Creates an identifier.
    pub fn new<S: Into<String>>(identifier: S, loc: SourceLocation) -> Self {
        Self { name: identifier.into(), loc }
    }
}

impl fmt::Display for AstIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<'a> From<&'a AstIdentifier> for SourceIdentifier<'a> {
    fn from(id: &'a AstIdentifier) -> SourceIdentifier<'a> {
        SourceIdentifier(&id.name, id.loc)
    }
}
