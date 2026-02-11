// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_declared_type` module defines [AstDeclaredType] and its related types.

use std::fmt;

use super::{AstBasicType, AstDeclarator, AstIdentifier, AstStorageClassSpecifier, AstType};

/// An `AstDeclaredType` represents a parsed type declaration which is not yet resolved to its canonical `AstType`.
///
/// The declared type consists of a basic type, optional storage class specifier, and optional declarator.
///
/// ```c
///  static unsigned long int *ptr = 0;
///  ~~~~~~ ~~~~~~~~~~~~~~~~~ ~~~~
///
/// // 'static':            the storage class specifier
/// // 'unsigned long int': the basic type
/// // '*ptr':              the declarator
/// ```
///
/// The semantic analysis typechecking stage resolves a declared type to a canonical `AstType`.
#[derive(Debug, Clone, PartialEq)]
pub struct AstDeclaredType {
    /// The basic type of the declaration, from which the optional declarator's derived type is resolved.
    pub basic_type: AstBasicType,

    /// Optional storage class specifier.
    pub storage_class: Option<AstStorageClassSpecifier>,

    /// The optional declarator which augments the basic type (e.g. a pointer). The declarator may be ommitted
    /// in certain contexts, e.g.
    ///
    /// ```c
    /// int calc(int, float, double);   // Declare a function `calc` with parameter types but no names
    /// (float)                         // A cast expression to `float` type.
    /// ```
    pub declarator: Option<AstDeclarator>,

    /// The resolved, canonical `AstType`. The semantic analysis stage fills this out.
    pub resolved_type: Option<AstType>,
}

impl fmt::Display for AstDeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.basic_type)?;
        if let Some(declarator) = &self.declarator {
            write!(f, " {declarator}")?;
        }
        Ok(())
    }
}

impl AstDeclaredType {
    /// Creates a new, unresolved `AstDeclaredType`.
    pub fn unresolved(
        basic_type: AstBasicType,
        storage_class: Option<AstStorageClassSpecifier>,
        declarator: Option<AstDeclarator>,
    ) -> Self {
        AstDeclaredType { basic_type, storage_class, declarator, resolved_type: None }
    }

    /// Creates a new, resolved `AstDeclaredType`.
    pub fn resolved(ty: &AstType) -> Self {
        AstDeclaredType {
            basic_type: AstBasicType::default(),
            storage_class: None,
            declarator: None,
            resolved_type: Some(ty.clone()),
        }
    }

    /// Has the declared type been resolved?
    pub fn is_resolved(&self) -> bool {
        self.resolved_type.is_some()
    }

    /// If the declared type has a declarator, and if that declarator has an identifier, returns that identifier.
    /// Otherwise returns `None`.
    pub fn get_identifier(&self) -> Option<&AstIdentifier> {
        if let Some(declarator) = &self.declarator { declarator.get_identifier() } else { None }
    }
}
