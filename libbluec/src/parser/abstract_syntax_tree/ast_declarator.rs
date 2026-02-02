// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_declarator` module defines the `AstDeclarator` type which represents parsed declarators in declarations.
//!
//! A declarator is the part of a declaration that specifies the name and attributes of a variable or function.
//! An abstract declarator omits the name.
//!
//! ```c
//! int foo;
//!     ~~~
//! int foo[4];
//!     ~~~~~~
//! int *foo;
//!     ~~~~
//! int foo(void);
//!     ~~~~~~~~~
//! int *;               // Abstract pointer declarator
//!     ~
//! int (void);          // Abstract function declarator
//!     ~~~~~~
//! ```

use std::fmt;

use crate::core::SourceLocation;

use super::super::{AstDeclaredType, AstFullExpression, AstIdentifier};

/// A parsed declarator.
///
/// Declarators are parsed inside-out due to recursive descent parsing. Call [AstDeclarator::get_derived_kind] to
/// get the derived kind, which is probably what you want when interpreting the declarator.
///
/// ```c
/// // Source                // AstDeclarator layout              // AstDeclarator::get_derived_kind
/// int ** get_ptr(void);    Pointer{Pointer{Function{Ident}}}    Function
/// int (*fn)(void);         Function{Pointer{Ident}}             Pointer
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct AstDeclarator {
    /// The kind of declarator. You probably want [AstDeclarator::get_derived_kind] instead.
    pub kind: AstDeclaratorKind,

    // The source location of the declarator.
    pub loc: SourceLocation,
}

impl fmt::Display for AstDeclarator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            AstDeclaratorKind::Ident(ident) => write!(f, "{ident}"),
            AstDeclaratorKind::Pointer(decl) => write!(f, "*{decl}"),
            AstDeclaratorKind::Array { decl, .. } => write!(f, "{decl}[]"),
            AstDeclaratorKind::Function { decl, params } => {
                write!(f, "{decl}(")?;
                fmt_params(params, f)?;
                write!(f, ")")
            }
            AstDeclaratorKind::AbstractPointer => write!(f, "*"),
            AstDeclaratorKind::AbstractArray { .. } => write!(f, "[]"),
            AstDeclaratorKind::AbstractFunction { params } => {
                write!(f, "()(")?;
                fmt_params(params, f)?;
                write!(f, ")")
            }
        }
    }
}

impl AstDeclarator {
    /// Makes a new `AstDeclarator`
    pub fn new(kind: AstDeclaratorKind, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }

    /// Gets the derived kind of the declarator. You probably want this instead of [AstDeclarator::kind].
    pub fn get_derived_kind(&self) -> &AstDeclaratorKind {
        let this_kind = &self.kind;

        let next_kind = match this_kind {
            AstDeclaratorKind::Ident(_) => this_kind,
            AstDeclaratorKind::Array { decl, .. } => decl.get_derived_kind(),
            AstDeclaratorKind::Pointer(decl) => decl.get_derived_kind(),
            AstDeclaratorKind::Function { decl, .. } => decl.get_derived_kind(),
            AstDeclaratorKind::AbstractPointer => this_kind,
            AstDeclaratorKind::AbstractArray { .. } => this_kind,
            AstDeclaratorKind::AbstractFunction { .. } => this_kind,
        };

        if matches!(next_kind, AstDeclaratorKind::Ident(_)) {
            return this_kind;
        }

        next_kind
    }

    /// Gets the identifier if the declarator has one, or returns `None`.
    pub fn get_identifier(&self) -> Option<&AstIdentifier> {
        match &self.kind {
            AstDeclaratorKind::Ident(ident) => Some(ident),
            AstDeclaratorKind::Pointer(decl) => decl.get_identifier(),
            AstDeclaratorKind::Array { decl, .. } => decl.get_identifier(),
            AstDeclaratorKind::Function { decl, .. } => decl.get_identifier(),
            AstDeclaratorKind::AbstractPointer => None,
            AstDeclaratorKind::AbstractArray { .. } => None,
            AstDeclaratorKind::AbstractFunction { .. } => None,
        }
    }

    /// Is this declarator a function declarator?
    pub fn is_function(&self) -> bool {
        matches!(&self.kind, AstDeclaratorKind::Function { .. } | AstDeclaratorKind::AbstractFunction { .. })
    }

    /// Is this declarator an abstract array? (Array with no identifier like '[]' or '[3]').
    pub fn is_abstract_array(&self) -> bool {
        matches!(&self.kind, AstDeclaratorKind::AbstractArray { .. })
    }
}

/// The kind of declarator.
///
/// A declarator is an 'abstract declarator' if it has no identifier.
///
/// ```c
/// // Declarator        // Abstract declarator
/// int x                int
/// int *p               int *
/// int arr[10]          int [10]
/// int (*ptr)[5]        int (*)[5]
/// int func(void)       int (void)
/// int (*fnptr)(void)   int (*)(void)
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum AstDeclaratorKind {
    Ident(AstIdentifier),
    Pointer(Box<AstDeclarator>),
    Array { decl: Box<AstDeclarator>, size_expr: Option<Box<AstFullExpression>> },
    Function { decl: Box<AstDeclarator>, params: Vec<AstDeclaredType> },
    AbstractPointer,
    AbstractArray { size_expr: Option<Box<AstFullExpression>> },
    AbstractFunction { params: Vec<AstDeclaredType> },
}

/// Writes the given parameters to the given formatter `f`.
fn fmt_params(params: &[AstDeclaredType], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first = true;
    for p in params {
        if !first {
            write!(f, ", ")?;
        }

        write!(f, "{}", p.basic_type)?;

        if let Some(declarator) = &p.declarator {
            write!(f, " {}", declarator)?;
        }

        first = false;
    }

    Ok(())
}
