// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_attributes` module defines various AST attribute types.

use std::fmt;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::ICE;
use crate::core::SourceLocation;

/// A unique numerical identifier for a node in the AST. Identifiers start from 1.
#[derive(Debug, Default, Copy, Clone, Hash, Eq, PartialEq)]
pub struct AstNodeId(u32);

impl fmt::Display for AstNodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AstNodeId {
    /// Creates a new, unique `AstNodeId`.
    pub fn new() -> Self {
        static NEXT_ID: AtomicU32 = AtomicU32::new(1);

        let next_id = NEXT_ID.fetch_add(1, Ordering::SeqCst); // Increments and returns previous value, so `1` is first.

        if next_id == u32::MAX {
            ICE!("Exhausted node ids"); // Technically we have 1 more available but we'll limit ourselves to MAX-1.
        }

        Self(next_id)
    }

    /// Creates an `AstNodeId` with the given value. This is used by some unit tests.
    #[cfg(test)]
    pub fn with_id(value: u32) -> Self {
        Self(value)
    }

    /// Creates a null sentinel value for an `AstNodeId`.
    ///
    /// It's invalid for a node in the AST to have this value, except when temporarily breaking the invariant.
    ///
    /// This function is used when moving a node out from the AST by temporarily replacing it with something else.
    /// That temporary replacement node has a null value, until it is replaced again or overwritten.
    pub fn null() -> Self {
        Self(0)
    }
}

/// The storage duration of an identifier determines its lifetime.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstStorageDuration {
    None, // E.g. for type aliases and functions
    Automatic,
    Static, // Aka global
}

/// The linkage of an identifier determines the scope that it can be referenced from.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstLinkage {
    None,
    Internal,
    External,
}

impl fmt::Display for AstLinkage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstLinkage::None => write!(f, "no linkage"),
            AstLinkage::Internal => write!(f, "internal linkage"),
            AstLinkage::External => write!(f, "external linkage"),
        }
    }
}

impl AstLinkage {
    /// Does the identifier have linkage?
    ///
    /// In other words, is the linkage `Internal` or `External` (but not `None`).
    /// An identifier with linkage must be defined at file scope, but an identifier can be declared with external
    /// linkage at any scope.
    pub fn has_linkage(&self) -> bool {
        matches!(self, AstLinkage::Internal | AstLinkage::External)
    }
}

/// Storage class specifiers determine an identifier's scope, storage lifetime, and linkage.
///
/// `typedef` is also categorized as a storage class specifier for syntactic convenience in the C grammar, but
/// does not affect storage in the way that other class specifiers do.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AstStorageClassSpecifierKind {
    // Future: Auto,
    // Future: Register,
    // Future: constexpr,
    // Future: thread_local
    Static,
    Extern,
    Typedef,
}

/// A storage class specifier in a declaration.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AstStorageClassSpecifier {
    pub kind: AstStorageClassSpecifierKind,
    pub loc: SourceLocation,
}

impl AstStorageClassSpecifier {
    /// Is this storage class specifier 'static'?
    pub fn is_static(&self) -> bool {
        self.kind == AstStorageClassSpecifierKind::Static
    }

    /// Is this storage class specifier 'extern'?
    pub fn is_extern(&self) -> bool {
        self.kind == AstStorageClassSpecifierKind::Extern
    }

    /// Is this storage class specifier 'typedef'?
    pub fn is_typedef(&self) -> bool {
        self.kind == AstStorageClassSpecifierKind::Typedef
    }
}

pub trait AstStorageClassSpecifierOption {
    fn is_static(&self) -> bool;
    fn is_extern(&self) -> bool;
    fn is_typedef(&self) -> bool;
}

impl AstStorageClassSpecifierOption for Option<AstStorageClassSpecifier> {
    /// Is this storage class specifier 'static'?
    fn is_static(&self) -> bool {
        self.as_ref().is_some_and(|st| st.is_static())
    }

    /// Is this storage class specifier 'extern'?
    fn is_extern(&self) -> bool {
        self.as_ref().is_some_and(|st| st.is_extern())
    }

    /// Is this storage class specifier 'typedef'?
    fn is_typedef(&self) -> bool {
        self.as_ref().is_some_and(|st| st.is_typedef())
    }
}
