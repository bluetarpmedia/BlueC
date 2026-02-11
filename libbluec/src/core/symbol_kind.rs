// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `symbol_kind` module defines the [SymbolKind] enum.

use std::fmt;

/// Kinds of symbols
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolKind {
    Variable,
    Function,
    TypeAlias,
    Constant,
}

impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolKind::Variable => write!(f, "Variable"),
            SymbolKind::Function => write!(f, "Function"),
            SymbolKind::TypeAlias => write!(f, "Type alias"),
            SymbolKind::Constant => write!(f, "Constant"),
        }
    }
}
