// Copyright 2025-2026 Neil Henderson
//
//! The `core` module provides foundational types and functions used by the various compiler stages.

pub mod string;

mod file_position;
mod internal_error;
mod source_ident;
mod source_location;
mod symbol_kind;
mod temp_file;

pub use file_position::FilePosition;
pub use internal_error::ICE;
pub use source_ident::SourceIdentifier;
pub use source_location::SourceLocation;
pub use symbol_kind::SymbolKind;
pub use temp_file::TempFile;
