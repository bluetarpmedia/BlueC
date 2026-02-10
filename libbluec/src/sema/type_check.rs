// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `type_check` module defines the main part of the semantic analysis stage, which is type checking.
//! This involves the following tasks:
//! - type resolution: Resolve all declared types in the AST to their canonical `AstType` representations.
//! - type annotation: Annotate every expression in the AST with its `AstType`, and add implicit type casts
//!   where necessary.
//! - type validation: Emit diagnostics for semantic errors where usage of types are invalid.
//! - build symbols:   Build a Symbol Table of all symbols in the AST and their types.

pub(super) mod utils;

mod checker;
mod symbols;
mod traverse;

pub use checker::TypeChecker;

use crate::compiler_driver::Driver;
use crate::parser::AstRoot;

use super::type_resolution;

/// Performs type checking on the AST.
pub fn type_check(ast_root: &mut AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    traverse::typecheck_ast(ast_root, chk, driver);
}
