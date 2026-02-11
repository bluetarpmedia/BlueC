// Copyright 2025-2026 Neil Henderson
//
//! The `recursive_descent` module defines the hand-written, top-down recursive descent parser.
//! We use a recursive descent parser except when parsing binary operations, where we use precedence climbing instead.

#[cfg(feature = "hex-float-literal")]
pub(super) mod hex_float;

pub(super) mod block;
pub(crate) mod decl;
pub(super) mod decl_fn;
pub(super) mod decl_var;
pub(super) mod declarator;
pub(super) mod literal;
pub(super) mod peek;
pub(super) mod stmt;
pub(super) mod utils;

use crate::compiler_driver::Driver;

use super::abstract_syntax_tree::*;
use super::expr;
use super::{ParseError, ParseResult, Parser};
use super::{add_error, add_error_at_eof};

/// Parses a translation unit and returns the AST.
pub fn parse_translation_unit(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstRoot> {
    let mut file_scope_decls = Vec::new();

    // At file scope we can have variable or function declarations.
    while !parser.token_stream.is_eof() {
        // Keep parsing other declarations if this one fails to parse
        if let Ok(decl) = decl::parse_declaration(parser, driver) {
            file_scope_decls.extend(decl);
        } else {
            utils::move_to_next_file_scope_declaration(parser);
        }
    }

    Ok(AstRoot(file_scope_decls))
}
