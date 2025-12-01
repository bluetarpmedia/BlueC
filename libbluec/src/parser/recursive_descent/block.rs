// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `block` module defines the parsing functionality for blocks.

use super::decl;
use super::peek;
use super::stmt;
use super::utils;
use super::{AstBlock, AstBlockItem};
use super::{ParseResult, Parser};

use crate::compiler_driver::Driver;
use crate::lexer;

/// Parses a block in a new scope.
pub fn parse_block(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstBlock> {
    parser.with_new_scope(|p| parse_block_in_current_scope(p, driver))
}

/// Parses a block without creating a new scope.
pub fn parse_block_in_current_scope(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstBlock> {
    _ = utils::expect_token(lexer::TokenType::OpenBrace, parser, driver)?;

    let mut block_items = Vec::new();

    while let Some(peek_next_token) = parser.token_stream.peek_next_token() {
        if peek_next_token.has_type(lexer::TokenType::CloseBrace) {
            break;
        }

        if peek::is_declaration(parser, driver) {
            let decl = decl::parse_declaration(parser, driver);

            if let Ok(decl) = decl {
                let decls = decl.into_iter().map(AstBlockItem::Declaration).collect::<Vec<AstBlockItem>>();
                block_items.extend(decls);
            } else {
                parser.token_stream.move_to_next_statement();
            }
        } else {
            let stmt = stmt::parse_statement(parser, driver);

            if let Ok(stmt) = stmt {
                block_items.push(AstBlockItem::Statement(stmt));
            } else {
                parser.token_stream.move_to_next_statement();
            }
        }
    }

    _ = utils::expect_token(lexer::TokenType::CloseBrace, parser, driver)?;

    Ok(AstBlock(block_items))
}
