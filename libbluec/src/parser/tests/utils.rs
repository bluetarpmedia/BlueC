// Copyright 2025 Neil Henderson, Blue Tarp Media.

use std::io::BufReader;
use std::io::Cursor;

use crate::compiler_driver;
use crate::lexer;
use crate::parser;
use crate::parser::{AstDeclaration, AstVariableDeclaration};

/// Creates a `Parser` for the given source code.
pub fn make_parser(driver: &mut compiler_driver::Driver, source_code: &str) -> parser::Parser {
    let cursor = Cursor::new(source_code.as_bytes());
    let mut reader = BufReader::new(cursor);
    let tokens = lexer::lex_buf_reader(driver, &mut reader);
    parser::Parser::new(tokens)
}

/// Returns the `AstVariableDeclaration` for the given declaration, or asserts and returns `None`.
pub fn expect_var_decl(decl: &AstDeclaration) -> Option<&AstVariableDeclaration> {
    if let AstDeclaration::Variable(var_decl) = decl {
        Some(var_decl)
    } else {
        assert!(false, "Declaration is not a variable");
        None
    }
}
