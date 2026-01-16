// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `string_literal` module defines lexing functions for string literals.

use super::char_literal::evaluate_chars;
use super::line_lexer::LineLexer;
use super::{SourceLocation, Token, TokenType};

use crate::ICE;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::utils::string;

/// Makes a token for a string literal.
pub fn make_string_literal(line_lexer: &mut LineLexer) -> Result<Token, ()> {
    // Expect the first char to be the opening double quote.
    let Some((idx, first)) = line_lexer.cursor().next() else {
        ICE!("Expected character");
    };
    line_lexer.set_column_no(idx + 1);

    let start_col = line_lexer.column_no();

    if first != '"' {
        ICE!("Expected double quote");
    }

    let (literal, is_closed) = take_remaining_literal_chars(line_lexer);

    // Is the literal unclosed?
    if !is_closed {
        let loc = SourceLocation::new(line_lexer.line_no(), start_col, 1);
        let err = "Missing closing single double quote (\") for string literal".to_string();
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    // Translate and validate any hex and octal escape sequences
    let char_count = literal.len() - 2; // Minus the quotes
    let (values, _) = evaluate_chars(literal.chars().skip(1).take(char_count), start_col, line_lexer)?;

    // Translate the integer values into strings or escape sequence strings.
    let ascii = values.into_iter().map(string::to_ascii).collect();

    let token_len = literal.len();
    let token_type = TokenType::StringLiteral { literal, ascii };
    let location = SourceLocation::new(line_lexer.line_no(), start_col, token_len);

    Ok(Token { token_type, location })
}

/// Consumes all of the string literal up to and including the closing double quote.
fn take_remaining_literal_chars(line_lexer: &mut LineLexer) -> (String, bool) {
    let mut literal = String::from("\""); // Includes the opening double quote.

    let mut is_closed = false;
    let mut prev_is_escape = false;

    while let Some((idx, ch)) = line_lexer.cursor().next() {
        literal.push(ch);
        line_lexer.set_column_no(idx + 1);

        match ch {
            '\\' => prev_is_escape = !prev_is_escape,

            '"' => {
                if prev_is_escape {
                    prev_is_escape = false;
                } else {
                    is_closed = true;
                    break;
                }
            }

            _ => prev_is_escape = false,
        }
    }

    (literal, is_closed)
}
