// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `lexer` module handles the lexical analysis of the source code and then
//! passes the results (a vector of tokens) to the parser.

mod line_lexer;
mod numeric_literals;
mod source_location;
mod tokens;

#[cfg(test)]
mod tests;

use crate::compiler_driver;
use crate::compiler_driver::DriverError;
use crate::compiler_driver::diagnostics::Diagnostic;
pub use crate::lexer::source_location::SourceLocation;
pub use crate::lexer::tokens::{IntegerLiteralSuffix, FloatLiteralSuffix, NumericLiteralBase, Token, TokenType};
use crate::parser;

use std::fs::File;
use std::io::{BufRead, BufReader};

/// Performs lexical analysis of the source translation unit (preprocessed file) and generates a stream of
/// tokens, and then passes ownership of the token stream to the parser stage.
pub fn lex(driver: &mut compiler_driver::Driver) -> Result<(), DriverError> {
    let file = match File::open(&driver.translation_unit_filename) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Cannot open '{}' for lexing: {e}", &driver.translation_unit_filename);
            return Err(DriverError::LexerFailed);
        }
    };

    let mut reader = BufReader::new(file);

    let tokens = lex_buf_reader(driver, &mut reader);

    // Don't proceed to the next stage if we've emitted errors, or if the user only wants to run the lexer.
    if driver.has_error_diagnostics() || driver.options().lex {
        return Ok(());
    }

    parser::parse(driver, tokens);

    Ok(())
}

/// Performs lexical analysis on the given `BufReader<T>`.
///
/// Pass a `BufReader<File>` to lex a file, or pass a `BufReader<Cursor>` to lex an in-memory buffer.
pub fn lex_buf_reader<T>(driver: &mut compiler_driver::Driver, reader: &mut BufReader<T>) -> Vec<Token>
where
    T: std::io::Read,
{
    let mut tokens = Vec::new();
    let mut line = String::new();
    let mut line_no = 1; // Line and column numbers are 1-based

    // Lex the lines one at a time
    loop {
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF

            Ok(_) => {
                tokens.extend(lex_one_line(driver, line_no, &line));

                line.clear();
                line_no += 1;
            }

            Err(err) => {
                driver.add_diagnostic(Diagnostic::error(format!("Error reading source file: {err}")));
                break;
            }
        }
    }

    tokens
}

fn lex_one_line(driver: &mut compiler_driver::Driver, line_no: usize, line: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut line_lexer = line_lexer::LineLexer::new(driver, line_no, line);

    loop {
        match line_lexer.get_next_token() {
            Ok(Some(token)) => tokens.push(token),
            Ok(None) => break, // No more tokens in this line
            _ => (),           // Move to next token if any error is diagnosed
        }
    }

    tokens
}
