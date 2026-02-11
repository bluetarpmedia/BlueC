// Copyright 2025-2026 Neil Henderson
//
//! The `lexer` module handles the lexical analysis of the source code and then passes the results (a vector of tokens)
//! to the parser.

use std::fs::File;
use std::io::{BufRead, BufReader};

mod char_literal;
mod line_lexer;
mod numeric_literals;
mod string_literal;
mod tokens;

#[cfg(test)]
mod tests;

pub use crate::lexer::tokens::{FloatLiteralSuffix, IntegerLiteralSuffix, NumericLiteralBase, Token, TokenType};

use crate::compiler_driver::{Diagnostic, Driver, DriverError};
use crate::core::FilePosition;
use crate::parser;

/// Performs lexical analysis of the source translation unit (preprocessed file) and generates a stream of
/// tokens, and then passes ownership of the token stream to the parser stage.
pub fn lex(driver: &mut Driver) -> Result<(), DriverError> {
    let file = match File::open(driver.tu_file.file_path()) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Cannot open '{}' for lexing: {e}", &driver.tu_file.file_path());
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
pub fn lex_buf_reader<T>(driver: &mut Driver, reader: &mut BufReader<T>) -> Vec<Token>
where
    T: std::io::Read,
{
    let mut tokens = Vec::new();
    let mut line = String::new();
    let mut file_pos = FilePosition::default();

    // Lex the lines one at a time
    loop {
        match reader.read_line(&mut line) {
            Ok(0) => break, // EOF

            Ok(bytes_read) => {
                debug_assert!(bytes_read <= u32::MAX as usize);

                // Keep track of where each new line begins.
                driver.tu_file.add_line_start_pos(file_pos);

                if line.starts_with("#") {
                    _ = parse_line_marker(driver, file_pos, &line);
                } else {
                    tokens.extend(lex_one_line(driver, file_pos, &line));
                }

                line.clear();
                file_pos += bytes_read as u32;
            }

            Err(err) => {
                driver.add_diagnostic(Diagnostic::error(format!("Error reading source file: {err}")));
                break;
            }
        }
    }

    tokens
}

fn parse_line_marker(driver: &mut Driver, file_pos: FilePosition, line: &str) -> Result<(), ()> {
    let parts: Vec<&str> = line.split_whitespace().collect();

    if parts.len() < 3 {
        return Err(());
    }

    let line_no = parts[1].parse::<u32>().map_err(|_| ())?;
    let file_name = parts[2].trim_matches('"');

    let mut flags = Vec::new();
    for flag_str in &parts[3..] {
        if let Ok(flag) = flag_str.parse::<u32>() {
            flags.push(flag);
        }
    }

    let line_marker_pos = file_pos + line.len();

    driver.tu_file.add_line_marker(line_marker_pos, file_name, line_no);
    Ok(())
}

fn lex_one_line(driver: &mut Driver, file_pos: FilePosition, line: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut line_lexer = line_lexer::LineLexer::new(driver, file_pos, line);

    loop {
        match line_lexer.get_next_token() {
            Ok(Some(token)) => tokens.push(token),
            Ok(None) => break, // No more tokens in this line
            _ => (),           // Move to next token if any error is diagnosed
        }
    }

    tokens
}
