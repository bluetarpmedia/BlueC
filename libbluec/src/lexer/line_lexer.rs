// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The line_lexer module provides single-line lexing functionality to the parent lexer module.

use crate::compiler_driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::internal_error;
use crate::lexer::numeric_literals::make_numeric_literal;
use crate::lexer::{SourceLocation, Token, TokenType};

use std::iter::Peekable;
use std::str::CharIndices;

/// The `LineLexer` performs lexical analysis on a single line of the source code.
pub struct LineLexer<'a, 'b> {
    driver: &'a mut compiler_driver::Driver,
    line_no: usize, // 1-based line number
    col_no: usize,  // 1-based column number
    _source_line: &'b str,
    cursor: Peekable<CharIndices<'b>>,
}

impl<'a, 'b> LineLexer<'a, 'b> {
    /// Creates a new `LineLexer` to lex the given source line.
    pub fn new(driver: &'a mut compiler_driver::Driver, line_no: usize, source_line: &'b str) -> Self {
        let cursor = source_line.char_indices().peekable();

        Self { driver, line_no, col_no: 1, _source_line: source_line, cursor }
    }

    /// The compiler driver.
    pub fn driver(&mut self) -> &mut compiler_driver::Driver {
        self.driver
    }

    /// The cursor over the source line.
    pub fn cursor(&mut self) -> &mut Peekable<CharIndices<'b>> {
        &mut self.cursor
    }

    /// The current 1-based line number.
    pub fn line_no(&self) -> usize {
        self.line_no
    }

    /// The current 1-based column number.
    pub fn column_no(&self) -> usize {
        self.col_no
    }

    /// Sets the current 1-based column number to the new value.
    pub fn set_column_no(&mut self, col_no: usize) {
        self.col_no = col_no;
    }

    /// Peeks at the next 2 chars.
    pub fn peek_2_chars(&mut self) -> (Option<char>, Option<char>) {
        match self.cursor.peek() {
            Some((_, first_ch)) => {
                let first_ch = *first_ch;

                // Clone the iterator state, advance the clone once, then peek the second char
                let mut clone = self.cursor.clone();
                clone.next();

                (Some(first_ch), clone.peek().map(|(_, ch)| *ch))
            }
            None => (None, None),
        }
    }

    /// Gets the next token from the source line.
    ///
    /// Returns `Ok(None)` if there are no more tokens.
    /// Returns `Err` if a lexing error occurs (a diagnostic is recorded on the compiler driver).
    #[rustfmt::skip]
    pub fn get_next_token(&mut self) -> Result<Option<Token>, ()> {
        self.consume_whitespace();

        match self.peek_2_chars() {
            (Some('.'), Some(digit)) if digit.is_ascii_digit() => make_numeric_literal(self).map(Some),
            (Some(digit), _) if digit.is_ascii_digit() => make_numeric_literal(self).map(Some),

            (Some(ch), _) => match ch {
                'a'..='z' | 'A'..='Z' | '_' => Ok(Some(self.make_identifier())),

                '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '!' | '=' | '<' | '>' => {
                    Ok(Some(self.maximal_munch_up_to_3_chars()))
                }

                _ => {
                    let (idx, ch) = self.cursor.next().unwrap(); // Safe to unwrap since we peeked ahead above
                    self.col_no = idx + 1;
                    let token_len = 1;

                    match ch {
                        '(' => Ok(Some(self.make_token(TokenType::OpenParen,      token_len))),
                        ')' => Ok(Some(self.make_token(TokenType::CloseParen,     token_len))),
                        '{' => Ok(Some(self.make_token(TokenType::OpenBrace,      token_len))),
                        '}' => Ok(Some(self.make_token(TokenType::CloseBrace,     token_len))),
                        '[' => Ok(Some(self.make_token(TokenType::OpenSqBracket,  token_len))),
                        ']' => Ok(Some(self.make_token(TokenType::CloseSqBracket, token_len))),
                        ';' => Ok(Some(self.make_token(TokenType::Semicolon,      token_len))),
                        ':' => Ok(Some(self.make_token(TokenType::Colon,          token_len))),
                        ',' => Ok(Some(self.make_token(TokenType::Comma,          token_len))),
                        '~' => Ok(Some(self.make_token(TokenType::BitwiseNot,     token_len))),
                        '?' => Ok(Some(self.make_token(TokenType::Ternary,        token_len))),

                        _ => {
                            let loc = SourceLocation::new(self.line_no, self.col_no, 1);
                            let diag = Diagnostic::error_at_location(format!("Unexpected character '{0}'", ch), loc);
                            self.driver.add_diagnostic(diag);
                            Err(())
                        }
                    }
                }
            }

            _ => Ok(None),
        }
    }

    /// Creates a token of the given type, column location, and character length.
    pub fn make_token_at_column(&self, token_type: TokenType, column: usize, length: usize) -> Token {
        Token { token_type, location: SourceLocation { line: self.line_no, column, length } }
    }

    /// Creates an identifier token (which may be a keyword).
    ///     The lexer does not distinguish between identifiers like variable names and keywords (like "int").
    fn make_identifier(&mut self) -> Token {
        let mut identifier = String::new();
        let mut start_col: Option<usize> = None;

        while let Some((idx, ch)) = self.cursor.next_if(|(_, ch)| ch.is_ascii_alphanumeric() || *ch == '_') {
            identifier.push(ch);
            self.col_no = idx + 1;

            if start_col.is_none() {
                start_col = Some(self.col_no);
            }
        }

        let token_length = identifier.len();
        let token_type = TokenType::Identifier(identifier);

        self.make_token_at_column(token_type, start_col.unwrap(), token_length)
    }

    /// Looks ahead at up to 3 characters create the most appropriate token.
    #[rustfmt::skip]
    fn maximal_munch_up_to_3_chars(&mut self) -> Token {
        let Some((idx, first)) = self.cursor.next() else {
            internal_error::ICE("Expected character");
        };
        self.col_no = idx + 1;

        let peek_second = self.cursor.peek().map(|char_idx| char_idx.1);

        let mut token_len = 1;

        let mut advance = |lexer: &mut LineLexer| {
            _ = lexer.cursor.next();
            token_len += 1;
        };

        match (first, peek_second) {

            ('=', Some('=')) => { advance(self); self.make_token(TokenType::EqualTo,                token_len) }
            ('=', _)         => {                self.make_token(TokenType::Assignment,             token_len) }

            ('!', Some('=')) => { advance(self); self.make_token(TokenType::NotEqualTo,             token_len) }
            ('!', _)         => {                self.make_token(TokenType::LogicalNot,             token_len) }

            ('+', Some('+')) => { advance(self); self.make_token(TokenType::Increment,              token_len) }
            ('+', Some('=')) => { advance(self); self.make_token(TokenType::AdditionAssignment,     token_len) }
            ('+', _)         => {                self.make_token(TokenType::Plus,                   token_len) }

            ('-', Some('-')) => { advance(self); self.make_token(TokenType::Decrement,              token_len) }
            ('-', Some('=')) => { advance(self); self.make_token(TokenType::SubtractionAssignment,  token_len) }
            ('-', _)         => {                self.make_token(TokenType::Minus,                  token_len) }

            ('*', Some('=')) => { advance(self); self.make_token(TokenType::MultiplyAssignment,     token_len) }
            ('*', _)         => {                self.make_token(TokenType::Multiply,               token_len) }

            ('/', Some('=')) => { advance(self); self.make_token(TokenType::DivideAssignment,       token_len) }
            ('/', _)         => {                self.make_token(TokenType::Divide,                 token_len) }

            ('%', Some('=')) => { advance(self); self.make_token(TokenType::RemainderAssignment,    token_len) }
            ('%', _)         => {                self.make_token(TokenType::Remainder,              token_len) }

            ('&', Some('&')) => { advance(self); self.make_token(TokenType::LogicalAnd,             token_len) }
            ('&', Some('=')) => { advance(self); self.make_token(TokenType::BitwiseAndAssignment,   token_len) }
            ('&', _)         => {                self.make_token(TokenType::BitwiseAnd,             token_len) }

            ('|', Some('|')) => { advance(self); self.make_token(TokenType::LogicalOr,              token_len) }
            ('|', Some('=')) => { advance(self); self.make_token(TokenType::BitwiseOrAssignment,    token_len) }
            ('|', _)         => {                self.make_token(TokenType::BitwiseOr,              token_len) }

            ('^', Some('=')) => { advance(self); self.make_token(TokenType::BitwiseXorAssignment,   token_len) }
            ('^', _)         => {                self.make_token(TokenType::BitwiseXor,             token_len) }

            // To distinguish between << and <<= we need to peek at a third char
            ('<', Some('<')) => {
                advance(self);
                let peek_third = self.cursor.peek().map(|char_idx| char_idx.1);
                match peek_third {
                    Some('=') => { advance(self); self.make_token(TokenType::LeftShiftAssignment,   token_len) }
                    _ =>         {                self.make_token(TokenType::LeftShift,             token_len) }
                }
            }

            ('<', Some('=')) => { advance(self); self.make_token(TokenType::LessThanOrEqualTo,      token_len) }
            ('<', _)         => {                self.make_token(TokenType::LessThan,               token_len) }

            // To distinguish between >> and >>= we need to peek at a third char
            ('>', Some('>')) => {
                advance(self);
                let peek_third = self.cursor.peek().map(|char_idx| char_idx.1);
                match peek_third {
                    Some('=') => { advance(self); self.make_token(TokenType::RightShiftAssignment,  token_len) }
                    _ =>         {                self.make_token(TokenType::RightShift,            token_len) }
                }
            }

            ('>', Some('=')) => { advance(self); self.make_token(TokenType::GreaterThanOrEqualTo,   token_len) }
            ('>', _)         => {                self.make_token(TokenType::GreaterThan,            token_len) }

            _ => internal_error::ICE("No token match for characters")
        }
    }

    /// Creates a token of the given type and character length.
    fn make_token(&self, token_type: TokenType, length: usize) -> Token {
        Token { token_type, location: SourceLocation { line: self.line_no, column: self.col_no, length } }
    }

    /// Consume whitespace. Next char will be non-whitespace.
    fn consume_whitespace(&mut self) {
        while let Some((idx, _)) = self.cursor.next_if(|(_, ch)| ch.is_ascii_whitespace()) {
            self.col_no = idx + 1;
        }
    }
}
