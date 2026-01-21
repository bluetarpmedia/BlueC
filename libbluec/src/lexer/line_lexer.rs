// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `line_lexer` module provides single-line lexing functionality to the parent lexer module.

use std::iter::Peekable;
use std::str::CharIndices;

use crate::ICE;
use crate::compiler_driver::{Diagnostic, Driver};
use crate::core::{FilePosition, SourceLocation};

use super::char_literal::make_char_literal;
use super::numeric_literals::make_numeric_literal;
use super::string_literal::make_string_literal;
use super::{Token, TokenType};

/// The `LineLexer` performs lexical analysis on a single line of the source code.
pub struct LineLexer<'a, 'b> {
    driver: &'a mut Driver,
    line_start_pos: FilePosition,
    cursor_pos: FilePosition,
    _source_line: &'b str,
    cursor: Peekable<CharIndices<'b>>,
}

impl<'a, 'b> LineLexer<'a, 'b> {
    /// Creates a new `LineLexer` to lex the given source line.
    pub fn new(driver: &'a mut Driver, file_pos: FilePosition, source_line: &'b str) -> Self {
        let cursor = source_line.char_indices().peekable();

        Self { driver, line_start_pos: file_pos, cursor_pos: file_pos, _source_line: source_line, cursor }
    }

    /// The compiler driver.
    pub fn driver(&mut self) -> &mut Driver {
        self.driver
    }

    /// The cursor over the source line.
    pub fn cursor(&mut self) -> &mut Peekable<CharIndices<'b>> {
        &mut self.cursor
    }

    /// The file position where the cursor is currently pointing to.
    pub fn cursor_pos(&self) -> FilePosition {
        self.cursor_pos
    }

    /// Sets the current file position to the new value.
    pub fn set_cursor_index(&mut self, index: usize) {
        self.cursor_pos = self.line_start_pos + index;
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

                '\'' => make_char_literal(self).map(Some),

                '"' => make_string_literal(self).map(Some),

                _ => {
                    let (idx, ch) = self.cursor.next().unwrap(); // Safe to unwrap since we peeked ahead above
                    self.set_cursor_index(idx);
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
                            let loc = SourceLocation::new(self.cursor_pos(), 1);
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
    pub fn make_token_at_location(&self, token_type: TokenType, file_pos: FilePosition, length: usize) -> Token {
        Token { token_type, location: SourceLocation::new(file_pos, length) }
    }

    /// Creates an identifier token (which may be a keyword).
    ///     The lexer does not distinguish between identifiers like variable names and keywords (like "int").
    fn make_identifier(&mut self) -> Token {
        let mut identifier = String::new();
        let mut start_pos: Option<FilePosition> = None;

        while let Some((idx, ch)) = self.cursor.next_if(|(_, ch)| ch.is_ascii_alphanumeric() || *ch == '_') {
            identifier.push(ch);
            self.set_cursor_index(idx);

            if start_pos.is_none() {
                start_pos = Some(self.cursor_pos());
            }
        }

        if let Some(start_pos) = start_pos {
            let token_length = identifier.len();
            let token_type = TokenType::Identifier(identifier);

            self.make_token_at_location(token_type, start_pos, token_length)
        } else {
            ICE!("Did not create identifier token");
        }
    }

    /// Looks ahead at up to 3 characters create the most appropriate token.
    #[rustfmt::skip]
    fn maximal_munch_up_to_3_chars(&mut self) -> Token {
        let Some((idx, first)) = self.cursor.next() else {
            ICE!("Expected character");
        };
        self.set_cursor_index(idx);

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

            _ => ICE!("No token match for character '{first}'")
        }
    }

    /// Creates a token of the given type and character length.
    fn make_token(&self, token_type: TokenType, length: usize) -> Token {
        Token { token_type, location: SourceLocation::new(self.cursor_pos(), length) }
    }

    /// Consume whitespace. Next char will be non-whitespace.
    fn consume_whitespace(&mut self) {
        while let Some((idx, _)) = self.cursor.next_if(|(_, ch)| ch.is_ascii_whitespace()) {
            self.set_cursor_index(idx);
        }
    }
}
