// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `token_stream` module provides a stateful [TokenStream] which allows the parent parser module to consume and
//! peek at tokens in the stream.

use crate::core::SourceLocation;
use crate::lexer;

/// A token stream is a list of tokens produced by the lexer which can be iterated over.
pub struct TokenStream {
    tokens: Vec<lexer::Token>,
    cursor: usize,
}

/// A copyable snapshot of the TokenStream's state, which can be used to restore it.
#[derive(Debug, Copy, Clone)]
pub struct TokenStreamSnapshot {
    cursor: usize,
}

impl TokenStream {
    /// Creates a new token stream.
    pub fn new(tokens: Vec<lexer::Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    /// Returns the next token in the stream and advances, or None if at the end of the stream.
    pub fn take_token(&mut self) -> Option<&lexer::Token> {
        if self.cursor >= self.tokens.len() {
            return None;
        }
        let token = Some(&self.tokens[self.cursor]);
        self.cursor += 1;
        token
    }

    /// Peeks at the next token in the stream and, if it has the expected type, returns the token
    /// and advances.
    /// Returns None if at the end of the stream.
    pub fn take_token_if_expected(&mut self, expected_type: lexer::TokenType) -> Option<&lexer::Token> {
        let peek_next_token = self.peek_next_token()?;

        if peek_next_token.token_type == expected_type { self.take_token() } else { None }
    }

    /// Peeks at the next token in the stream without advancing past it.
    pub fn peek_next_token(&self) -> Option<&lexer::Token> {
        if self.cursor >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[self.cursor])
    }

    /// Peeks at the next token in the stream and returns whether its type matches the given type.
    ///
    /// Returns false if the stream has no more tokens.
    pub fn next_token_has_type(&self, token_type: lexer::TokenType) -> bool {
        matches!(self.peek_next_token(), Some(token) if token.token_type == token_type)
    }

    /// Peeks at the next 2 tokens in the stream without advancing.
    pub fn peek_next_2_tokens(&self) -> (Option<&lexer::Token>, Option<&lexer::Token>) {
        let first = self.peek_next_token();
        let second = if self.tokens.len() - self.cursor >= 2 { Some(&self.tokens[self.cursor + 1]) } else { None };
        (first, second)
    }

    /// The previous token in the stream, or None.
    ///
    /// If you call `take_token` first followed by `prev_token` then you receive the same token.
    pub fn prev_token(&self) -> Option<&lexer::Token> {
        if self.cursor == 0 {
            return None;
        }

        if self.cursor >= self.tokens.len() {
            Some(&self.tokens[self.tokens.len() - 1])
        } else {
            Some(&self.tokens[self.cursor - 1])
        }
    }

    /// The last token in the stream, or None.
    ///
    /// You can always retrieve the last token (if the stream is not empty), even if `is_eof` == true.
    pub fn last_token(&self) -> Option<&lexer::Token> {
        self.tokens.last()
    }

    /// Is end of stream? I.e. no more tokens.
    pub fn is_eof(&self) -> bool {
        self.cursor == self.tokens.len()
    }

    /// Advances to the next statement.
    ///
    /// There are two cases to consider. The first is that there is a ';' token that signals the
    /// end of the current statement, so we need to advance up to it and consume it. Then the
    /// token stream will be positioned for the parser to parse the next statement.
    ///
    /// However, before attempting to eat tokens until we find a semicolon, we also need to check if
    /// there's an opening or closing brace in the stream. If there's an opening brace then we'll advance
    /// up to it (without consuming it) so that the parser can parse the block. If there's a closing
    /// brace then we must be in the middle of a block, so we want to advance up to it without consuming it.
    pub fn move_to_next_statement(&mut self) {
        while !self.is_eof() {
            if let Some(tok) = self.peek_next_token()
                && (tok.token_type == lexer::TokenType::OpenBrace || tok.token_type == lexer::TokenType::CloseBrace)
            {
                // Return before consuming the brace.
                break;
            } else {
                match self.take_token() {
                    Some(tok) if tok.token_type == lexer::TokenType::Semicolon => break,
                    None => break,
                    _ => {}
                }
            }
        }
    }

    /// Capture the current state of the TokenStream.
    pub fn snapshot(&self) -> TokenStreamSnapshot {
        TokenStreamSnapshot { cursor: self.cursor }
    }

    /// Restore/rollback the TokenStream to the given snapshot.
    pub fn restore(&mut self, snapshot: TokenStreamSnapshot) {
        self.cursor = snapshot.cursor;
    }

    /// Peeks at the next token's source location in the stream, without advancing past it.
    pub fn peek_next_source_location(&self) -> Option<SourceLocation> {
        if self.cursor >= self.tokens.len() {
            return None;
        }
        Some(self.tokens[self.cursor].location)
    }

    /// The source location of the previous token in the stream, or None.
    pub fn prev_token_source_location(&self) -> Option<SourceLocation> {
        if self.cursor == 0 || self.cursor > self.tokens.len() {
            return None;
        }
        Some(self.tokens[self.cursor - 1].location)
    }
}
