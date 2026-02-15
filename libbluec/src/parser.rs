// Copyright 2025-2026 Neil Henderson
//
//! The `parser` module parses the stream of tokens produced by the lexer and produces an AST of the C code.
//!
//! We use a recursive descent parser except when parsing binary operations, where we use precedence climbing instead.
//! The parser performs identifier resolution. This allows us to solve the "typedef-name: identifier" ambiguity problem.

pub mod printer;

pub(super) mod expr;
pub(super) mod recursive_descent;

mod abstract_syntax_tree;
mod identifier_resolution;
mod meta;
mod token_stream;

#[cfg(test)]
pub(super) mod tests;

pub use abstract_syntax_tree::*;
pub use meta::{AstExpressionFlag, AstExpressionFlags, AstMetadata};

use identifier_resolution::{DeclaredIdentifier, SearchScope};

use crate::ICE;
use crate::compiler_driver::{Diagnostic, Driver};
use crate::core::{FilePosition, SourceLocation};
use crate::lexer;
use crate::sema;

/// The parser interprets tokens produced by the lexer to generate an abstract syntax tree.
pub struct Parser {
    pub metadata: meta::AstMetadata,
    token_stream: token_stream::TokenStream,
    identifiers: identifier_resolution::IdentifierResolver,
    current_enclosing_statement: Option<EnclosingStatementChain>,
}

/// An error type signaling a parse error. The error is emitted to the compiler driver's diagnostics.
#[derive(Debug)]
pub struct ParseError;

/// The result type returned by parsing functions.
///
/// Errors are emitted to the compiler driver's diagnostics rather than returned through the type system.
/// One reason for this is that a parsing function may need to emit more than one error. In addition, parsing
/// functions may need to emit warnings too and we use the same diagnostics for both errors and warnings
/// (since warnings may be treated as errors).
pub type ParseResult<T> = Result<T, ParseError>;

/// An outer switch or loop statement which is in effect while we are parsing statements inside it.
#[derive(Debug, Copy, Clone)]
pub enum EnclosingStatement {
    Loop(AstNodeId),
    Switch(AstNodeId),
}

/// An outer switch/loop statement which is in effect while we are parsing statements inside it, plus
/// an option parent switch/loop statement.
#[derive(Debug, Copy, Clone)]
pub enum EnclosingStatementChain {
    // For loops, we also need to track if there is an enclosing switch statement around the loop,
    // because `case` is still valid for the outer switch (if one exists), even though we're inside a loop.
    Loop { loop_node_id: AstNodeId, parent_switch_id: Option<AstNodeId> },

    // For switch statements, we also need to track if there is an enclosing loop statement around the switch,
    // because `break` will apply to the inner switch but `continue` will apply to the outer loop, if one exists.
    Switch { switch_node_id: AstNodeId, parent_loop_id: Option<AstNodeId> },
}

impl Parser {
    /// Creates a new parser which consumes the given vector of tokens.
    pub fn new(tokens: Vec<lexer::Token>) -> Self {
        Self {
            metadata: meta::AstMetadata::new(),
            token_stream: token_stream::TokenStream::new(tokens),
            identifiers: identifier_resolution::IdentifierResolver::new(),
            current_enclosing_statement: None,
        }
    }

    /// Gets the metadata for the given identifier, if the identifier has been declared in the current scope. Otherwise
    /// returns `None`.
    pub fn get_identifier_if_declared_in_current_scope(&self, identifier: &str) -> Option<&DeclaredIdentifier> {
        self.identifiers.resolve_identifier(identifier, SearchScope::Current)
    }

    /// Gets the metadata for the given identifier, if the identifier has been declared and is visible from the current
    /// scope. Otherwise returns `None`.
    pub fn get_identifier_if_visible_from_current_scope(&self, identifier: &str) -> Option<&DeclaredIdentifier> {
        self.identifiers.resolve_identifier(identifier, SearchScope::All)
    }

    // An enclosing switch or loop statement that is in effect while we are parsing statements inside it, or None.
    pub fn current_enclosing_statement(&self) -> Option<EnclosingStatementChain> {
        self.current_enclosing_statement
    }

    /// The ID of the enclosing switch statement node, or None.
    ///
    /// A switch statement may have other statements, like loops, inside it and this method will still return the
    /// ID of the enclosing switch statement node.
    pub fn current_enclosing_switch_statement_id(&self) -> Option<AstNodeId> {
        match self.current_enclosing_statement {
            Some(EnclosingStatementChain::Switch { switch_node_id, parent_loop_id: _ }) => Some(switch_node_id),

            Some(EnclosingStatementChain::Loop { loop_node_id: _, parent_switch_id }) => parent_switch_id,

            _ => None,
        }
    }

    /// Runs the given closure after creating a new Symbol Table scope.
    pub fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Parser) -> R,
    {
        self.identifiers.begin_scope();
        let result = f(self);
        self.identifiers.end_scope();
        result
    }

    /// Runs the given closure after capturing the id of the enclosing loop or switch statement.
    pub fn with_enclosing_statement<F, R>(&mut self, new_enclosing_stmt: EnclosingStatement, f: F) -> R
    where
        F: FnOnce(&mut Parser) -> R,
    {
        // Take a copy of the existing enclosing statement
        let original_enclosing_stmt = self.current_enclosing_statement;

        // Extract the IDs of the parent switch & loop statements, if they exist.
        // These will become grandparents in the new enclosing statement.
        let (parent_switch_id, parent_loop_id) = match original_enclosing_stmt {
            Some(EnclosingStatementChain::Loop { loop_node_id, parent_switch_id }) => {
                (parent_switch_id, Some(loop_node_id))
            }
            Some(EnclosingStatementChain::Switch { switch_node_id, parent_loop_id }) => {
                (Some(switch_node_id), parent_loop_id)
            }
            _ => (None, None),
        };

        // Set the new enclosing statement and update it with the appropriate grandparent.
        match new_enclosing_stmt {
            EnclosingStatement::Loop(loop_node_id) => {
                self.current_enclosing_statement =
                    Some(EnclosingStatementChain::Loop { loop_node_id, parent_switch_id })
            }

            EnclosingStatement::Switch(switch_node_id) => {
                self.current_enclosing_statement =
                    Some(EnclosingStatementChain::Switch { switch_node_id, parent_loop_id })
            }
        }

        // Run the closure
        let result = f(self);

        // Restore the original enclosing statement
        self.current_enclosing_statement = original_enclosing_stmt;

        result
    }

    /// Runs the given closure without allowing any diagnostics to be recorded.
    pub fn disable_diagnostics_during<F, R>(&mut self, driver: &mut Driver, f: F) -> R
    where
        F: FnOnce(&mut Parser, &mut Driver) -> R,
    {
        let original_enabled = driver.diagnostics_enabled();
        driver.set_diagnostics_enabled(false);

        let result = f(self, driver);

        driver.set_diagnostics_enabled(original_enabled);
        result
    }

    /// Runs the given closure and then restores the TokenStream to its original position before the closure.
    pub fn restore_token_stream_after<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Parser) -> R,
    {
        let snap = self.token_stream.snapshot();
        let result = f(self);
        self.token_stream.restore(snap);
        result
    }
}

/// Parses the stream of tokens produced by the lexer and generates an abstract syntax tree (AST)
/// representing the source C code, and then passes ownership of the AST to the semantic analysis stage.
pub fn parse(driver: &mut Driver, tokens: Vec<lexer::Token>) {
    let mut parser = Parser::new(tokens);

    let ast_root = recursive_descent::parse_translation_unit(&mut parser, driver);

    // Consume the Parser struct and destructure it to move out the field we want.
    //      We're finished with the parser but we want the AstMetadata that it created, so
    //      we consume the struct and destructure it to access the field. Otherwise there are borrow-checker
    //      problems because parser has a lifetime-bound reference to Driver. The field driver borrows Driver
    //      for lifetime 'a. Because of that, Rust treats the entire Parser<'a> as borrowed, and we can't move
    //      out fields without consuming the whole struct.
    let Parser { metadata, .. } = parser;

    if ast_root.is_err() && !driver.has_error_diagnostics() {
        ICE!("AST return type is Err, but no error diagnostics emitted");
    }

    // Don't proceed to the next stage if we've emitted errors, or if client only wants to run up to this stage.
    if driver.has_error_diagnostics() || driver.options().parse {
        return;
    }

    let ast = ast_root.unwrap();

    // If client wants to print the parsed AST then we're done.
    if driver.options().print_ast {
        printer::print(&ast, &metadata, driver);
        return;
    }

    sema::semantic_analysis(ast, metadata, driver);
}

/// Emits an error diagnostic with the given message.
///
/// The diagnostic will be ignored if the parser is being used in a call to `Parser::disable_diagnostics_during`.
/// This is a helper function which simplifies the construction of error diagnostics.
pub fn add_error<S: Into<String>>(driver: &mut Driver, error: S, location: SourceLocation) {
    driver.add_diagnostic(Diagnostic::error_at_location(error.into(), location));
}

/// Emits an error diagnostic at the end of the source file.
///
/// The diagnostic will be ignored if the parser is being used in a call to `Parser::disable_diagnostics_during`.
/// This is a helper function which simplifies the construction of error diagnostics when the parser is at EOF.
pub fn add_error_at_eof<S: Into<String>>(parser: &Parser, driver: &mut Driver, error: S) {
    let eof_location = if let Some(last_token) = parser.token_stream.last_token() {
        last_token.location.get_next_location()
    } else {
        SourceLocation::new(FilePosition::default(), 1)
    };

    driver.add_diagnostic(Diagnostic::error_at_location(error.into(), eof_location));
}
