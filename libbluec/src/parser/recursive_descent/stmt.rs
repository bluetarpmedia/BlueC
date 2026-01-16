// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `stmt` module defines the various parsing functions for C statements.

use super::super::{EnclosingStatement, EnclosingStatementChain};
use super::block;
use super::decl;
use super::expr;
use super::peek;
use super::utils;
use super::{AstForInitializer, AstNodeId, AstStatement};
use super::{ParseError, ParseResult, Parser, add_error};

use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::internal_error;
use crate::lexer;
use crate::parser::{AstDeclaration, meta};

/// Parses a statement.
///
/// ```markdown
/// <control_conditional_stmt> ::= <if_stmt> | <switch_stmt>
/// <control_loop_stmt>        ::= <while_stmt> | <do_while_stmt> | <for_stmt>
/// <control_jump_stmt>        ::= <break_stmt> | <continue_stmt> | <goto_stmt> | <return_stmt>
/// <control_stmt>             ::= <control_conditional_stmt> | <control_loop_stmt> | <control_jump_stmt>
/// <statement>                ::= <expr_stmt> | <labeled_stmt> | <compound_stmt> | <null_stmt> | <control_stmt>
/// ```
pub fn parse_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let stmt: ParseResult<AstStatement>;

    match parser.token_stream.peek_next_2_tokens() {
        // Labeled statement
        (Some(t1), Some(t2))
            if utils::token_is_identifier_and_not_reserved(t1) && t2.has_type(lexer::TokenType::Colon) =>
        {
            stmt = parse_labeled_statement(parser, driver);
        }

        (Some(token), _) => {
            match token.token_type {
                // Null statement
                lexer::TokenType::Semicolon => {
                    stmt = parse_null_statement(parser, driver);
                }

                // Compound statement
                lexer::TokenType::OpenBrace => {
                    let block = block::parse_block(parser, driver)?;
                    stmt = Ok(AstStatement::Compound(block));
                }

                // Control statement
                lexer::TokenType::Identifier(ref id) if utils::is_control_statement_identifier(id) => match id.as_str()
                {
                    "if" => stmt = parse_if_statement(parser, driver),
                    "else" => {
                        // 'else' is handled by `parse_if_statement`, so if we encounter one here
                        // then it's an invalid clause without a parent 'if'.
                        add_error(driver, "Mismatched 'else' clause", token.location);
                        return Err(ParseError);
                    }
                    "switch" => stmt = parse_switch_statement(parser, driver),
                    "case" => stmt = parse_switch_case_statement(parser, driver),
                    "default" => stmt = parse_switch_default_statement(parser, driver),
                    "while" => stmt = parse_while_statement(parser, driver),
                    "do" => stmt = parse_do_while_statement(parser, driver),
                    "for" => stmt = parse_for_statement(parser, driver),
                    "break" => stmt = parse_break_statement(parser, driver),
                    "continue" => stmt = parse_continue_statement(parser, driver),
                    "goto" => stmt = parse_goto_statement(parser, driver),
                    "return" => stmt = parse_return_statement(parser, driver),
                    _ => internal_error::ICE("Parser: Unhandled control statement"),
                },

                // Expression statement
                _ => {
                    stmt = parse_expression_statement(parser, driver);
                }
            }
        }

        _ => {
            internal_error::ICE("Parser: Expected token to parse statement");
        }
    }

    stmt
}

/// Parses an expression statement.
///
/// ```markdown
/// <expr_stmt> ::= <full_expr> ";"
/// ```
pub fn parse_expression_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let expr = expr::parse_full_expression(parser, driver)?;
    utils::expect_end_of_statement(parser, driver, "Expected `;` after expression")?;
    Ok(AstStatement::Expression(expr))
}

/// Parses a labeled statement.
///
/// ```markdown
/// <labeled_stmt> ::= <identifier> ":" <statement>
/// ```
pub fn parse_labeled_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let label_token = utils::expect_identifier(parser, driver)?;
    let colon_token_loc = utils::expect_token(lexer::TokenType::Colon, parser, driver)?;

    let label_name = label_token
        .get_identifier()
        .or_else(|| {
            internal_error::ICE("Parser: Expected identifier token");
        })
        .unwrap();

    // A common user mistake is to try and label a variable/type alias declaration, which is not standard C.
    if peek::is_declaration(parser, driver) {
        let err = "Label must be followed by a statement and not a declaration".to_string();
        let mut diag = Diagnostic::error_at_location(err, colon_token_loc.get_next_location());

        let note_loc = parser.token_stream.peek_next_token().unwrap().location;
        diag.add_note("This is a declaration, not a statement".to_string(), Some(note_loc));

        let suggested_code = format!("{}: ;", &label_name);
        diag.add_note_with_suggested_code(
            "Add a semicolon after the label to create a null/empty statement. Then the declaration can proceed after the null statement.".into(),
            suggested_code,
            None,
        );

        driver.add_diagnostic(diag);
        return Err(ParseError);
    }

    // Parse the statement after the label
    let stmt = parse_statement(parser, driver);
    if stmt.is_err() {
        let err = "Label must be followed by a statement".to_string();
        let mut diag = Diagnostic::error_at_location(err, colon_token_loc.get_next_location());

        // Check if the next token is the end of the scope block; if so, the user is probably
        // trying to jump to the end of the block without realising a statement is needed.
        if parser.token_stream.next_token_has_type(lexer::TokenType::CloseBrace) {
            let suggested_code = format!("{}: ;", &label_name);
            diag.add_note_with_suggested_code(
                "If you're trying to jump to the end of the scope/block then add a semicolon after the label to create a null/empty statement.".into(),
                suggested_code,
                None,
            );
        }

        driver.add_diagnostic(diag);
        return Err(ParseError);
    }

    let stmt = stmt.unwrap();

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpan::from_source_location_pair(&label_token.location, &colon_token_loc),
    );

    Ok(AstStatement::Labeled { node_id, label_name: label_name.clone(), stmt: Box::new(stmt) })
}

/// Parses a null/empty statement.
///
/// ```markdown
/// <null_stmt> ::= ";"
/// ```
pub fn parse_null_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::Semicolon, parser, driver)?;
    Ok(AstStatement::Null)
}

/// Parses an if statement.
///
/// ```markdown
/// <if_stmt> ::= "if" "(" <controlling_expr> ")" <statement> [ "else" <statement> ]
/// ```
pub fn parse_if_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::new_identifier("if"), parser, driver)?;
    _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;
    let controlling_expr = expr::parse_full_expression(parser, driver)?;
    let right_paren_loc = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

    let then_stmt = parse_statement(parser, driver);
    if then_stmt.is_err() {
        add_error(
            driver,
            "'if' statement has no \"then\" clause; the statement is missing",
            right_paren_loc.get_next_location(),
        );
        return Err(ParseError);
    }

    let then_stmt = Box::new(then_stmt.unwrap());

    // Optional "else" <stmt>.
    //      We look for the "else" keyword immediately after parsing the "then" statement above.
    //      This allows us to avoid the "dangling else" grammar ambiguity.
    //
    //         if (x)
    //             if (x > 0)
    //                 return x;
    //             else           <-- Does this `else` belong to the first or second `if`? Should be second.
    //                 return 0;
    //
    let else_stmt = if parser.token_stream.next_token_has_type(lexer::TokenType::new_identifier("else")) {
        let else_token = parser.token_stream.take_token().ok_or(ParseError)?;
        let else_token_loc = else_token.location;

        let parsed_else_stmt = parse_statement(parser, driver);
        if parsed_else_stmt.is_err() {
            add_error(driver, "'else' clause has no statement", else_token_loc.get_next_location());
            return Err(ParseError);
        }

        parsed_else_stmt.ok().map(Box::new)
    } else {
        None
    };

    Ok(AstStatement::If { controlling_expr, then_stmt, else_stmt })
}

/// Parses a switch statement.
///
/// ```markdown
/// <switch_stmt> ::= "switch" "(" <controlling_expr> ")" <statement>
/// ```
pub fn parse_switch_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::new_identifier("switch"), parser, driver)?;
    _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;
    let controlling_expr = expr::parse_full_expression(parser, driver)?;
    _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

    let node_id = AstNodeId::new();
    let body = parser.with_enclosing_statement(EnclosingStatement::Switch(node_id), |p| parse_statement(p, driver))?;
    let body = Box::new(body);

    Ok(AstStatement::Switch { node_id, controlling_expr, body })
}

/// Parses a case statement.
///
/// ```markdown
/// <case_stmt> ::= "case" <constant_expr> ":" <statement>
/// ```
pub fn parse_switch_case_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let case_token_loc = utils::expect_token(lexer::TokenType::new_identifier("case"), parser, driver)?;

    // Sema: Validate that a case statement is always inside a switch statement.
    let switch_node_id = parser.current_enclosing_switch_statement_id();
    if switch_node_id.is_none() {
        add_error(driver, "Case statement not inside switch body", case_token_loc);
    }

    // Constant expression
    let constant_expr = expr::parse_full_expression(parser, driver)?;
    
    let colon_token_loc = utils::expect_token(lexer::TokenType::Colon, parser, driver)?;

    // Parse the statement after the label
    let stmt = parse_statement(parser, driver);
    if stmt.is_err() {
        let err = "Label must be followed by a statement".to_string();
        let mut diag = Diagnostic::error_at_location(err, colon_token_loc.get_next_location());

        // Check if the next token is the end of the scope block; if so, the user is probably
        // trying to jump to the end of the block without realising a statement is needed.
        if parser.token_stream.next_token_has_type(lexer::TokenType::CloseBrace) {
            let note = String::from(
                "If you're trying to jump to the end of the scope/block then add a semicolon after the label to \
                create a null/empty statement.",
            );
            let suggested_code = "case <expr>: ;".to_string();
            diag.add_note_with_suggested_code(note, suggested_code, None);
        }

        driver.add_diagnostic(diag);
        return Err(ParseError);
    }

    let stmt = stmt.unwrap();
    let stmt = Box::new(stmt);
    let switch_node_id = switch_node_id.unwrap_or_default();

    Ok(AstStatement::Case { switch_node_id, constant_expr, stmt })
}

/// Parses a default statement.
///
/// ```markdown
/// <default_stmt> ::= "default" ":" <statement>
/// ````
pub fn parse_switch_default_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let default_token_loc = utils::expect_token(lexer::TokenType::new_identifier("default"), parser, driver)?;

    // Sema: Validate that a default statement is always inside a switch statement.
    let switch_node_id = parser.current_enclosing_switch_statement_id();
    if switch_node_id.is_none() {
        add_error(driver, "Default statement not inside switch body", default_token_loc);
    }

    _ = utils::expect_token(lexer::TokenType::Colon, parser, driver)?;

    let stmt = parse_statement(parser, driver)?;
    let stmt = Box::new(stmt);
    let switch_node_id = switch_node_id.unwrap_or_default();

    // Sema: Validate that there's only one default label.
    if let Some(existing_loc) = parser.metadata.add_switch_default_label(switch_node_id, default_token_loc) {
        let err = "Multiple default labels in one switch statement".to_string();
        let mut diag = Diagnostic::error_at_location(err, default_token_loc);
        diag.add_note("Previous `default` label was declared here:".to_string(), Some(existing_loc));
        driver.add_diagnostic(diag);
    }

    Ok(AstStatement::Default { switch_node_id, stmt })
}

/// Parses a while statement.
///
/// ```markdown
/// <while_stmt> ::= "while" "(" <controlling_expr> ")" <statement>
/// ```
pub fn parse_while_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::new_identifier("while"), parser, driver)?;
    let open_paren_loc = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

    if !peek::is_expression(parser, driver) {
        add_error(driver, "Missing expression", open_paren_loc.get_next_location());
        return Err(ParseError);
    }

    let controlling_expr = expr::parse_full_expression(parser, driver)?;
    _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

    // Loop body
    let node_id = AstNodeId::new();
    let body = parser.with_enclosing_statement(EnclosingStatement::Loop(node_id), |p| parse_statement(p, driver))?;
    let body = Box::new(body);

    Ok(AstStatement::While { node_id, controlling_expr, body })
}

/// Parses a do-while statement.
///
/// ```markdown
/// <do_while_stmt> ::= "do" <statement> "while" "(" <controlling_expr> ")"
/// ```
pub fn parse_do_while_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::new_identifier("do"), parser, driver)?;

    // Loop body
    let node_id = AstNodeId::new();
    let body = parser.with_enclosing_statement(EnclosingStatement::Loop(node_id), |p| parse_statement(p, driver))?;

    _ = utils::expect_token(lexer::TokenType::new_identifier("while"), parser, driver)?;
    let open_paren_loc = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

    if !peek::is_expression(parser, driver) {
        add_error(driver, "Missing expression", open_paren_loc.get_next_location());
        return Err(ParseError);
    }

    let controlling_expr = expr::parse_full_expression(parser, driver)?;
    let _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;
    utils::expect_end_of_statement(parser, driver, "Expected `;` after do-while statement")?;

    let body = Box::new(body);
    Ok(AstStatement::DoWhile { node_id, body, controlling_expr })
}

/// Parses a for statement.
///
/// ```markdown
/// <for_stmt> ::= "for" "(" [ <init> ] ";" [ <controlling_expr> ] ";" <update_expr> ")" <statement>
/// ```
pub fn parse_for_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    parser.with_new_scope(|parser| {
        _ = utils::expect_token(lexer::TokenType::new_identifier("for"), parser, driver)?;
        _ = utils::expect_token(lexer::TokenType::OpenParen, parser, driver)?;

        // Optional initializer: may be a declaration or an expression, or nothing.
        //
        //      Parsing the declaration will also consume the semicolon for us, but parsing the
        //      expression will not, so we do that ourselves. A declaration cannot have static storage.
        //
        let init = if peek::is_declaration(parser, driver) {
            let declarations = decl::parse_declaration(parser, driver)?;

            if declarations.is_empty() {
                AstForInitializer::Expression(None)
            } else {
                // Do any of the variable declarations have 'static' storage?
                let var_decl_has_static_storage = declarations.iter().find_map(|decl| {
                    if let AstDeclaration::Variable(var_decl) = decl
                        && var_decl.declared_type.storage_class.as_ref().is_some_and(|sp| sp.is_static())
                    {
                        Some(var_decl)
                    } else {
                        None
                    }
                });

                if let Some(var_decl) = var_decl_has_static_storage {
                    add_error(
                        driver,
                        format!("Variable '{}' cannot be declared 'static' in a for-loop initializer", var_decl.ident),
                        parser.metadata.get_source_span_as_loc(&var_decl.node_id).unwrap(),
                    );
                }

                AstForInitializer::Declaration(declarations)
            }
        } else {
            let expr = expr::parse_optional_full_expression(parser, driver, lexer::TokenType::Semicolon).ok();
            _ = utils::expect_token(lexer::TokenType::Semicolon, parser, driver)?;
            AstForInitializer::Expression(expr)
        };

        // Optional loop controlling expression
        let controlling_expr = expr::parse_optional_full_expression(parser, driver, lexer::TokenType::Semicolon).ok();
        _ = utils::expect_token(lexer::TokenType::Semicolon, parser, driver)?;

        // Optional post/update expression
        let post = expr::parse_optional_full_expression(parser, driver, lexer::TokenType::CloseParen).ok();
        _ = utils::expect_token(lexer::TokenType::CloseParen, parser, driver)?;

        // Loop body
        let node_id = AstNodeId::new();
        let body =
            parser.with_enclosing_statement(EnclosingStatement::Loop(node_id), |p| parse_statement(p, driver))?;

        let init = Box::new(init);
        let body = Box::new(body);

        Ok(AstStatement::For { node_id, init, controlling_expr, post, body })
    })
}

/// Parses a break statement.
///
/// ```markdown
/// <break_stmt> ::= "break" ";"
/// ```
pub fn parse_break_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let break_token_loc = utils::expect_token(lexer::TokenType::new_identifier("break"), parser, driver)?;

    let enclosing_stmt_node_id = match parser.current_enclosing_statement() {
        Some(EnclosingStatementChain::Loop { loop_node_id, parent_switch_id: _ }) => Some(loop_node_id),
        Some(EnclosingStatementChain::Switch { switch_node_id, parent_loop_id: _ }) => Some(switch_node_id),
        _ => None,
    };

    // Sema: Validate that a break statement is always inside a loop or switch body statement.
    if enclosing_stmt_node_id.is_none() {
        add_error(driver, "Break statement not inside loop or switch body", break_token_loc);
    }

    utils::expect_end_of_statement(parser, driver, "Expected `;` after break statement")?;

    let enclosing_stmt_node_id = enclosing_stmt_node_id.unwrap_or_default();

    Ok(AstStatement::Break { enclosing_stmt_node_id })
}

/// Parses a continue statement.
///
/// ```markdown
/// <continue_stmt> ::= "continue" ";"
/// ```
pub fn parse_continue_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let continue_token_loc = utils::expect_token(lexer::TokenType::new_identifier("continue"), parser, driver)?;

    let loop_node_id = match parser.current_enclosing_statement() {
        Some(EnclosingStatementChain::Loop { loop_node_id, parent_switch_id: _ }) => Some(loop_node_id),
        Some(EnclosingStatementChain::Switch { switch_node_id: _, parent_loop_id }) => parent_loop_id,
        _ => None,
    };

    // Sema: Validate that a continue statement is always inside a loop body statement.
    if loop_node_id.is_none() {
        add_error(driver, "Continue statement not inside loop body", continue_token_loc);
    }

    utils::expect_end_of_statement(parser, driver, "Expected `;` after continue statement")?;

    let loop_node_id = loop_node_id.unwrap_or_default();

    Ok(AstStatement::Continue { loop_node_id })
}

/// Parses a goto statement.
///
/// ```markdown
/// <goto_stmt> ::= "goto" <identifier> ";"
/// ```
pub fn parse_goto_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    let goto_token_loc = utils::expect_token(lexer::TokenType::new_identifier("goto"), parser, driver)?;
    let label_token = utils::expect_identifier(parser, driver)?;
    utils::expect_end_of_statement(parser, driver, "Expected `;` after goto statement")?;

    let label_name = label_token
        .get_identifier()
        .or_else(|| {
            internal_error::ICE("Parser: Expected identifier token");
        })
        .unwrap();

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpan::from_source_location_pair(&goto_token_loc, &label_token.location),
    );

    Ok(AstStatement::Goto { node_id, label_name: label_name.clone() })
}

/// Parses a return statement.
///
/// ```markdown
/// <return_stmt> ::= "return" <expr> ";"
/// ```
pub fn parse_return_statement(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstStatement> {
    _ = utils::expect_token(lexer::TokenType::new_identifier("return"), parser, driver)?;
    let expr = expr::parse_full_expression(parser, driver)?;
    utils::expect_end_of_statement(parser, driver, "Expected `;` after return statement")?;
    Ok(AstStatement::Return(expr))
}
