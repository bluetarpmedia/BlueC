// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `decl` module provides functionality for parsing variable and function declarations.

use super::super::symbol::SymbolKind;
use super::decl_fn;
use super::decl_var;
use super::declarator;
use super::utils;
use super::{
    AstBasicType, AstBasicTypeSpecifier, AstDeclaration, AstDeclaratorKind, AstDeclaredType, AstStorageClassSpecifier,
    AstStorageClassSpecifierKind, AstStorageClassSpecifierOption,
};
use super::{ParseError, ParseResult, Parser, add_error};

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::{Diagnostic, SourceIdentifier};
use crate::compiler_driver::errors::Error;
use crate::compiler_driver::warnings::Warning;
use crate::lexer;

/// Parses one or more declarations.
///
/// Multiple declarations can be combined with a comma, as long as they share the same "basic type".
///
/// ```c
/// int points[4] = {}, calculate(float tax, float salary), age = 30;
/// ```
///
/// Declarations include:
/// - variable declaration
/// - function declaration (with optional definition)
/// - type alias (typedef) declaration
pub fn parse_declaration(parser: &mut Parser, driver: &mut Driver) -> ParseResult<Vec<AstDeclaration>> {
    // Keep track of the start of the declaration.
    let start_decl_loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;

    // Parse the type and storage class specifiers first.
    //      Remember: This includes `typedef`.
    //
    const EXPECTS_IDENTIFIER: bool = true;
    let (basic_type, storage) = super::decl::parse_type_and_storage_specifiers(parser, driver, EXPECTS_IDENTIFIER)?;

    // If the next token is the end of statement ';' then emit a warning because it's a useless declaration, and
    // then return.
    //
    if parser.token_stream.next_token_has_type(lexer::TokenType::Semicolon) {
        let end_decl_loc = parser.token_stream.prev_token_source_location().unwrap();
        let loc = start_decl_loc.merge_with(end_decl_loc);
        let is_typedef = storage.is_typedef();
        Warning::missing_declaration_identifier(is_typedef, loc, driver);
        _ = parser.token_stream.take_token();
        return Ok(vec![]);
    }

    // Old C standards allow that the type can be omitted and 'int' is assumed, but C99 and beyond do not support
    // implicit 'int'.
    //
    if basic_type.0.is_empty() {
        Error::missing_type_specifier(utils::get_next_source_location_after_previous_token(parser), driver);
        return Err(ParseError);
    }

    let mut declarations = Vec::new();
    let mut parse_fn_with_body = false;

    loop {
        // Parse the declarator which includes the identifier and may also augment the basic type, e.g.
        // pointer/array/function.
        //
        let declarator = declarator::parse_declarator(parser, driver, declarator::ParseOption::RequireIdentifier)?;

        // A declaration requires an identifier. (We handled the case above for valid but useless `int;`).
        if declarator.get_identifier().is_none() {
            Error::expect_identifier(declarator.loc, driver);
            return Err(ParseError);
        }

        let basic_type = basic_type.clone();
        let storage_class = storage;

        let decl = match declarator.get_derived_kind() {
            AstDeclaratorKind::Function { .. } => {
                let allow = if !declarations.is_empty() {
                    decl_fn::Allow::DeclarationOnly
                } else {
                    decl_fn::Allow::DeclarationOrDefinition
                };

                let declared_type = AstDeclaredType::unresolved(basic_type, storage_class, Some(declarator));
                let declaration = decl_fn::parse_function(declared_type, allow, parser, driver)?;

                if let AstDeclaration::Function(ref fn_decl) = declaration
                    && fn_decl.body.is_some()
                {
                    parse_fn_with_body = true;
                }

                declaration
            }

            AstDeclaratorKind::AbstractFunction { .. } => ICE!("AbstractFunction should have been detected"),

            _ => {
                let declared_type = AstDeclaredType::unresolved(basic_type, storage_class, Some(declarator));
                decl_var::parse_variable_declaration(declared_type, parser, driver)?
            }
        };

        declarations.push(decl);

        // If we parse a function definition then there can be no other declarators after it.
        if parse_fn_with_body {
            break;
        }

        // Allow multiple declarators separated by comma.
        //
        match parser.token_stream.peek_next_token() {
            Some(tok) if tok.has_type(lexer::TokenType::Comma) => {
                _ = parser.token_stream.take_token();
                continue;
            }
            Some(tok) if tok.has_type(lexer::TokenType::Semicolon) => break,
            _ => break,
        }
    }

    if !parse_fn_with_body {
        utils::expect_end_of_statement(parser, driver, "Expected `;` at end of declaration")?;
    }

    Ok(declarations)
}

/// Parse the basic type and storage class specifiers.
///
/// Type specifiers can be built-in ('int', 'signed', 'float', etc) or type aliases.
/// Storage class specifiers are 'extern', 'static', or 'typedef' (for now).
pub fn parse_type_and_storage_specifiers(
    parser: &mut Parser,
    driver: &mut Driver,
    expects_identifier: bool,
) -> ParseResult<(AstBasicType, Option<AstStorageClassSpecifier>)> {
    let mut type_alias_token = None; // Can only have one
    let mut type_specifier_tokens = Vec::new();
    let mut storage_class_tokens = Vec::new();
    let mut found_type_or_alias = false;

    // Parse the type and storage class specifier tokens.
    //
    //      If we expect to find an identifier for a declaration after the type and storage specifiers then
    //      stop parsing when we find a suitable candidate.
    //
    while let Some(peek_next_token) = parser.token_stream.peek_next_token() {
        let id = peek_next_token.get_identifier();

        if id.is_none() {
            break;
        }

        let id = id.cloned().unwrap();
        match id {
            // If we're expecting an identifier for a variable/function declaration then stop once we find a
            // candidate, but only if we've already parsed a valid type specifier.
            //
            id if expects_identifier && found_type_or_alias && !utils::is_reserved_keyword(&id) => {
                break;
            }

            id if utils::is_storage_class_specifier(&id) => {
                let token = parser.token_stream.take_token().unwrap().clone();
                storage_class_tokens.push(token);
            }

            id if utils::is_builtin_type_specifier(&id) => {
                let token = parser.token_stream.take_token().unwrap().clone();
                type_specifier_tokens.push(token);
                found_type_or_alias = true;
            }

            id if utils::is_type_alias_visible_from_current_scope(parser, &id) && type_alias_token.is_none() => {
                let token = parser.token_stream.take_token().unwrap().clone();
                type_alias_token = Some(token);
                found_type_or_alias = true;
            }

            _ => break,
        }
    }

    // Must have some specifiers.
    if type_alias_token.is_none() && type_specifier_tokens.is_empty() && storage_class_tokens.is_empty() {
        let loc = parser.token_stream.peek_next_source_location().ok_or(ParseError)?;
        add_error(driver, "Expected a type name", loc);
        return Err(ParseError);
    }

    // Transform the type specifiers into an `AstBasicType`.
    //
    let basic_type = if let Some(type_alias_token) = type_alias_token {
        parse_basic_type(std::slice::from_ref(&type_alias_token), parser, driver)
    } else {
        parse_basic_type(&type_specifier_tokens, parser, driver)
    }?;

    // We allow duplicate storage class specifiers (of the same, e.g. `typedef typedef` or `extern extern`).
    //      Remove the duplicates and warn of each unnecessary duplicate.
    //
    let storage_specifier_tokens = remove_and_warn_of_duplicates(storage_class_tokens, "typedef", driver);
    let storage_specifier_tokens = remove_and_warn_of_duplicates(storage_specifier_tokens, "extern", driver);
    let storage_specifier_tokens = remove_and_warn_of_duplicates(storage_specifier_tokens, "static", driver);

    // Max of 1 storage class specifiers can be supplied.
    if storage_specifier_tokens.len() > 1 {
        add_error(driver, "Invalid storage class specifiers", storage_specifier_tokens[0].location);
        return Err(ParseError);
    }

    let storage_specifier_token = storage_specifier_tokens.first();
    let storage_specifier = match storage_specifier_token {
        Some(token) if token.is_identifier_with_name("static") => {
            Some(AstStorageClassSpecifier { kind: AstStorageClassSpecifierKind::Static, loc: token.location })
        }
        Some(token) if token.is_identifier_with_name("extern") => {
            Some(AstStorageClassSpecifier { kind: AstStorageClassSpecifierKind::Extern, loc: token.location })
        }
        Some(token) if token.is_identifier_with_name("typedef") => {
            Some(AstStorageClassSpecifier { kind: AstStorageClassSpecifierKind::Typedef, loc: token.location })
        }
        _ => None,
    };

    Ok((basic_type, storage_specifier))
}

fn parse_basic_type(
    type_specifier_tokens: &[lexer::Token],
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstBasicType> {
    if type_specifier_tokens.is_empty() {
        return Ok(AstBasicType(Vec::new()));
    }

    // If there's only one token then check if it's an alias. This is the only valid situation to use a type alias.
    //
    let is_alias = type_specifier_tokens.len() == 1
        && type_specifier_tokens[0]
            .get_identifier()
            .map(|id| utils::is_type_alias_visible_from_current_scope(parser, id))
            .unwrap_or(false);

    if is_alias {
        let alias_name = type_specifier_tokens[0].get_identifier().unwrap();
        let alias_name_loc = type_specifier_tokens[0].location;

        let alias_specifier = if let Some(alias_decl) = parser.get_identifier_if_visible_from_current_scope(alias_name)
            && alias_decl.kind == SymbolKind::TypeAlias
        {
            AstBasicTypeSpecifier::Alias { alias_name: alias_decl.unique.clone(), loc: alias_name_loc }
        } else {
            let type_name = SourceIdentifier(alias_name, alias_name_loc);
            Error::unknown_type_name(type_name, driver);
            return Err(ParseError);
        };

        return Ok(AstBasicType(vec![alias_specifier]));
    }

    // Since we have multiple type specifiers, verify that none of them is an alias.
    //
    //      typedef int MyInt;
    //      signed MyInt x = 0;    // Invalid
    //      MyInt unsigned y = 0;  // Invalid
    //
    let found_alias_name = type_specifier_tokens.iter().find_map(|token| {
        if let Some(id) = token.get_identifier()
            && utils::is_type_alias_visible_from_current_scope(parser, id)
        {
            Some(id)
        } else {
            None
        }
    });

    if let Some(alias_name) = found_alias_name {
        let err = "Cannot combine type alias with type specifier".to_string();
        let mut diag = Diagnostic::error_at_location(err, type_specifier_tokens[0].location);
        let type_alias_decl = parser.get_identifier_if_visible_from_current_scope(alias_name).unwrap();
        diag.add_note(format!("Type alias `{}` was previously declared here:", alias_name), Some(type_alias_decl.loc));
        driver.add_diagnostic(diag);
        return Err(ParseError);
    }

    let basic_type_specifiers = type_specifier_tokens
        .iter()
        .filter_map(|token| {
            let lexer::TokenType::Identifier(ref type_specifier) = token.token_type else {
                ICE!("Parser: Expected identifier token");
            };

            if !utils::is_builtin_type_specifier(type_specifier.as_str()) {
                Error::invalid_type_specifier(token.location, driver);
                return None;
            }

            Some(AstBasicTypeSpecifier::BuiltinType { specifier: type_specifier.clone(), loc: token.location })
        })
        .collect();

    Ok(AstBasicType(basic_type_specifiers))
}

fn remove_and_warn_of_duplicates(
    storage_specifier_tokens: Vec<lexer::Token>,
    specifier: &str,
    driver: &mut Driver,
) -> Vec<lexer::Token> {
    // `partition` consumes the iterator and separates the search specifier from the rest, and keeps the relative order
    // of elements.
    let (search, mut others): (Vec<_>, Vec<_>) = storage_specifier_tokens
        .into_iter()
        .partition(|token| token.get_identifier().is_some_and(|id| id == specifier));

    if search.len() > 1 {
        for typedef_token in &search[1..] {
            let source_id = SourceIdentifier(specifier, typedef_token.location);
            Warning::duplicate_decl_specifier(source_id, driver);
        }
    }

    if !search.is_empty() {
        others.extend(search.into_iter().take(1));
    }

    others
}
