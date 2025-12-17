// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `decl_var` module defines the various parsing functions for variable declarations.

use super::super::identifier_resolution::{ResolveError, SearchScope};
use super::super::meta;
use super::super::symbol::SymbolKind;
use super::expr;
use super::utils;
use super::{
    AstDeclaration, AstDeclaredType, AstIdentifier, AstLinkage, AstNodeId, AstStorageDuration, AstTypeAliasDeclaration,
    AstUniqueName, AstVariableDeclaration, AstVariableInitializer,
};
use super::{ParseError, ParseResult, Parser, add_error};
use crate::parser::AstStorageClassSpecifierOption;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::{Diagnostic, SourceIdentifier};
use crate::compiler_driver::errors::Error;
use crate::compiler_driver::warnings::Warning;
use crate::lexer::{SourceLocation, TokenType};

/// Parses the remainder of a variable declaration after the type-and-storage specifiers and declarator.
pub fn parse_variable_declaration(
    declared_type: AstDeclaredType,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstDeclaration> {
    let is_declared_extern = declared_type.storage_class.is_extern();
    let is_declared_static = declared_type.storage_class.is_static();
    let is_declared_typedef = declared_type.storage_class.is_typedef();

    let is_file_scope = parser.identifiers.is_at_file_scope();

    let Some(var_declarator) = &declared_type.declarator else {
        ICE!("Variable declaration should have a declarator");
    };

    // Get variable identifier. (This will be the new alias name for a `typedef`).
    let var_ident = var_declarator.get_identifier();
    if var_ident.is_none() {
        Error::expect_identifier(utils::get_next_source_location_after_previous_token(parser), driver);
        return Err(ParseError);
    }
    let var_ident = var_ident.unwrap();

    // Check for a rare error case. An extern variable declaration would usually take the linkage of a previous
    // static declaration, but it's possible that another local variable is hiding the static variable, in which
    // case the extern declaration has external linkage, and then conflicts with the static declaration. See the
    // function for more comments.
    //
    if is_declared_extern && is_static_variable_redeclared_as_extern(var_ident.into(), parser, driver) {
        return Err(ParseError);
    }

    // Determine the variable's linkage and storage
    //
    let (linkage, storage) = if is_file_scope {
        if is_declared_static {
            (AstLinkage::Internal, AstStorageDuration::Static)
        } else {
            (AstLinkage::External, AstStorageDuration::Static)
        }
    } else if is_declared_extern {
        (AstLinkage::External, AstStorageDuration::Static)
    } else if is_declared_static {
        (AstLinkage::None, AstStorageDuration::Static) // A local 'static' variable has no linkage
    } else if is_declared_typedef {
        (AstLinkage::None, AstStorageDuration::None)
    } else {
        (AstLinkage::None, AstStorageDuration::Automatic)
    };

    // Identifier resolution.
    //
    let unique_name = if is_declared_typedef {
        resolve_type_alias(var_ident, parser, driver)
    } else {
        resolve_variable(var_ident, is_file_scope, is_declared_extern, linkage, parser, driver)
    }?;

    // Is there an initializer expression following the declaration?
    let has_initializer = parser.token_stream.next_token_has_type(TokenType::Assignment);

    if is_declared_typedef && has_initializer {
        let error = "Cannot initialize a type alias ('typedef') declaration".to_string();
        let loc = parser.token_stream.peek_next_source_location().unwrap();
        add_error(driver, error, loc);
    }

    // Optional scalar/aggregate initializer for the variable.
    //      Parse this after identifier resolution, because it's valid to use the variable in its initializer.
    //
    let initializer = if has_initializer {
        _ = utils::expect_token(TokenType::Assignment, parser, driver)?;

        let init = parse_initializer(parser, driver)?;
        Some(init)
    } else {
        if !is_declared_typedef {
            Warning::uninitialized_variable(var_ident.into(), driver);
        }
        None
    };

    let end_decl_loc = parser.token_stream.prev_token_source_location().unwrap();

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location_pair(&var_ident.loc, &end_decl_loc),
    );

    let is_declaration_only = is_declared_extern && initializer.is_none();
    let ident = var_ident.clone();

    let var_decl = AstDeclaration::Variable(AstVariableDeclaration {
        node_id,
        is_declaration_only,
        is_file_scope,
        declared_type,
        ident,
        unique_name,
        initializer,
        init_constant_eval: Vec::new(),
        linkage,
        storage,
    });

    if is_declared_typedef {
        let node_id = AstNodeId::new();
        Ok(AstDeclaration::TypeAlias(AstTypeAliasDeclaration { node_id, decl: Box::new(var_decl) }))
    } else {
        Ok(var_decl)
    }
}

fn resolve_type_alias(ident: &AstIdentifier, parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstUniqueName> {
    // Add the type alias identifer in the current scope.
    //
    let unique_name = match parser.identifiers.add_type_alias_declaration(ident) {
        Ok(unique_name) => unique_name,

        Err(resolve_err) => match resolve_err {
            ResolveError::RedefinitionOfExistingIdentifier(kind, loc) => {
                Error::redefine_as_different_symbol(ident.into(), kind, loc, driver);
                return Err(ParseError);
            }
            _ => ICE!("Unexpected error"),
        },
    };

    Ok(unique_name)
}

fn resolve_variable(
    var_id: &AstIdentifier,
    is_at_file_scope: bool,
    is_declared_extern: bool,
    linkage: AstLinkage,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstUniqueName> {
    if is_at_file_scope {
        match parser.identifiers.add_file_scope_variable_declaration(var_id, linkage) {
            Ok(unique_name) => Ok(unique_name),

            Err(resolve_err) => match resolve_err {
                ResolveError::NotAtFileScope => {
                    ICE!("File scope variable decl not at file scope")
                }
                ResolveError::RedefinitionOfExistingIdentifier(kind, loc) => {
                    emit_redefinition_error(var_id.into(), is_declared_extern, kind, loc, driver);
                    Err(ParseError)
                }
            },
        }
    } else {
        match parser.identifiers.add_block_scope_variable_declaration(var_id, is_declared_extern, linkage) {
            Ok(unique_name) => Ok(unique_name),

            Err(resolve_err) => match resolve_err {
                ResolveError::RedefinitionOfExistingIdentifier(kind, loc) => {
                    emit_redefinition_error(var_id.into(), is_declared_extern, kind, loc, driver);
                    Err(ParseError)
                }
                _ => ICE!("Unhandled error when resolving block scope variable decl"),
            },
        }
    }
}

fn emit_redefinition_error(
    variable: SourceIdentifier,
    is_declared_extern: bool,
    existing_kind: SymbolKind,
    existing_loc: SourceLocation,
    driver: &mut Driver,
) {
    if existing_kind == SymbolKind::Variable {
        if is_declared_extern {
            Error::redefine_variable_linkage(variable, AstLinkage::External, existing_loc, driver);
        } else {
            Error::redefine_variable_value(variable, existing_loc, driver);
        }
    } else {
        Error::redefine_as_different_symbol(variable, existing_kind, existing_loc, driver);
    }
}

fn is_static_variable_redeclared_as_extern(
    variable: SourceIdentifier,
    parser: &mut Parser,
    driver: &mut Driver,
) -> bool {
    let variable_name = variable.0;
    let variable_loc = variable.1;

    // Check if there's a local variable in an encapsulating outer scope, but not at file scope, so that we can emit a
    // diagnostic if the 'extern' declaration is invalid.
    //
    // This is to handle a very rare case. See:
    // - gcc bug report 90472
    // - integration test "invalid/storage_class/extern_prior_declaration_no_linkage.c"
    // - "C17 6.2.2"
    //
    let identifier_with_no_linkage_loc = if let Some(decl) =
        parser.identifiers.resolve_identifier(variable_name, SearchScope::All)
        && decl.kind == SymbolKind::Variable
        && !decl.linkage.has_linkage()
    {
        Some(decl.loc)
    } else {
        None
    };

    if let Some(identifier_with_no_linkage_loc) = identifier_with_no_linkage_loc
        && let Some(decl) = parser.identifiers.resolve_identifier(variable_name, SearchScope::File)
        && decl.kind == SymbolKind::Variable
        && decl.linkage == AstLinkage::Internal
    {
        let err =
            format!("Variable '{}' was previously declared 'static' and is now redeclared 'extern'", variable_name);
        let mut diag = Diagnostic::error_at_location(err, variable_loc);

        diag.add_note(format!("'{}' was previously declared 'static' here:", variable_name), Some(decl.loc));

        diag.add_note(
            format!(
                "This local variable declaration of '{variable_name}' changes the 'extern' behavior. \
                The local variable has no linkage, which causes the subsequent 'extern' \
                declaration to have external linkage, instead of taking the internal \
                linkage of the 'static' declaration.",
            ),
            Some(identifier_with_no_linkage_loc),
        );

        driver.add_diagnostic(diag);

        return true;
    }

    false
}

fn parse_initializer(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstVariableInitializer> {
    if parser.token_stream.next_token_has_type(TokenType::OpenBrace) {
        let aggregate = parse_aggregate_initializer(parser, driver)?;
        Ok(aggregate)
    } else {
        let full_expr = expr::parse_full_expression(parser, driver)?;
        Ok(AstVariableInitializer::Scalar(full_expr))
    }
}

fn parse_aggregate_initializer(parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstVariableInitializer> {
    let start_loc = utils::expect_token(TokenType::OpenBrace, parser, driver)?;

    let mut aggregate_items = Vec::new();

    while !parser.token_stream.next_token_has_type(TokenType::CloseBrace) {
        let initializer = parse_initializer(parser, driver)?;
        aggregate_items.push(initializer);

        if parser.token_stream.next_token_has_type(TokenType::Comma) {
            _ = parser.token_stream.take_token();
        }
    }

    let end_loc = utils::expect_token(TokenType::CloseBrace, parser, driver)?;

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(
        node_id,
        meta::AstNodeSourceSpanMetadata::from_source_location_pair(&start_loc, &end_loc),
    );

    Ok(AstVariableInitializer::Aggregate { node_id, init: aggregate_items })
}
