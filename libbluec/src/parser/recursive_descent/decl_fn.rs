// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `decl_fn` module defines the parsing functionality for function declarations (and definitions).

use super::super::identifier_resolution::ResolveError;
use super::super::meta;
use super::AstStorageClassSpecifierOption;
use super::block;
use super::utils;
use super::{
    AstDeclaration, AstDeclaratorKind, AstDeclaredType, AstFunction, AstIdentifier, AstLinkage, AstNodeId,
    AstTypeAliasDeclaration, AstUniqueName,
};
use super::{ParseError, ParseResult, Parser, add_error};

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::errors::Error;
use crate::lexer;

/// Whether `parse_function` is allowed to parse a declaration-only, or a declaration with optional definition.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Allow {
    DeclarationOnly,
    DeclarationOrDefinition,
}

/// Parses the remainder of a function declaration (or definition) after the type-and-storage specifiers and
/// declarator.
pub fn parse_function(
    declared_type: AstDeclaredType,
    allow: Allow,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstDeclaration> {
    let Some(fn_declarator) = &declared_type.declarator else {
        ICE!("Function declaration should have a declarator");
    };

    // Get function identifier.
    // Future: Add support for function pointers, but for now, an identifier is required.
    let fn_ident = fn_declarator.get_identifier();
    if fn_ident.is_none() {
        Error::expect_identifier(utils::get_next_source_location_after_previous_token(parser), driver);
        return Err(ParseError);
    }
    let fn_ident = fn_ident.unwrap();

    let params = match &fn_declarator.get_derived_kind() {
        AstDeclaratorKind::Function { params, .. } => params,
        AstDeclaratorKind::AbstractFunction { params } => params,
        _ => ICE!("Expected a function declarator for '{fn_ident}'"),
    };

    let is_declared_typedef = declared_type.storage_class.is_typedef();
    let is_declared_static = declared_type.storage_class.is_static();

    let linkage = if is_declared_typedef {
        AstLinkage::None
    } else if is_declared_static {
        AstLinkage::Internal
    } else {
        AstLinkage::External
    };

    // Is there a definition following the declaration?
    let has_body = parser.token_stream.next_token_has_type(lexer::TokenType::OpenBrace);
    let is_declaration_only = !has_body;

    if allow == Allow::DeclarationOnly && has_body {
        let loc = utils::get_next_source_location_after_previous_token(parser);
        Error::expect_semicolon_at_end_of_declaration(loc, driver);
        return Err(ParseError);
    }

    if is_declared_typedef && has_body {
        let error = "A function definition cannot be declared with 'typedef'".to_string();
        let loc = declared_type.storage_class.unwrap().loc;
        add_error(driver, error, loc);
        return Err(ParseError);
    }

    // Verify the function declaration is valid in block scope.
    //
    //      Function declaration cannot be 'static' in block scope.
    //      Function cannot be defined in block scope.
    //
    let is_at_block_scope = !parser.identifiers.is_at_file_scope();

    if is_at_block_scope {
        if is_declared_static {
            add_error(
                driver,
                "A function declared in block scope (i.e. inside a function) cannot have 'static' storage class",
                declared_type.storage_class.as_ref().unwrap().loc,
            );
        }

        if has_body {
            add_error(
                driver,
                "Function definition must be at file scope; a function cannot be defined inside another function",
                fn_ident.loc,
            );
            return Err(ParseError); // Don't try and parse the invalid body
        }
    }

    // Identifier resolution. Add the function identifier in the current scope.
    //      The function's unique name remains as its declared name.
    //      We do this before parsing the function body in case it makes recursive calls to itself.
    //
    let unique_name = if is_declared_typedef {
        resolve_type_alias(fn_ident, parser, driver)
    } else {
        resolve_function(fn_ident, linkage, parser, driver)
    }?;

    // If this is a function definition then we require params to have identifiers.
    if has_body && params.iter().any(|param_decl_type| param_decl_type.get_identifier().is_none()) {
        let err = "Function definition cannot omit parameter names";
        add_error(driver, err, fn_ident.loc);
        return Err(ParseError);
    }

    // Try and parse the function body.
    //
    // Before doing so, first add the function parameter identifiers to the symbol table. We perform this step for
    // function declarations too because parameter identifiers must be unique even in declarations.
    //
    // If this is a function declaration and there is no body, we return Ok(None).
    // For a function definition, we either return Ok(Some(block)) or Err(()).
    //
    let (unique_param_names, fn_body) = parser.with_new_scope(|parser| {
        // Identifier resolution
        //
        let unique_param_names = params
            .iter()
            .filter_map(|param_decl_type| {
                let param_ident = param_decl_type.get_identifier()?;

                // Add the parameter identifier in the current scope (parameters are like block scope variable decls).
                //      Parameters are not extern.
                //
                match parser.identifiers.add_block_scope_variable_declaration(param_ident, false, AstLinkage::None) {
                    Ok(unique_name) => Some((param_ident.clone(), unique_name)),

                    Err(resolve_err) => match resolve_err {
                        ResolveError::RedefinitionOfExistingIdentifier(_, loc) => {
                            let param_name = &param_ident.name;
                            let param_loc = param_ident.loc;

                            let err = format!("Redefinition of parameter '{}'", param_name);
                            let mut diag = Diagnostic::error_at_location(err, param_loc);

                            diag.add_note(format!("'{}' was previously declared here:", param_name), Some(loc));
                            driver.add_diagnostic(diag);

                            None
                        }
                        _ => ICE!("Unexpected error"),
                    },
                }
            })
            .collect::<Vec<(AstIdentifier, AstUniqueName)>>();

        if is_declaration_only {
            Ok((unique_param_names, None))
        } else {
            // We've already created our own scope for the function and its parameters,
            // so parse the function body in our existing scope. That way we'll emit errors
            // if there's a variable declaration for an existing parameter identifier.
            Ok((unique_param_names, block::parse_block_in_current_scope(parser, driver).ok()))
        }
    })?;

    let node_id = AstNodeId::new();
    parser.metadata.add_source_span(node_id, meta::AstNodeSourceSpanMetadata::from_source_location(&fn_ident.loc));

    let ident = fn_ident.clone();

    let fn_decl = AstDeclaration::Function(AstFunction {
        node_id,
        declared_type,
        ident,
        unique_name,
        param_names: unique_param_names,
        body: fn_body,
        linkage,
    });

    if is_declared_typedef {
        let node_id = AstNodeId::new();
        Ok(AstDeclaration::TypeAlias(AstTypeAliasDeclaration { node_id, decl: Box::new(fn_decl) }))
    } else {
        Ok(fn_decl)
    }
}

fn resolve_type_alias(ident: &AstIdentifier, parser: &mut Parser, driver: &mut Driver) -> ParseResult<AstUniqueName> {
    // Add the type alias identifer in the current scope.
    //
    match parser.identifiers.add_type_alias_declaration(ident) {
        Ok(unique_name) => Ok(unique_name),

        Err(resolve_err) => match resolve_err {
            ResolveError::RedefinitionOfExistingIdentifier(kind, loc) => {
                Error::redefine_as_different_symbol(ident.into(), kind, loc, driver);
                Err(ParseError)
            }
            _ => ICE!("Unexpected error"),
        },
    }
}

fn resolve_function(
    ident: &AstIdentifier,
    linkage: AstLinkage,
    parser: &mut Parser,
    driver: &mut Driver,
) -> ParseResult<AstUniqueName> {
    // Add the function identifier in the current scope.
    //
    match parser.identifiers.add_function_declaration(ident, linkage) {
        Ok(unique_name) => Ok(unique_name),

        Err(resolve_err) => match resolve_err {
            ResolveError::RedefinitionOfExistingIdentifier(kind, loc) => {
                Error::redefine_as_different_symbol(ident.into(), kind, loc, driver);
                Err(ParseError)
            }
            _ => ICE!("Unexpected error"),
        },
    }
}
