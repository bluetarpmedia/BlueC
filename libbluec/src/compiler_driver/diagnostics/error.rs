// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `error` module defines functions to emit error diagnostics.

use crate::ICE;
use crate::core::{SourceIdentifier, SourceLocation, SymbolKind};
use crate::lexer::{NumericLiteralBase, TokenType};
use crate::parser::{AstLinkage, AstType};

use super::super::Driver;
use super::{Diagnostic, SuggestedCode};

/// An error diagnostic.
pub struct Error;

impl Error {
    /// Emits an error that a semicolon ';' is expected at the end of a declaration.
    pub fn expect_semicolon_at_end_of_declaration(loc: SourceLocation, driver: &mut Driver) {
        let err = "Expected ';' at end of declaration".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a symbol cannot be redefined to a different kind of symbol.
    pub fn redefine_as_different_symbol(
        identifier: SourceIdentifier,
        existing_kind: SymbolKind,
        existing_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Redefinition of '{}' as a different kind of symbol", identifier.0);
        let mut diag = Diagnostic::error_at_location(err, identifier.1);
        diag.add_note(
            format!("{} '{}' was previously declared here:", existing_kind, identifier.0),
            Some(existing_loc),
        );
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a variable cannot be redefined with a different type.
    pub fn redefine_variable_type(
        variable: SourceIdentifier,
        old_type: &AstType,
        new_type: &AstType,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err =
            format!("Redefinition of variable '{}' with a different type: '{new_type}' vs '{old_type}'", variable.0);
        let mut diag = Diagnostic::error_at_location(err, variable.1);
        diag.add_note(format!("'{}' was previously declared here", variable.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a variable cannot be redefined with a different value.
    pub fn redefine_variable_value(
        variable: SourceIdentifier,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Redefinition of variable '{}'", variable.0);
        let mut diag = Diagnostic::error_at_location(err, variable.1);
        diag.add_note(format!("`{}` was previously declared here:", variable.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a variable cannot be redefined with different linkage.
    pub fn redefine_variable_linkage(
        variable: SourceIdentifier,
        linkage: AstLinkage,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err =
            format!("Variable '{}' is declared with {} and does not match previous declaration", variable.0, &linkage);
        let mut diag = Diagnostic::error_at_location(err, variable.1);
        diag.add_note(format!("`{}` was previously declared here:", variable.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a local extern variable cannot have an initializer.
    pub fn local_extern_variable_with_initializer(loc: SourceLocation, driver: &mut Driver) {
        let err = "An 'extern' variable cannot have an initializer in block scope (i.e. inside a function)".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a type alias cannot be redefined with different types.
    pub fn redefine_type_alias_type(
        alias: SourceIdentifier,
        old_type: &AstType,
        new_type: &AstType,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!(
            "Type alias redefinition for '{}' with different types ('{}' vs '{}')",
            alias.0, new_type, old_type
        );

        let mut diag = Diagnostic::error_at_location(err, alias.1);
        diag.add_note(format!("type alias `{}` was previously declared here:", alias.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a function cannot be redefined with a different type.
    pub fn redefine_function_type(
        function: SourceIdentifier,
        old_type: &AstType,
        new_type: &AstType,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err =
            format!("Redefinition of function '{}' with a different type: '{new_type}' vs '{old_type}'", function.0);
        let mut diag = Diagnostic::error_at_location(err, function.1);
        diag.add_note(format!("'{}' was previously declared here", function.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a function cannot be redefined with internal linkage if its previous declaration has
    /// external linkage.
    pub fn redefine_external_linkage_function_with_internal_linkage(
        function: SourceIdentifier,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Static declaration of '{}' follows non-static declaration", function.0);
        let mut diag = Diagnostic::error_at_location(err, function.1);
        diag.add_note(format!("'{}' was previously declared here", function.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that a function cannot be redefined with a different body.
    pub fn redefine_function_body(
        function: SourceIdentifier,
        existing_symbol_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Redefinition of function '{}'", function.0);
        let mut diag = Diagnostic::error_at_location(err, function.1);
        diag.add_note(format!("'{}' was previously declared here", function.0), Some(existing_symbol_loc));
        driver.add_diagnostic(diag);
    }

    /// Emits an error that there is an attempt to call an undeclared function.
    pub fn call_undeclared_function(function: SourceIdentifier, driver: &mut Driver) {
        let err = format!("Call to undeclared function '{}'", function.0);
        driver.add_diagnostic(Diagnostic::error_at_location(err, function.1));
    }

    /// Emits an error that a type name is invalid.
    pub fn unknown_type_name(type_name: SourceIdentifier, driver: &mut Driver) {
        let err = format!("Unknown type name '{}'", type_name.0);
        driver.add_diagnostic(Diagnostic::error_at_location(err, type_name.1));
    }

    /// Emits an error that a type specifier is missing.
    pub fn missing_type_specifier(loc: SourceLocation, driver: &mut Driver) {
        let err = "Missing type specifier (C99 and beyond do not support implicit 'int')".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a type specifier is invalid.
    pub fn invalid_type_specifier(loc: SourceLocation, driver: &mut Driver) {
        let err = "Invalid type specifier".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a type specifier cannot be combined with previous specifiers.
    pub fn cannot_combine_type_specifier(specifier: SourceIdentifier, driver: &mut Driver) {
        let err = format!("Cannot combine '{}' with previous specifiers", specifier.0);
        driver.add_diagnostic(Diagnostic::error_at_location(err, specifier.1));
    }

    /// Emits an error that an identifier is expected.
    pub fn expect_identifier(loc: SourceLocation, driver: &mut Driver) {
        let err = "Expected identifier or '('".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a token is unexpected.
    pub fn identifier_cannot_be_keyword(identifier: SourceIdentifier, driver: &mut Driver) {
        let err = format!("Expected identifier instead of keyword '{}'", identifier.0);
        driver.add_diagnostic(Diagnostic::error_at_location(err, identifier.1));
    }

    /// Emits an error that a token is unexpected.
    pub fn unexpected_token(token_type: &TokenType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Unexpected token '{token_type}'");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that an integer or floating-point literal is invalid.
    pub fn invalid_numeric_literal(
        literal: &str,
        is_float: bool,
        base: NumericLiteralBase,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = if is_float {
            format!("Invalid floating-point literal '{literal}'")
        } else {
            format!("Invalid {base} literal '{literal}'")
        };

        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that the suffix is invalid on an integer or floating-point literal.
    pub fn invalid_numeric_literal_suffix(suffix: &str, is_float: bool, loc: SourceLocation, driver: &mut Driver) {
        let literal_type = if is_float { "floating point" } else { "integer" };
        let err = format!("Invalid suffix '{suffix}' on {literal_type} constant.");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that an expression is not assignable because it's not an lvalue.
    pub fn expression_is_not_assignable(
        assign_op_loc: SourceLocation,
        lhs_loc: SourceLocation,
        is_lvalue: bool,
        assign_type: &AstType,
        driver: &mut Driver,
    ) {
        let err = if is_lvalue {
            let type_description = get_type_description(assign_type);
            format!("Cannot assign to {type_description} '{assign_type}'")
        } else {
            "Expression is not assignable (must be an l-value)".to_string()
        };

        let mut diag = Diagnostic::error_at_location(err, assign_op_loc);
        diag.add_location(lhs_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits an error that an expression cannot be incremented or decremented.
    pub fn cannot_increment_or_decrement_expression(
        is_increment: bool,
        ty: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let type_description = get_type_description(ty);
        let op_description = if is_increment { "increment" } else { "decrement" };
        let err =
            format!("Cannot {op_description} expression of {type_description} '{ty}'; operand must be an l-value");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that the '*' operator requires a pointer type to dereference.
    pub fn indirection_requires_pointer_type(expr_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err =
            format!("Indirection requires a pointer type (cannot dereference an expression of type '{}')", expr_type);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that the '&' operator cannot take the address of an rvalue.
    pub fn cannot_take_address_of_rvalue(rvalue_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Cannot take the address of an r-value expression of type '{rvalue_type}'");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that two types in an expression are incompatible (i.e. there is no common type).
    pub fn incompatible_types(
        a: &AstType,
        b: &AstType,
        loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Incompatible types in expression ('{a}' and '{b}')");
        let mut diag = Diagnostic::error_at_location(err, loc);
        diag.add_location(a_loc);
        diag.add_location(b_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits an error that two pointer types are incompatible (i.e. there is no common type).
    ///
    /// -Wincompatible-pointer-types
    pub fn incompatible_pointer_types(a: &AstType, b: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Incompatible pointer types ('{a}' and '{b}')");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that two function pointer types are incompatible.
    ///
    /// -Wincompatible-function_pointer-types
    pub fn incompatible_fn_pointer_types(a: &AstType, b: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Incompatible function pointer types ('{a}' and '{b}')");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a binary operation has operands with invalid types.
    pub fn invalid_binary_expression_operands(
        operator_loc: SourceLocation,
        lhs: &AstType,
        rhs: &AstType,
        lhs_loc: SourceLocation,
        rhs_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let err = format!("Invalid operand types in binary expression ('{lhs}' and '{rhs}')");
        let mut diag = Diagnostic::error_at_location(err, operator_loc);
        diag.add_location(lhs_loc);
        diag.add_location(rhs_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits an error that an expression cannot be called because it has the wrong type.
    pub fn invalid_call_type(call_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Cannot call expression of type ('{call_type}'); must be a function or function pointer");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a function has an invalid return type.
    pub fn function_invalid_return_type(return_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let type_description = get_type_description(return_type);
        let err = format!("Function cannot return {type_description} '{return_type}'");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a function type is not assignable
    pub fn cannot_assign_to_fn_type(fn_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Cannot assign a value to an expression of function type '{fn_type}'");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that an expression is being casted to a function type (not function pointer) or array type.
    pub fn cannot_cast_to_function_or_array_type(
        cast_op_loc: SourceLocation,
        expr_loc: SourceLocation,
        cast_to_type: &AstType,
        driver: &mut Driver,
    ) {
        let type_description = get_type_description(cast_to_type);
        let err = format!("Cannot cast an expression to {type_description} ('{cast_to_type}')");
        let mut diag = Diagnostic::error_at_location(err, cast_op_loc);
        diag.add_location(expr_loc);

        let column_no = driver.tu_file.get_column_no(cast_op_loc);
        let indent = " ".repeat(column_no as usize - 1);
        let suggested_code = SuggestedCode::FormatString(
            format!("{indent}({})$$1", AstType::new_pointer_to(cast_to_type.clone())),
            vec![expr_loc],
        );

        let suggested_type = if cast_to_type.is_function() { "function pointer" } else { "pointer" };
        diag.add_note_with_suggested_code(format!("Try casting to a {suggested_type} instead."), suggested_code, None);

        driver.add_diagnostic(diag);
    }

    /// Emits an error that an expression cannot be cast to the given dest type. E.g. pointer type to floating-point,
    /// or vice versa.
    pub fn invalid_cast(expr_loc: SourceLocation, expr_type: &AstType, dest_type: &AstType, driver: &mut Driver) {
        let err = format!("Cannot cast an expression of type '{expr_type}' to '{dest_type}'");
        driver.add_diagnostic(Diagnostic::error_at_location(err, expr_loc));
    }

    /// Emits an error that a pointer cannot be converted to an arithmetic type.
    pub fn incompatible_pointer_to_arithmetic_conversion(
        ptr_type: &AstType,
        arith_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let arith = arithmetic_type_description(arith_type);
        let err = format!("Incompatible pointer type to {arith} conversion ('{ptr_type}' to '{arith_type}')");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that a pointer cannot be converted to an arithmetic type.
    pub fn incompatible_arithmetic_to_pointer_conversion(
        arith_type: &AstType,
        ptr_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let arith = arithmetic_type_description(arith_type);
        let err = format!("Incompatible {arith} to pointer type conversion ('{arith_type}' to '{ptr_type}')");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    /// Emits an error that an array must be initialized with an initializer list.
    pub fn cannot_initialize_array_with_scalar(init_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        debug_assert!(init_type.is_scalar());

        let err =
            format!("Cannot initialize an array with a value of type '{init_type}'; must be an initializer list.");
        let mut diag = Diagnostic::error_at_location(err, loc);

        {
            let note = "Add braces around the expression to create an initializer list, which will \
                        initialize the first element of the array with this value, and the remaining \
                        elements with zero.";

            let column_no = driver.tu_file.get_column_no(loc);

            let indent = if column_no == 1 { " ".to_string() } else { " ".repeat(column_no as usize - 2) };
            let space = if loc.length == 1 { " ".to_string() } else { " ".repeat(loc.length as usize - 2) };

            let suggested_code = SuggestedCode::Code(format!("{indent}{{{space}}}"));

            diag.add_note_with_suggested_code(note.to_string(), suggested_code, Some(loc));
        }

        driver.add_diagnostic(diag);
    }

    /// Emits an error that an array cannot be initialized with a string literal.
    pub fn cannot_initialize_array_with_string_literal(array_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let err = format!("Cannot initialize an array of type '{array_type}' with a string literal.");
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }
}

fn arithmetic_type_description(arith_type: &AstType) -> &str {
    if arith_type.is_integer() {
        "integer"
    } else if arith_type.is_floating_point() {
        "floating-point"
    } else {
        ICE!("Unhandled arithmetic type '{arith_type}'")
    }
}

fn get_type_description(ty: &AstType) -> &str {
    if ty.is_function() {
        "function type"
    } else if ty.is_array() {
        "array type"
    } else {
        "type"
    }
}
