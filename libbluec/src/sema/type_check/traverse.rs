// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `traverse` module defines recursive type checking functions which traverse the AST and annotate each
//! expression and variable initializer with its `AstType`.
//!
//! Functions beginning with `typecheck_` perform validation and set the `AstType` for the expression/initializer.
//! Functions beginning with `check_` only perform validation.

use crate::ICE;
use crate::compiler_driver::{Diagnostic, Driver, Error, Warning};
use crate::parser::expr::ops;
use crate::parser::{
    AstAssignmentOp, AstBinaryOp, AstBlock, AstBlockItem, AstConstantValue, AstDeclaration, AstExpression,
    AstForInitializer, AstFullExpression, AstNodeId, AstRoot, AstStatement, AstStaticStorageInitializer,
    AstStorageDuration, AstType, AstUnaryOp, AstUniqueName, AstVariableInitializer,
};

use super::super::constant_eval;
use super::super::type_resolution;
use super::checker::{CastWarningPolicy, TypeCheckError, TypeCheckResult, TypeChecker};
use super::symbols;
use super::utils;

/// Traverses the AST and performs type checking.
pub fn typecheck_ast(ast_root: &mut AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    _ = typecheck_declarations(&mut ast_root.0, chk, driver);
}

/// Type checks a list of declarations.
fn typecheck_declarations(
    declarations: &mut [AstDeclaration],
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<()> {
    // Create a new scope so that we can track typedef declarations.
    //
    chk.begin_declaration_scope();

    let mut is_err = false;

    // Type check all the declarations; if one fails, keep going with the next.
    for decl in declarations {
        if typecheck_declaration(decl, chk, driver).is_err() {
            is_err = true;
        }
    }

    chk.end_declaration_scope();

    if is_err { Err(TypeCheckError) } else { Ok(()) }
}

/// Type checks a variable, function, or type alias declaration.
fn typecheck_declaration(decl: &mut AstDeclaration, chk: &mut TypeChecker, driver: &mut Driver) -> TypeCheckResult<()> {
    match decl {
        AstDeclaration::Variable(decl) => {
            if decl.is_file_scope {
                symbols::verify_file_scope_variable_declaration(decl, chk, driver)?;
            } else {
                symbols::verify_local_variable_declaration(decl, chk, driver)?;
            }

            let var_type =
                decl.declared_type.resolved_type.as_ref().expect("Expected variable decl type to be resolved");

            // Typecheck the initializer expression, if there is one.
            //
            if let Some(initializer) = &mut decl.initializer {
                let (_, new_initializer) = typecheck_variable_initializer(var_type, initializer, chk, driver)?;

                if let Some(new_initializer) = new_initializer {
                    *initializer = new_initializer;
                }

                // An initializer expression for a static storage duration variable must be a constant expression, so
                // we'll verify that by trying to evaluate it.
                //
                if decl.storage == AstStorageDuration::Static {
                    decl.init_constant_eval = evaluate_static_storage_initializer(initializer, chk, driver)?;
                }
            }
        }

        AstDeclaration::Function(function) => {
            symbols::verify_function_declaration(function, chk, driver)?;

            let fn_type = function.declared_type.resolved_type.as_ref().expect("Expected function type to be resolved");
            let (return_type, param_types) = utils::extract_fn_return_and_param_types(fn_type);

            if return_type.is_array() || return_type.is_function() {
                let loc = chk.metadata.get_source_location(&function.node_id);
                Error::function_invalid_return_type(return_type, loc, driver);
            }

            let param_names = &function.param_names;

            let valid_params = param_types.iter().zip(param_names).all(|(param_type, (param_ident, param_unique))| {
                symbols::verify_function_parameter_declaration(param_unique, param_ident, param_type, chk, driver)
                    .is_ok()
            });

            if valid_params && let Some(body) = function.body.as_mut() {
                // Record the function's return type so we can use it when we check the `return` statement(s)
                // inside `typecheck_block`.
                chk.set_current_function_return_type(return_type);

                let valid_block = typecheck_block(body, chk, driver);

                chk.clear_current_function();

                valid_block?;
            }
        }

        AstDeclaration::TypeAlias(alias_decl) => {
            symbols::verify_type_alias_declaration(alias_decl, chk, driver)?;
        }
    }

    Ok(())
}

/// Type checks a variable initializer expression.
///
/// Returns the resolved `AstType` along with an optional new initializer if it needs to be rewritten.
fn typecheck_variable_initializer(
    variable_type: &AstType,
    initializer: &mut AstVariableInitializer,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<(AstType, Option<AstVariableInitializer>)> {
    match initializer {
        AstVariableInitializer::Scalar(full_expr) => {
            // If the variable's type is an array and the full expression is a string literal then verify that the array
            // is an array of characters, and also check their corresponding lengths.
            //
            if let AstType::Array { element_type, count } = variable_type
                && let AstExpression::StringLiteral { .. } = &full_expr.expr
            {
                // This also truncates the string if necessary.
                check_array_string_initializer(variable_type, element_type, *count, full_expr, chk, driver)?;

                // The type of the full expression is the variable's array type. Later we'll use this type to fill
                // in extra zeros, if necessary. E.g. `char arr[10] = "hi";` needs zeros.
                chk.set_data_type(&full_expr.node_id, variable_type);
                chk.set_data_type(&full_expr.expr.node_id(), variable_type);

                return Ok((variable_type.clone(), None));
            }

            let expr_type = typecheck_full_expression_for_result_type(full_expr, variable_type, chk, driver)?;
            Ok((expr_type, None))
        }

        AstVariableInitializer::Aggregate { .. } => {
            typecheck_variable_aggregate_initializer(variable_type, initializer, chk, driver)
        }
    }
}

fn typecheck_variable_aggregate_initializer(
    variable_type: &AstType,
    initializer: &mut AstVariableInitializer,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<(AstType, Option<AstVariableInitializer>)> {
    let AstVariableInitializer::Aggregate { node_id, init } = initializer else {
        ICE!("Expected aggregate initializer");
    };

    // An array of character type can be initialized with a string literal wrapped in an aggregate.
    //      char arr[3] = { "ab" };        // Initialized to {'a', 'b', 0}.
    //      char arr[3] = { "a", "bc" };   // Initialized to {'a', 0, 0}.
    //
    if variable_type.is_character_array()
        && !init.is_empty()
        && let AstVariableInitializer::Scalar(full_expr) = &init[0]
        && full_expr.expr.is_string_literal()
    {
        if init.len() > 1 {
            let next_node_id = init[1].node_id();
            let loc = chk.metadata.get_source_location(next_node_id);
            Warning::too_many_elements_for_initializer(variable_type, loc, driver);
        }

        let scalar_initializer = utils::take_first_scalar_initializer(&mut init[0]);
        *initializer = scalar_initializer;

        return typecheck_variable_initializer(variable_type, initializer, chk, driver);
    }

    let (element_type, element_count) = if variable_type.is_scalar() {
        (variable_type, 1)
    } else if let AstType::Array { element_type, count } = variable_type {
        (element_type.as_ref(), *count)
    } else {
        todo!() // Future: Structs
    };

    // Allow a sub-array to be initialized with a scalar, but warn about it unless the scalar value is zero.
    if element_type.is_aggregate()
        && init.len() == 1
        && let AstVariableInitializer::Scalar(full_expr) = &init[0]
    {
        let loc = chk.metadata.get_source_location(&full_expr.node_id);
        let scalar_is_literal_zero = full_expr.expr.is_integer_literal_with_value(0);
        let scalar = std::mem::take(init);
        *init = vec![AstVariableInitializer::Aggregate { node_id: AstNodeId::new(), init: scalar }];

        if !scalar_is_literal_zero {
            Warning::missing_braces_around_sub_object(loc, driver);
        }
    }

    // Type check each element in the initializer list.
    //      Don't short-circuit if one fails because we want a diagnostic for each element that is invalid.
    //      clippy suggests turning the `fold(..)?` into a short-circuiting `try_fold`, which we don't want.
    //
    #[allow(clippy::manual_try_fold)]
    init.iter_mut().fold(Ok(()), |result, ini| {
        result.and(typecheck_variable_initializer(element_type, ini, chk, driver).map(|_| ()))
    })?;

    // Add constant zero expressions for any missing elements in the initializer list. This also gives us
    // an implicit zero for a case like `int x = {};`.
    while init.len() < element_count {
        init.push(make_zero_initializer(element_type, chk, driver)?);
    }

    // Initializing scalar type with initializer list; take the first scalar expression in the list, warn about
    // the remainder, and return a new scalar initializer so that the caller can transform the aggregate
    // initializer to a scalar one.
    //
    if variable_type.is_scalar() {
        match init.len() {
            0 => unreachable!("Should have added zero initializer(s)"),
            1 => {
                // If there is a nested aggregate initializer, e.g. `int a = {{1}};`, then warn about the
                // unnecessary extra braces.
                if let AstVariableInitializer::Aggregate { .. } = init[0] {
                    let loc = chk.metadata.get_source_location(node_id);
                    Warning::too_many_braces_for_scalar_initializer(loc, driver);
                }
            }
            _ => {
                let loc = chk.metadata.get_source_location(node_id);
                Warning::too_many_elements_for_initializer(variable_type, loc, driver);
            }
        }

        chk.set_data_type(node_id, variable_type);

        let scalar_initializer = utils::take_first_scalar_initializer(&mut init[0]);
        Ok((variable_type.clone(), Some(scalar_initializer)))
    }
    // Initializing array type with initializer list
    //
    else if let AstType::Array { count, .. } = variable_type {
        // Warn if there are too many elements in the initializer list, and trim it to length.
        if init.len() > *count {
            let loc = chk.metadata.get_source_location(node_id);
            Warning::too_many_elements_for_initializer(variable_type, loc, driver);

            init.truncate(*count);
        }

        chk.set_data_type(node_id, variable_type);

        Ok((variable_type.clone(), None))
    }
    // Future: Struct
    //
    else {
        todo!()
    }
}

/// Type checks a block (compound statement).
fn typecheck_block(block: &mut AstBlock, chk: &mut TypeChecker, driver: &mut Driver) -> TypeCheckResult<()> {
    // Create a new scope so that we can track typedef declarations.
    //
    chk.begin_declaration_scope();

    // If typechecking fails for a declaration or statement, move to the next.
    for item in &mut block.0 {
        match item {
            AstBlockItem::Declaration(decl) => _ = typecheck_declaration(decl, chk, driver),
            AstBlockItem::Statement(stmt) => _ = typecheck_statement(stmt, chk, driver),
        }
    }

    chk.end_declaration_scope();

    Ok(())
}

/// Type checks a statement.
fn typecheck_statement(stmt: &mut AstStatement, chk: &mut TypeChecker, driver: &mut Driver) -> TypeCheckResult<()> {
    match stmt {
        AstStatement::Expression(full_expr) => {
            typecheck_full_expression(full_expr, chk, driver)?;
        }

        AstStatement::Labeled { stmt, .. } => typecheck_statement(stmt, chk, driver)?,

        AstStatement::Compound(block) => typecheck_block(block, chk, driver)?,

        AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            typecheck_full_expression(controlling_expr, chk, driver)?;
            typecheck_statement(then_stmt, chk, driver)?;
            if else_stmt.is_some() {
                typecheck_statement(else_stmt.as_mut().unwrap(), chk, driver)?;
            }
        }

        AstStatement::Switch { .. } => typecheck_switch_statement(stmt, chk, driver)?,

        AstStatement::Case { constant_expr, stmt, .. } => {
            typecheck_full_expression(constant_expr, chk, driver)?;
            typecheck_statement(stmt, chk, driver)?
        }

        AstStatement::Default { stmt, .. } => typecheck_statement(stmt, chk, driver)?,

        AstStatement::While { controlling_expr, body, .. } => {
            typecheck_full_expression(controlling_expr, chk, driver)?;
            typecheck_statement(body, chk, driver)?;
        }

        AstStatement::DoWhile { body, controlling_expr, .. } => {
            typecheck_statement(body, chk, driver)?;
            typecheck_full_expression(controlling_expr, chk, driver)?;
        }

        AstStatement::For { init, controlling_expr, post, body, .. } => {
            let init: &mut AstForInitializer = init;

            match init {
                AstForInitializer::Declaration(declarations) => {
                    // Future: C standard specifies only objects of automatic/register storage can be declared
                    // in a for loop declaration, but gcc/clang only warn about this with '-std=c99 -pedantic'.
                    typecheck_declarations(declarations, chk, driver)?;
                }
                AstForInitializer::Expression(full_expr) if full_expr.is_some() => {
                    typecheck_full_expression(full_expr.as_mut().unwrap(), chk, driver)?;
                }
                _ => (),
            }

            if controlling_expr.is_some() {
                typecheck_full_expression(controlling_expr.as_mut().unwrap(), chk, driver)?;
            }

            if post.is_some() {
                typecheck_full_expression(post.as_mut().unwrap(), chk, driver)?;
            }

            typecheck_statement(body, chk, driver)?;
        }

        AstStatement::Return(_) => typecheck_return_statement(stmt, chk, driver)?,

        _ => (),
    }

    Ok(())
}

/// Type checks a switch statement.
fn typecheck_switch_statement(
    stmt: &mut AstStatement,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<()> {
    let AstStatement::Switch { controlling_expr, body, .. } = stmt else {
        ICE!("Expected switch statement");
    };

    // Type check the switch statement's controlling expression.
    let controlling_expr_type = typecheck_full_expression(controlling_expr, chk, driver)?;

    // The type of the controlling expression must be an integer type.
    //      Promote the integer type to 'int' if its rank is below that. This includes character types, 'short',
    //      boolean types (C23 'bool' / '_Bool'), an `enum` type, or bitfields of an integer type.
    //
    if controlling_expr_type.is_integer() {
        let (promoted_type, promoted) = controlling_expr_type.promote_if_rank_lower_than_int();
        if promoted {
            // Cast the controlling expression to 'int'.
            let converted_expr = convert_expression_type(
                &controlling_expr.node_id,
                &mut controlling_expr.expr,
                &promoted_type,
                chk,
                driver,
            )?;

            // Update the full expression to use the converted expression
            *controlling_expr = AstFullExpression { node_id: controlling_expr.node_id, expr: converted_expr };

            chk.set_data_type(&controlling_expr.node_id, &promoted_type);
        }
    } else {
        let err = format!("Switch statement requires an integer expression ('{controlling_expr_type}' is invalid)");
        let loc = chk.metadata.get_source_location(&controlling_expr.node_id);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    typecheck_statement(body, chk, driver)
}

/// Type checks a return statement.
fn typecheck_return_statement(
    stmt: &mut AstStatement,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<()> {
    let AstStatement::Return(full_expr) = stmt else {
        ICE!("Expected return statement");
    };

    let function_return_type = chk.get_current_function_return_type();

    typecheck_full_expression_for_result_type(full_expr, &function_return_type, chk, driver).map(|_| ())
}

/// Type checks a full expression which must evaluate to a type of `result_type`. If the expression does not already
/// evaluate to that type, then a cast is added.
fn typecheck_full_expression_for_result_type(
    full_expr: &mut AstFullExpression,
    result_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let expr_type = typecheck_full_expression(full_expr, chk, driver)?;

    if expr_type == *result_type {
        return Ok(expr_type);
    }

    let converted_expr = convert_expression_type(&full_expr.node_id, &mut full_expr.expr, result_type, chk, driver)?;

    // Update the full expression to use the converted expression
    *full_expr = AstFullExpression { node_id: full_expr.node_id, expr: converted_expr };

    chk.set_data_type(&full_expr.node_id, result_type);

    Ok(result_type.clone())
}

/// Type checks a full expression.
fn typecheck_full_expression(
    full_expr: &mut AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let inner_expr_data_type = typecheck_expression_with_decay(&mut full_expr.expr, chk, driver)?;

    // The full expression itself has the same type as its inner expression root.
    chk.set_data_type(&full_expr.node_id, &inner_expr_data_type);

    Ok(inner_expr_data_type)
}

/// Type checks an expression and performs function-to-function pointer and array-to-pointer decay, where appropriate.
fn typecheck_expression_with_decay(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let ty = typecheck_expression(expr, chk, driver)?;

    if let AstType::Array { element_type, .. } = ty {
        let original_expr = utils::take_expression(expr);

        // The source span for the new AddressOf expression will be the same as the original expression,
        // since our new AddressOf that we're inserting doesn't appear in the source code.
        let node_id = AstNodeId::new();
        let original_expr_loc = chk.metadata.get_source_location(&original_expr.node_id());
        chk.metadata.add_source_location(node_id, original_expr_loc);
        chk.metadata.propagate_const_flag_from_child(original_expr.node_id(), node_id);

        *expr = AstExpression::AddressOf { node_id, expr: Box::new(original_expr) };

        let decayed_type = AstType::new_pointer_to(*element_type);
        chk.set_data_type(&node_id, &decayed_type);

        Ok(decayed_type)
    } else if let AstType::Function { .. } = ty {
        let original_expr = utils::take_expression(expr);

        // The source span for the new AddressOf expression will be the same as the original expression,
        // since our new AddressOf that we're inserting doesn't appear in the source code.
        let node_id = AstNodeId::new();
        let original_expr_loc = chk.metadata.get_source_location(&original_expr.node_id());
        chk.metadata.add_source_location(node_id, original_expr_loc);
        chk.metadata.propagate_const_flag_from_child(original_expr.node_id(), node_id);

        *expr = AstExpression::AddressOf { node_id, expr: Box::new(original_expr) };

        let decayed_type = AstType::new_pointer_to(ty);
        chk.set_data_type(&node_id, &decayed_type);

        Ok(decayed_type)
    } else {
        Ok(ty)
    }
}

/// Type checks an expression.
fn typecheck_expression(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    match expr {
        AstExpression::Identifier { node_id, unique_name, .. } => {
            // Record that the symbol has been used.
            chk.symbols.set_symbol_used(unique_name);

            // If the symbol doesn't exist then there was a previous typechecking error with its declaration, so
            // we'll abort typechecking this expression.
            let symbol = chk.symbols.get(&unique_name).ok_or(TypeCheckError)?;

            let symbol_type = symbol.data_type.clone();

            chk.set_data_type(node_id, &symbol_type);
            Ok(symbol_type)
        }

        AstExpression::CharLiteral { node_id, .. } => {
            chk.set_data_type(node_id, &AstType::Int);
            Ok(AstType::Int)
        }

        AstExpression::StringLiteral { node_id, ascii, .. } => {
            let data_type = AstType::new_array(AstType::Char, ascii.len() + 1); // Plus NULL
            chk.set_data_type(node_id, &data_type);
            Ok(data_type)
        }

        AstExpression::IntegerLiteral { node_id, kind, .. } => {
            let data_type = kind.data_type();
            chk.set_data_type(node_id, &data_type);
            Ok(data_type)
        }

        AstExpression::FloatLiteral { node_id, kind, .. } => {
            let data_type = kind.data_type();
            chk.set_data_type(node_id, &data_type);
            Ok(data_type)
        }

        AstExpression::Cast { .. } => typecheck_cast_expression(expr, chk, driver),

        AstExpression::Deref { node_id, expr } => {
            let expr_type = typecheck_expression_with_decay(expr, chk, driver)?;

            if let AstType::Pointer(referenced) = expr_type {
                chk.set_data_type(node_id, &referenced);
                Ok(*referenced)
            }
            // A function type can be dereferenced
            else if let AstType::Function { .. } = expr_type {
                chk.set_data_type(node_id, &expr_type);
                Ok(expr_type)
            } else {
                let loc = chk.metadata.get_source_location(node_id);
                Error::indirection_requires_pointer_type(&expr_type, loc, driver);
                Err(TypeCheckError) // Can't set data type, so must abort.
            }
        }

        AstExpression::AddressOf { node_id, expr } => {
            let expr_type = typecheck_expression(expr, chk, driver)?; // Note: No decay

            if expr.is_lvalue() {
                let ptr_type = AstType::Pointer(Box::new(expr_type));
                chk.set_data_type(node_id, &ptr_type);
                Ok(ptr_type)
            } else {
                let loc = chk.metadata.get_source_location(node_id);
                Error::cannot_take_address_of_rvalue(&expr_type, loc, driver);
                Err(TypeCheckError) // Can't set data type, so must abort.
            }
        }

        AstExpression::Subscript { .. } => typecheck_subscript_expression(expr, chk, driver),

        AstExpression::UnaryOperation { .. } => typecheck_unary_operation(expr, chk, driver),

        AstExpression::BinaryOperation { .. } => typecheck_binary_operation(expr, chk, driver),

        AstExpression::Assignment { .. } => typecheck_assignment(expr, chk, driver),

        AstExpression::Conditional { .. } => typecheck_conditional(expr, chk, driver),

        AstExpression::FunctionCall { .. } => typecheck_function_call(expr, chk, driver),
    }
}

/// Type checks a cast expression.
fn typecheck_cast_expression(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::Cast { node_id, target_type, expr } = expr else {
        ICE!("Expected cast expression");
    };

    let expr_type = typecheck_expression_with_decay(expr, chk, driver)?;

    let target_resolved_type =
        type_resolution::resolve_declared_type(target_type, chk, driver).map_err(|_| TypeCheckError)?;

    target_type.resolved_type = Some(target_resolved_type);

    let cast_to_type = target_type.resolved_type.clone().unwrap();

    // Warn if casting a pointer to an integer type smaller than pointer size.
    if expr_type.is_pointer() && cast_to_type.is_integer() && cast_to_type.bits() < expr_type.bits() {
        let cast_op_loc = chk.metadata.get_source_location(node_id);
        let expr_loc = chk.metadata.get_source_location(&expr.node_id());
        Warning::pointer_to_smaller_int_cast(&expr_type, &cast_to_type, cast_op_loc, expr_loc, driver);
    }

    // Cannot cast to a function or array type
    if cast_to_type.is_function() || cast_to_type.is_array() {
        let cast_op_loc = chk.metadata.get_source_location(node_id);
        let expr_loc = chk.metadata.get_source_location(&expr.node_id());

        Error::cannot_cast_to_function_or_array_type(cast_op_loc, expr_loc, &cast_to_type, driver);
    }

    // Cannot cast between pointer and floating-point types.
    if cast_to_type.is_pointer() && expr_type.is_floating_point()
        || cast_to_type.is_floating_point() && expr_type.is_pointer()
    {
        let expr_loc = chk.metadata.get_source_location(&expr.node_id());
        Error::invalid_cast(expr_loc, &expr_type, &cast_to_type, driver);
    }

    // Future: Warn about UB casting from a negative float literal to an unsigned int, e.g. `(unsigned int)-1`

    chk.set_data_type(node_id, &cast_to_type);
    Ok(cast_to_type)
}

/// Type checks an array subscript expression.
fn typecheck_subscript_expression(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::Subscript { node_id, expr1, expr2 } = expr else {
        ICE!("Expected subscript expression");
    };

    let expr1_type = typecheck_expression_with_decay(expr1, chk, driver)?;
    let expr2_type = typecheck_expression_with_decay(expr2, chk, driver)?;

    let valid_types =
        (expr1_type.is_pointer() && expr2_type.is_integer()) || (expr2_type.is_pointer() && expr1_type.is_integer());

    if !valid_types {
        let loc = chk.metadata.get_source_location(node_id);

        let expr1_loc = chk.metadata.get_source_location(&expr1.node_id());
        let expr2_loc = chk.metadata.get_source_location(&expr2.node_id());

        let neither_is_pointer = !expr1_type.is_pointer() && !expr2_type.is_pointer();

        let err = if neither_is_pointer {
            "Subscripted value is not an array or pointer".to_string()
        } else {
            "Array subscript must be an integer".to_string()
        };

        let mut diag = Diagnostic::error_at_location(err, loc);
        if neither_is_pointer {
            diag.add_location(expr1_loc);
            diag.add_location(expr2_loc);
        } else if expr1_type.is_pointer() && !expr2_type.is_integer() {
            diag.add_location(expr2_loc);
        } else if expr2_type.is_pointer() && !expr1_type.is_integer() {
            diag.add_location(expr1_loc);
        }

        driver.add_diagnostic(diag);
        return Err(TypeCheckError);
    }

    let ptr_type = if expr1_type.is_pointer() { expr1_type } else { expr2_type };
    let AstType::Pointer(referent_type) = ptr_type else {
        ICE!("Expected an AstType::Pointer");
    };

    // The type of the subscript expression is the pointer's referent type.
    chk.set_data_type(node_id, &referent_type);

    Ok(*referent_type)
}

/// Type checks the operand expression in the unary operation and then checks the unary operation itself with
/// that same type.
fn typecheck_unary_operation(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::UnaryOperation { node_id, op, expr: operand_expr } = expr else {
        ICE!("Expected unary operation");
    };

    // LogicalNot (and the binary operations for LogicalAnd/Or) have type of 'int'.
    if *op == AstUnaryOp::LogicalNot {
        typecheck_expression_with_decay(operand_expr, chk, driver)?;
        chk.set_data_type(node_id, &AstType::Int);
        return Ok(AstType::Int);
    }

    // The unary operation takes the type of its operand expression
    let operand_type = typecheck_expression_with_decay(operand_expr, chk, driver)?;

    // Integer promotion.
    let operand_type =
        if operand_type.is_integer() && matches!(op, AstUnaryOp::Negate | AstUnaryOp::Plus | AstUnaryOp::BitwiseNot) {
            let (promoted_type, promoted) = operand_type.clone().promote_if_rank_lower_than_int();
            if promoted {
                // Cast the expression to 'int'.
                let converted_expr =
                    convert_expression_type(&operand_expr.node_id(), operand_expr, &promoted_type, chk, driver)?;

                *operand_expr = Box::new(converted_expr);

                promoted_type
            } else {
                operand_type
            }
        } else {
            operand_type
        };

    // Pre/postfix increment & decrement require a modifiable lvalue.
    if ops::is_incr_or_decr_op(op) && !utils::is_modifiable_lvalue(operand_expr, &operand_type) {
        let loc = chk.metadata.get_source_location(node_id);
        let is_increment = ops::is_increment_op(op);
        Error::cannot_increment_or_decrement_expression(is_increment, &operand_type, loc, driver);
        return Err(TypeCheckError);
    }

    if operand_type.is_pointer() && utils::unary_operator_incompatible_with_ptr_operand(*op) {
        let err = format!("Unary operation is invalid on operand of type '{operand_type}'");
        let loc = chk.metadata.get_source_location(node_id);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    if operand_type.is_function() && utils::unary_operator_incompatible_with_fn_operand(*op) {
        let err = format!("Unary operation is invalid on operand of type '{operand_type}'");
        let loc = chk.metadata.get_source_location(node_id);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    if *op == AstUnaryOp::BitwiseNot && operand_type.is_floating_point() {
        let err = "Cannot take the bitwise complement of a floating-point type".to_string();
        let loc = chk.metadata.get_source_location(node_id);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    chk.set_data_type(node_id, &operand_type);
    Ok(operand_type)
}

/// Type checks the left and right expressions in the binary operation, wraps the binary operation with a cast
/// to their common type, and returns the common type.
fn typecheck_binary_operation(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::BinaryOperation { node_id, op, left, right } = expr else {
        ICE!("Expected binary operation");
    };

    let left_type = typecheck_expression_with_decay(left, chk, driver)?;
    let right_type = typecheck_expression_with_decay(right, chk, driver)?;

    // LogicalAnd `&&` and LogicalOr `||` evaluate to a value of type 'int' so we don't need to cast the operands.
    if *op == AstBinaryOp::LogicalAnd || *op == AstBinaryOp::LogicalOr {
        chk.set_data_type(node_id, &AstType::Int);
        return Ok(AstType::Int);
    }

    // Various binary operators cannot have an operand of pointer type, which includes operand of function type
    // because the function type decays into a function pointer type.
    //
    if (left_type.is_pointer() || right_type.is_pointer() || left_type.is_function() || right_type.is_function())
        && utils::binary_operator_incompatible_with_ptr_operand(*op)
    {
        utils::error_invalid_binary_expression_operands(node_id, left, right, &left_type, &right_type, chk, driver);
        return Err(TypeCheckError);
    }

    // Various binary operators cannot have floating-point operands
    let has_fp_operand = left_type.is_floating_point() || right_type.is_floating_point();
    if has_fp_operand && utils::binary_operator_incompatible_with_fp_operand(*op) {
        utils::error_invalid_binary_expression_operands(node_id, left, right, &left_type, &right_type, chk, driver);
        return Err(TypeCheckError);
    }

    // Binary operations where one operand is a pointer and the other is an integer, e.g. `1 + ptr` or `ptr == 0`.
    //
    if (left_type.is_pointer() && right_type.is_integer()) || (right_type.is_pointer() && left_type.is_integer()) {
        return typecheck_ptr_and_int_binary_operation(expr, chk, driver);
    }

    // Binary operation where both operands are pointers of the same type
    //
    if left_type.is_pointer() && left_type == right_type {
        return typecheck_ptr_binary_operation(expr, chk, driver);
    }

    // Take ownership of the expressions (by replacing them with a 'null' value, which will never be used again).
    let left = utils::take_boxed_expression(left);
    let right = utils::take_boxed_expression(right);

    // For arithmetic operators, relational operators, and bitwise and/or/xor we have to cast both operands to their
    // common type.
    //
    // LogicalAnd `&&` and LogicalOr `||` are handled above. That leaves LeftShift and RightShift which evaluate
    // to their left expression's type, except we also promote the type to 'int' if it's a smaller integer type.
    //
    let cast_both_to_common_type = matches!(
        op,
        AstBinaryOp::Add
            | AstBinaryOp::Subtract
            | AstBinaryOp::Multiply
            | AstBinaryOp::Divide
            | AstBinaryOp::Remainder
            | AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::EqualTo
            | AstBinaryOp::NotEqualTo
            | AstBinaryOp::LessThan
            | AstBinaryOp::GreaterThan
            | AstBinaryOp::LessThanOrEqualTo
            | AstBinaryOp::GreaterThanOrEqualTo
    );

    // Determine the data type to cast the operands
    let operand_data_type = if cast_both_to_common_type {
        match utils::get_common_type(&left, &right, chk, driver) {
            Ok(common_type) => common_type,
            Err(e) => match e {
                utils::CommonTypeError::WarnDifferentPointerTypes { a_type, b_type } => {
                    utils::warn_compare_different_pointer_types(node_id, &left, &right, &a_type, &b_type, chk, driver);
                    AstType::Pointer(Box::new(AstType::Void))
                }

                utils::CommonTypeError::WarnPointerAndInteger { a_type, b_type } => {
                    utils::warn_compare_pointer_and_integer(node_id, &left, &right, &a_type, &b_type, chk, driver);
                    if a_type.is_pointer() { a_type } else { b_type }
                }

                utils::CommonTypeError::NoCommonType { a_type, b_type } => {
                    let loc = chk.metadata.get_source_location(node_id);
                    let a_loc = chk.metadata.get_source_location(&left.node_id());
                    let b_loc = chk.metadata.get_source_location(&right.node_id());
                    Error::incompatible_types(&a_type, &b_type, loc, a_loc, b_loc, driver);

                    a_type
                }
            },
        }
    } else {
        let (promoted_type, _) = left_type.promote_if_rank_lower_than_int();
        promoted_type
    };

    // If necessary, cast both operands to the binary operation's data type.
    //      Above, we picked either their common type or the lhs type based on the operator.
    //      But we still cast the lhs to the data type because the lhs may have been promoted to 'int'
    //      if it was a smaller integer type (_Bool, char, short).
    //
    let left = chk.add_cast_if_needed(&operand_data_type, left, CastWarningPolicy::WarnOnImplicitConversion, driver);
    let right = chk.add_cast_if_needed(&operand_data_type, right, CastWarningPolicy::WarnOnImplicitConversion, driver);

    // Determine the data type of the binary operation itself
    let binary_op_data_type = if op.is_relational() { AstType::Int } else { operand_data_type };

    // Set the data type of the binary operation
    chk.set_data_type(node_id, &binary_op_data_type);

    // Update the binary operation to use the new left and right expresssions
    *expr = AstExpression::BinaryOperation { node_id: *node_id, op: *op, left, right };

    Ok(binary_op_data_type)
}

/// Type checks a binary operation with a pointer and integer operand (in either order).
fn typecheck_ptr_and_int_binary_operation(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::BinaryOperation { node_id, op, left, right } = expr else {
        ICE!("Expected binary operation");
    };

    let left = utils::take_boxed_expression(left);
    let right = utils::take_boxed_expression(right);

    let left_type = chk.get_data_type(&left.node_id());
    let right_type = chk.get_data_type(&right.node_id());

    assert!(left_type.is_pointer() && right_type.is_integer() || right_type.is_pointer() && left_type.is_integer());

    // Valid operations are addition, subtraction and relational operators.
    let valid_op = matches!(op, AstBinaryOp::Add | AstBinaryOp::Subtract) || op.is_relational();
    if !valid_op {
        utils::error_invalid_binary_expression_operands(node_id, &left, &right, &left_type, &right_type, chk, driver);
        return Err(TypeCheckError);
    }

    // Can subtract an integer from a pointer, but not vice versa.
    if *op == AstBinaryOp::Subtract && left_type.is_integer() {
        utils::error_invalid_binary_expression_operands(node_id, &left, &right, &left_type, &right_type, chk, driver);
        return Err(TypeCheckError);
    }

    // Determine which operand is the pointer and which is the integer.
    let (left_is_ptr, ptr_type, ptr_expr, int_type, mut int_expr) = if left_type.is_pointer() {
        (true, left_type, left, right_type, right)
    } else {
        (false, right_type, right, left_type, left)
    };

    // Emit a warning about if we're comparing a pointer and an integer, except for a null pointer constant.
    if op.is_relational() && !utils::is_null_pointer_constant(&int_expr, &ptr_type, chk, driver) {
        utils::warn_compare_pointer_and_integer(node_id, &ptr_expr, &int_expr, &ptr_type, &int_type, chk, driver);
    }

    // Cast the integer expression to a 'long' type. This helps us later with IR lowering.
    let original_int_expr = utils::take_boxed_expression(&mut int_expr);
    int_expr = chk.add_cast_if_needed(&AstType::Long, original_int_expr, CastWarningPolicy::NoWarning, driver);

    // The data type of the binary operation is 'int' if the operation is relational or otherwise the pointer's type.
    let expression_type = if op.is_relational() { AstType::Int } else { ptr_type };

    chk.set_data_type(node_id, &expression_type);

    // Update the binary operation to use the new cast expression.
    let (left, right) = if left_is_ptr { (ptr_expr, int_expr) } else { (int_expr, ptr_expr) };
    *expr = AstExpression::BinaryOperation { node_id: *node_id, op: *op, left, right };

    Ok(expression_type)
}

/// Type checks a binary operation with two pointer operands.
fn typecheck_ptr_binary_operation(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::BinaryOperation { node_id, op, left, right } = expr else {
        ICE!("Expected binary operation");
    };

    let left_type = chk.get_data_type(&left.node_id());
    let right_type = chk.get_data_type(&right.node_id());

    assert!(left_type.is_pointer() && left_type == right_type);

    let valid_op = op.is_relational() || *op == AstBinaryOp::Subtract;

    if !valid_op {
        utils::error_invalid_binary_expression_operands(node_id, left, right, &left_type, &right_type, chk, driver);
        return Err(TypeCheckError);
    }

    // Subtracting a pointer from another evaluates to a value of type '__ptrdiff_t' (aka 'long'). Relational operators
    // evaluate to 'int'.
    let expression_type = if *op == AstBinaryOp::Subtract { AstType::__ptrdiff_t() } else { AstType::Int };

    chk.set_data_type(node_id, &expression_type);

    Ok(expression_type)
}

/// Type checks the left and right expressions in the assignment (including compound assignment) operation, wraps the
/// rhs expression with a cast to the lhs type, and returns the lhs type.
fn typecheck_assignment(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::Assignment { node_id, computation_node_id, op, lhs, rhs } = expr else {
        ICE!("Expected assignment expression");
    };

    let lhs_type = typecheck_expression_with_decay(lhs, chk, driver)?;

    // The lhs must be a modifiable lvalue.
    if !utils::is_modifiable_lvalue(lhs, &lhs_type) {
        let assign_op_loc = chk.metadata.get_source_location(node_id);
        let lhs_loc = chk.metadata.get_source_location(&lhs.node_id());
        Error::expression_is_not_assignable(assign_op_loc, lhs_loc, lhs.is_lvalue(), &lhs_type, driver);
        return Err(TypeCheckError);
    }

    let rhs_type = typecheck_expression_with_decay(rhs, chk, driver)?;

    // Compound assignment evaluates to the type of the lvalue. Depending on the operation we may need to cast the
    // rhs rvalue to the computation type of the lhs and rhs, but we can't do the cast here because we only have
    // the compound operation. So we determine the appropriate type for the computation and set it in the metadata
    // for the IR lowering stage to use later.
    //
    //      int a = 1;
    //      long b = 2;
    //      a += b;      <---- this must be evaluated as:  a = (int)((long)a + b);
    //
    if op.is_compound_assignment() {
        // Some compound assignment operators cannot have an operand of pointer type, which includes operand of function
        // type because the function type decays into a function pointer type.
        //
        if (lhs_type.is_pointer() || rhs_type.is_pointer() || lhs_type.is_function() || rhs_type.is_function())
            && utils::assignment_operator_incompatible_with_ptr_operand(*op)
        {
            utils::error_invalid_binary_expression_operands(node_id, lhs, rhs, &lhs_type, &rhs_type, chk, driver);
            return Err(TypeCheckError);
        }

        // Some compound assignment operators cannot have floating-point operands
        let has_fp_operand = lhs_type.is_floating_point() || rhs_type.is_floating_point();
        if has_fp_operand && utils::assignment_operator_incompatible_with_fp_operand(*op) {
            utils::error_invalid_binary_expression_operands(node_id, lhs, rhs, &lhs_type, &rhs_type, chk, driver);
            return Err(TypeCheckError);
        }

        // For compound addition & subtraction (`+=` and `-=`), either both operands must be arithmetic types
        // or the lhs can be a pointer and the rhs can be arithmetic.
        if matches!(op, AstAssignmentOp::Addition | AstAssignmentOp::Subtraction) {
            let valid = (lhs_type.is_arithmetic() && rhs_type.is_arithmetic())
                || (lhs_type.is_pointer() && rhs_type.is_integer());

            if !valid {
                utils::error_invalid_binary_expression_operands(node_id, lhs, rhs, &lhs_type, &rhs_type, chk, driver);
                return Err(TypeCheckError);
            }

            // Pointer arithmetic
            if lhs_type.is_pointer() && rhs_type.is_integer() {
                chk.set_data_type(node_id, &lhs_type);
                chk.set_data_type(computation_node_id, &lhs_type);
                return Ok(lhs_type);
            }
        }

        // Some operations use the lhs type as the computation type, including a function pointer with `+=` or `-=`
        // and an integer rhs.
        let is_fn_ptr_incr_decr = lhs_type.is_function_pointer()
            && rhs_type.is_integer()
            && matches!(op, AstAssignmentOp::Addition | AstAssignmentOp::Subtraction);

        let is_shift = matches!(op, AstAssignmentOp::LeftShift | AstAssignmentOp::RightShift);

        if is_fn_ptr_incr_decr || is_shift {
            chk.set_data_type(node_id, &lhs_type);
            chk.set_data_type(computation_node_id, &lhs_type);

            return Ok(lhs_type);
        }

        // Work out the common type for the compound computation (e.g. the type of `lhs + rhs` if the op is `+=`).
        //      The IR lowering stage will use this type when translating the compound operation to IR.
        let computation_type = utils::get_common_type(lhs, rhs, chk, driver).map_err(|_| TypeCheckError)?;

        chk.set_data_type(node_id, &lhs_type);
        chk.set_data_type(computation_node_id, &computation_type);

        return Ok(lhs_type);
    }

    let original_lhs = utils::take_boxed_expression(lhs);
    let converted_rhs = convert_expression_type(&rhs.node_id(), rhs, &lhs_type, chk, driver)?;

    // Data type of an assignment is the lhs type
    chk.set_data_type(node_id, &lhs_type);

    // Rewrite the original assignment expression and insert the possibly-casted rhs.
    *expr = AstExpression::Assignment {
        node_id: *node_id,
        computation_node_id: *computation_node_id,
        op: *op,
        lhs: original_lhs,
        rhs: Box::new(converted_rhs),
    };

    Ok(lhs_type)
}

/// Type checks the conditional expression plus the consequent and alternative expressions in the conditional operation,
/// wraps both the consequent and alternative expressions in casts to their common type, and returns the common type.
fn typecheck_conditional(
    conditional_expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::Conditional { node_id, expr: condition, consequent, alternative } = conditional_expr else {
        ICE!("Expected conditional expression");
    };

    typecheck_expression_with_decay(condition, chk, driver)?;
    typecheck_expression_with_decay(consequent, chk, driver)?;
    typecheck_expression_with_decay(alternative, chk, driver)?;

    let common_type = match utils::get_common_type(consequent, alternative, chk, driver) {
        Ok(common_type) => common_type,
        Err(e) => match e {
            utils::CommonTypeError::WarnDifferentPointerTypes { a_type, b_type } => {
                utils::warn_pointer_type_mismatch(node_id, consequent, alternative, &a_type, &b_type, chk, driver);
                AstType::Pointer(Box::new(AstType::Void))
            }

            utils::CommonTypeError::WarnPointerAndInteger { a_type, b_type } => {
                utils::warn_conditional_type_mismatch(node_id, consequent, alternative, &a_type, &b_type, chk, driver);
                if a_type.is_pointer() { a_type } else { b_type }
            }

            utils::CommonTypeError::NoCommonType { a_type, b_type } => {
                let loc = chk.metadata.get_source_location(node_id);
                let a_loc = chk.metadata.get_source_location(&consequent.node_id());
                let b_loc = chk.metadata.get_source_location(&alternative.node_id());
                Error::incompatible_types(&a_type, &b_type, loc, a_loc, b_loc, driver);

                a_type
            }
        },
    };

    // Take ownership of the expressions (by replacing with a 'null' value, which will never be used).
    let original_condition = utils::take_boxed_expression(condition);
    let original_consequent = utils::take_boxed_expression(consequent);
    let original_alternative = utils::take_boxed_expression(alternative);

    // Wrap each operand in a Cast
    let cast_enable_warning = CastWarningPolicy::WarnOnImplicitConversion;
    let casted_consequent = chk.add_cast_if_needed(&common_type, original_consequent, cast_enable_warning, driver);
    let casted_alternative = chk.add_cast_if_needed(&common_type, original_alternative, cast_enable_warning, driver);

    // Data type of the conditional is the common type of the consequent and alternative
    chk.set_data_type(node_id, &common_type);

    // Wrap the original conditional expression in a cast to the common type
    *conditional_expr = AstExpression::Conditional {
        node_id: *node_id,
        expr: original_condition,
        consequent: casted_consequent,
        alternative: casted_alternative,
    };

    Ok(common_type)
}

/// Type checks each argument expression in the function call, wraps each argument in a cast to the appropriate function
/// parameter type, then checks the function call expression itself with the function return type, and returns the
/// function's return type.
fn typecheck_function_call(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let AstExpression::FunctionCall { node_id, designator, args, args_node_id } = expr else {
        ICE!("Expected function call expression");
    };

    // Type check the designator expression.
    _ = typecheck_expression(designator, chk, driver)?; // Note: No decay

    // Get the type of the function designator (function type or function pointer type) and the optional name
    // of the function. The name might be `None` if the designator is a function pointer. We only need the name
    // for diagnostics.
    //
    let (unique_name, designator_type) = get_function_designator_type(designator, chk, driver)?;

    // Verify the designator is a function or a function pointer.
    let valid_designator = designator_type.is_function() || designator_type.is_function_pointer();
    if !valid_designator {
        let loc = chk.metadata.get_source_location(&designator.node_id());
        Error::invalid_call_type(&designator_type, loc, driver);
        return Err(TypeCheckError);
    }

    // Get the function's return type and parameter types
    let (return_type, params) = if designator_type.is_function_pointer() {
        let AstType::Pointer(referent) = &designator_type else {
            ICE!("Expected an AstType::Pointer");
        };

        utils::extract_fn_return_and_param_types(referent)
    } else {
        utils::extract_fn_return_and_param_types(&designator_type)
    };

    // Validate that the arguments match the function parameters.
    if symbols::verify_function_arguments(unique_name, args_node_id, params, args, &chk.metadata, &chk.symbols, driver)
        .is_ok()
    {
        // Make new args with casts for each one to the appropriate parameter type.
        let casted_args = args
            .iter_mut()
            .zip(params)
            .filter_map(|(arg, param_type)| {
                _ = typecheck_expression_with_decay(arg, chk, driver).ok()?;

                let converted_arg = convert_expression_type(&arg.node_id(), arg, param_type, chk, driver).ok()?;

                Some(converted_arg)
            })
            .collect::<Vec<AstExpression>>();

        *args = casted_args;
    }

    // The type of the function call expression itself is the function's return type.
    chk.set_data_type(node_id, return_type);

    Ok(return_type.clone())
}

/// Get the type of the function designator and, if it designates a function's name, that too.
fn get_function_designator_type(
    expr: &mut AstExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<(Option<AstUniqueName>, AstType)> {
    match expr {
        AstExpression::Identifier { node_id, unique_name, .. } => {
            let ident_type = chk.get_data_type(node_id);
            Ok((Some(unique_name.clone()), ident_type))
        }

        AstExpression::Deref { expr, .. } => {
            let (unique_name, expr_type) = get_function_designator_type(expr, chk, driver)?;

            if let AstType::Function { .. } = expr_type {
                return Ok((unique_name, expr_type.clone()));
            }

            let AstType::Pointer(referent) = expr_type else {
                ICE!("Expression of AstExpression::Deref should have pointer type");
            };

            Ok((unique_name, referent.as_ref().clone()))
        }

        AstExpression::Conditional { node_id, consequent, alternative, .. } => {
            let (consequent_name, consequent_t) = get_function_designator_type(consequent, chk, driver)?;
            let (_, alternative_t) = get_function_designator_type(alternative, chk, driver)?;

            if consequent_t != alternative_t {
                let loc = chk.metadata.get_source_location(node_id);
                Error::incompatible_fn_pointer_types(&consequent_t, &alternative_t, loc, driver);
                return Err(TypeCheckError);
            }

            // We can choose to return either the consequent or alternative name/type here, because we've verified
            // that they both have the same types. Our caller, typecheck_function_call, will proceed to validate
            // the function arguments are correct.
            Ok((consequent_name, consequent_t))
        }

        AstExpression::Cast { target_type, expr, .. } => {
            let (unique_name, _) = get_function_designator_type(expr, chk, driver)?;

            let target_resolved_type =
                type_resolution::resolve_declared_type(target_type, chk, driver).map_err(|_| TypeCheckError)?;

            Ok((unique_name, target_resolved_type))
        }

        AstExpression::AddressOf { expr, .. } => {
            let (unique_name, expr_type) = get_function_designator_type(expr, chk, driver)?;
            let fn_ptr_type = AstType::Pointer(Box::new(expr_type));
            Ok((unique_name, fn_ptr_type))
        }

        AstExpression::Subscript { node_id, expr1, expr2 } => {
            let (unique_name1, expr1_t) = get_function_designator_type(expr1, chk, driver)?;
            let (unique_name2, expr2_t) = get_function_designator_type(expr2, chk, driver)?;

            if let AstType::Pointer(referent) = expr1_t
                && let AstType::Array { element_type, .. } = *referent
            {
                return Ok((unique_name1, element_type.as_ref().clone()));
            }

            if let AstType::Pointer(referent) = expr2_t
                && let AstType::Array { element_type, .. } = *referent
            {
                return Ok((unique_name2, element_type.as_ref().clone()));
            }

            let call_type = chk.get_data_type(node_id);
            let loc = chk.metadata.get_source_location(node_id);
            Error::invalid_call_type(&call_type, loc, driver);
            Err(TypeCheckError)
        }

        AstExpression::UnaryOperation { node_id, op, .. } => {
            if ops::is_incr_or_decr_op(op) {
                let expr_type = chk.get_data_type(node_id);
                Ok((None, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_location(node_id);
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::BinaryOperation { node_id, op, .. } => {
            if matches!(op, AstBinaryOp::Add | AstBinaryOp::Subtract) {
                let expr_type = chk.get_data_type(node_id);
                Ok((None, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_location(node_id);
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::Assignment { node_id, op, .. } => {
            if matches!(op, AstAssignmentOp::Assignment | AstAssignmentOp::Addition | AstAssignmentOp::Subtraction) {
                let expr_type = chk.get_data_type(node_id);
                Ok((None, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_location(node_id);
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::FunctionCall { .. } => {
            let ret_type = typecheck_function_call(expr, chk, driver)?;
            Ok((None, ret_type))
        }

        AstExpression::CharLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            Ok((None, expr_type))
        }

        AstExpression::StringLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            Ok((None, expr_type))
        }

        AstExpression::IntegerLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            Ok((None, expr_type))
        }

        AstExpression::FloatLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            let loc = chk.metadata.get_source_location(node_id);
            Error::invalid_call_type(&expr_type, loc, driver);
            Err(TypeCheckError)
        }
    }
}

/// Evaluates a variable initializer expression for a static storage variable (must evaluate to a constant value).
fn evaluate_static_storage_initializer(
    initializer: &mut AstVariableInitializer,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<Vec<AstStaticStorageInitializer>> {
    let mut eval = Vec::new();

    match initializer {
        AstVariableInitializer::Scalar(full_expr) => {
            let full_expr_type = chk.get_data_type(&full_expr.node_id);

            if full_expr_type.is_array() && full_expr.expr.is_string_literal() {
                eval.push(evaluate_static_storage_array_string_literal_initializer(full_expr, chk, driver)?);
            } else {
                eval.push(evaluate_static_storage_scalar_initializer(full_expr, chk, driver)?);
            }
        }
        AstVariableInitializer::Aggregate { init, .. } => {
            for ini in init {
                let mut values = evaluate_static_storage_initializer(ini, chk, driver)?;
                eval.append(&mut values);
            }
        }
    }

    // Merge consecutive ZeroBytes elements, e.g. `[ZeroBytes(4), ZeroBytes(4)]` becomes `[ZeroBytes(8)]`.
    eval = utils::combine_consecutive_zero_bytes(eval);

    Ok(eval)
}

/// Evaluates a string initializer expression for a static storage array variable.
fn evaluate_static_storage_array_string_literal_initializer(
    full_expr: &mut AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstStaticStorageInitializer> {
    let full_expr_type = chk.get_data_type(&full_expr.node_id);

    let AstType::Array { element_type, count } = &full_expr_type else {
        ICE!("Expected AstType::Array");
    };

    let AstExpression::StringLiteral { .. } = &full_expr.expr else {
        ICE!("Expected StringLiteral");
    };

    // Verify that the array is an array of characters, and also check their corresponding lengths. This also
    // truncates the string literal if necessary.
    check_array_string_initializer(&full_expr_type, element_type, *count, full_expr, chk, driver)?;

    let constant_value = evaluate_constant_full_expression(full_expr, chk, driver)?;

    let AstConstantValue::String { mut ascii, .. } = constant_value else {
        ICE!("Should have evaluated to an AstConstantValue::String");
    };

    if ascii.is_empty() {
        Ok(AstStaticStorageInitializer::ZeroBytes(*count))
    } else {
        // If necessary, append zeros to fit the array length.
        //
        let array_length = *count;
        debug_assert!(array_length >= ascii.len());

        let mut append_zero_count = array_length - ascii.len();

        while append_zero_count > 0 {
            ascii.push("\\000".to_string());
            append_zero_count -= 1;
        }

        Ok(AstStaticStorageInitializer::String { ascii })
    }
}

/// Evaluates a variable initializer expression for a static storage variable which evaluates as a scalar value.
fn evaluate_static_storage_scalar_initializer(
    full_expr: &mut AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstStaticStorageInitializer> {
    let constant_value = evaluate_constant_full_expression(full_expr, chk, driver)?;

    // If the constant value is zero then we return a special marker type, which tracks the size in bytes of the
    // required zero value (e.g. 4 bytes for a zero 'int', 8 bytes for a zero 'double' or pointer). This helps us
    // later during codegen because we can save space by using a dedicated directive for zero.
    if constant_value.is_zero() {
        let ty = constant_value.get_ast_type();
        return Ok(AstStaticStorageInitializer::ZeroBytes(ty.bits() / 8));
    }

    // If the constant value's type matches the required full expression type then we're done.
    let constant_value_type = constant_value.get_ast_type();
    let full_expr_type = chk.get_data_type(&full_expr.node_id);
    if constant_value_type == full_expr_type {
        return Ok(AstStaticStorageInitializer::from(constant_value));
    }

    let original_value = constant_value.clone();

    // Cast the constant value to the full expression's type and emit a warning.
    //      If the full expression's type is a pointer then we cast the constant value to u64.
    //
    let casted_value = if full_expr_type.is_pointer() {
        constant_value.cast_to(&AstType::UnsignedLong)
    } else {
        constant_value.cast_to(&full_expr_type)
    };

    if casted_value != original_value {
        let old_value = original_value.to_string();
        let new_value = casted_value.to_string();
        let sign_change = constant_value_type.is_integer()
            && full_expr_type.is_integer()
            && !constant_value_type.same_signedness(&full_expr_type);

        let loc = chk.metadata.get_source_location(&full_expr.node_id);

        Warning::constant_conversion(
            &constant_value_type,
            &full_expr_type,
            &old_value,
            &new_value,
            sign_change,
            loc,
            driver,
        );
    }

    Ok(AstStaticStorageInitializer::from(casted_value))
}

/// Attempts to evaluate the constant full expression, and emits an error if the expression cannot be evaluated
/// at compile-time.
fn evaluate_constant_full_expression(
    full_expr: &AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstConstantValue> {
    let constant_value = constant_eval::evaluate_constant_full_expr(full_expr, chk, driver);

    if constant_value.is_none() {
        let loc = chk.metadata.get_source_location(&full_expr.node_id);
        let err = "Initializer must be a constant expression for a variable with static storage duration".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(TypeCheckError);
    }

    constant_value.ok_or(TypeCheckError)
}

/// Converts the given expression to the given target type by wrapping it in a cast, unless it already has the
/// desired type, or emits an error if the conversion is not valid.
fn convert_expression_type(
    expr_node_id: &AstNodeId, // In case `expr` is from a larger full-expression.
    expr: &mut AstExpression,
    target_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstExpression> {
    let expr_type = chk.get_data_type(&expr.node_id());

    if *target_type == expr_type {
        return Ok(utils::take_expression(expr));
    }

    // Ensure the conversion is valid.
    check_conversion(expr_node_id, expr, &expr_type, target_type, chk, driver)?;

    // Take ownership of the expression (by replacing with a 'null' value, which will never be used).
    let original_expr = utils::take_expression(expr);

    // Cast the expression to the target type
    Ok(chk.add_cast(target_type, Box::new(original_expr), CastWarningPolicy::WarnOnImplicitConversion, driver))
}

/// Checks whether a conversion from a `source_type` to a `dest_type` is valid; emits a diagnostic if not.
fn check_conversion(
    source_expr_node_id: &AstNodeId, // In case `source_expr` is from a larger full-expression.
    source_expr: &AstExpression,
    source_type: &AstType,
    dest_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<()> {
    // A function type should decay/implicitly convert to a function pointer.
    let source_type =
        if source_type.is_function() { &AstType::new_pointer_to(source_type.clone()) } else { source_type };

    if source_type == dest_type {
        return Ok(());
    }

    if dest_type.is_arithmetic() && source_type.is_pointer() {
        let loc = chk.metadata.get_source_location(source_expr_node_id);
        Error::incompatible_pointer_to_arithmetic_conversion(source_type, dest_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_function_pointer() && source_type.is_function_pointer() {
        let loc = chk.metadata.get_source_location(source_expr_node_id);
        Error::incompatible_fn_pointer_types(dest_type, source_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_pointer() && source_type.is_pointer() {
        let loc = chk.metadata.get_source_location(source_expr_node_id);
        Error::incompatible_pointer_types(dest_type, source_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_pointer()
        && source_type.is_arithmetic()
        && !utils::is_null_pointer_constant(source_expr, dest_type, chk, driver)
    {
        let loc = chk.metadata.get_source_location(source_expr_node_id);
        Error::incompatible_arithmetic_to_pointer_conversion(source_type, dest_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_array() && source_type.is_scalar() {
        let loc = chk.metadata.get_source_location(source_expr_node_id);
        Error::cannot_initialize_array_with_scalar(source_type, loc, driver);
        return Err(TypeCheckError);
    }

    Ok(())
}

/// Checks whether an array of the given `array_type` can be initialized with the given string initializer,
/// and, if necessary, also truncates the string to the array's length.
fn check_array_string_initializer(
    array_type: &AstType,
    array_element_type: &AstType,
    array_length: usize,
    initializer_full_expr: &mut AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<()> {
    let AstExpression::StringLiteral { node_id, ascii, .. } = &mut initializer_full_expr.expr else {
        ICE!("Expected a StringLiteral");
    };

    // Must be an array of characters
    //
    if !array_element_type.is_character() {
        let loc = chk.metadata.get_source_location(node_id);
        Error::cannot_initialize_array_with_string_literal(array_type, loc, driver);
        return Err(TypeCheckError);
    }

    // Warn if the string literal is too long for the array, and then truncate it.
    //
    //      char a[5] = "test";   -->   {'t', 'e', 's', 't', 0}
    //      char b[4] = "test";   -->   {'t', 'e', 's', 't'}
    //      char c[3] = "test";   -->   {'t', 'e', 's'}
    //
    if ascii.len() > array_length {
        let loc = chk.metadata.get_source_location(node_id);
        Warning::initializer_string_too_long_for_array(array_type, loc, driver);

        ascii.truncate(array_length);
    }

    Ok(())
}

/// Makes an `AstVariableInitializer` with a constant zero expression (or an aggregate of constant zeros).
fn make_zero_initializer(
    element_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstVariableInitializer> {
    if let AstType::Array { element_type: array_elem_type, count } = element_type {
        let node_id = AstNodeId::new();

        let mut init = Vec::with_capacity(*count);
        for _ in 0..*count {
            init.push(make_zero_initializer(array_elem_type, chk, driver)?);
        }

        chk.set_data_type(&node_id, element_type);

        Ok(AstVariableInitializer::Aggregate { node_id, init })
    } else if element_type.is_scalar() {
        // If necessary, this 'int' will get promoted to the `element_type`.
        let mut constant_zero = AstFullExpression::new(AstExpression::new_int_literal(0));

        _ = typecheck_full_expression_for_result_type(&mut constant_zero, element_type, chk, driver)?;

        Ok(AstVariableInitializer::Scalar(constant_zero))
    } else {
        ICE!("Did not make a zero initializer for '{element_type}'")
    }
}
