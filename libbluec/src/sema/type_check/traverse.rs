// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `traverse` module defines recursive type checking functions which traverse the AST.

use super::super::constant_eval;
use super::super::type_resolution;
use super::checker::{TypeCheckError, TypeCheckResult, TypeChecker};
use super::symbols;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::errors::Error;
use crate::compiler_driver::warnings::Warning;
use crate::parser::{self, AstUnaryOp};
use crate::parser::{
    AstAssignmentOp, AstBinaryOp, AstBlock, AstBlockItem, AstConstantValue, AstDeclaration, AstExpression,
    AstForInitializer, AstFullExpression, AstNodeId, AstStatement, AstStorageDuration, AstType, AstUniqueName,
};

/// Traverses the AST and performs type checking.
pub fn typecheck_ast(ast_root: &mut parser::AstRoot, chk: &mut TypeChecker, driver: &mut Driver) {
    _ = typecheck_declarations(&mut ast_root.0, chk, driver);
}

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

fn typecheck_declaration(decl: &mut AstDeclaration, chk: &mut TypeChecker, driver: &mut Driver) -> TypeCheckResult<()> {
    match decl {
        AstDeclaration::Variable(decl) => {
            if decl.is_file_scope {
                symbols::verify_file_scope_variable_declaration(decl, &mut chk.symbols, driver)?;
            } else {
                symbols::verify_local_variable_declaration(decl, &mut chk.symbols, driver)?;
            }

            let var_type =
                decl.declared_type.resolved_type.as_ref().expect("Expected variable decl type to be resolved");

            // Typecheck the initializer expression, if it exists.
            //
            if let Some(init_expr) = &mut decl.init_expr {
                typecheck_full_expression_for_result_type(init_expr, var_type, chk, driver)?;

                // An initializer expression for a static storage duration variable must be a constant expression, so
                // we'll verify that by trying to evaluate it.
                //
                if decl.storage == AstStorageDuration::Static {
                    let constant_value = evaluate_static_storage_initializer(var_type, init_expr, chk, driver)?;
                    decl.init_constant_value = Some(constant_value);
                }
            }
        }

        parser::AstDeclaration::Function(function) => {
            symbols::verify_function_declaration(function, &mut chk.symbols, driver)?;

            let fn_type = function.declared_type.resolved_type.as_ref().expect("Expected function type to be resolved");
            let (return_type, params) = extract_fn_return_and_param_types(fn_type);
            let param_names = &function.param_names;

            let valid_params = params.iter().zip(param_names).all(|(param_type, (param_ident, param_unique))| {
                symbols::verify_function_parameter_declaration(
                    param_unique,
                    param_ident,
                    param_type,
                    &mut chk.symbols,
                    driver,
                )
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

        parser::AstDeclaration::TypeAlias(alias_decl) => {
            symbols::verify_type_alias_declaration(alias_decl, chk, driver)?;
        }
    }

    Ok(())
}

fn typecheck_block(block: &mut AstBlock, chk: &mut TypeChecker, driver: &mut Driver) -> TypeCheckResult<()> {
    // Create a new scope so that we can track typedef declarations.
    //
    chk.begin_declaration_scope();

    for item in &mut block.0 {
        match item {
            AstBlockItem::Declaration(decl) => typecheck_declaration(decl, chk, driver)?,
            AstBlockItem::Statement(stmt) => typecheck_statement(stmt, chk, driver)?,
        }
    }

    chk.end_declaration_scope();

    Ok(())
}

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

        AstStatement::Switch { controlling_expr, body, .. } => {
            let controlling_expr_type = typecheck_full_expression(controlling_expr, chk, driver)?;
            if !controlling_expr_type.is_integer() {
                let err =
                    format!("Switch statement requires an integer expression ('{}' is invalid)", controlling_expr_type);
                let loc = chk.metadata.get_source_span_as_loc(&controlling_expr.node_id).unwrap();
                driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
            }
            typecheck_statement(body, chk, driver)?;
        }

        AstStatement::Case { stmt, .. } => typecheck_statement(stmt, chk, driver)?,

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
            let init: &mut parser::AstForInitializer = init;

            match init {
                AstForInitializer::Declaration(declarations) => {
                    // TODO: C standard specifies only objects of automatic/register storage can be declared
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

fn typecheck_full_expression_for_result_type(
    full_expr: &mut AstFullExpression,
    result_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let expr_type = typecheck_full_expression(full_expr, chk, driver)?;

    // If the full expression's type doesn't match the given result type then add a cast.
    //
    if &expr_type != result_type {
        let converted_expr = convert_expression_type(&mut full_expr.expr, result_type, chk, driver)?;

        // Re-write the full expression to use the converted expression
        *full_expr = AstFullExpression { node_id: full_expr.node_id, expr: converted_expr };
    }

    Ok(expr_type)
}

fn typecheck_full_expression(
    full_expr: &mut AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let inner_expr_data_type = typecheck_expression(&mut full_expr.expr, None, chk, driver)?;

    // The full expression itself has the same type as its inner expression root.
    chk.set_data_type(&full_expr.node_id, &inner_expr_data_type);

    Ok(inner_expr_data_type)
}

/// The context of a parent expression.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ParentExprCtx {
    SuppressDecay,
}

fn typecheck_expression(
    expr: &mut AstExpression,
    parent_ctx: Option<ParentExprCtx>,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstType> {
    let parent_ctx_suppresses_decay = parent_ctx.is_some_and(|ctx| ctx == ParentExprCtx::SuppressDecay);

    match expr {
        AstExpression::Identifier { node_id, unique_name, .. } => {
            // Record that the symbol has been used.
            chk.symbols.set_symbol_used(unique_name);

            let symbol = chk.symbols.get(&unique_name).unwrap();
            let symbol_type = symbol.data_type.clone();

            // If the identifier is a function designator of function type and the parent expression is not preventing
            // a decay (implicit conversion) then the type of the function designator is a function pointer.
            // Some parent expressions (like AddressOf) prevent a decay.
            //
            if let AstType::Function { .. } = symbol_type
                && !parent_ctx_suppresses_decay
            {
                let fn_ptr_type = AstType::new_pointer_to(symbol_type);
                chk.set_data_type(node_id, &fn_ptr_type);
                return Ok(fn_ptr_type);
            }

            chk.set_data_type(node_id, &symbol_type);
            Ok(symbol_type)
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

        AstExpression::Cast { node_id, target_type, expr } => {
            let expr_type = typecheck_expression(expr, None, chk, driver)?;

            let target_resolved_type =
                type_resolution::resolve_declared_type(target_type, Some(&mut chk.symbols), Some(driver))
                    .map_err(|_| TypeCheckError)?;

            target_type.resolved_type = Some(target_resolved_type);

            let cast_to_type = target_type.resolved_type.clone().unwrap();

            // Warn if casting a pointer to an integer type smaller than pointer size.
            if expr_type.is_pointer() && cast_to_type.is_integer() && cast_to_type.bits() < expr_type.bits() {
                let cast_op_loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                let expr_loc = chk.metadata.get_source_span_as_loc(&expr.node_id()).unwrap();

                Warning::pointer_to_smaller_int_cast(&expr_type, &cast_to_type, cast_op_loc, expr_loc, driver);
            }

            // Cannot cast to a function type
            if cast_to_type.is_function() {
                let cast_op_loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                let expr_loc = chk.metadata.get_source_span_as_loc(&expr.node_id()).unwrap();

                Error::cannot_cast_to_function_type(cast_op_loc, expr_loc, &cast_to_type, driver);
            }

            // Cannot cast between pointer and floating-point types.
            if cast_to_type.is_pointer() && expr_type.is_floating_point()
                || cast_to_type.is_floating_point() && expr_type.is_pointer()
            {
                let expr_loc = chk.metadata.get_source_span_as_loc(&expr.node_id()).unwrap();
                Error::invalid_cast(expr_loc, &expr_type, &cast_to_type, driver);
            }

            // Future: Warn about UB casting from a negative float literal to an unsigned int, e.g. `(unsigned int)-1`

            chk.set_data_type(node_id, &cast_to_type);
            Ok(cast_to_type)
        }

        AstExpression::Deref { node_id, expr } => {
            let expr_type = typecheck_expression(expr, None, chk, driver)?;

            if let AstType::Pointer(referenced) = expr_type {
                chk.set_data_type(node_id, &referenced);
                Ok(*referenced)
            }
            // A function type can be dereferenced
            else if let AstType::Function { .. } = expr_type {
                chk.set_data_type(node_id, &expr_type);
                Ok(expr_type)
            } else {
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::indirection_requires_pointer_type(&expr_type, loc, driver);
                Err(TypeCheckError) // Can't set data type, so must abort.
            }
        }

        AstExpression::AddressOf { node_id, expr } => {
            let expr_type = typecheck_expression(expr, Some(ParentExprCtx::SuppressDecay), chk, driver)?;
            if expr.is_lvalue() {
                let ptr_type = AstType::Pointer(Box::new(expr_type));
                chk.set_data_type(node_id, &ptr_type);
                Ok(ptr_type)
            } else {
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::cannot_take_address_of_rvalue(&expr_type, loc, driver);
                Err(TypeCheckError) // Can't set data type, so must abort.
            }
        }

        AstExpression::UnaryOperation { .. } => typecheck_unary_operation(expr, chk, driver),

        AstExpression::BinaryOperation { .. } => typecheck_binary_operation(expr, chk, driver),

        AstExpression::Assignment { .. } => typecheck_assignment(expr, chk, driver),

        AstExpression::Conditional { .. } => typecheck_conditional(expr, chk, driver),

        AstExpression::FunctionCall { .. } => typecheck_function_call(expr, chk, driver),
    }
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
    if *op == parser::AstUnaryOp::LogicalNot {
        typecheck_expression(operand_expr, None, chk, driver)?;
        chk.set_data_type(node_id, &AstType::Int);
        return Ok(AstType::Int);
    }

    // Otherwise the unary operation takes the type of its operand expression
    let operand_type = typecheck_expression(operand_expr, Some(ParentExprCtx::SuppressDecay), chk, driver)?;

    if operand_type.is_pointer() && unary_operator_incompatible_with_ptr_operand(*op) {
        let err = format!("Unary operation is invalid on operand of type '{operand_type}'");
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    if operand_type.is_function() && unary_operator_incompatible_with_fn_operand(*op) {
        let err = format!("Unary operation is invalid on operand of type '{operand_type}'");
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }

    if *op == parser::AstUnaryOp::BitwiseNot && operand_type.is_floating_point() {
        let err = "Cannot take the bitwise complement of a floating-point type".to_string();
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
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

    let left_type = typecheck_expression(left, None, chk, driver)?;
    let right_type = typecheck_expression(right, None, chk, driver)?;

    // LogicalAnd `&&` and LogicalOr `||` evaluate to a value of type 'int' so we don't need to cast the operands.
    if *op == AstBinaryOp::LogicalAnd || *op == AstBinaryOp::LogicalOr {
        chk.set_data_type(node_id, &AstType::Int);
        return Ok(AstType::Int);
    }

    // Various binary operators cannot have an operand of pointer type, which includes operand of function type
    // because the function type decays into a function pointer type.
    //
    if (left_type.is_pointer() || right_type.is_pointer() || left_type.is_function() || right_type.is_function())
        && binary_operator_incompatible_with_ptr_operand(*op)
    {
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
        Error::invalid_binary_expression_operands(&left_type, &right_type, loc, driver);
        return Err(TypeCheckError);
    }

    // Various binary operators cannot have floating-point operands
    let has_fp_operand = left_type.is_floating_point() || right_type.is_floating_point();
    if has_fp_operand && binary_operator_incompatible_with_fp_operand(*op) {
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
        Error::invalid_binary_expression_operands(&left_type, &right_type, loc, driver);
    }

    // Take ownership of the left and right expressions (by replacing them with a 'null' value, which will never be used).
    let left = take_boxed_expression(left);
    let right = take_boxed_expression(right);

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
        match get_common_type_for_expressions(&left, &right, chk, driver) {
            Ok(common_type) => common_type,
            Err(e) => match e {
                CommonTypeError::WarnDifferentPointerTypes { a_type, b_type } => {
                    warn_compare_different_pointer_types(node_id, &left, &right, &a_type, &b_type, chk, driver);
                    AstType::Pointer(Box::new(AstType::Void))
                }

                CommonTypeError::WarnPointerAndInteger { a_type, b_type } => {
                    warn_compare_pointer_and_integer(node_id, &left, &right, &a_type, &b_type, chk, driver);
                    if a_type.is_pointer() { a_type } else { b_type }
                }

                CommonTypeError::NoCommonType { a_type, b_type } => {
                    let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
                    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
                    Error::incompatible_types(&a_type, &b_type, loc, a_loc, b_loc, driver);

                    a_type
                }
            },
        }
    } else {
        left_type.promote_if_rank_lower_than_int()
    };

    // If necessary, cast both operands to the binary operation's data type.
    //      Above, we picked either their common type or the lhs type based on the operator.
    //      But we still cast the lhs to the data type because the lhs may have been promoted to 'int'
    //      if it was a smaller integer type (_Bool, char, short).
    //
    let left = chk.wrap_in_cast_if_neeeded(&operand_data_type, left, driver);
    let right = chk.wrap_in_cast_if_neeeded(&operand_data_type, right, driver);

    // Determine the data type of the binary operation itself
    let binary_op_data_type = if op.is_relational() { AstType::Int } else { operand_data_type };

    // Set the data type of the binary operation
    chk.set_data_type(node_id, &binary_op_data_type);

    // Update the binary operation to use the new left and right expresssions
    *expr = AstExpression::BinaryOperation { node_id: *node_id, op: *op, left, right };

    Ok(binary_op_data_type)
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

    let lhs_type = typecheck_expression(lhs, Some(ParentExprCtx::SuppressDecay), chk, driver)?;
    let rhs_type = typecheck_expression(rhs, None, chk, driver)?;

    // The lhs must be a modifiable lvalue.
    if !is_modifiable_lvalue(lhs, &lhs_type) {
        let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
        Error::expression_is_not_assignable(loc, driver);
    }

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
            && assignment_operator_incompatible_with_ptr_operand(*op)
        {
            let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
            Error::invalid_binary_expression_operands(&lhs_type, &rhs_type, loc, driver);
            return Err(TypeCheckError);
        }

        // Some compound assignment operators cannot have floating-point operands
        let has_fp_operand = lhs_type.is_floating_point() || rhs_type.is_floating_point();
        if has_fp_operand && assignment_operator_incompatible_with_fp_operand(*op) {
            let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
            Error::invalid_binary_expression_operands(&lhs_type, &rhs_type, loc, driver);
        }

        // There are strict rules about types for `+=` and `-=`.
        if matches!(op, AstAssignmentOp::Addition | AstAssignmentOp::Subtraction) {
            let valid = (lhs_type.is_arithmetic() && rhs_type.is_arithmetic())
                || (lhs_type.is_pointer() && rhs_type.is_integer());

            if !valid {
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::invalid_binary_expression_operands(&lhs_type, &rhs_type, loc, driver);
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

        // Work out the common type for the compound computation (e.g. `lhs + rhs` if the op is `+=`).
        let computation_type = match get_common_type_for_expressions(lhs, rhs, chk, driver) {
            Ok(common_type) => common_type,
            Err(e) => match e {
                // TODO: change warn/error to assignment versions
                CommonTypeError::WarnDifferentPointerTypes { a_type, b_type } => {
                    warn_compare_different_pointer_types(node_id, lhs, rhs, &a_type, &b_type, chk, driver);
                    AstType::Pointer(Box::new(AstType::Void))
                }

                CommonTypeError::WarnPointerAndInteger { a_type, b_type } => {
                    warn_compare_pointer_and_integer(node_id, lhs, rhs, &a_type, &b_type, chk, driver);
                    if a_type.is_pointer() { a_type } else { b_type }
                }

                CommonTypeError::NoCommonType { a_type, b_type } => {
                    let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                    let a_loc = chk.metadata.get_source_span_as_loc(&lhs.node_id()).unwrap();
                    let b_loc = chk.metadata.get_source_span_as_loc(&rhs.node_id()).unwrap();
                    Error::incompatible_types(&a_type, &b_type, loc, a_loc, b_loc, driver);

                    a_type
                }
            },
        };

        // Verify that the conversion is valid (but don't apply the conversion; see above, we do that in translation)
        check_conversion(lhs, &rhs_type, &computation_type, chk, driver)?;

        chk.set_data_type(node_id, &lhs_type);
        chk.set_data_type(computation_node_id, &computation_type);

        return Ok(lhs_type);
    }

    let original_lhs = take_boxed_expression(lhs);
    let converted_rhs = convert_expression_type(rhs, &lhs_type, chk, driver)?;

    // Data type of an assignment is the lhs type
    chk.set_data_type(node_id, &lhs_type);

    // Re-write the original assignment expression and insert the possibly-casted rhs.
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

    typecheck_expression(condition, None, chk, driver)?;
    typecheck_expression(consequent, None, chk, driver)?;
    typecheck_expression(alternative, None, chk, driver)?;

    let common_type = match get_common_type_for_expressions(consequent, alternative, chk, driver) {
        Ok(common_type) => common_type,
        Err(e) => match e {
            CommonTypeError::WarnDifferentPointerTypes { a_type, b_type } => {
                warn_pointer_type_mismatch(node_id, consequent, alternative, &a_type, &b_type, chk, driver);
                AstType::Pointer(Box::new(AstType::Void))
            }

            CommonTypeError::WarnPointerAndInteger { a_type, b_type } => {
                warn_conditional_type_mismatch(node_id, consequent, alternative, &a_type, &b_type, chk, driver);
                if a_type.is_pointer() { a_type } else { b_type }
            }

            CommonTypeError::NoCommonType { a_type, b_type } => {
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                let a_loc = chk.metadata.get_source_span_as_loc(&consequent.node_id()).unwrap();
                let b_loc = chk.metadata.get_source_span_as_loc(&alternative.node_id()).unwrap();
                Error::incompatible_types(&a_type, &b_type, loc, a_loc, b_loc, driver);

                a_type
            }
        },
    };

    // Take ownership of the expressions (by replacing with a 'null' value, which will never be used).
    let original_condition = take_boxed_expression(condition);
    let original_consequent = take_boxed_expression(consequent);
    let original_alternative = take_boxed_expression(alternative);

    // Wrap each operand in a Cast
    let casted_consequent = chk.wrap_in_cast_if_neeeded(&common_type, original_consequent, driver);
    let casted_alternative = chk.wrap_in_cast_if_neeeded(&common_type, original_alternative, driver);

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
    _ = typecheck_expression(designator, Some(ParentExprCtx::SuppressDecay), chk, driver)?;

    // Get the type of the function designator (function type or function pointer type) and the optional name
    // of the function. The name might be `None` if the designator is a function pointer. We only need the name
    // for diagnostics.
    //
    let (unique_name, designator_type) = get_function_designator_type(designator, chk, driver)?;

    // Verify the designator is a function or a function pointer.
    let valid_designator = designator_type.is_function() || designator_type.is_function_pointer();
    if !valid_designator {
        let loc = chk.metadata.get_source_span_as_loc(&designator.node_id()).unwrap();
        Error::invalid_call_type(&designator_type, loc, driver);
        return Err(TypeCheckError);
    }

    // Get the function's return type and parameter types
    let (return_type, params) = if designator_type.is_function_pointer() {
        let AstType::Pointer(referent) = &designator_type else {
            ICE!("Expected an AstType::Pointer");
        };

        extract_fn_return_and_param_types(referent)
    } else {
        extract_fn_return_and_param_types(&designator_type)
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
                _ = typecheck_expression(arg, None, chk, driver).ok()?;

                let converted_arg = convert_expression_type(arg, param_type, chk, driver).ok()?;

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
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
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
                type_resolution::resolve_declared_type(target_type, Some(&mut chk.symbols), Some(driver))
                    .map_err(|_| TypeCheckError)?;

            Ok((unique_name, target_resolved_type))
        }

        AstExpression::AddressOf { expr, .. } => {
            let (unique_name, expr_type) = get_function_designator_type(expr, chk, driver)?;
            let fn_ptr_type = AstType::Pointer(Box::new(expr_type));
            Ok((unique_name, fn_ptr_type))
        }

        AstExpression::UnaryOperation { node_id, op, expr } => {
            if parser::expr::ops::is_incr_or_decr_op(op) {
                let (unique_name, expr_type) = get_function_designator_type(expr, chk, driver)?;
                Ok((unique_name, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::BinaryOperation { node_id, op, left, .. } => {
            if matches!(op, AstBinaryOp::Add | AstBinaryOp::Subtract) {
                let (unique_name, expr_type) = get_function_designator_type(left, chk, driver)?;
                Ok((unique_name, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::Assignment { node_id, op, lhs, .. } => {
            if matches!(op, AstAssignmentOp::Assignment | AstAssignmentOp::Addition | AstAssignmentOp::Subtraction) {
                let (unique_name, expr_type) = get_function_designator_type(lhs, chk, driver)?;
                Ok((unique_name, expr_type))
            } else {
                let call_type = chk.get_data_type(node_id);
                let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
                Error::invalid_call_type(&call_type, loc, driver);
                Err(TypeCheckError)
            }
        }

        AstExpression::FunctionCall { .. } => {
            let ret_type = typecheck_function_call(expr, chk, driver)?;
            Ok((None, ret_type))
        }

        AstExpression::IntegerLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            Ok((None, expr_type))
        }

        AstExpression::FloatLiteral { node_id, .. } => {
            let expr_type = chk.get_data_type(node_id);
            let loc = chk.metadata.get_source_span_as_loc(node_id).unwrap();
            Error::invalid_call_type(&expr_type, loc, driver);
            Err(TypeCheckError)
        }
    }
}

fn evaluate_static_storage_initializer(
    var_type: &AstType,
    init_full_expr: &AstFullExpression,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstConstantValue> {
    let constant_value = constant_eval::evaluate_constant_full_expr(init_full_expr, Some(chk));

    let constant_value = match constant_value {
        Some(val) => val,

        None => {
            let loc = chk.metadata.get_source_span_as_loc(&init_full_expr.node_id).unwrap();
            let err =
                "Initializer must be a constant expression for a variable with static storage duration".to_string();
            driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
            return Err(TypeCheckError);
        }
    };

    let constant_value_type = constant_value.get_ast_type();

    if constant_value_type == *var_type {
        return Ok(constant_value);
    }

    let original_value = constant_value.clone();

    // Cast the constant value to the variable's type and emit a warning.
    //      If the variable's type is a pointer then we cast the constant value to u64.
    //
    let casted_value = if var_type.is_pointer() {
        constant_value.cast_to(&AstType::UnsignedLong)
    } else {
        constant_value.cast_to(var_type)
    };

    if casted_value != original_value {
        let old_value = original_value.to_string();
        let new_value = casted_value.to_string();
        let sign_change =
            constant_value_type.is_integer() && var_type.is_integer() && !constant_value_type.same_signedness(var_type);

        let loc = chk.metadata.get_source_span_as_loc(&init_full_expr.node_id).unwrap();

        Warning::implicit_conversion(&constant_value_type, var_type, &old_value, &new_value, sign_change, loc, driver);
    }

    Ok(casted_value)
}

/// Converts the given expression to the given target type by wrapping it in a cast, unless it already has the
/// desired type, or emits an error if the conversion is not valid.
fn convert_expression_type(
    expr: &mut AstExpression,
    target_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> TypeCheckResult<AstExpression> {
    let expr_type = chk.get_data_type(&expr.node_id());

    if *target_type == expr_type {
        return Ok(take_expression(expr));
    }

    // Ensure the conversion is valid.
    check_conversion(expr, &expr_type, target_type, chk, driver)?;

    // Take ownership of the expression (by replacing with a 'null' value, which will never be used).
    let original_expr = take_expression(expr);

    // Cast the expression to the target type
    Ok(chk.wrap_in_cast(target_type, Box::new(original_expr), driver))
}

fn check_conversion(
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
        let loc = chk.metadata.get_source_span_as_loc(&source_expr.node_id()).unwrap();
        Error::incompatible_pointer_to_arithmetic_conversion(source_type, dest_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_function_pointer() && source_type.is_function_pointer() {
        let loc = chk.metadata.get_source_span_as_loc(&source_expr.node_id()).unwrap();
        Error::incompatible_fn_pointer_types(dest_type, source_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_pointer() && source_type.is_pointer() {
        let loc = chk.metadata.get_source_span_as_loc(&source_expr.node_id()).unwrap();
        Error::incompatible_pointer_types(dest_type, source_type, loc, driver);
        return Err(TypeCheckError);
    }

    if dest_type.is_pointer()
        && source_type.is_arithmetic()
        && !is_null_pointer_constant(source_expr, dest_type, chk, driver)
    {
        let loc = chk.metadata.get_source_span_as_loc(&source_expr.node_id()).unwrap();
        Error::incompatible_arithmetic_to_pointer_conversion(source_type, dest_type, loc, driver);
        return Err(TypeCheckError);
    }

    Ok(())
}

enum CommonTypeError {
    WarnDifferentPointerTypes { a_type: AstType, b_type: AstType },
    WarnPointerAndInteger { a_type: AstType, b_type: AstType },
    NoCommonType { a_type: AstType, b_type: AstType },
}

/// Gets the common `AstType` for the types of the two given expressions.
fn get_common_type_for_expressions(
    a: &AstExpression,
    b: &AstExpression,
    chk: &TypeChecker,
    driver: &mut Driver,
) -> Result<AstType, CommonTypeError> {
    let a_type = chk.get_data_type(&a.node_id());
    let b_type = chk.get_data_type(&b.node_id());

    // A function type should decay/implicitly convert to a function pointer.
    //
    let a_type = if a_type.is_function() { AstType::new_pointer_to(a_type) } else { a_type };
    let b_type = if b_type.is_function() { AstType::new_pointer_to(b_type) } else { b_type };

    // If either (or both) types are pointers then we handle the types differently, including detecting an
    // expression of integer type that evaluates to a null pointer constant.
    //
    if a_type.is_pointer() || b_type.is_pointer() {
        if a_type == b_type {
            return Ok(a_type);
        }

        // If both types are pointers but not the same (see above), ask the caller to emit a warning.
        if a_type.is_pointer() && b_type.is_pointer() {
            return Err(CommonTypeError::WarnDifferentPointerTypes { a_type, b_type });
        }

        if is_null_pointer_constant(a, &b_type, chk, driver) {
            return Ok(b_type);
        }

        if is_null_pointer_constant(b, &a_type, chk, driver) {
            return Ok(a_type);
        }

        // If one of the types is an integer (and not a null pointer constant) then ask the caller to emit a warning.
        if a_type.is_integer() || b_type.is_integer() {
            return Err(CommonTypeError::WarnPointerAndInteger { a_type, b_type });
        }

        return Err(CommonTypeError::NoCommonType { a_type, b_type });
    }

    // Get the common type and then promote to `int` if the common type is smaller, like _Bool, char or short.
    Ok(AstType::get_common_type(&a_type, &b_type).promote_if_rank_lower_than_int())
}

fn is_null_pointer_constant(expr: &AstExpression, ptr_type: &AstType, chk: &TypeChecker, driver: &mut Driver) -> bool {
    match expr {
        AstExpression::IntegerLiteral { value, .. } => *value == 0,
        _ => match constant_eval::evaluate_constant_expr(expr, Some(chk)) {
            Some(const_value) if const_value.is_zero() && const_value.get_ast_type().is_integer() => {
                let loc = chk.metadata.get_source_span_as_loc(&expr.node_id()).unwrap();
                Warning::expression_interpreted_as_null_ptr_constant(loc, ptr_type, driver);
                true
            }
            _ => false,
        },
    }
}

/// Takes ownership of the given boxed `AstExpression` by replacing it with a 'null' value.
fn take_boxed_expression(expr: &mut Box<AstExpression>) -> Box<AstExpression> {
    std::mem::replace(
        expr,
        Box::new(AstExpression::IntegerLiteral {
            node_id: AstNodeId::null(),
            literal: String::new(),
            literal_base: 10,
            value: 0,
            kind: parser::AstIntegerLiteralKind::Int,
        }),
    )
}

/// Takes ownership of the given `AstExpression` by replacing it with a 'null' value.
fn take_expression(expr: &mut AstExpression) -> AstExpression {
    std::mem::replace(
        expr,
        AstExpression::IntegerLiteral {
            node_id: AstNodeId::null(),
            literal: String::new(),
            literal_base: 10,
            value: 0,
            kind: parser::AstIntegerLiteralKind::Int,
        },
    )
}

/// Is the given unary operator incompatible with pointer operands?
fn unary_operator_incompatible_with_ptr_operand(op: AstUnaryOp) -> bool {
    matches!(op, AstUnaryOp::Negate | AstUnaryOp::Plus | AstUnaryOp::BitwiseNot)
}

/// Is the given unary operator incompatible with an operand of function type?
fn unary_operator_incompatible_with_fn_operand(op: AstUnaryOp) -> bool {
    matches!(
        op,
        AstUnaryOp::Negate
            | AstUnaryOp::Plus
            | AstUnaryOp::BitwiseNot
            | AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PrefixDecrement
            | AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PostfixDecrement
    )
}

/// Is the given binary operator incompatible with a pointer operand?
fn binary_operator_incompatible_with_ptr_operand(op: AstBinaryOp) -> bool {
    matches!(
        op,
        AstBinaryOp::Multiply
            | AstBinaryOp::Divide
            | AstBinaryOp::Remainder
            | AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::LeftShift
            | AstBinaryOp::RightShift
    )
}

/// Is the given assignment operator incompatible with a pointer operand?
fn assignment_operator_incompatible_with_ptr_operand(op: AstAssignmentOp) -> bool {
    matches!(
        op,
        AstAssignmentOp::Multiply
            | AstAssignmentOp::Divide
            | AstAssignmentOp::Remainder
            | AstAssignmentOp::BitwiseAnd
            | AstAssignmentOp::BitwiseOr
            | AstAssignmentOp::BitwiseXor
            | AstAssignmentOp::LeftShift
            | AstAssignmentOp::RightShift
    )
}

/// Is the given binary operator incompatible with floating-point operands?
fn binary_operator_incompatible_with_fp_operand(op: AstBinaryOp) -> bool {
    matches!(
        op,
        AstBinaryOp::BitwiseAnd
            | AstBinaryOp::BitwiseOr
            | AstBinaryOp::BitwiseXor
            | AstBinaryOp::LeftShift
            | AstBinaryOp::RightShift
            | AstBinaryOp::Remainder
    )
}

/// Is the given assignment operator incompatible with floating-point operands?
fn assignment_operator_incompatible_with_fp_operand(op: AstAssignmentOp) -> bool {
    matches!(
        op,
        AstAssignmentOp::BitwiseAnd
            | AstAssignmentOp::BitwiseOr
            | AstAssignmentOp::BitwiseXor
            | AstAssignmentOp::LeftShift
            | AstAssignmentOp::RightShift
            | AstAssignmentOp::Remainder
    )
}

fn warn_compare_different_pointer_types(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::compare_different_pointer_types(left_type, right_type, loc, a_loc, b_loc, driver);
}

fn warn_compare_pointer_and_integer(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let cmp_loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let left_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let right_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();

    let (ptr_type, int_type, ptr_loc, int_loc) = if left_type.is_pointer() {
        (left_type, right_type, left_loc, right_loc)
    } else {
        (right_type, left_type, right_loc, left_loc)
    };

    Warning::compare_pointer_and_integer(ptr_type, int_type, cmp_loc, ptr_loc, int_loc, driver);
}

fn warn_conditional_type_mismatch(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::conditional_type_mismatch(left_type, right_type, loc, a_loc, b_loc, driver);
}

fn warn_pointer_type_mismatch(
    expr_node_id: &AstNodeId,
    left: &AstExpression,
    right: &AstExpression,
    left_type: &AstType,
    right_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) {
    let loc = chk.metadata.get_source_span_as_loc(expr_node_id).unwrap();
    let a_loc = chk.metadata.get_source_span_as_loc(&left.node_id()).unwrap();
    let b_loc = chk.metadata.get_source_span_as_loc(&right.node_id()).unwrap();
    Warning::pointer_type_mismatch(left_type, right_type, loc, a_loc, b_loc, driver);
}

fn extract_fn_return_and_param_types(fn_type: &AstType) -> (&AstType, &Vec<AstType>) {
    let AstType::Function { return_type, params } = fn_type else {
        ICE!("Expected AstType::Function");
    };

    (return_type, params)
}

fn is_modifiable_lvalue(expr: &AstExpression, expr_type: &AstType) -> bool {
    // To be modifiable, the lvalue must designate an object (not a function), and the object cannot be const (future).
    expr.is_lvalue() && !expr_type.is_function()
}
