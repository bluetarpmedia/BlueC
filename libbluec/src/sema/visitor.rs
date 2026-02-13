// Copyright 2025-2026 Neil Henderson
//
//! The `visitor` module provides functionality to visit nodes in the parser's AST.

use crate::parser::{
    AstBlock, AstBlockItem, AstDeclaration, AstExpression, AstExpressionKind, AstForInitializer, AstFunction, AstRoot,
    AstStatement, AstVariableDeclaration, AstVariableInitializer,
};

/// Visits all function definitions in the AST and, for each one, calls `visitor_func` with the [AstFunction].
pub fn visit_function_defns<F>(ast_root: &mut AstRoot, visitor_func: &mut F)
where
    F: FnMut(&mut AstFunction),
{
    for decl in &mut ast_root.0 {
        if let AstDeclaration::Function(function) = decl
            && function.body.is_some()
        {
            visitor_func(function);
        }
    }
}

/// Visits all statements in the given [AstBlock] and, for each one, calls `visitor_func` with the [AstStatement].
pub fn visit_statements_in_block<F>(block: &mut AstBlock, visitor_func: &mut F)
where
    F: FnMut(&mut AstStatement),
{
    let block_items = &mut block.0;
    for block_item in block_items {
        if let AstBlockItem::Statement(stmt) = block_item {
            visit_statements(stmt, visitor_func);
        }
    }
}

/// Visits all full expressions in the AST and, for each one, calls `visitor_func` with the [AstExpression].
///
/// A "full expression" is an expression that is not a sub-expression of another expression. In other words, it is
/// the root of a potential expression tree. If an [AstStatement] has an expression field then that expression is a full
/// expression.
///
/// To visit sub-expressions under the full-[AstExpression], call [visit_sub_expressions].
///
/// ```c
/// if (a > b) { ... }  // If statement; the `a > b` binary expression is a full expression.
/// a = 5 + 10;         // Expression statement; the `a = 5 + 10` assignment expression is a full expression.
/// return -a;          // Return statement; the `-a` unary expression is a full expression.
/// ```
pub fn visit_full_expressions<F>(ast_root: &mut AstRoot, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    for decl in &mut ast_root.0 {
        match decl {
            AstDeclaration::Variable(var_decl) => {
                if let Some(ref mut initializer) = var_decl.initializer {
                    visit_expressions_in_variable_initializer(initializer, visitor_func);
                }
            }

            AstDeclaration::Function(func) => {
                if let Some(ref mut block) = func.body {
                    let block_items = &mut block.0;
                    for block_item in block_items {
                        match block_item {
                            AstBlockItem::Declaration(decl) => {
                                if let AstDeclaration::Variable(var_decl) = decl
                                    && let Some(ref mut initializer) = var_decl.initializer
                                {
                                    visit_expressions_in_variable_initializer(initializer, visitor_func);
                                }
                            }

                            AstBlockItem::Statement(stmt) => visit_statements(stmt, &mut |stmt: &mut AstStatement| {
                                visit_expressions_in_stmt(stmt, visitor_func);
                            }),
                        }
                    }
                }
            }

            AstDeclaration::TypeAlias(_) => (),
        }
    }
}

/// Visits the given [AstExpression] first and then visits all sub-expressions recursively in the given [AstExpression],
/// using pre-order traversal (root, left, right), and, for each one, calls `visitor_func` with the sub-[AstExpression].
pub fn visit_sub_expressions<F>(expr: &mut AstExpression, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    visit_expression_recursive(expr, visitor_func);
}

/// Visits all statements recursively in the given [AstStatement] and, for each one, calls `visitor_func` with
/// the [AstStatement].
pub fn visit_statements<F>(stmt: &mut AstStatement, visitor_func: &mut F)
where
    F: FnMut(&mut AstStatement),
{
    visitor_func(stmt);

    match stmt {
        AstStatement::Labeled { stmt: labeled_stmt, .. } => {
            visit_statements(labeled_stmt, visitor_func);
        }

        AstStatement::Compound(block) => visit_statements_in_block(block, visitor_func),

        AstStatement::If { then_stmt, else_stmt, .. } => {
            visit_statements(then_stmt, visitor_func);

            if let Some(else_stmt) = else_stmt {
                visit_statements(else_stmt, visitor_func);
            }
        }

        AstStatement::Switch { body, .. } => {
            visit_statements(body, visitor_func);
        }

        AstStatement::Case { stmt: inner_stmt, .. } => {
            visit_statements(inner_stmt, visitor_func);
        }

        AstStatement::Default { stmt: inner_stmt, .. } => {
            visit_statements(inner_stmt, visitor_func);
        }

        AstStatement::While { body, .. } => {
            visit_statements(body, visitor_func);
        }

        AstStatement::DoWhile { body, .. } => {
            visit_statements(body, visitor_func);
        }

        AstStatement::For { body, .. } => {
            visit_statements(body, visitor_func);
        }

        _ => (), // Already visited the statement above, and these statements have no child statements
    }
}

/// Visits all variable declarations in the AST and, for each one, calls `visitor_func` with the
/// [AstVariableDeclaration].
///
/// Variable declarations may exist at file scope or block scope.
pub fn visit_variable_declarations<F>(ast_root: &mut AstRoot, visitor_func: &mut F)
where
    F: FnMut(&mut AstVariableDeclaration),
{
    for decl in &mut ast_root.0 {
        match decl {
            AstDeclaration::Variable(var_decl) => {
                visitor_func(var_decl);
            }

            AstDeclaration::Function(func) => {
                if let Some(ref mut block) = func.body {
                    let block_items = &mut block.0;
                    for block_item in block_items {
                        match block_item {
                            AstBlockItem::Declaration(decl) => {
                                if let AstDeclaration::Variable(var_decl) = decl {
                                    visitor_func(var_decl);
                                }
                            }

                            AstBlockItem::Statement(stmt) => visit_statements(stmt, &mut |stmt: &mut AstStatement| {
                                // The only statement that can contain variable initializers is 'for'.
                                //
                                if let AstStatement::For { init, .. } = stmt
                                    && let AstForInitializer::Declaration(decls) = init.as_mut()
                                    && !decls.is_empty()
                                {
                                    for decl in decls {
                                        if let AstDeclaration::Variable(var_decl) = decl {
                                            visitor_func(var_decl);
                                        }
                                    }
                                }
                            }),
                        }
                    }
                }
            }

            AstDeclaration::TypeAlias(_) => (),
        }
    }
}

/// Visits an expression and its children, if it has any, using pre-order traversal (root, left, right).
fn visit_expression_recursive<F>(expr: &mut AstExpression, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    visitor_func(expr);

    match expr.kind_mut() {
        AstExpressionKind::Unary { operand, .. } => {
            visit_expression_recursive(operand, visitor_func);
        }
        AstExpressionKind::Binary { lhs, rhs, .. } => {
            visit_expression_recursive(lhs, visitor_func);
            visit_expression_recursive(rhs, visitor_func);
        }
        AstExpressionKind::Assignment { lhs, rhs, .. } => {
            visit_expression_recursive(lhs, visitor_func);
            visit_expression_recursive(rhs, visitor_func);
        }
        AstExpressionKind::Conditional { condition, consequent, alternative, .. } => {
            visit_expression_recursive(condition, visitor_func);
            visit_expression_recursive(consequent, visitor_func);
            visit_expression_recursive(alternative, visitor_func);
        }
        AstExpressionKind::FunctionCall { args, .. } => {
            for arg in args {
                visit_expression_recursive(arg, visitor_func);
            }
        }
        AstExpressionKind::Deref { pointer } => {
            visit_expression_recursive(pointer, visitor_func);
        }
        AstExpressionKind::AddressOf { target } => {
            visit_expression_recursive(target, visitor_func);
        }
        AstExpressionKind::Subscript { expr1, expr2 } => {
            visit_expression_recursive(expr1, visitor_func);
            visit_expression_recursive(expr2, visitor_func);
        }
        AstExpressionKind::Cast { inner, .. } => {
            visit_expression_recursive(inner, visitor_func);
        }
        _ => (), // Already visited the expression above, and these expression kinds have no child expressions
    }
}

fn visit_expressions_in_stmt<F>(stmt: &mut AstStatement, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    match stmt {
        AstStatement::Expression(expr) => visitor_func(expr),
        AstStatement::If { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::Switch { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::Case { constant_expr, .. } => visitor_func(constant_expr),
        AstStatement::While { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::DoWhile { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::For { init, controlling_expr, post_expr: post, .. } => {
            match init.as_mut() {
                AstForInitializer::Declaration(declarations) => {
                    for decl in declarations {
                        if let AstDeclaration::Variable(var_decl) = decl
                            && let Some(ref mut initializer) = var_decl.initializer
                        {
                            visit_expressions_in_variable_initializer(initializer, visitor_func);
                        }
                    }
                }

                AstForInitializer::Expression(expr) => {
                    if let Some(expr) = expr {
                        visitor_func(expr);
                    }
                }
            }

            if let Some(controlling_expr) = controlling_expr {
                visitor_func(controlling_expr);
            }

            if let Some(post) = post {
                visitor_func(post);
            }
        }
        AstStatement::Return(expr) => visitor_func(expr),
        _ => (),
    }
}

fn visit_expressions_in_variable_initializer<F>(initializer: &mut AstVariableInitializer, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    match initializer {
        AstVariableInitializer::Scalar(expr) => visitor_func(expr),

        AstVariableInitializer::Aggregate { init, .. } => {
            for initializer in init {
                visit_expressions_in_variable_initializer(initializer, visitor_func);
            }
        }
    }
}
