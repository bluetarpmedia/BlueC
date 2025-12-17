// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `visitor` module provides functionality to visit nodes in the parser's AST.

use crate::parser::{
    AstBlock, AstBlockItem, AstDeclaration, AstExpression, AstForInitializer, AstFullExpression, AstFunction, AstRoot,
    AstStatement, AstVariableInitializer,
};

/// Visits all function definitions in the AST and, for each one, calls the `visitor_func` with the `AstFunction`.
pub fn visit_functions<F>(ast_root: &mut AstRoot, visitor_func: &mut F)
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

/// Visits all statements in the given block and, for each one, calls the `visitor_func` with the `AstStatement`.
pub fn visit_statements_in_block<F>(block: &mut AstBlock, visitor_func: &mut F)
where
    F: FnMut(&mut AstStatement),
{
    let block_items = &mut block.0;
    for block_item in block_items {
        if let AstBlockItem::Statement(stmt) = block_item {
            visit_statement(stmt, visitor_func);
        }
    }
}

/// Visits all full expressions in the AST and, for each one, calls the `visitor_func` with the `AstFullExpression`.
pub fn visit_full_expressions<F>(ast_root: &mut AstRoot, visitor_func: &mut F)
where
    F: FnMut(&mut AstFullExpression),
{
    for decl in &mut ast_root.0 {
        match decl {
            AstDeclaration::Variable(var_decl) => {
                if let Some(ref mut initializer) = var_decl.initializer {
                    visit_full_expressions_in_variable_initializer(initializer, visitor_func);
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
                                    visit_full_expressions_in_variable_initializer(initializer, visitor_func);
                                }
                            }

                            AstBlockItem::Statement(stmt) => visit_statement(stmt, &mut |stmt: &mut AstStatement| {
                                visit_full_expressions_in_stmt(stmt, visitor_func);
                            }),
                        }
                    }
                }
            }

            AstDeclaration::TypeAlias(_) => (),
        }
    }
}

/// Visits each sub-expression recursively in the full expression.
pub fn visit_expressions_in_full_expression<F>(full_expr: &mut AstFullExpression, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    visitor_func(&mut full_expr.expr);

    visit_expression(&mut full_expr.expr, visitor_func);
}

fn visit_expression<F>(expr: &mut AstExpression, visitor_func: &mut F)
where
    F: FnMut(&mut AstExpression),
{
    match expr {
        AstExpression::UnaryOperation { expr, .. } => {
            visitor_func(expr);
            visit_expression(expr, visitor_func);
        }
        AstExpression::BinaryOperation { left, right, .. } => {
            visitor_func(left);
            visit_expression(left, visitor_func);

            visitor_func(right);
            visit_expression(right, visitor_func);
        }
        AstExpression::Assignment { lhs, rhs, .. } => {
            visitor_func(lhs);
            visit_expression(lhs, visitor_func);

            visitor_func(rhs);
            visit_expression(rhs, visitor_func);
        }
        AstExpression::Conditional { expr, consequent, alternative, .. } => {
            visitor_func(expr);
            visit_expression(expr, visitor_func);

            visitor_func(consequent);
            visit_expression(consequent, visitor_func);

            visitor_func(alternative);
            visit_expression(alternative, visitor_func);
        }
        AstExpression::FunctionCall { args, .. } => {
            for arg in args {
                visitor_func(arg);
                visit_expression(arg, visitor_func);
            }
        }
        AstExpression::Cast { expr, .. } => {
            visitor_func(expr);
            visit_expression(expr, visitor_func);
        }
        _ => (),
    }
}

/// Visits a statement recursively and, for each one, calls the `visitor_func` with the `AstStatement`.
pub fn visit_statement<F>(stmt: &mut AstStatement, visitor_func: &mut F)
where
    F: FnMut(&mut AstStatement),
{
    visitor_func(stmt);

    match stmt {
        AstStatement::Labeled { stmt: labeled_stmt, .. } => {
            visit_statement(labeled_stmt, visitor_func);
        }

        AstStatement::Compound(block) => visit_statements_in_block(block, visitor_func),

        AstStatement::If { then_stmt, else_stmt, .. } => {
            visit_statement(then_stmt, visitor_func);

            if let Some(else_stmt) = else_stmt {
                visit_statement(else_stmt, visitor_func);
            }
        }

        AstStatement::Switch { body, .. } => {
            visit_statement(body, visitor_func);
        }

        AstStatement::Case { stmt: inner_stmt, .. } => {
            visit_statement(inner_stmt, visitor_func);
        }

        AstStatement::Default { stmt: inner_stmt, .. } => {
            visit_statement(inner_stmt, visitor_func);
        }

        AstStatement::While { body, .. } => {
            visit_statement(body, visitor_func);
        }

        AstStatement::DoWhile { body, .. } => {
            visit_statement(body, visitor_func);
        }

        AstStatement::For { body, .. } => {
            visit_statement(body, visitor_func);
        }

        AstStatement::Null => visitor_func(stmt),
        AstStatement::Expression(..) => visitor_func(stmt),
        AstStatement::Break { .. } => visitor_func(stmt),
        AstStatement::Continue { .. } => visitor_func(stmt),
        AstStatement::Goto { .. } => visitor_func(stmt),
        AstStatement::Return(..) => visitor_func(stmt),
    }
}

fn visit_full_expressions_in_stmt<F>(stmt: &mut AstStatement, visitor_func: &mut F)
where
    F: FnMut(&mut AstFullExpression),
{
    match stmt {
        AstStatement::Expression(full_expr) => visitor_func(full_expr),
        AstStatement::If { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::Switch { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::Case { constant_expr, .. } => visitor_func(constant_expr),
        AstStatement::While { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::DoWhile { controlling_expr, .. } => visitor_func(controlling_expr),
        AstStatement::For { init, controlling_expr, post, .. } => {
            match init.as_mut() {
                AstForInitializer::Declaration(declarations) => {
                    for decl in declarations {
                        if let AstDeclaration::Variable(var_decl) = decl
                            && let Some(ref mut initializer) = var_decl.initializer
                        {
                            visit_full_expressions_in_variable_initializer(initializer, visitor_func);
                        }
                    }
                }

                AstForInitializer::Expression(full_expr) => {
                    if let Some(full_expr) = full_expr {
                        visitor_func(full_expr);
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
        AstStatement::Return(full_expr) => visitor_func(full_expr),
        _ => (),
    }
}

fn visit_full_expressions_in_variable_initializer<F>(initializer: &mut AstVariableInitializer, visitor_func: &mut F)
where
    F: FnMut(&mut AstFullExpression),
{
    match initializer {
        AstVariableInitializer::Scalar(full_expr) => visitor_func(full_expr),

        AstVariableInitializer::Aggregate { init, .. } => {
            for initializer in init {
                visit_full_expressions_in_variable_initializer(initializer, visitor_func);
            }
        }
    }
}
