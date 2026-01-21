// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `checker` module defines `TypeChecker`, which holds mutable state necessary for type checking.

use std::collections::HashMap;

use crate::ICE;
use crate::compiler_driver::{Driver, Warning};
use crate::core::SourceLocation;
use crate::parser::{AstDeclaredType, AstExpression, AstMetadata, AstNodeId, AstType, AstUnaryOp};

use super::super::constant_table::ConstantTable;
use super::super::symbol_table::SymbolTable;
use super::super::type_conversion;

/// The error result for `TypeCheckResult`.
pub struct TypeCheckError;

/// The result of a type checking operation.
pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

/// The scope that a type alias is declared.
#[derive(Debug, Default)]
pub struct DeclarationScope {
    pub type_alias_map: HashMap<String, (AstType, SourceLocation)>,
}

/// The Type Checker holds mutable state necessary for type checking.
#[derive(Debug)]
pub struct TypeChecker {
    pub metadata: AstMetadata,
    pub symbols: SymbolTable,
    pub constants: ConstantTable,
    current_func_return_type: Option<AstType>,
    scopes: Vec<DeclarationScope>,
}

/// Whether to warn about implicit conversions when adding a cast.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CastWarningPolicy {
    WarnOnImplicitConversion,
    NoWarning,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new(AstMetadata::default(), SymbolTable::default(), ConstantTable::default())
    }
}

impl TypeChecker {
    /// Creates a new Type Checker.
    pub fn new(metadata: AstMetadata, symbols: SymbolTable, constants: ConstantTable) -> Self {
        let scopes = vec![DeclarationScope::default()];
        Self { metadata, symbols, constants, current_func_return_type: None, scopes }
    }

    /// The current declaration scope.
    pub fn current_declaration_scope(&mut self) -> &mut DeclarationScope {
        self.scopes.last_mut().expect("Expect to always have a declaration scope")
    }

    /// Starts a new declaration scope.
    pub fn begin_declaration_scope(&mut self) {
        self.scopes.push(DeclarationScope::default());
    }

    /// Ends the current declaration scope.
    pub fn end_declaration_scope(&mut self) {
        _ = self.scopes.pop();
        assert!(!self.scopes.is_empty());
    }

    /// Gets a node's data type
    pub fn get_data_type(&self, node_id: &AstNodeId) -> AstType {
        match self.metadata.get_node_type(node_id) {
            Some(data_type) => data_type.clone(),
            None => ICE!("Data type not found for node {node_id}"),
        }
    }

    /// Sets a node's data type
    pub fn set_data_type(&mut self, node_id: &AstNodeId, data_type: &AstType) {
        self.metadata.set_node_type(*node_id, data_type.clone());
    }

    /// Sets the current function's return type.
    ///
    /// This is needed when processing a return statement, since we need to know what type the function returns
    /// in order to insert a cast, if necessary.
    pub fn set_current_function_return_type(&mut self, return_type: &AstType) {
        self.current_func_return_type = Some(return_type.clone());
    }

    /// Gets the current function's return type.
    pub fn get_current_function_return_type(&self) -> AstType {
        let Some(ref function_return_type) = self.current_func_return_type else {
            ICE!("Current function return type not set");
        };

        function_return_type.clone()
    }

    /// Clears the current function.
    pub fn clear_current_function(&mut self) {
        self.current_func_return_type = None;
    }

    /// Wraps the given `AstExpression` in a cast to the given `target_type`.
    pub fn add_cast(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
        warning_policy: CastWarningPolicy,
        driver: &mut Driver,
    ) -> AstExpression {
        let node_id = AstNodeId::new();

        self.set_data_type(&node_id, target_type);

        if warning_policy == CastWarningPolicy::WarnOnImplicitConversion {
            let mut emitted_warning = false;

            // Warn if implicitly casting a UnaryNegate with an integer literal to an unsigned integer, which will
            // change signedness.
            if target_type.is_unsigned_integer()
                && let AstExpression::UnaryOperation { node_id, op, expr: inner_expr } = expr.as_ref()
                && *op == AstUnaryOp::Negate
                && inner_expr.is_integer_literal()
            {
                let old_type = self.metadata.get_node_type(node_id).cloned().unwrap();
                let loc = self.metadata.get_source_location(node_id);
                let sign_change = true;

                let AstExpression::IntegerLiteral { value, .. } = inner_expr.as_ref() else {
                    ICE!("Expected an integer literal");
                };

                // Don't warn on implicit unsigned cast applied to '-0'.
                if *value != 0 {
                    emit_implicit_literal_conversion_warning(
                        &old_type,
                        target_type,
                        *value,
                        true,
                        loc,
                        sign_change,
                        driver,
                    );

                    emitted_warning = true;
                }
            }

            // Warn if there's an implicit narrowing conversion for an integer literal (with or without change in signedness).
            if let AstExpression::IntegerLiteral { node_id, value, kind, .. } = expr.as_ref()
                && target_type.is_integer()
                && !target_type.can_hold_value(*value)
            {
                let old_type = kind.data_type();
                let loc = self.metadata.get_source_location(node_id);
                let sign_change =
                    old_type.is_integer() && target_type.is_integer() && !old_type.same_signedness(target_type);

                emit_implicit_literal_conversion_warning(
                    &old_type,
                    target_type,
                    *value,
                    false,
                    loc,
                    sign_change,
                    driver,
                );

                emitted_warning = true;
            }

            // Warn if there's an implicit arithmetic conversion for non-literals
            if !emitted_warning && !expr.is_arithmetic_literal() {
                let expr_type = self.get_data_type(&expr.node_id());
                let both_integers = target_type.is_integer() && expr_type.is_integer();
                let both_floating_point = target_type.is_floating_point() && expr_type.is_floating_point();

                if (both_integers || both_floating_point) && !expr_type.fits_inside(target_type) {
                    let loc = self.metadata.get_source_location(&expr.node_id());
                    Warning::implicit_arithmetic_conversion(&expr_type, target_type, loc, driver);
                }
            }
        }

        let target_type = AstDeclaredType::resolved(target_type);

        AstExpression::Cast { node_id, target_type, expr }
    }

    /// If the given boxed `AstExpression`'s type already matches the `target_type` then the existing expression
    /// is returned as-is. Otherwise, wraps the expression in a cast to the target type.
    pub fn add_cast_if_needed(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
        warning_policy: CastWarningPolicy,
        driver: &mut Driver,
    ) -> Box<AstExpression> {
        let expr_type = self.get_data_type(&expr.node_id());

        if expr_type == *target_type {
            expr
        } else {
            Box::new(self.add_cast(target_type, expr, warning_policy, driver))
        }
    }
}

fn emit_implicit_literal_conversion_warning(
    old_type: &AstType,
    new_type: &AstType,
    old_value: u64,
    negate_old_value: bool,
    loc: SourceLocation,
    sign_change: bool,
    driver: &mut Driver,
) {
    let old_value = if negate_old_value { -(old_value as i128) } else { old_value as i128 };

    let old_value_str = old_value.to_string();

    let new_value_str = if new_type.is_integer() {
        let new_value = type_conversion::cast_i128_to_integer_type(old_value, new_type);

        match new_value {
            (Some(u), None) => u.to_string(),
            (None, Some(i)) => i.to_string(),
            _ => ICE!("'cast_i128_to_integer_type' did not return a value"),
        }
    } else if new_type.is_floating_point() {
        let new_value = old_value as f64;

        // We're casting from an integer to floating point, so there will be no fractional digits.
        // Rust's `f64::to_string` will prefer to create a round-trippable decimal string value
        // which may not perfectly match the actual value. So we specify that we want zero fractional
        // digits for our string.
        format!("{:.0}", new_value)
    } else {
        ICE!("Unhandled type");
    };

    Warning::constant_conversion(old_type, new_type, &old_value_str, &new_value_str, sign_change, loc, driver);
}
