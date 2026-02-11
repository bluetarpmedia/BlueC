// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `checker` module defines `TypeChecker`, which holds mutable state necessary for type checking.

use std::collections::HashMap;

use crate::ICE;
use crate::core::SourceLocation;
use crate::parser::{AstDeclaredType, AstExpression, AstMetadata, AstNodeId, AstType};

use super::super::constant_table::ConstantTable;
use super::super::symbol_table::SymbolTable;

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

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new(AstMetadata::default())
    }
}

impl TypeChecker {
    /// Creates a new Type Checker.
    pub fn new(metadata: AstMetadata) -> Self {
        let symbols = SymbolTable::new();
        let constants = ConstantTable::new();
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
    pub fn get_data_type(&self, node_id: AstNodeId) -> AstType {
        self.metadata.get_node_type(node_id).clone()
    }

    /// Sets a node's data type
    pub fn set_data_type(&mut self, node_id: AstNodeId, data_type: &AstType) {
        self.metadata.set_node_type(node_id, data_type.clone());
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
    pub fn add_cast(&mut self, target_type: &AstType, expr: Box<AstExpression>, is_implicit: bool) -> AstExpression {
        let node_id = AstNodeId::new();

        self.set_data_type(node_id, target_type);
        self.metadata.copy_source_location_from_child(expr.node_id(), node_id);
        self.metadata.propagate_const_flag_from_child(expr.node_id(), node_id);

        let target_type = AstDeclaredType::resolved(target_type);

        AstExpression::Cast { node_id, target_type, expr, is_implicit }
    }

    /// If the given boxed `AstExpression`'s type already matches the `target_type` then the existing expression
    /// is returned as-is. Otherwise, wraps the expression in an implicit cast to the target type.
    pub fn add_implicit_cast_if_needed(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
    ) -> Box<AstExpression> {
        let expr_type = self.get_data_type(expr.node_id());

        if expr_type == *target_type { expr } else { Box::new(self.add_cast(target_type, expr, true)) }
    }

    /// If the given boxed `AstExpression`'s type already matches the `target_type` then the existing expression
    /// is returned as-is. Otherwise, wraps the expression in an explicit cast to the target type.
    pub fn add_explicit_cast_if_needed(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
    ) -> Box<AstExpression> {
        let expr_type = self.get_data_type(expr.node_id());

        if expr_type == *target_type { expr } else { Box::new(self.add_cast(target_type, expr, false)) }
    }
}
