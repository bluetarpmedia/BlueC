// Copyright 2025-2026 Neil Henderson
//
//! The `checker` module defines `TypeChecker`, which holds mutable state necessary for type checking.

use std::collections::HashMap;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::core::SourceLocation;
use crate::parser::{AstDeclaredType, AstExpression, AstExpressionKind, AstMetadata, AstNodeId, AstType};

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
    current_fn: Option<(AstNodeId, String, AstType)>, // Id, Name, Return type
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
        Self { metadata, symbols, constants, current_fn: None, scopes }
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

    /// Sets the current function.
    ///
    /// This is needed when processing a return statement, since we need to know what type the function returns
    /// in order to type check the return statement.
    pub fn set_current_function(&mut self, id: AstNodeId, name: &str, return_type: &AstType) {
        self.current_fn = Some((id, name.to_string(), return_type.clone()));
    }

    /// Gets the current function.
    ///
    /// Emits an ICE if `set_current_function` was not previously called.
    pub fn get_current_function(&self) -> (AstNodeId, &str, &AstType) {
        let Some((id, name, return_type)) = &self.current_fn else {
            ICE!("Current function not set");
        };

        (*id, name, return_type)
    }

    /// Clears the current function.
    pub fn clear_current_function(&mut self) {
        self.current_fn = None;
    }

    /// Wraps the given `AstExpression` in a cast to the given `target_type`.
    pub fn add_cast(
        &mut self,
        target_type: &AstType,
        inner: Box<AstExpression>,
        is_implicit: bool,
        driver: &mut Driver,
    ) -> AstExpression {
        let node_id = driver.make_node_id();

        self.set_data_type(node_id, target_type);
        self.metadata.copy_source_location_from_child(inner.id(), node_id);
        self.metadata.propagate_const_flag_from_child(inner.id(), node_id);

        let target_type = AstDeclaredType::resolved(target_type);

        AstExpression::new(node_id, AstExpressionKind::Cast { target_type, inner, is_implicit })
    }

    /// If the given boxed `AstExpression`'s type already matches the `target_type` then the existing expression
    /// is returned as-is. Otherwise, wraps the expression in an implicit cast to the target type.
    pub fn add_implicit_cast_if_needed(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
        driver: &mut Driver,
    ) -> Box<AstExpression> {
        let expr_type = self.get_data_type(expr.id());

        if expr_type == *target_type { expr } else { Box::new(self.add_cast(target_type, expr, true, driver)) }
    }

    /// If the given boxed `AstExpression`'s type already matches the `target_type` then the existing expression
    /// is returned as-is. Otherwise, wraps the expression in an explicit cast to the target type.
    pub fn add_explicit_cast_if_needed(
        &mut self,
        target_type: &AstType,
        expr: Box<AstExpression>,
        driver: &mut Driver,
    ) -> Box<AstExpression> {
        let expr_type = self.get_data_type(expr.id());

        if expr_type == *target_type { expr } else { Box::new(self.add_cast(target_type, expr, false, driver)) }
    }
}
