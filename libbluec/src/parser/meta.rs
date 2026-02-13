// Copyright 2025-2026 Neil Henderson
//
//! The `meta` module defines metadata that the parser generates, such as source code spans for AST nodes.

mod expression_flags;

use std::collections::HashMap;

pub use expression_flags::{AstExpressionFlag, AstExpressionFlags};

use crate::ICE;
use crate::core::SourceLocation;
use crate::parser::{AstConstantInteger, AstNodeId, AstType};

/// AST metadata produced by the parser.
#[derive(Debug)]
pub struct AstMetadata {
    sloc_nodes: HashMap<AstNodeId, SourceLocation>,
    operator_sloc_nodes: HashMap<AstNodeId, SourceLocation>,
    switch_cases: HashMap<AstNodeId, HashMap<AstConstantInteger, AstNodeId>>,
    switch_defaults: HashMap<AstNodeId, SourceLocation>,
    node_types: HashMap<AstNodeId, AstType>,
    expr_flags: HashMap<AstNodeId, AstExpressionFlags>,
}

impl Default for AstMetadata {
    fn default() -> Self {
        Self::new()
    }
}

impl AstMetadata {
    /// Creates the AST metadata.
    pub fn new() -> Self {
        Self {
            sloc_nodes: HashMap::new(),
            operator_sloc_nodes: HashMap::new(),
            switch_cases: HashMap::new(),
            switch_defaults: HashMap::new(),
            node_types: HashMap::new(),
            expr_flags: HashMap::new(),
        }
    }

    /// Sets the data type of a node. This either adds or updates the data type for the node.
    pub fn set_node_type(&mut self, node_id: AstNodeId, data_type: AstType) {
        let _ = self.node_types.insert(node_id, data_type);
    }

    /// If a data type has been set for this node, returns `Some(&AstType)`, or otherwise returns `None`.
    pub fn try_get_node_type(&self, node_id: AstNodeId) -> Option<&AstType> {
        self.node_types.get(&node_id)
    }

    /// Gets a node's data type.
    ///
    /// Pre: Expects the data type to previously have been set; emits an ICE otherwise.
    pub fn get_node_type(&self, node_id: AstNodeId) -> &AstType {
        if let Some(data_type) = self.node_types.get(&node_id) {
            data_type
        } else {
            ICE!("Cannot find data type for AST node '{node_id}'")
        }
    }

    /// Adds the source location metadata for the given node.
    pub fn add_source_location(&mut self, node_id: AstNodeId, source_loc: SourceLocation) {
        self.sloc_nodes.insert(node_id, source_loc);
    }

    /// Gets the source location metadata for the given node.
    ///
    /// Pre: Expects the source location to previously have been set; emits an ICE otherwise.
    pub fn get_source_location(&self, node_id: AstNodeId) -> SourceLocation {
        if let Some(loc) = self.sloc_nodes.get(&node_id) {
            *loc
        } else {
            ICE!("Cannot find source location for AST node '{node_id}'")
        }
    }

    /// Copies a source location from a child expression to a parent expression.
    pub fn copy_source_location_from_child(&mut self, child_node_id: AstNodeId, parent_node_id: AstNodeId) {
        if let Some(loc) = self.sloc_nodes.get(&child_node_id) {
            self.add_source_location(parent_node_id, *loc);
        }
    }

    /// Adds the source location metadata of the operator for the given binary/ternary expression node.
    pub fn add_operator_sloc(&mut self, node_id: AstNodeId, source_loc: SourceLocation) {
        self.operator_sloc_nodes.insert(node_id, source_loc);
    }

    /// Gets the source location metadata of the operator for the given binary/ternary expression node.
    ///
    /// Pre: Expects the source location to previously have been set; emits an ICE otherwise.
    pub fn get_operator_sloc(&self, node_id: AstNodeId) -> SourceLocation {
        if let Some(loc) = self.operator_sloc_nodes.get(&node_id) {
            *loc
        } else {
            ICE!("Cannot find operator source location for AST node '{node_id}'")
        }
    }

    /// Propagates the constant expression flag from a child sub-expression to a parent expression.
    pub fn propagate_const_flag_from_child(&mut self, child_node_id: AstNodeId, parent_node_id: AstNodeId) {
        if self.is_expr_flag_set(child_node_id, AstExpressionFlag::IsConstant) {
            self.set_expr_flag(parent_node_id, AstExpressionFlag::IsConstant);
        }
    }

    /// If all child sub-expressions have the constant expression flag set, then propagates the flag to the parent.
    pub fn propagate_const_flag_from_children(&mut self, child_node_ids: &[AstNodeId], parent_node_id: AstNodeId) {
        let all_set = child_node_ids
            .iter()
            .all(|child_node_id| self.is_expr_flag_set(*child_node_id, AstExpressionFlag::IsConstant));

        if all_set {
            self.set_expr_flag(parent_node_id, AstExpressionFlag::IsConstant);
        } else {
            self.clear_expr_flag(parent_node_id, AstExpressionFlag::IsConstant);
        }
    }

    /// Sets a metadata flag for the given expression.
    pub fn set_expr_flag(&mut self, node_id: AstNodeId, flag: AstExpressionFlag) {
        self.expr_flags.entry(node_id).and_modify(|flags| flags.insert(flag)).or_insert(AstExpressionFlags::from(flag));
    }

    /// Clears a metadata flag for the given expression.
    pub fn clear_expr_flag(&mut self, node_id: AstNodeId, flag: AstExpressionFlag) {
        self.expr_flags.entry(node_id).and_modify(|flags| flags.remove(flag));
    }

    /// Is the metadata flag set for the given expression?
    pub fn is_expr_flag_set(&self, node_id: AstNodeId, flag: AstExpressionFlag) -> bool {
        self.expr_flags.get(&node_id).is_some_and(|flags| flags.contains(flag))
    }

    /// Adds metadata about a switch `case` statement.
    ///
    /// Returns `None` if a case did not already exist with this `constant_value`.
    ///
    /// If a case already exists with this `constant_value`, `Some` is returned with the ID of the
    /// previous case statement node whose expression evaluated to this value.
    pub fn add_switch_case(
        &mut self,
        switch_node_id: AstNodeId,
        constant_expr_node_id: AstNodeId,
        constant_value: AstConstantInteger,
    ) -> Option<AstNodeId> {
        let cases = self.switch_cases.entry(switch_node_id).or_default();

        match cases.get(&constant_value) {
            Some(node_id) => Some(*node_id),
            None => {
                cases.insert(constant_value, constant_expr_node_id);
                None
            }
        }
    }

    /// Adds a `default` label to the given switch statement.
    ///
    /// Returns `None` if a default label did not already exist.
    ///
    /// If a default label already exists, returns `Some` with the source location of the previous default statement.
    pub fn add_switch_default_label(
        &mut self,
        switch_node_id: AstNodeId,
        location: SourceLocation,
    ) -> Option<SourceLocation> {
        match self.switch_defaults.get(&switch_node_id) {
            Some(loc) => Some(*loc),
            None => {
                self.switch_defaults.insert(switch_node_id, location);
                None
            }
        }
    }

    /// Gets the `case` statement values and node ids for the given switch statement, or an empty vector if the given
    /// switch node id is not found.
    ///
    /// The cases are sorted by the case value in ascending order.
    pub fn get_switch_cases(&self, switch_node_id: AstNodeId) -> Vec<(AstConstantInteger, AstNodeId)> {
        let mut cases = match self.switch_cases.get(&switch_node_id) {
            None => Vec::new(),
            Some(cases) => cases.iter().map(|case| (*case.0, *case.1)).collect(),
        };

        cases.sort_by_key(|case| case.0);
        cases
    }

    /// Does the given switch statement have a `default` label?
    pub fn switch_has_default(&self, switch_node_id: AstNodeId) -> bool {
        self.switch_defaults.contains_key(&switch_node_id)
    }
}
