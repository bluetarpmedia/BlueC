// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `meta` module defines metadata that the parser generates, such as source code spans for AST nodes.

use std::collections::{HashMap, HashSet};

use crate::ICE;
use crate::core::SourceLocation;
use crate::parser::{AstConstantInteger, AstNodeId, AstType};

/// AST metadata produced by the parser.
#[derive(Debug)]
pub struct AstMetadata {
    source_location_nodes: HashMap<AstNodeId, SourceLocation>,
    switch_cases: HashMap<AstNodeId, HashMap<AstConstantInteger, AstNodeId>>,
    switch_defaults: HashMap<AstNodeId, SourceLocation>,
    node_types: HashMap<AstNodeId, AstType>,
    expr_has_parens: HashSet<AstNodeId>,
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
            source_location_nodes: HashMap::new(),
            switch_cases: HashMap::new(),
            switch_defaults: HashMap::new(),
            node_types: HashMap::new(),
            expr_has_parens: HashSet::new(),
        }
    }

    /// Sets the data type of a node. This either adds or updates the data type for the node.
    pub fn set_node_type(&mut self, node_id: AstNodeId, data_type: AstType) {
        let _ = self.node_types.insert(node_id, data_type);
    }

    /// Gets a node's data type.
    pub fn get_node_type(&self, node_id: &AstNodeId) -> Option<&AstType> {
        self.node_types.get(node_id)
    }

    /// Adds the source location metadata for the given node.
    pub fn add_source_location(&mut self, node_id: AstNodeId, source_loc: SourceLocation) {
        self.source_location_nodes.insert(node_id, source_loc);
    }

    /// Gets the source location metadata for the given node.
    pub fn get_source_location(&self, node_id: &AstNodeId) -> SourceLocation {
        if let Some(loc) = self.source_location_nodes.get(node_id) {
            *loc
        } else {
            ICE!("Cannot find source location for AST node '{node_id}'")
        }
    }

    /// Records that an expression is wrapped in parentheses in the original source code.
    pub fn set_expr_has_parens(&mut self, node_id: AstNodeId) {
        self.expr_has_parens.insert(node_id);
    }

    /// Returns whether an expression is wrapped in parentheses in the original source code.
    pub fn expr_has_parens(&self, node_id: AstNodeId) -> bool {
        self.expr_has_parens.contains(&node_id)
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
