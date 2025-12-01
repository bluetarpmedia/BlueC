// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The meta module defines metadata that the parser generates, such as source code spans for AST nodes.

use crate::lexer;
use crate::parser::{AstConstantInteger, AstNodeId, AstType};

use std::collections::{HashMap, HashSet};

/// AST metadata produced by the parser.
#[derive(Debug)]
pub struct AstMetadata {
    source_span_nodes: HashMap<AstNodeId, AstNodeSourceSpanMetadata>,
    switch_cases: HashMap<AstNodeId, HashMap<AstConstantInteger, AstNodeId>>,
    switch_defaults: HashMap<AstNodeId, lexer::SourceLocation>,
    node_types: HashMap<AstNodeId, AstType>,
    expr_has_parens: HashSet<AstNodeId>,
}

/// Source span metadata for an AST node.
///
/// The start and end lines and columns are inclusive. `[start, end]`
#[derive(Debug)]
pub struct AstNodeSourceSpanMetadata {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl From<AstNodeSourceSpanMetadata> for lexer::SourceLocation {
    /// Creates a `lexer::SourceLocation` from an `AstNodeSourceSpanMetadata`.
    fn from(node_metadata: AstNodeSourceSpanMetadata) -> Self {
        (&node_metadata).into()
    }
}

impl From<&AstNodeSourceSpanMetadata> for lexer::SourceLocation {
    /// Creates a `lexer::SourceLocation` from an `&AstNodeSourceSpanMetadata`.
    fn from(node_metadata: &AstNodeSourceSpanMetadata) -> Self {
        let length = if node_metadata.start_line == node_metadata.end_line {
            node_metadata.end_column - node_metadata.start_column
        } else {
            1
        };

        lexer::SourceLocation { line: node_metadata.start_line, column: node_metadata.start_column, length }
    }
}

impl AstNodeSourceSpanMetadata {
    /// Creates an `AstNodeSourceSpanMetadata` from the given source location.
    pub fn from_source_location(loc: &lexer::SourceLocation) -> Self {
        Self { start_line: loc.line, start_column: loc.column, end_line: loc.line, end_column: loc.column + loc.length }
    }

    /// Creates an `AstNodeSourceSpanMetadata` from the given start and end pair of source locations.
    pub fn from_source_location_pair(start_loc: &lexer::SourceLocation, end_loc: &lexer::SourceLocation) -> Self {
        Self {
            start_line: start_loc.line,
            start_column: start_loc.column,
            end_line: end_loc.line,
            end_column: end_loc.column + end_loc.length,
        }
    }
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
            source_span_nodes: HashMap::new(),
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

    /// Adds source span metadata for a given node.
    pub fn add_source_span(&mut self, node_id: AstNodeId, source_span: AstNodeSourceSpanMetadata) {
        self.source_span_nodes.insert(node_id, source_span);
    }

    /// Gets source span metadata for a given node.
    pub fn get_source_span(&self, node_id: &AstNodeId) -> Option<&AstNodeSourceSpanMetadata> {
        self.source_span_nodes.get(node_id)
    }

    /// Gets source span metadata for a given node and returns it as a `SourceLocation`.
    pub fn get_source_span_as_loc(&self, node_id: &AstNodeId) -> Option<lexer::SourceLocation> {
        self.get_source_span(node_id).map(|span| span.into())
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
    /// If a case did not already exist with this `constant_value`, `None` is returned.
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
    /// If a default label did not already exist, `None` is returned.
    ///
    /// If a default label already exists, `Some` is returned with the source location of the previous default statement.
    pub fn add_switch_default_label(
        &mut self,
        switch_node_id: AstNodeId,
        location: lexer::SourceLocation,
    ) -> Option<lexer::SourceLocation> {
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
