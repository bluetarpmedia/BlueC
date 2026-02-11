// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `label_maker` module defines the [LabelMaker] type.

use crate::ICE;
use crate::parser;

use super::BtLabelIdentifier;

/// The Label Maker provides functions to generate unique label names for use in the IR.
#[derive(Debug)]
pub struct LabelMaker {
    current_function_name: String,
    current_function_index: usize,
    next_label_index: usize,
}

impl LabelMaker {
    /// Creates a new label maker.
    pub fn new() -> Self {
        Self { current_function_name: String::new(), current_function_index: 0, next_label_index: 0 }
    }

    /// Resets the label maker for a new function definition.
    pub fn reset_for_new_function(&mut self, function_name: &str) {
        self.current_function_name = function_name.to_string();
        self.current_function_index += 1;
        self.next_label_index = 0;
    }

    /// Creates a label identifier for a user-declared label.
    pub fn make_user_label(&mut self, name: &str) -> BtLabelIdentifier {
        BtLabelIdentifier(format!("{}{}_{}", &self.current_function_name, self.current_function_index, name))
    }

    /// Creates a label identifier for a control statement.
    pub fn make_control_label(&mut self, name: &str, stmt_node_id: &parser::AstNodeId) -> BtLabelIdentifier {
        BtLabelIdentifier(format!(
            "{}{}_ctrl_{}_{}",
            &self.current_function_name, self.current_function_index, stmt_node_id, name
        ))
    }

    /// Creates a label identifier for a switch statement.
    pub fn make_switch_label(&mut self, name: &str, switch_node_id: &parser::AstNodeId) -> BtLabelIdentifier {
        BtLabelIdentifier(format!(
            "{}{}_switch_{}_{}",
            &self.current_function_name, self.current_function_index, switch_node_id, name
        ))
    }

    /// Creates a label identifier for a case statement inside a switch statement.
    pub fn make_switch_case_label(
        &mut self,
        switch_node_id: &parser::AstNodeId,
        case_node_id: &parser::AstNodeId,
    ) -> BtLabelIdentifier {
        BtLabelIdentifier(format!(
            "{}{}_switch_{}_case_{}",
            &self.current_function_name, self.current_function_index, switch_node_id, case_node_id
        ))
    }

    /// Creates a unique label.
    ///
    /// Multiple calls to this function with the same `label_name` return different, unique label identifiers.
    pub fn make_unique_label(&mut self, label_name: &str) -> BtLabelIdentifier {
        let idx = self.next_label_index;
        if idx == usize::MAX {
            ICE!("Exhausted label indices"); // Technically we have 1 more available
        }
        self.next_label_index += 1;
        BtLabelIdentifier(format!(
            "{}{}_{}_{}",
            &self.current_function_name, self.current_function_index, label_name, idx
        ))
    }
}
