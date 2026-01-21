// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `label_maker` module defines the `AsmLabelMaker` type which generates unique labels in the global scope
//! and in each function.

use std::collections::HashMap;

use crate::ICE;
use crate::ir;

use super::ast::{AsmConstantId, AsmLabelName};

#[derive(Debug)]
pub struct AsmLabelMaker {
    next_function_local_id: (usize, usize),
    ir_translated_labels: HashMap<String, AsmLabelName>,
}

impl AsmLabelMaker {
    /// Creates a new label maker.
    pub fn new() -> Self {
        Self { next_function_local_id: (0, 0), ir_translated_labels: HashMap::new() }
    }

    /// Resets the label maker for a new function.
    pub fn reset_for_new_function(&mut self) {
        let (func_idx, _) = self.next_function_local_id;

        if func_idx == usize::MAX {
            ICE!("Exhausted AsmLabelMaker function indices");
        }

        self.next_function_local_id = (func_idx + 1, 0);
    }

    /// Returns the `AsmLabelId` for the given IR label.
    pub fn translate_ir_label_into_function_local_label(&mut self, ir_label: &ir::BtLabelIdentifier) -> AsmLabelName {
        if let Some(label) = self.ir_translated_labels.get(&ir_label.0) {
            return label.clone();
        }

        let label = self.make_function_local_label();
        self.ir_translated_labels.insert(ir_label.0.clone(), label.clone());
        label
    }

    /// Makes a unique label within the current function.
    pub fn make_function_local_label(&mut self) -> AsmLabelName {
        let (func_idx, local_idx) = self.next_function_local_id;

        if local_idx == usize::MAX {
            ICE!("Exhausted AsmLabelMaker local indices for function");
        }

        self.next_function_local_id = (func_idx, local_idx + 1);

        AsmLabelName(format!("BF{func_idx}_{local_idx}"))
    }

    /// Makes a unique label for the given `AsmConstantId`.
    pub fn make_constant_label(&self, constant_id: AsmConstantId) -> AsmLabelName {
        AsmLabelName(format!("CT_{}", constant_id.0))
    }
}
