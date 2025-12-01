// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_table` module defines the `ConstantTable` type, which tracks all floating-point constants,
//! and, in future, string literal constants.

use std::collections::HashMap;

use crate::internal_error;

/// A table of constant values mapped to their unique identifiers.
///
/// The code generator uses the constant table to avoid emitting duplicate values. For example, floating-point
/// constants have to be stored in the read-only data section in x86_64. So every instance of a float constant
/// in the source code has to be replaced with a reference to the read-only value via a label. We only want one
/// instance of the constant stored in the data section, and each reference in the source code should refer to it.
pub struct ConstantTable {
    float_table: Vec<FloatValue>,

    float32_map: HashMap<u32, usize>, // Map of `f32::to_bits` to the index in the `float_table`.
    float64_map: HashMap<u64, usize>, // Map of `f64::to_bits` to the index in the `float_table`.
}

enum FloatValue {
    F32 { value: f32, alignment: usize },
    F64 { value: f64, alignment: usize },
}

/// The constant value as an unsigned integer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnsignedValue {
    U32(u32),
    U64(u64),
}

/// An entry in the Constant Table.
pub struct ConstantEntry {
    pub index: usize,
    pub value: UnsignedValue,
    pub alignment: usize,
}

impl Default for ConstantTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantTable {
    /// Creates a new constant table.
    pub fn new() -> Self {
        Self { float_table: Vec::new(), float32_map: HashMap::new(), float64_map: HashMap::new() }
    }

    /// Adds a new `f32` constant to the table and returns its index, or if the constant already exists then
    /// returns the existing index that represents the constant value.
    pub fn add_f32(&mut self, constant_value: f32, required_alignment: usize) -> usize {
        assert!(required_alignment >= 4 && required_alignment % 4 == 0);

        let key = constant_value.to_bits();

        // If this constant already exists then return its index, but also update its alignment if necessary.
        if let Some(&index) = self.float32_map.get(&key) {
            let Some(FloatValue::F32 { alignment, .. }) = self.float_table.get_mut(index) else {
                internal_error::ICE("ConstantTable is missing entry");
            };

            if *alignment < required_alignment {
                *alignment = required_alignment;
            }

            return index;
        }

        if self.float_table.len() == usize::MAX {
            internal_error::ICE("Exhausted ConstantTable entries");
        }

        self.float_table.push(FloatValue::F32 { value: constant_value, alignment: required_alignment });
        let idx = self.float_table.len() - 1;

        self.float32_map.insert(key, idx);

        idx
    }

    /// Adds a new `f64` constant to the table and returns its index, or if the constant already exists then
    /// returns the existing index that represents the constant value.
    pub fn add_f64(&mut self, constant_value: f64, required_alignment: usize) -> usize {
        assert!(required_alignment >= 8 && required_alignment % 8 == 0);

        let key = constant_value.to_bits();

        // If this constant already exists then return its index, but also update its alignment if necessary.
        if let Some(&index) = self.float64_map.get(&key) {
            let Some(FloatValue::F64 { alignment, .. }) = self.float_table.get_mut(index) else {
                internal_error::ICE("ConstantTable is missing entry");
            };

            if *alignment < required_alignment {
                *alignment = required_alignment;
            }

            return index;
        }

        if self.float_table.len() == usize::MAX {
            internal_error::ICE("Exhausted ConstantTable entries");
        }

        self.float_table.push(FloatValue::F64 { value: constant_value, alignment: required_alignment });
        let idx = self.float_table.len() - 1;

        self.float64_map.insert(key, idx);

        idx
    }

    /// Returns a `Vec<ConstantEntry>` of all the constants.
    pub fn get_constants(&self) -> Vec<ConstantEntry> {
        self.float_table
            .iter()
            .enumerate()
            .map(|(index, float_value)| match float_value {
                FloatValue::F32 { value, alignment } => {
                    ConstantEntry { index, value: UnsignedValue::U32(value.to_bits()), alignment: *alignment }
                }
                FloatValue::F64 { value, alignment } => {
                    ConstantEntry { index, value: UnsignedValue::U64(value.to_bits()), alignment: *alignment }
                }
            })
            .collect()
    }
}
