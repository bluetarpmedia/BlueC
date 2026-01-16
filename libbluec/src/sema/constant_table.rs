// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `constant_table` module defines the `ConstantTable` type, which tracks all floating-point and string literal
//! constants.

use std::collections::HashMap;

use crate::ICE;
use crate::parser::AstUniqueName;

/// A unique index for a constant value in the constant table.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ConstantIndex(pub usize);

impl PartialEq<usize> for ConstantIndex {
    fn eq(&self, other: &usize) -> bool {
        self.0 == *other
    }
}

/// A table of constant values.
///
/// The constant table allows us to deduplicate data. Instead of repeating the same constant value (e.g. string
/// literal, or floating-point value) in the generated assembly code, we store it once and refer to it by its unique
/// index.
#[derive(Debug)]
pub struct ConstantTable {
    values: Vec<ConstantValue>,
    string_lookup: HashMap<String, ConstantIndex>, // Map of the string value to its index in the table.
    float32_lookup: HashMap<u32, ConstantIndex>,   // Map of `f32::to_bits` to the index in the table.
    float64_lookup: HashMap<u64, ConstantIndex>,   // Map of `f64::to_bits` to the index in the table.
}

/// The constant value.
#[derive(Debug)]
pub enum ConstantValue {
    String { value: String },
    StringArray { variable_name: AstUniqueName, values: Vec<String> },
    F32 { value: f32, alignment: usize },
    F64 { value: f64, alignment: usize },
}

/// The constant floating-point value as an unsigned integer.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnsignedValue {
    U32(u32),
    U64(u64),
}

/// A floating-point entry in the Constant Table.
pub struct ConstantFloatEntry {
    pub index: ConstantIndex,
    pub value: UnsignedValue,
    pub alignment: usize,
}

/// A string entry in the Constant Table.
pub struct ConstantStringEntry {
    pub index: ConstantIndex,
    pub value: String,
}

impl Default for ConstantTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantTable {
    /// Creates a new constant table.
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            string_lookup: HashMap::new(),
            float32_lookup: HashMap::new(),
            float64_lookup: HashMap::new(),
        }
    }

    /// Returns `true` if the table contains no entries.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// The number of entries in the table.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Adds a new string constant to the table and returns its unique identifier, or if the constant already exists
    /// then returns the existing unique identifier that represents the constant value.
    pub fn add_string(&mut self, constant_string: &str) -> ConstantIndex {
        if let Some(&index) = self.string_lookup.get(constant_string) {
            return index;
        }

        if self.values.len() == usize::MAX {
            ICE!("Exhausted ConstantTable string entries");
        }

        self.values.push(ConstantValue::String { value: constant_string.to_string() });

        let index = ConstantIndex(self.values.len() - 1);
        self.string_lookup.insert(constant_string.to_string(), index);

        index
    }

    /// Adds a new constant array of strings to the table for the given variable name.
    pub fn add_string_array(&mut self, variable_name: AstUniqueName, values: Vec<String>) -> ConstantIndex {
        if self.values.len() == usize::MAX {
            ICE!("Exhausted ConstantTable string entries");
        }

        self.values.push(ConstantValue::StringArray { variable_name, values });

        ConstantIndex(self.values.len() - 1)
    }

    /// Adds a new `f32` constant to the table and returns its unique identifier, or if the constant already exists then
    /// returns the existing unique identifier that represents the constant value.
    pub fn add_f32(&mut self, constant_value: f32, required_alignment: usize) -> ConstantIndex {
        assert!(required_alignment >= 4 && required_alignment.is_multiple_of(4));

        let key = constant_value.to_bits();

        // If this constant already exists then return its index, but also update its alignment if necessary.
        if let Some(&index) = self.float32_lookup.get(&key) {
            let Some(ConstantValue::F32 { alignment, .. }) = self.values.get_mut(index.0) else {
                ICE!("ConstantTable is missing entry");
            };

            if *alignment < required_alignment {
                *alignment = required_alignment;
            }

            return index;
        }

        if self.values.len() == usize::MAX {
            ICE!("Exhausted ConstantTable float entries");
        }

        self.values.push(ConstantValue::F32 { value: constant_value, alignment: required_alignment });

        let index = ConstantIndex(self.values.len() - 1);
        self.float32_lookup.insert(key, index);

        index
    }

    /// Adds a new `f64` constant to the table and returns its unique identifier, or if the constant already exists then
    /// returns the existing unique identifier that represents the constant value.
    pub fn add_f64(&mut self, constant_value: f64, required_alignment: usize) -> ConstantIndex {
        assert!(required_alignment >= 8 && required_alignment.is_multiple_of(8));

        let key = constant_value.to_bits();

        // If this constant already exists then return its index, but also update its alignment if necessary.
        if let Some(&index) = self.float64_lookup.get(&key) {
            let Some(ConstantValue::F64 { alignment, .. }) = self.values.get_mut(index.0) else {
                ICE!("ConstantTable is missing entry");
            };

            if *alignment < required_alignment {
                *alignment = required_alignment;
            }

            return index;
        }

        if self.values.len() == usize::MAX {
            ICE!("Exhausted ConstantTable float entries");
        }

        self.values.push(ConstantValue::F64 { value: constant_value, alignment: required_alignment });

        let index = ConstantIndex(self.values.len() - 1);
        self.float64_lookup.insert(key, index);

        index
    }

    /// Returns the value for the constant at the given index.
    pub fn get_constant_value_by_index(&self, index: ConstantIndex) -> &ConstantValue {
        &self.values[index.0]
    }

    /// Returns the value for the given constant.
    pub fn get_constant_value_by_name(&self, name: &str) -> &ConstantValue {
        debug_assert!(name.starts_with("const."));

        let Ok(index) = name.strip_prefix("const.").unwrap().parse::<usize>() else {
            ICE!("Cannot convert '{name}' to index");
        };

        self.get_constant_value_by_index(ConstantIndex(index))
    }

    /// Creates a symbol name for the given constant index.
    pub fn make_const_symbol_name(&self, index: ConstantIndex) -> String {
        match &self.values[index.0] {
            ConstantValue::StringArray { variable_name, .. } => format!("const.{}.{}", index.0, variable_name),
            _ => format!("const.{}", index.0),
        }
    }

    /// Returns a vector of all the floating point constants.
    pub fn get_float_constants(&self) -> Vec<ConstantFloatEntry> {
        self.values
            .iter()
            .enumerate()
            .filter_map(|(index, const_value)| {
                let index = ConstantIndex(index);

                match const_value {
                    ConstantValue::F32 { value, alignment } => Some(ConstantFloatEntry {
                        index,
                        value: UnsignedValue::U32(value.to_bits()),
                        alignment: *alignment,
                    }),
                    ConstantValue::F64 { value, alignment } => Some(ConstantFloatEntry {
                        index,
                        value: UnsignedValue::U64(value.to_bits()),
                        alignment: *alignment,
                    }),
                    _ => None,
                }
            })
            .collect()
    }
}
