// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `utils` module provides utility functions to assist with type checking.

use crate::parser::AstConstantValue;

/// Takes ownership of the given `Vec` and returns a new `Vec` with all consecutive `ZeroBytes` items merged together.
pub fn combine_consecutive_zero_bytes(values: Vec<AstConstantValue>) -> Vec<AstConstantValue> {
    values.into_iter().fold(Vec::new(), |mut acc, this_value| {
        let prev_value = acc.last_mut();

        let merged = match (prev_value, &this_value) {
            (Some(AstConstantValue::ZeroBytes(prev_size)), AstConstantValue::ZeroBytes(this_size)) => {
                *prev_size += this_size;
                true
            }
            _ => false,
        };

        if !merged {
            acc.push(this_value);
        }

        acc
    })
}
