// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `utils` module defines utility functions for the BlueTac translator.

use super::BlueTacTranslator;

use crate::compiler_driver::Driver;
use crate::parser::AstVariableInitializer;
use crate::sema::constant_eval;

/// Merges adjacent null string escape codes ("\\000") into a single string.
///
/// { "abc\\000", "\\000", "\\000", "def" }  -->  { "abc\\000", "\\000\\000", "def" }
pub fn merge_adjacent_nulls(strings: &mut Vec<String>) {
    if strings.is_empty() {
        return;
    }

    *strings = strings.drain(..).fold(Vec::new(), |mut acc, current| {
        if let Some(prev) = acc.last_mut() {
            let is_null = current == "\\000";
            let prev_is_only_nulls = prev.as_bytes().chunks(4).all(|c| c == b"\\000");

            if is_null && prev_is_only_nulls {
                prev.push_str(&current);
            } else {
                acc.push(current);
            }
        } else {
            acc.push(current);
        }

        acc
    });
}

/// Is the given variable initializer a constant initializer?
pub fn is_constant_initializer(
    init: &AstVariableInitializer,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) -> bool {
    match init {
        AstVariableInitializer::Scalar(full_expr) => {
            let ctx = constant_eval::ConstantEvalContext::new(
                &translator.metadata,
                &mut translator.symbols,
                &mut translator.constants,
                driver,
            );

            let constant_value = constant_eval::evaluate_constant_full_expr(full_expr, ctx);
            constant_value.is_some()
        }

        AstVariableInitializer::Aggregate { init, .. } => {
            init.iter().all(|ini| is_constant_initializer(ini, translator, driver))
        }
    }
}

