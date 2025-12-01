// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::lexer::*;

pub fn compare_token_types(actual: &Vec<Token>, expected: &Vec<Token>) {
    assert!(actual.len() == expected.len());
    for (act, exp) in actual.iter().zip(expected.iter()) {
        assert_eq!(act.token_type, exp.token_type);
    }
}
