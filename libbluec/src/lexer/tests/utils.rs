// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver::Driver;
use crate::core::FilePosition;

use super::super::Token;
use super::super::line_lexer::LineLexer;
use super::super::tests::utils;

pub fn compare_token_types(actual: &Vec<Token>, expected: &Vec<Token>) {
    assert!(actual.len() == expected.len());
    for (act, exp) in actual.iter().zip(expected.iter()) {
        assert_eq!(act.token_type, exp.token_type);
    }
}

pub fn test_lexer(source_code: &str, expected: &Vec<Token>) {
    let mut driver = Driver::for_testing();
    let mut line_lexer = LineLexer::new(&mut driver, FilePosition::default(), source_code);
    let mut actual = Vec::new();
    loop {
        match line_lexer.get_next_token() {
            Ok(Some(token)) => {
                actual.push(token);
            }
            Ok(None) => break, // No more tokens in this line
            Err(_) => {
                eprintln!("LineLexer failed.");
                driver.debug_print_diagnostics();
                std::process::exit(1);
            }
        }
    }
    utils::compare_token_types(&actual, &expected);
}
