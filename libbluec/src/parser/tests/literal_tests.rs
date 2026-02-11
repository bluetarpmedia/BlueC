// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver;
use crate::parser::expr;

use super::super::AstExpression;
use super::utils::make_parser;

#[test]
fn string_literals() {
    test_string_literal(r#" "single" "#, "single", 6);
    test_string_literal(r#" "\a\b\f\n" "#, "\\007\\b\\f\\n", 4);
    test_string_literal(r#" "test \xFF with hex\n" "#, "test \\377 with hex\\n", 16);
    test_string_literal(r#" "this" "should" "be" "joined" "together" "#, "thisshouldbejoinedtogether", 26);
}

fn test_string_literal(source: &str, expected: &str, expected_len: usize) {
    if let Some(expr) = parse(source) {
        if let AstExpression::StringLiteral { ascii, .. } = expr {
            let ascii_joined = ascii.join("");
            assert_eq!(ascii_joined, expected, "Source: '{source}'");
            assert_eq!(ascii.len(), expected_len, "Source: '{source}'");
        } else {
            assert!(false, "Source '{source}' did not parse as StringLiteral");
        }
    } else {
        assert!(false, "Did not parse '{source}'");
    }
}

fn parse(source: &str) -> Option<AstExpression> {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);
    expr::parse_expression(&mut parser, &mut driver).ok()
}
