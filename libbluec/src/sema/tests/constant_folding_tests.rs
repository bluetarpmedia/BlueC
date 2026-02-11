// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver::Driver;
use crate::parser::recursive_descent;
use crate::parser::tests::utils;
use crate::parser::{
    AstBinaryOp, AstExpression, AstFullExpression, AstFunction, AstRoot, AstStatement, AstVariableDeclaration,
    AstVariableInitializer, Parser,
};

use super::super::constant_folding;
use super::super::type_check;
use super::super::visitor;

#[test]
fn return_int() {
    let test_return_expr = |source: &str| -> u64 {
        let mut driver = Driver::for_testing();
        let mut ast_root = parse_and_fold(source, &mut driver);
        get_return_integer_value(&mut ast_root)
    };

    assert_eq!(test_return_expr("int main(void) { return 1 + 2 + 3; }"), 6);
    assert_eq!(test_return_expr("int main(void) { return 3 * 4; }"), 12);
    assert_eq!(test_return_expr("int main(void) { return 3 * 4 + 1; }"), 13);
    assert_eq!(test_return_expr("int main(void) { return 1 + 3 * 4 + 1; }"), 14);
    assert_eq!(test_return_expr("int main(void) { return 10 - 10; }"), 0);
    assert_eq!(test_return_expr("int main(void) { return 11 / 11 + 12 * 2; }"), 25);
}

#[test]
fn return_double() {
    let test_return_expr = |source: &str| -> f64 {
        let mut driver = Driver::for_testing();
        let mut ast_root = parse_and_fold(source, &mut driver);
        get_return_f64_value(&mut ast_root)
    };

    assert_eq!(test_return_expr("double calc(void) { return 1.0 + 0.5; }"), 1.5);
    assert_eq!(test_return_expr("double calc(void) { return 3.14 * 2 / 1.0; }"), 6.28);
    assert_eq!(test_return_expr("double calc(void) { return 1.0 * 2 - 0.1; }"), 1.9);
}

#[test]
fn function_call_argument() {
    let test_function_call_int_argument = |source: &str| -> u64 {
        let mut driver = Driver::for_testing();
        let mut ast_root = parse_and_fold(source, &mut driver);
        get_function_call_int_argument(&mut ast_root)
    };

    assert_eq!(test_function_call_int_argument("int calc(int age); int main(void) { return calc(10 - 5); }"), 5);
    assert_eq!(test_function_call_int_argument("int calc(int age); int main(void) { return calc(2 * 3 * 4); }"), 24);
    assert_eq!(test_function_call_int_argument("int calc(int age); int main(void) { return calc(100 / 100 + 1); }"), 2);
}

#[test]
fn partial_fold_add() {
    // The a-l variable initializers should all fold to the same expression: (x + y) + 3
    let source = "
        int main(void) {
            int x = 1;
            int y = 1;

            int a = x + 1 + y + 2;
            int b = (x + 1) + (y + 2);
            int c = 1 + 2 + x + y;
            int d = x + y + 1 + 2;
            int e = x + (y + 1) + 2;
            int f = 1 + x + y + 2;
            int g = 1 + 1 + 1 + x + y;
            int h = 1 + x + 1 + y + 1;
            int i = x + y + (3);
            int j = (3) + x + y;
            int k = (1 + 1 + 1) + x + y;
            int l = x + y + (1 + 1 + 1);
        }";

    let mut driver = Driver::for_testing();
    let mut ast_root = parse_and_fold(source, &mut driver);

    let is_expected = |expr: &AstExpression| -> bool {
        let AstExpression::BinaryOperation { op, lhs, rhs, .. } = expr else {
            return false;
        };

        if *op != AstBinaryOp::Add {
            return false;
        }

        let AstExpression::BinaryOperation { op: l_op, lhs: l_left, rhs: l_right, .. } = lhs.as_ref() else {
            return false;
        };

        if *l_op != AstBinaryOp::Add {
            return false;
        }

        let valid_lhs = l_left.is_identifier_with_name("x") && l_right.is_identifier_with_name("y");
        if !valid_lhs {
            return false;
        }

        rhs.is_integer_literal_with_value(3)
    };

    let mut found_expected_count = 0;

    visitor::visit_variable_declarations(&mut ast_root, &mut |var_decl: &mut AstVariableDeclaration| {
        if let Some(init) = var_decl.initializer.as_ref() {
            match init {
                AstVariableInitializer::Scalar(full_expr) => {
                    if is_expected(&full_expr.expr) {
                        found_expected_count += 1;
                    }
                }
                _ => (),
            }
        }
    });

    assert_eq!(found_expected_count, 12);
}

#[test]
fn partial_fold_multiply() {
    // The a-i variable initializers should all fold to the same expression: x * 24
    let source = "
        int main(void) {
            int x = 1;

            int a = x * 24;
            int b = 1 * 24 * x;
            int c = 1 * x * 12 * 2;
            int d = x * 1 * 2 * 3 * 4;
            int e = 1 * x * 2 * 3 * 4;
            int f = 1 * 2 * x * 3 * 4;
            int g = 1 * 2 * 3 * x * 4;
            int h = 1 * 2 * 3 * 4 * x;
            int i = x * (23 + 1);
        }";

    let mut driver = Driver::for_testing();
    let mut ast_root = parse_and_fold(source, &mut driver);

    let is_expected = |expr: &AstExpression| -> bool {
        let AstExpression::BinaryOperation { op, lhs, rhs, .. } = expr else {
            return false;
        };

        if *op != AstBinaryOp::Multiply {
            return false;
        }

        lhs.is_identifier_with_name("x") && rhs.is_integer_literal_with_value(24)
    };

    let mut found_expected_count = 0;

    visitor::visit_variable_declarations(&mut ast_root, &mut |var_decl: &mut AstVariableDeclaration| {
        if let Some(init) = var_decl.initializer.as_ref() {
            match init {
                AstVariableInitializer::Scalar(full_expr) => {
                    if is_expected(&full_expr.expr) {
                        found_expected_count += 1;
                    }
                }
                _ => (),
            }
        }
    });

    assert_eq!(found_expected_count, 9);
}

fn get_return_integer_value(ast_root: &mut AstRoot) -> u64 {
    let mut ret_value = 0;

    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| match stmt {
            AstStatement::Return(full_expr) => {
                if let AstExpression::IntegerLiteral { value, .. } = full_expr.expr {
                    ret_value = value;
                } else {
                    assert!(false, "Not an integer literal");
                }
            }
            _ => (),
        });
    });

    ret_value
}

fn get_return_f64_value(ast_root: &mut AstRoot) -> f64 {
    let mut ret_value = 0.0_f64;

    visitor::visit_functions(ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| match stmt {
            AstStatement::Return(full_expr) => {
                if let AstExpression::FloatLiteral { value, .. } = full_expr.expr {
                    ret_value = value;
                } else {
                    assert!(false, "Not a floating-point literal");
                }
            }
            _ => (),
        });
    });

    ret_value
}

fn get_function_call_int_argument(ast_root: &mut AstRoot) -> u64 {
    let mut ret_value = 0;

    visitor::visit_full_expressions(ast_root, &mut |full_expr: &mut AstFullExpression| {
        visitor::visit_expressions_in_full_expression(full_expr, &mut |expr: &mut AstExpression| match expr {
            AstExpression::FunctionCall { args, .. } => {
                if args.len() == 1 {
                    match args[0] {
                        AstExpression::IntegerLiteral { value, .. } => ret_value = value,
                        _ => (),
                    }
                }
            }
            _ => (),
        });
    });

    ret_value
}

fn parse_and_fold(source: &str, driver: &mut Driver) -> AstRoot {
    let mut parser = utils::make_parser(driver, source);
    let parsed = recursive_descent::parse_translation_unit(&mut parser, driver);

    if parsed.is_err() {
        println!("Expect parse to succeed but it failed with:\n{source}");
        assert!(false);
    }

    let mut ast_root = parsed.unwrap();
    let Parser { metadata, .. } = parser;
    let mut chk = type_check::TypeChecker::new(metadata);

    type_check::type_check(&mut ast_root, &mut chk, driver);

    constant_folding::fold(&mut ast_root, &mut chk, driver);

    ast_root
}
