// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::expr;
use super::super::recursive_descent::block;
use super::super::{AstBlockItem, AstExpression, AstStatement, ParseResult, Parser};
use super::utils::make_parser;

use crate::compiler_driver::Driver;
use crate::lexer::{Token, TokenType};

#[test]
fn unary_negate_operation() {
    // -5
    let tokens =
        vec![Token::without_location(TokenType::Minus), Token::without_location(TokenType::make_int_literal("5"))];
    test_expression(tokens, "Negate(5)");
}

#[test]
fn unary_bitwise_not_operation() {
    // ~123
    let tokens = vec![
        Token::without_location(TokenType::BitwiseNot),
        Token::without_location(TokenType::make_int_literal("123")),
    ];
    test_expression(tokens, "BitwiseNot(123)");
}

#[test]
fn parenthesised_unary_operations() {
    // -(~321)
    let tokens = vec![
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::BitwiseNot),
        Token::without_location(TokenType::make_int_literal("321")),
        Token::without_location(TokenType::CloseParen),
    ];
    test_expression(tokens, "Negate(BitwiseNot(321))");
}

#[test]
fn binary_plus_operation() {
    // 1 + 2
    let tokens = vec![
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("2")),
    ];
    test_expression(tokens, "Add(1,2)");
}

#[test]
fn binary_add_subtract() {
    // 1 + 2 - 3 + 4 - 5   --->  (((1 + 2) - 3) + 4) - 5
    let tokens = vec![
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("2")),
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::make_int_literal("3")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("4")),
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::make_int_literal("5")),
    ];
    test_expression(tokens, "Subtract(Add(Subtract(Add(1,2),3),4),5)");
}

#[test]
fn binary_mul_div_rem() {
    // 1 * 2 / 3 % 4   --->  ((1 * 2) / 3) % 4
    let tokens = vec![
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::make_int_literal("2")),
        Token::without_location(TokenType::Divide),
        Token::without_location(TokenType::make_int_literal("3")),
        Token::without_location(TokenType::Remainder),
        Token::without_location(TokenType::make_int_literal("4")),
    ];
    test_expression(tokens, "Remainder(Divide(Multiply(1,2),3),4)");
}

#[test]
fn binary_operations() {
    // 1 + 2 * 3 - 4 / 5 + 6 % 7   --->   ((1 + (2 * 3)) - (4 / 5)) + (6 % 7)
    let tokens = vec![
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("2")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::make_int_literal("3")),
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::make_int_literal("4")),
        Token::without_location(TokenType::Divide),
        Token::without_location(TokenType::make_int_literal("5")),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::make_int_literal("6")),
        Token::without_location(TokenType::Remainder),
        Token::without_location(TokenType::make_int_literal("7")),
    ];
    test_expression(tokens, "Add(Subtract(Add(1,Multiply(2,3)),Divide(4,5)),Remainder(6,7))");
}

#[test]
fn block_with_chain_compound_assignment() {
    // int x;
    // int a;
    // int b;
    // int c;
    // int d;
    // int e;
    // int f;
    // x = a += b *= c -= d /= e += f %= 5   --->  x = (a += (b *= (c -= (d /= (e += (f %= 5)))));
    let tokens = vec![
        Token::without_location(TokenType::OpenBrace),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("x")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("a")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("b")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("c")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("d")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("e")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("int")),
        Token::without_location(TokenType::make_identifier("f")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::make_identifier("x")),
        Token::without_location(TokenType::Assignment),
        Token::without_location(TokenType::make_identifier("a")),
        Token::without_location(TokenType::AdditionAssignment),
        Token::without_location(TokenType::make_identifier("b")),
        Token::without_location(TokenType::MultiplyAssignment),
        Token::without_location(TokenType::make_identifier("c")),
        Token::without_location(TokenType::SubtractionAssignment),
        Token::without_location(TokenType::make_identifier("d")),
        Token::without_location(TokenType::DivideAssignment),
        Token::without_location(TokenType::make_identifier("e")),
        Token::without_location(TokenType::AdditionAssignment),
        Token::without_location(TokenType::make_identifier("f")),
        Token::without_location(TokenType::RemainderAssignment),
        Token::without_location(TokenType::make_int_literal("5")),
        Token::without_location(TokenType::Semicolon),
        Token::without_location(TokenType::CloseBrace),
    ];

    let expected_compound_assignment_expr_str = "AdditionAssignment(a,MultiplyAssignment(b,SubtractionAssignment(c,DivideAssignment(d,AdditionAssignment(e,RemainderAssignment(f,5))))))";

    let mut driver = Driver::for_testing();
    let mut parser = Parser::new(tokens);
    let block = block::parse_block(&mut parser, &mut driver);
    if block.is_err() || driver.has_error_diagnostics() {
        // Need to run `cargo test -- --show-output` to see this
        driver.debug_print_diagnostics();
    }
    assert!(block.is_ok());
    let block = block.unwrap();
    let block_items = block.0;
    assert_eq!(block_items.len(), 8);

    let verify_decl = |block_item: &AstBlockItem| {
        if let AstBlockItem::Declaration(_) = block_item {
            assert!(true);
        } else {
            assert!(false, "Block item is not a declaration");
        }
    };

    // First 7 block items are variable declarations
    for i in 0..7 {
        verify_decl(&block_items[i]);
    }

    // Next block item is an expression statement
    if let AstBlockItem::Statement(stmt) = &block_items[7] {
        if let AstStatement::Expression(full_expr) = stmt {
            if let AstExpression::Assignment { rhs, .. } = &full_expr.expr {
                let rhs_str = to_string(rhs);
                assert!(rhs_str == expected_compound_assignment_expr_str);
            } else {
                assert!(false, "Expected assignment expression");
            }
        } else {
            assert!(false, "Expected expression statement");
        }
    } else {
        assert!(false, "Expected statement");
    }
}

#[test]
fn complex_expression() {
    // ~((1 * 2) / -3) + (5 % 1)
    let tokens = vec![
        Token::without_location(TokenType::BitwiseNot),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::Multiply),
        Token::without_location(TokenType::make_int_literal("2")),
        Token::without_location(TokenType::CloseParen),
        Token::without_location(TokenType::Divide),
        Token::without_location(TokenType::Minus),
        Token::without_location(TokenType::make_int_literal("3")),
        Token::without_location(TokenType::CloseParen),
        Token::without_location(TokenType::Plus),
        Token::without_location(TokenType::OpenParen),
        Token::without_location(TokenType::make_int_literal("5")),
        Token::without_location(TokenType::Remainder),
        Token::without_location(TokenType::make_int_literal("1")),
        Token::without_location(TokenType::CloseParen),
    ];
    test_expression(tokens, "Add(BitwiseNot(Divide(Multiply(1,2),Negate(3))),Remainder(5,1))");
}

#[test]
fn valid_cast_syntax() {
    let cases = vec![
        "(short)-1",
        "(short)8589934592",
        "(short)-1.0",
        "(short)1.0",
        "(unsigned short)8589934592",
        "(unsigned short)-3.0",
        "(unsigned short)3.0",
        "(int)8589934592",
        "(int)(8589934592 + 8589934592)",
        "(int)(long)(int)1",
        "(int)(long)(int)-1",
        "(int)-5.0",
        "(int)-0.123",
        "(int)0.123",
        "(int)5.0",
        "(unsigned int)0",
        "(unsigned int)-1",
        "(unsigned int)100",
        "(unsigned int)8589934592",
        "(unsigned int)100.123",
        "(long)(short)8589934592",
        "(long)8589934592",
        "(long)8589934592 + 1LL",
        "(long)8589934592 + 1L",
        "(long)8589934592 + 1",
        "(long)-99.99",
        "(long)99.99",
        "(unsigned long)0",
        "(unsigned long)100",
        "(unsigned long long)100",
        "(unsigned long)-1",
        "(unsigned long)(-1LL)",
        "(unsigned long)3.14",
        "(double)1",
        "(double)-1",
        "(double)1 + (double)2",
        "(unsigned int)(1 + 2 + 3) / (double)2",
        "(double)0.0f",
        "(double)-0.0f",
        "(double)1.0f",
        "(double)(12e30f + -12e30f)",
        "(double)((2.0f + 3.0f) - 127.5f * 4.0f)",
        "(float)0.0",
        "(float)-0.0",
        "(float)1.0",
        "(float)(12e30 + -12e30)",
        "(float)((2.0 + 3.0) - 127.5 * 4.0)",
        "(int *)0",
        "(int (*))0",
        "(int ((*)))0",
        "(int **)0",
        "(int ***)0",
        "(int *)(float *)(void *)0",
        "((int *)(float *)(void *)0)",
        "((int *)(float *)(void *)(0))",
        "(int (*))(float (*))(void (*))0",
    ];

    let mut driver = Driver::for_testing();
    for case in cases {
        assert!(try_parse_expression(&mut driver, case).is_ok());
    }
}

#[test]
fn invalid_cast_syntax() {
    let cases = vec![
        "(extern int)-1",
        "(static int)-1",        
        "(int *ptr)0",
        "(void (*ptr))0",
        "(void ((*)ptr))0",
        "((int *))(float *)(void *)0",
        "((int *)(float *))(void *)0",
    ];

    let mut driver = Driver::for_testing();
    for case in cases {
        assert!(try_parse_expression(&mut driver, case).is_err());
    }
}

fn try_parse_expression(driver: &mut Driver, expr: &str) -> ParseResult<AstExpression> {
    let mut parser = make_parser(driver, expr);
    expr::parse_expression(&mut parser, driver)
}

fn test_expression(tokens: Vec<Token>, expected_str: &str) {
    let mut driver = Driver::for_testing();
    let mut parser = Parser::new(tokens);
    let ast = expr::parse_expression(&mut parser, &mut driver);
    if ast.is_err() {
        // Need to run `cargo test -- --show-output` to see this
        driver.debug_print_diagnostics();
    }
    assert!(ast.is_ok());
    let ast = ast.unwrap();
    let ast_str = to_string(&ast);
    assert!(ast_str == expected_str, "{}", ast_str);
}

fn to_string(expr: &AstExpression) -> String {
    let mut str = String::new();
    match expr {
        AstExpression::BinaryOperation { op, left, right, .. } => {
            str.push_str(&format!("{}(", op));

            str.push_str(to_string(left).as_ref());
            str.push(',');
            str.push_str(to_string(right).as_ref());

            str.push(')');
        }

        AstExpression::UnaryOperation { op, expr, .. } => {
            str.push_str(&format!("{}(", op));

            str.push_str(to_string(expr).as_ref());

            str.push(')');
        }

        AstExpression::IntegerLiteral { value, .. } => {
            str.push_str(&format!("{}", value));
        }

        AstExpression::Variable { name, .. } => {
            str.push_str(&format!("{}", name));
        }

        #[allow(unreachable_patterns)]
        _ => panic!("Unhandled"),
    }
    str
}
