// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver::Driver;
use crate::parser::recursive_descent;
use crate::parser::tests::utils;
use crate::parser::{
    AstExpression, AstExpressionKind, AstFunction, AstRoot, AstStatement, AstVariableDeclaration,
    AstVariableInitializer,
};

use super::super::visitor;

#[test]
fn visit_function_definitions() {
    let mut driver = Driver::for_testing();

    let source = "
        int calc(int a, int b, int c);
        float get_salary(int age, float baseline);
        void no_op(void);

        int main(void) {
            return 0;
        }

        float get_salary(int age, float baseline) {
            return age * baseline;
        }";

    let mut ast_root = parse_source(source, &mut driver);

    let mut func_count = 0;

    visitor::visit_function_defns(&mut ast_root, &mut |_: &mut AstFunction| {
        func_count += 1;
    });

    assert_eq!(func_count, 2);
}

#[test]
fn visit_statements() {
    let mut driver = Driver::for_testing();

    let source = "
        int main(void) {
            int a = 1;
            a += 1;
            return a;
        }";

    let mut ast_root = parse_source(source, &mut driver);

    let mut expr_stmt_count = 0;
    let mut ret_stmt_count = 0;
    let mut other_stmt_count = 0;

    visitor::visit_function_defns(&mut ast_root, &mut |func: &mut AstFunction| {
        let block = func.body.as_mut().unwrap();

        visitor::visit_statements_in_block(block, &mut |stmt: &mut AstStatement| match stmt {
            AstStatement::Expression(_) => expr_stmt_count += 1,
            AstStatement::Return(_) => ret_stmt_count += 1,
            _ => other_stmt_count += 1,
        });
    });

    assert_eq!(expr_stmt_count, 1);
    assert_eq!(ret_stmt_count, 1);
    assert_eq!(other_stmt_count, 0);
}

#[test]
fn visit_nonliteral_expressions() {
    let mut driver = Driver::for_testing();

    let source = r#"
        int square(int x) { return x * x; }
        int main(void) {
            int a = 1, b = 2;
            int *ptr = 0;

            a = -b;
            a = a + b;
            a = b;
            a = (a > b) ? a : b;
            a = square(b);
            ptr = &a;
            b = *ptr;
            a = ptr[0];
            a = (int)b;

            return a;
        }"#;

    let mut ast_root = parse_source(source, &mut driver);

    let mut ident_a_count = 0;
    let mut ident_b_count = 0;
    let mut ident_ptr_count = 0;
    let mut unary_count = 0;
    let mut binary_count = 0;
    let mut assign_count = 0;
    let mut ternary_count = 0;
    let mut fncall_count = 0;
    let mut addressof_count = 0;
    let mut deref_count = 0;
    let mut subscript_count = 0;
    let mut cast_count = 0;

    visitor::visit_full_expressions(&mut ast_root, &mut |full_expr: &mut AstExpression| {
        visitor::visit_sub_expressions(full_expr, &mut |expr: &mut AstExpression| match expr.kind() {
            AstExpressionKind::Unary { .. } => unary_count += 1,
            AstExpressionKind::Binary { .. } => binary_count += 1,
            AstExpressionKind::Assignment { .. } => assign_count += 1,
            AstExpressionKind::Conditional { .. } => ternary_count += 1,
            AstExpressionKind::FunctionCall { .. } => fncall_count += 1,
            AstExpressionKind::Deref { .. } => deref_count += 1,
            AstExpressionKind::AddressOf { .. } => addressof_count += 1,
            AstExpressionKind::Subscript { .. } => subscript_count += 1,
            AstExpressionKind::Cast { .. } => cast_count += 1,
            AstExpressionKind::Ident { name, .. } => {
                if name == "a" {
                    ident_a_count += 1;
                } else if name == "b" {
                    ident_b_count += 1;
                } else if name == "ptr" {
                    ident_ptr_count += 1;
                }
            }
            _ => (),
        });
    });

    assert_eq!(ident_a_count, 12);
    assert_eq!(ident_b_count, 8);
    assert_eq!(ident_ptr_count, 3);
    assert_eq!(unary_count, 1);
    assert_eq!(binary_count, 3);
    assert_eq!(assign_count, 9);
    assert_eq!(ternary_count, 1);
    assert_eq!(fncall_count, 1);
    assert_eq!(addressof_count, 1);
    assert_eq!(deref_count, 1);
    assert_eq!(subscript_count, 1);
    assert_eq!(cast_count, 1);
}

#[test]
fn visit_literal_expressions() {
    let mut driver = Driver::for_testing();

    let source = r#"
        int main(void) {
            char c = 'a';
            int a = 100;
            short s = 20;
            long l = 1;
            float f = 1.0f;
            double d = 1.0;
            char *str = "test";
            return 111;
        }"#;

    let mut ast_root = parse_source(source, &mut driver);

    let mut char_lit_count = 0;
    let mut int_literals = Vec::new();
    let mut fp_lit_count = 0;
    let mut str_lit_count = 0;

    visitor::visit_full_expressions(&mut ast_root, &mut |full_expr: &mut AstExpression| {
        visitor::visit_sub_expressions(full_expr, &mut |expr: &mut AstExpression| match expr.kind() {
            AstExpressionKind::CharLiteral { .. } => char_lit_count += 1,
            AstExpressionKind::StringLiteral { .. } => str_lit_count += 1,
            AstExpressionKind::IntegerLiteral { value, .. } => int_literals.push(*value),
            AstExpressionKind::FloatLiteral { .. } => fp_lit_count += 1,
            _ => (),
        });
    });

    assert_eq!(char_lit_count, 1);
    assert_eq!(int_literals, vec![100, 20, 1, 111]);
    assert_eq!(fp_lit_count, 2);
    assert_eq!(str_lit_count, 1);
}

#[test]
fn visit_variable_declarations() {
    let mut driver = Driver::for_testing();

    let source = "
        int global_int = 1;
        char arr[4] = {'t', 'e', 's', 't'};
        
        int main(void) {
            int local1 = 1;
            short local2 = 2;
            int nums[3] = {1, 2, 3};
            for (int i = 0; i < 10; ++i) {}
            return 0;
        }";

    let mut ast_root = parse_source(source, &mut driver);

    let mut scalar_init_count = 0;
    let mut aggregate_init_count = 0;

    visitor::visit_variable_declarations(&mut ast_root, &mut |var_decl: &mut AstVariableDeclaration| {
        if let Some(init) = var_decl.initializer.as_ref() {
            match init {
                AstVariableInitializer::Scalar(_) => scalar_init_count += 1,
                AstVariableInitializer::Aggregate { .. } => aggregate_init_count += 1,
            }
        }
    });

    assert_eq!(scalar_init_count, 4);
    assert_eq!(aggregate_init_count, 2);
}

fn parse_source(source: &str, driver: &mut Driver) -> AstRoot {
    let mut parser = utils::make_parser(driver, source);
    let parsed = recursive_descent::parse_translation_unit(&mut parser, driver);

    if driver.has_error_diagnostics() {
        println!("Expect parse to succeed but it failed with:\n{source}");
        assert!(false);
    }

    parsed.unwrap()
}
