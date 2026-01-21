// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::compiler_driver;
use crate::parser::recursive_descent::parse_translation_unit;

use super::utils::make_parser;

#[test]
fn invalid_decl_syntax() {
    let cases = vec![
        "int calculate(int a ;",
        "int calculate(int a, ;",
        "int calculate(int a, , ;",
        "int calculate(, ;",
        "int calculate(;",
        "int calculate((());",
        "int (calc(int, int);",
        "int (calc))(int, int);",
        "int a = 1, calculate(void) { return 0; }",
        "int calculate(void) { return 0; }, secret = 333;",
        "static int calculate(int a, ;",
        "extern int calculate(float a, b double ;",
        "int* get_ptr(*);",
    ];

    let mut driver = compiler_driver::Driver::for_testing();
    for case in cases {
        let mut parser = make_parser(&mut driver, case);
        _ = parse_translation_unit(&mut parser, &mut driver);
        let has_errors = driver.has_error_diagnostics();
        if !has_errors {
            println!("Source: {case}");
        }
        assert!(has_errors);
    }
}

#[test]
fn valid_decl_syntax() {
    let cases = vec![
        "int calculate(int, float, double);",
        "int calculate(void), do_tax(float salary), count_numbers(unsigned int);",
        "unsigned foo(float salary, int age, double tax);",
        "extern int add(int a, int b);",
        "static int add(int a, int b);",
        "int a = 1, calculate(int a, int b), b = 2;",
        "int calc(int (a), int (b));",
        "int (calc)(int, int);",
        "int ((calc)(int, int));",
        "int ((calc))(int, int);",
        "int ((calc))(int ((a)), int ((b)));",
        "int* get_ptr(void);",
        "int* get_ptr(int*, float*, double*);",
        "int* get_ptr(int *a, float *b, double *c);",
    ];

    let mut driver = compiler_driver::Driver::for_testing();
    for case in cases {
        let mut parser = make_parser(&mut driver, case);
        _ = parse_translation_unit(&mut parser, &mut driver);
        let has_errors = driver.has_error_diagnostics();
        if has_errors {
            println!("Source: {case}");
            driver.debug_print_diagnostics();
        }
        assert!(!has_errors);
    }
}
