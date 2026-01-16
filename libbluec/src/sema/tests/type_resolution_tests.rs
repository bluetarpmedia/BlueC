// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::type_check;
use super::super::type_resolution;

use crate::compiler_driver;
use crate::lexer;
use crate::parser;
use crate::parser::AstType;
use crate::parser::recursive_descent;

use std::io::BufReader;
use std::io::Cursor;
use std::vec;

#[test]
fn invalid_types() {
    // Function cannot return a function type
    verify_sema_error("int (foo(void))(void);");
    verify_sema_error("typedef int MyFun(int); MyFun func(void);");

    // Function pointer cannot return a function type
    verify_sema_error("int ((*invalid_func_ptr)(int, int))(void);");
    verify_sema_error("typedef int MyFun(int); MyFun (*fn)(int);");
    verify_sema_error("typedef int MyFun(int); MyFun (*array_of_fn_ptr[10])(int);");

    // Cannot declare an array of function type
    verify_sema_error("int arr[5](void);");
    verify_sema_error("int (arr[5])(void);");
    verify_sema_error("typedef int MyFun(int); MyFun arr[5];");
}

#[test]
fn simple_variables() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "int a;", vec![AstType::Int]);
    verify_types(&mut driver, "int (a);", vec![AstType::Int]);
    verify_types(&mut driver, "int ((((a)))), (b), (((c)));", vec![AstType::Int, AstType::Int, AstType::Int]);
    verify_types(&mut driver, "int ((a)) = 1;", vec![AstType::Int]);
    verify_types(&mut driver, "int (a) = (1), (b) = (2), (c) = (3);", vec![AstType::Int, AstType::Int, AstType::Int]);
    verify_types(&mut driver, "unsigned a;", vec![AstType::UnsignedInt]);
    verify_types(&mut driver, "signed a, b;", vec![AstType::Int, AstType::Int]);
    verify_types(&mut driver, "int a, b, c, d;", vec![AstType::Int, AstType::Int, AstType::Int, AstType::Int]);
    verify_types(&mut driver, "int a = 1, b = 2, c = 3;", vec![AstType::Int, AstType::Int, AstType::Int]);
    verify_types(&mut driver, "double x = 1.0, y = 2.0;", vec![AstType::Double, AstType::Double]);
    verify_types(&mut driver, "extern int a;", vec![AstType::Int]);
    verify_types(&mut driver, "static int a;", vec![AstType::Int]);
}

#[test]
fn pointer_variables() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "int*a;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int *a;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int **a;", vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int))]);
    verify_types(
        &mut driver,
        "int ***a;",
        vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int)))],
    );
    verify_types(
        &mut driver,
        "int *(*(*(a)));",
        vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int)))],
    );
    verify_types(
        &mut driver,
        "int (*(*(*(a))));",
        vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int)))],
    );

    verify_types(&mut driver, "int *a, b, c;", vec![AstType::new_pointer_to(AstType::Int), AstType::Int, AstType::Int]);

    verify_types(
        &mut driver,
        "int *a, *b, *c;",
        vec![
            AstType::new_pointer_to(AstType::Int),
            AstType::new_pointer_to(AstType::Int),
            AstType::new_pointer_to(AstType::Int),
        ],
    );

    verify_types(
        &mut driver,
        "long long a, b, *c;",
        vec![AstType::LongLong, AstType::LongLong, AstType::new_pointer_to(AstType::LongLong)],
    );
    verify_types(&mut driver, "int *a = 0;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int **a = 0;", vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int))]);
    verify_types(&mut driver, "int *(a) = 0;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int *a = 10 - 10;", vec![AstType::new_pointer_to(AstType::Int)]);

    // Pointer to an array of 3 ints
    verify_types(&mut driver, "int (*ptr)[3];", vec![AstType::new_pointer_to(AstType::new_array(AstType::Int, 3))]);
}

#[test]
fn arrays() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "int array[3];", vec![AstType::new_array(AstType::Int, 3)]);
    verify_types(&mut driver, "int array[3][5];", vec![AstType::new_array(AstType::new_array(AstType::Int, 5), 3)]);
    verify_types(
        &mut driver,
        "int array[33][44][55];",
        vec![AstType::new_array(AstType::new_array(AstType::new_array(AstType::Int, 55), 44), 33)],
    );

    // Array of 3 pointers to int
    verify_types(&mut driver, "int *arr[3];", vec![AstType::new_array(AstType::new_pointer_to(AstType::Int), 3)]);

    // Array of 5 pointers to arrays of 3 ints
    verify_types(
        &mut driver,
        "int (*array_of_five_ptrs[5])[3];",
        vec![AstType::new_array(AstType::new_pointer_to(AstType::new_array(AstType::Int, 3)), 5)],
    );

    // Array of 4 arrays of 3 pointers to long
    verify_types(
        &mut driver,
        "long *coords[4][3];",
        vec![AstType::new_array(AstType::new_array(AstType::new_pointer_to(AstType::Long), 3), 4)],
    );

    // Array of 10 function pointers
    verify_types(
        &mut driver,
        "float (*array_of_fn_ptr[10])(int);",
        vec![AstType::new_array(AstType::new_pointer_to(AstType::new_fn(AstType::Float, vec![AstType::Int])), 10)],
    );
}

#[test]
fn functions() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "int main(void);", vec![AstType::new_fn(AstType::Int, vec![])]);
    verify_types(&mut driver, "void no_op(void);", vec![AstType::new_fn(AstType::Void, vec![])]);
    verify_types(&mut driver, "static void no_op(void);", vec![AstType::new_fn(AstType::Void, vec![])]);
    verify_types(&mut driver, "extern void no_op(void);", vec![AstType::new_fn(AstType::Void, vec![])]);

    verify_types(
        &mut driver,
        "unsigned long long no_op(long long);",
        vec![AstType::new_fn(AstType::UnsignedLongLong, vec![AstType::LongLong])],
    );

    verify_types(
        &mut driver,
        "int calculate(int a, int b);",
        vec![AstType::new_fn(AstType::Int, vec![AstType::Int, AstType::Int])],
    );

    verify_types(
        &mut driver,
        "float calculate(double, long double);",
        vec![AstType::new_fn(AstType::Float, vec![AstType::Double, AstType::LongDouble])],
    );

    verify_types(
        &mut driver,
        "int calc1(void), calc2(void), calc3(void);",
        vec![
            AstType::new_fn(AstType::Int, vec![]),
            AstType::new_fn(AstType::Int, vec![]),
            AstType::new_fn(AstType::Int, vec![]),
        ],
    );

    verify_types(
        &mut driver,
        "int* get_ptr(void);",
        vec![AstType::new_fn(AstType::new_pointer_to(AstType::Int), vec![])],
    );
    verify_types(
        &mut driver,
        "int* get_ptr(float *a);",
        vec![AstType::new_fn(AstType::new_pointer_to(AstType::Int), vec![AstType::new_pointer_to(AstType::Float)])],
    );

    // Function returning a pointer to an array
    verify_types(
        &mut driver,
        "int (*my_func(void))[5];",
        vec![AstType::new_fn(AstType::new_pointer_to(AstType::new_array(AstType::Int, 5)), vec![])],
    );
}

#[test]
fn function_pointers() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "int (*fn)(void);", vec![AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![]))]);

    verify_types(
        &mut driver,
        "int (*fn)(float, double);",
        vec![AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![AstType::Float, AstType::Double]))],
    );

    verify_types(
        &mut driver,
        "int (*fn)(float (*)(void));",
        vec![AstType::new_pointer_to(AstType::new_fn(
            AstType::Int,
            vec![AstType::new_pointer_to(AstType::new_fn(AstType::Float, vec![]))],
        ))],
    );

    verify_types(
        &mut driver,
        "int (**fn)(void);",
        vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![])))],
    );

    verify_types(
        &mut driver,
        "int (***fn)(void);",
        vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_pointer_to(AstType::new_fn(
            AstType::Int,
            vec![],
        ))))],
    );
}

#[test]
fn type_aliases() {
    let mut driver = compiler_driver::Driver::for_testing();

    verify_types(&mut driver, "typedef float* MyF32Ptr;", vec![AstType::new_pointer_to(AstType::Float)]);
    verify_types(
        &mut driver,
        "typedef float* MyF32Ptr; MyF32Ptr ptr = 0;",
        vec![AstType::new_pointer_to(AstType::Float), AstType::new_pointer_to(AstType::Float)],
    );

    verify_types(&mut driver, "typedef int MyInt;", vec![AstType::Int]);
    verify_types(
        &mut driver,
        "typedef int MyInt; MyInt a, b, c;",
        vec![AstType::Int, AstType::Int, AstType::Int, AstType::Int],
    );

    verify_types(
        &mut driver,
        "typedef int (*MyFn)(void);",
        vec![AstType::new_pointer_to(AstType::new_fn(AstType::Int, vec![]))],
    );
}

#[test]
fn string_round_trip() {
    let mut driver = compiler_driver::Driver::for_testing();

    {
        let expected_t = AstType::new_array(AstType::new_array(AstType::new_array(AstType::Int, 55), 44), 33);
        verify_types(&mut driver, "int array[33][44][55];", vec![expected_t.clone()]);
        assert_eq!(expected_t.to_string(), "int [33][44][55]");
    }
}

fn verify_sema_error(source: &str) {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);
    let parsed = recursive_descent::parse_translation_unit(&mut parser, &mut driver);

    if parsed.is_err() {
        println!("Expect parse to succeed but it failed with:\n{source}");
        assert!(false);
        return;
    }

    if driver.has_error_diagnostics() {
        driver.debug_print_diagnostics();
    }
    assert!(!driver.has_error_diagnostics(), "Did not expect errors after parsing");

    let mut ast_root = parsed.unwrap();

    _ = type_check::type_check(&mut ast_root, parser.metadata, &mut driver);

    assert!(driver.has_error_diagnostics(), "Expect errors after sema type checking");
}

fn verify_types(driver: &mut compiler_driver::Driver, source: &str, expected: Vec<AstType>) {
    let types = parse_and_resolve_types(driver, source);
    if expected.is_empty() && types.is_none() {
        return;
    }
    let types = types.unwrap();

    let equal = types == expected;
    if !equal {
        println!("{source}");
    }

    assert_eq!(types, expected);
}

fn parse_and_resolve_types(driver: &mut compiler_driver::Driver, source: &str) -> Option<Vec<AstType>> {
    let mut parser = make_parser(driver, source);
    let parsed = recursive_descent::parse_translation_unit(&mut parser, driver);

    if parsed.is_err() {
        println!("Expect parse to succeed but it failed with:\n{source}");
        assert!(false);
        return None;
    }
    let mut ast_root = parsed.unwrap();

    let (mut symbols, ..) = type_check::type_check(&mut ast_root, parser.metadata, driver);

    let decls = ast_root.0;
    let types = decls
        .into_iter()
        .filter_map(|decl| {
            let declared_type = decl.get_declared_type();
            type_resolution::resolve_declared_type(declared_type, Some(&mut symbols), Some(driver)).ok()
        })
        .collect();

    Some(types)
}

fn make_parser(driver: &mut compiler_driver::Driver, source: &str) -> parser::Parser {
    let cursor = Cursor::new(source.as_bytes());
    let mut reader = BufReader::new(cursor);
    let tokens = lexer::lex_buf_reader(driver, &mut reader);
    parser::Parser::new(tokens)
}
