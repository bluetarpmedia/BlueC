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
fn variables() {
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
        "long long a, b, *c;",
        vec![AstType::LongLong, AstType::LongLong, AstType::new_pointer_to(AstType::LongLong)],
    );
    verify_types(&mut driver, "int *a = 0;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int **a = 0;", vec![AstType::new_pointer_to(AstType::new_pointer_to(AstType::Int))]);
    verify_types(&mut driver, "int *(a) = 0;", vec![AstType::new_pointer_to(AstType::Int)]);
    verify_types(&mut driver, "int *a = 10 - 10;", vec![AstType::new_pointer_to(AstType::Int)]);
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

    // TODO: Function pointers

    // TODO
    //verify_types(&mut driver, "float (double, long double);", vec![AstType::new_fn(AstType::Float, vec![AstType::Double, AstType::LongDouble])]);
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
        println!("Parse failed {source}");
        return None;
    }
    let mut ast_root = parsed.unwrap();

    let (mut symbols, _) = type_check::type_check(&mut ast_root, parser.metadata, driver);

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
