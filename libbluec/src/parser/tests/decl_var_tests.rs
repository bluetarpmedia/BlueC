// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::utils::{expect_var_decl, make_parser};

use super::super::recursive_descent::decl;
use super::super::{AstBasicTypeSpecifier, AstLinkage, AstStorageDuration, AstVariableDeclaration};

use crate::compiler_driver;
use crate::lexer::SourceLocation;

#[test]
fn invalid_decl_syntax() {
    let cases = vec![
        "int a,",
        "int a, ;",
        "int a, , , ;",
        "int (a;",
        "int (a = 1);",
        "int a);",
        "int (a));",
        "int ((a);",
        "float a, int b;",
        "extern static int x;",
        "int &* a;",
        "int *& a;",
        "int *_* a;",
        "int *a*;",
        "int a = {,};",
        "int a = {{,};",
        "int a = {,,,};",
    ];

    let mut driver = compiler_driver::Driver::for_testing();
    for case in cases {
        let mut parser = make_parser(&mut driver, case);
        let parsed = decl::parse_declaration(&mut parser, &mut driver).is_ok();
        if parsed {
            println!("{case}");
        }
        assert!(!parsed);
    }
}

#[test]
fn valid_decl_syntax() {
    let cases = vec![
        "int ;", // Useless but valid
        "int a;",
        "int (a);",
        "int ((((a)))), (b), (((c)));",
        "int ((a)) = 1;",
        "int (a) = (1), (b) = (2), (c) = (3);",
        "unsigned a;",
        "signed a, b;",
        "int a, b, c, d;",
        "int a = 1, b = 2, c = 3;",
        "double x = 1.0, y = 2.0;",
        "extern int a;",
        "static int a;",
        "int a = {};",
        "int a = {1};",
        "int*a;",
        "int *a;",
        "int **a;",
        "int ***a;",
        "int *(*(*(a)));",
        "int (*(*(*(a))));",
        "int *a, b, c;",
        "long long a, b, *c;",
        "int *a = 0;",
        "int **a = 0;",
        "int *(a) = 0;",
        "int *a = 10 - 10;",
        "int arr[3] = {};",
        "int arr[3] = {0};",
        "int arr[3] = {1, 2};",
        "int arr[3] = {1, 2, 3};",
        "int arr[3][2] = {{1, 11}, {2, 22}, {3, 33}};",
        "int arr[3][2] = {1, 11, 2, 22, 3, 33};",
    ];

    let mut driver = compiler_driver::Driver::for_testing();
    for case in cases {
        let mut parser = make_parser(&mut driver, case);
        assert!(decl::parse_declaration(&mut parser, &mut driver).is_ok());
    }
}

#[test]
fn multiple_declarators_file_scope() {
    let source = "int a, b, c;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let decls = decl::parse_declaration(&mut parser, &mut driver).expect("Should have parsed");

    assert_eq!(decls.len(), 3);

    let decl1 = expect_var_decl(&decls[0]).unwrap();
    let decl2 = expect_var_decl(&decls[1]).unwrap();
    let decl3 = expect_var_decl(&decls[2]).unwrap();

    assert_eq!(decl1.ident.name, "a");
    assert_eq!(decl1.unique_name, "a");

    assert_eq!(decl2.ident.name, "b");
    assert_eq!(decl2.unique_name, "b");

    assert_eq!(decl3.ident.name, "c");
    assert_eq!(decl3.unique_name, "c");

    for decl in decls {
        let decl = expect_var_decl(&decl).unwrap();

        verify_basic_type(&decl, "int");
        assert_eq!(decl.is_declaration_only, false);
        assert!(decl.initializer.is_none());
        assert!(decl.init_constant_eval.is_empty());
        assert_eq!(decl.linkage, AstLinkage::External);
        assert_eq!(decl.storage, AstStorageDuration::Static);
    }
}

#[test]
fn multiple_declarators_file_scope_with_initializers() {
    let source = "float a = 1.0f, b, c = 3.0f, d;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let decls = decl::parse_declaration(&mut parser, &mut driver).expect("Should have parsed");

    assert_eq!(decls.len(), 4);

    for decl in &decls {
        let decl = expect_var_decl(&decl).unwrap();

        verify_basic_type(&decl, "float");
        assert_eq!(decl.is_declaration_only, false);
        assert_eq!(decl.linkage, AstLinkage::External);
        assert_eq!(decl.storage, AstStorageDuration::Static);
    }

    let decl1 = expect_var_decl(&decls[0]).unwrap();
    let decl2 = expect_var_decl(&decls[1]).unwrap();
    let decl3 = expect_var_decl(&decls[2]).unwrap();
    let decl4 = expect_var_decl(&decls[3]).unwrap();

    assert!(decl1.initializer.is_some());
    assert!(decl1.init_constant_eval.is_empty());

    assert!(decl2.initializer.is_none());
    assert!(decl2.init_constant_eval.is_empty());

    assert!(decl3.initializer.is_some());
    assert!(decl3.init_constant_eval.is_empty());

    assert!(decl4.initializer.is_none());
    assert!(decl4.init_constant_eval.is_empty());
}

#[test]
fn multiple_declarators_block_scope() {
    let source = "int a, b, c;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    // Pretend we're in a scope
    let decls =
        parser.with_new_scope(|parser| decl::parse_declaration(parser, &mut driver).expect("Should have parsed"));

    assert_eq!(decls.len(), 3);

    let decl1 = expect_var_decl(&decls[0]).unwrap();
    let decl2 = expect_var_decl(&decls[1]).unwrap();
    let decl3 = expect_var_decl(&decls[2]).unwrap();

    assert_eq!(decl1.ident.name, "a");
    assert_ne!(decl1.unique_name, "a");

    assert_eq!(decl2.ident.name, "b");
    assert_ne!(decl2.unique_name, "b");

    assert_eq!(decl3.ident.name, "c");
    assert_ne!(decl3.unique_name, "c");

    for decl in decls {
        let decl = expect_var_decl(&decl).unwrap();

        verify_basic_type(&decl, "int");
        assert_eq!(decl.is_declaration_only, false);
        assert!(decl.initializer.is_none());
        assert!(decl.init_constant_eval.is_empty());
        assert!(decl.declared_type.storage_class.is_none());
        assert_eq!(decl.linkage, AstLinkage::None);
        assert_eq!(decl.storage, AstStorageDuration::Automatic);
    }
}

fn verify_basic_type(decl: &AstVariableDeclaration, expected: &str) {
    assert_eq!(
        decl.declared_type.basic_type.0,
        vec![AstBasicTypeSpecifier::BuiltinType { specifier: expected.into(), loc: SourceLocation::default() }]
    );
}
