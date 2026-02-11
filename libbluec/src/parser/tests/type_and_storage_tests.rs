// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver;
use crate::core::SourceLocation;

use super::super::recursive_descent::decl::parse_type_and_storage_specifiers;
use super::super::recursive_descent::utils;
use super::super::{AstBasicTypeSpecifier, AstStorageClassSpecifierKind};
use super::utils::make_parser;

#[test]
fn invalid_type_and_storage_specifiers() {
    let mut driver = compiler_driver::Driver::for_testing();

    let cases_without_identifier = vec![
        "notarealtype;",
        "extern static int;",
        "typedef static int;",
        "static typedef int;",
        "typedef extern int;",
        "extern typedef int;",
        "typedef extern static int;",
    ];

    for case in cases_without_identifier {
        let mut parser = make_parser(&mut driver, case);
        let ts = parse_type_and_storage_specifiers(&mut parser, &mut driver, false);
        if !ts.is_err() {
            println!("Source: {case}");
        }
        assert!(ts.is_err());
    }

    let cases_with_identifier = vec![
        "notarealtype x;",
        "extern static unsigned long long age;",
        "static extern long double salary;",
        "typedef extern static int y;",
    ];

    for case in cases_with_identifier {
        let mut parser = make_parser(&mut driver, case);
        let ts = parse_type_and_storage_specifiers(&mut parser, &mut driver, true);
        if !ts.is_err() {
            println!("Source: {case}");
        }
        assert!(ts.is_err());
    }
}

#[test]
fn valid_type_and_storage_specifiers() {
    verify_specifiers("int", None);
    verify_specifiers("signed", None);
    verify_specifiers("signed int", None);
    verify_specifiers("short", None);
    verify_specifiers("short int", None);
    verify_specifiers("long", None);
    verify_specifiers("long int", None);
    verify_specifiers("long long", None);
    verify_specifiers("long long int", None);
    verify_specifiers("long int long", None);
    verify_specifiers("int long long", None);

    verify_specifiers("unsigned short", None);
    verify_specifiers("short unsigned", None);
    verify_specifiers("unsigned", None);
    verify_specifiers("unsigned int", None);
    verify_specifiers("unsigned long", None);
    verify_specifiers("unsigned long int", None);
    verify_specifiers("unsigned long long", None);
    verify_specifiers("unsigned long long int", None);
    verify_specifiers("long long unsigned int", None);

    verify_specifiers("float", None);
    verify_specifiers("double", None);
    verify_specifiers("long double", None);
    verify_specifiers("double long", None);

    verify_specifiers("extern", Some(AstStorageClassSpecifierKind::Extern));
    verify_specifiers("static", Some(AstStorageClassSpecifierKind::Static));
    verify_specifiers("typedef", Some(AstStorageClassSpecifierKind::Typedef));

    verify_specifiers("extern extern int", Some(AstStorageClassSpecifierKind::Extern));
    verify_specifiers("static static int", Some(AstStorageClassSpecifierKind::Static));
    verify_specifiers("extern extern extern", Some(AstStorageClassSpecifierKind::Extern));
    verify_specifiers("static static static", Some(AstStorageClassSpecifierKind::Static));
    verify_specifiers("typedef typedef typedef", Some(AstStorageClassSpecifierKind::Typedef));

    verify_specifiers("extern int", Some(AstStorageClassSpecifierKind::Extern));
    verify_specifiers("static int", Some(AstStorageClassSpecifierKind::Static));
    verify_specifiers("typedef int MyInt", Some(AstStorageClassSpecifierKind::Typedef));
    verify_specifiers("int typedef MyInt", Some(AstStorageClassSpecifierKind::Typedef));

    verify_specifiers("static unsigned long long int", Some(AstStorageClassSpecifierKind::Static));
    verify_specifiers("long long int unsigned extern", Some(AstStorageClassSpecifierKind::Extern));
    verify_specifiers("typedef unsigned long long int MyULL", Some(AstStorageClassSpecifierKind::Typedef));
}

fn verify_specifiers(source: &str, expected_storage: Option<AstStorageClassSpecifierKind>) {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let (basic_type, storage) =
        parse_type_and_storage_specifiers(&mut parser, &mut driver, false).expect("Expected to parse correctly");

    let expected_specifiers = source
        .split_ascii_whitespace()
        .filter_map(|sp| {
            if utils::is_builtin_type_specifier(sp) {
                Some(AstBasicTypeSpecifier::BuiltinType { specifier: sp.into(), loc: SourceLocation::default() })
            } else {
                None
            }
        })
        .collect::<Vec<AstBasicTypeSpecifier>>();

    assert_eq!(basic_type.0, expected_specifiers);

    let actual_storage_kind = storage.map(|st| st.kind);

    assert_eq!(expected_storage, actual_storage_kind);
}
