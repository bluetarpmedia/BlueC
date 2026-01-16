// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::utils::make_parser;

use super::super::recursive_descent::declarator::*;
use super::super::{
    AstBasicType, AstBasicTypeSpecifier, AstDeclarator, AstDeclaratorKind, AstDeclaredType, AstIdentifier,
    AstStorageClassSpecifier,
};

use crate::compiler_driver;
use crate::lexer::SourceLocation;

#[test]
fn invalid_declarators() {
    assert!(parse("123").is_none());
    assert!(parse("9.0f").is_none());
    assert!(parse("1_abc").is_none());

    assert!(parse("(something").is_none());
    assert!(parse("((something)").is_none());

    assert!(parse("(*my_ptr").is_none());
    assert!(parse("((*my_ptr)").is_none());

    assert!(parse("array[-1]").is_none());
    assert!(parse("array[2.5f]").is_none());
    assert!(parse("array[array2]").is_none());
    assert!(parse("array[(void)]").is_none());
}

#[test]
fn identifier_declarators() {
    assert!(is_identifier(parse("foo"), "foo"));
    assert!(is_identifier(parse("(single)"), "single"));
    assert!(is_identifier(parse("((((lots))))"), "lots"));
    assert!(is_identifier(parse("my_long_variable_name"), "my_long_variable_name"));
}

#[test]
fn variable_pointer_declarators() {
    assert!(is_variable_pointer(parse("*foo"), 1, "foo"));
    assert!(is_variable_pointer(parse("**calc"), 2, "calc"));
    assert!(is_variable_pointer(parse("***three"), 3, "three"));
    assert!(is_variable_pointer(parse("*********nine"), 9, "nine"));

    assert!(is_variable_pointer(parse("(*my_ptr)"), 1, "my_ptr"));
    assert!(is_variable_pointer(parse("*(*my_ptr))"), 2, "my_ptr"));
    assert!(is_variable_pointer(parse("*(*(*my_ptr)))"), 3, "my_ptr"));
}

#[test]
fn array_declarators() {
    assert!(is_array(parse("arr[]"), 0));
    assert!(is_array(parse("zero[0]"), 0));
    assert!(is_array(parse("one[1]"), 1));
    assert!(is_array(parse("two[2]"), 2));
    assert!(is_array(parse("char_lit_as_size['A']"), 65));
    assert!(is_array(parse("char_lit_as_size['\\n']"), 10));
    assert!(is_array(parse("(eleven[11])"), 11));
    assert!(is_array(parse("((fifteen[15]))"), 15));

    assert!(is_2d_array(parse("x[][3]"), 3, 0));
    assert!(is_2d_array(parse("y[0][4]"), 4, 0));
    assert!(is_2d_array(parse("coords[3][4]"), 4, 3));
    assert!(is_3d_array(parse("lots[22][33][44]"), 44, 33, 22));
    assert!(is_3d_array(parse("lits['A']['B']['C']"), 67, 66, 65));
    assert!(is_3d_array(parse("hex_lits['\\xA']['\\xB']['\\xFF']"), 255, 11, 10));
}

#[test]
fn abstract_pointer_declarators() {
    assert!(is_abstract_pointer(parse("*"), 1));
    assert!(is_abstract_pointer(parse("**"), 2));
    assert!(is_abstract_pointer(parse("***"), 3));
    assert!(is_abstract_pointer(parse("*******"), 7));

    assert!(is_abstract_pointer(parse("(*)"), 1));
    assert!(is_abstract_pointer(parse("*(*))"), 2));
    assert!(is_abstract_pointer(parse("*(*(*)))"), 3));
}

#[test]
fn abstract_array_declarators() {
    assert!(is_abstract_array(parse("[]"), 0));
    assert!(is_abstract_array(parse("[0]"), 0));
    assert!(is_abstract_array(parse("[1]"), 1));
    assert!(is_abstract_array(parse("[2]"), 2));
    assert!(is_abstract_array(parse("([11])"), 11));
    assert!(is_abstract_array(parse("(([15]))"), 15));

    assert!(is_abstract_2d_array(parse("[][3]"), 3, 0));
    assert!(is_abstract_2d_array(parse("[0][4]"), 4, 0));
    assert!(is_abstract_2d_array(parse("[5][8]"), 8, 5));
    assert!(is_abstract_2d_array(parse("[100][200]"), 200, 100));
}

#[test]
fn abstract_function_declarators() {
    assert!(is_abstract_function(parse("(void)"), &vec![]));

    assert!(is_abstract_function(
        parse("(int, float)"),
        &vec![make_param_with_type_only("int"), make_param_with_type_only("float"),]
    ));

    assert!(is_abstract_function(
        parse("(unsigned short, unsigned int, unsigned long, unsigned long long)"),
        &vec![
            make_param_with_type_only("unsigned short"),
            make_param_with_type_only("unsigned int"),
            make_param_with_type_only("unsigned long"),
            make_param_with_type_only("unsigned long long"),
        ]
    ));
}

#[test]
fn function_declarators() {
    assert!(is_function(
        parse("calculate(int, double)"),
        "calculate",
        &vec![make_param_with_type_only("int"), make_param_with_type_only("double"),]
    ));

    assert!(is_function(
        parse("calculate(int age, double salary, float tax)"),
        "calculate",
        &vec![
            make_param_with_identifier("int", "age", SourceLocation::new(1, 15, 3)),
            make_param_with_identifier("double", "salary", SourceLocation::new(1, 27, 6)),
            make_param_with_identifier("float", "tax", SourceLocation::new(1, 41, 3)),
        ]
    ));

    let pf = AstDeclarator { kind: AstDeclaratorKind::AbstractPointer, loc: SourceLocation::new(1, 44, 1) };
    let ppf = AstDeclarator { kind: AstDeclaratorKind::Pointer(Box::new(pf)), loc: SourceLocation::new(1, 43, 2) };
    let pppf = AstDeclarator { kind: AstDeclaratorKind::Pointer(Box::new(ppf)), loc: SourceLocation::new(1, 42, 3) };

    assert!(is_function(
        parse("calculate(int age, double *salary, float ***)"),
        "calculate",
        &vec![
            make_param_with_identifier("int", "age", SourceLocation::new(1, 15, 3)),
            make_declared_type(
                "double",
                None,
                Some(make_pointer_declarator("salary", SourceLocation::new(1, 28, 6), SourceLocation::new(1, 27, 7)))
            ),
            make_declared_type("float", None, Some(pppf)),
        ]
    ));

    assert!(is_function(
        parse("calculate(long*, long double*, unsigned short*)"),
        "calculate",
        &vec![
            make_declared_type(
                "long",
                None,
                Some(AstDeclarator { kind: AstDeclaratorKind::AbstractPointer, loc: SourceLocation::new(1, 15, 1) })
            ),
            make_declared_type(
                "long double",
                None,
                Some(AstDeclarator { kind: AstDeclaratorKind::AbstractPointer, loc: SourceLocation::new(1, 29, 1) })
            ),
            make_declared_type(
                "unsigned short",
                None,
                Some(AstDeclarator { kind: AstDeclaratorKind::AbstractPointer, loc: SourceLocation::new(1, 46, 1) })
            ),
        ]
    ));

    assert!(is_function(
        parse("calculate(long *a, long double *b, unsigned short *c)"),
        "calculate",
        &vec![
            make_declared_type(
                "long",
                None,
                Some(make_pointer_declarator("a", SourceLocation::new(1, 17, 1), SourceLocation::new(1, 16, 2)))
            ),
            make_declared_type(
                "long double",
                None,
                Some(make_pointer_declarator("b", SourceLocation::new(1, 33, 1), SourceLocation::new(1, 32, 2)))
            ),
            make_declared_type(
                "unsigned short",
                None,
                Some(make_pointer_declarator("c", SourceLocation::new(1, 52, 1), SourceLocation::new(1, 51, 2)))
            ),
        ]
    ));

    assert!(is_function(parse("* get_ptr(void)"), "get_ptr", &vec![]));

    assert!(is_function(
        parse("*** get_ptr(int *p)"),
        "get_ptr",
        &vec![make_declared_type(
            "int",
            None,
            Some(make_pointer_declarator("p", SourceLocation::new(1, 18, 1), SourceLocation::new(1, 17, 2)))
        )]
    ));
}

fn is_identifier(declarator: Option<AstDeclarator>, expected_identifier: &str) -> bool {
    if declarator.is_none() {
        return false;
    }
    let declarator = declarator.unwrap();
    match declarator.get_derived_kind() {
        AstDeclaratorKind::Ident(ident) => ident.name == expected_identifier,
        _ => false,
    }
}

fn is_abstract_pointer(declarator: Option<AstDeclarator>, expected_count: i32) -> bool {
    if declarator.is_none() {
        return expected_count == 0;
    }
    let declarator = declarator.unwrap();

    if expected_count < 0 {
        return false;
    }

    match declarator.kind {
        AstDeclaratorKind::AbstractPointer => is_abstract_pointer(None, expected_count - 1),

        AstDeclaratorKind::Pointer(referent) => is_abstract_pointer(Some(*referent.clone()), expected_count - 1),

        _ => return false,
    }
}

fn is_array(declarator: Option<AstDeclarator>, expected_size: usize) -> bool {
    let declarator = declarator.unwrap();

    match declarator.kind {
        AstDeclaratorKind::Array { size, .. } => size == expected_size,

        _ => return false,
    }
}

fn is_2d_array(declarator: Option<AstDeclarator>, dim1: usize, dim2: usize) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, size } = declarator.kind {
        assert_eq!(size, dim1);

        is_array(Some(*decl), dim2)
    } else {
        false
    }
}

fn is_3d_array(declarator: Option<AstDeclarator>, dim1: usize, dim2: usize, dim3: usize) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, size } = declarator.kind {
        assert_eq!(size, dim1);

        is_2d_array(Some(*decl), dim2, dim3)
    } else {
        false
    }
}

fn is_abstract_array(declarator: Option<AstDeclarator>, expected_size: usize) -> bool {
    let declarator = declarator.unwrap();

    match declarator.kind {
        AstDeclaratorKind::AbstractArray { size, .. } => size == expected_size,

        _ => return false,
    }
}

fn is_abstract_2d_array(declarator: Option<AstDeclarator>, dim1: usize, dim2: usize) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, size } = declarator.kind {
        assert_eq!(size, dim1);

        is_abstract_array(Some(*decl), dim2)
    } else {
        false
    }
}

fn is_variable_pointer(declarator: Option<AstDeclarator>, expected_count: i32, expected_identifier: &str) -> bool {
    if declarator.is_none() {
        return false;
    }
    let declarator = declarator.unwrap();

    if expected_count < 0 {
        return false;
    }

    match declarator.kind {
        AstDeclaratorKind::Ident(ident) => return ident.name == expected_identifier && expected_count == 0,

        AstDeclaratorKind::AbstractPointer => is_variable_pointer(None, expected_count - 1, expected_identifier),

        AstDeclaratorKind::Pointer(referent) => {
            is_variable_pointer(Some(*referent.clone()), expected_count - 1, expected_identifier)
        }

        _ => return false,
    }
}

fn is_abstract_function(declarator: Option<AstDeclarator>, expected_params: &[AstDeclaredType]) -> bool {
    if declarator.is_none() {
        return false;
    }
    let declarator = declarator.unwrap();

    match declarator.get_derived_kind() {
        AstDeclaratorKind::AbstractFunction { params } => {
            let match_params = params.iter().zip(expected_params).all(|(actual, expected)| actual == expected);
            if !match_params {
                println!("{:?}", params);
            }
            match_params
        }
        _ => false,
    }
}

fn is_function(
    declarator: Option<AstDeclarator>,
    expected_identifier: &str,
    expected_params: &[AstDeclaredType],
) -> bool {
    if declarator.is_none() {
        return false;
    }
    let declarator = declarator.unwrap();

    match declarator.get_derived_kind() {
        AstDeclaratorKind::Function { decl, params } => {
            let match_id = is_identifier(Some(*decl.clone()), expected_identifier);
            if !match_id {
                println!("{:?}", decl);
            }
            let match_params = params.iter().zip(expected_params).all(|(actual, expected)| actual == expected);
            if !match_params {
                println!("Actual params: {:?}", params);
                println!("Expected params: {:?}", expected_params);
            }
            match_id && match_params
        }
        _ => false,
    }
}

fn parse(source: &str) -> Option<AstDeclarator> {
    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let declarator = parse_declarator(&mut parser, &mut driver, ParseOption::AllowAbstract);
    if declarator.is_err() {
        None
    } else {
        let declarator = declarator.unwrap();
        Some(declarator)
    }
}

fn make_ident_declarator(ident: &str, loc: SourceLocation) -> AstDeclarator {
    AstDeclarator::new(AstDeclaratorKind::Ident(AstIdentifier::new(ident, loc)), loc)
}

fn make_pointer_declarator(ident: &str, loc: SourceLocation, ptr_loc: SourceLocation) -> AstDeclarator {
    let ident_decl = make_ident_declarator(ident, loc);
    AstDeclarator { kind: AstDeclaratorKind::Pointer(Box::new(ident_decl)), loc: ptr_loc }
}

fn make_param_with_type_only(basic_type: &str) -> AstDeclaredType {
    make_declared_type(basic_type, None, None)
}

fn make_param_with_identifier(basic_type: &str, ident: &str, loc: SourceLocation) -> AstDeclaredType {
    let ident_decl = make_ident_declarator(ident, loc);
    make_declared_type(basic_type, None, Some(ident_decl))
}

fn make_declared_type(
    basic_type: &str,
    storage_class: Option<AstStorageClassSpecifier>,
    declarator: Option<AstDeclarator>,
) -> AstDeclaredType {
    let specifiers = basic_type
        .split_ascii_whitespace()
        .map(|sp| AstBasicTypeSpecifier::BuiltinType { specifier: sp.into(), loc: SourceLocation::default() })
        .collect();

    AstDeclaredType { basic_type: AstBasicType(specifiers), storage_class, declarator, resolved_type: None }
}
