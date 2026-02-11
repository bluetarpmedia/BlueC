// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver::Driver;
use crate::core::{FilePosition, SourceLocation};

use super::super::recursive_descent::declarator::*;
use super::super::{
    AstBasicType, AstBasicTypeSpecifier, AstDeclarator, AstDeclaratorKind, AstDeclaredType, AstIdentifier,
    AstStorageClassSpecifier,
};
use super::utils::make_parser;

#[test]
fn invalid_declarators() {
    assert!(parse("123").is_none());
    assert!(parse("9.0f").is_none());
    assert!(parse("1_abc").is_none());

    assert!(parse("(something").is_none());
    assert!(parse("((something)").is_none());

    assert!(parse("(*my_ptr").is_none());
    assert!(parse("((*my_ptr)").is_none());

    assert!(parse("array[;]").is_none());
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
    assert!(is_array(parse("arr[]")));
    assert!(is_array(parse("zero[0]")));
    assert!(is_array(parse("one[1]")));
    assert!(is_array(parse("two[2]")));
    assert!(is_array(parse("char_lit_as_size['A']")));
    assert!(is_array(parse("char_lit_as_size['\\n']")));
    assert!(is_array(parse("(eleven[11])")));
    assert!(is_array(parse("((fifteen[15]))")));
    assert!(is_array(parse("arr[3 + 3]")));
    assert!(is_array(parse("arr[2 * 100]")));
    assert!(is_array(parse("arr[-5]"))); // This is valid to parse but sema typecheck would return error

    assert!(is_2d_array(parse("x[][3]")));
    assert!(is_2d_array(parse("y[0][4]")));
    assert!(is_2d_array(parse("coords[3][4]")));
    assert!(is_2d_array(parse("coords[3 + 1][4 - 1]")));

    assert!(is_3d_array(parse("lots[22][33][44]")));
    assert!(is_3d_array(parse("lots[20 + 2 + 1][30 + 3 + 1][40 + 4 + 1]")));
    assert!(is_3d_array(parse("lits['A']['B']['C']")));
    assert!(is_3d_array(parse("hex_lits['\\xA']['\\xB']['\\xFF']")));
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
    assert!(is_abstract_array(parse("[]")));
    assert!(is_abstract_array(parse("[0]")));
    assert!(is_abstract_array(parse("[1]")));
    assert!(is_abstract_array(parse("[2]")));
    assert!(is_abstract_array(parse("([11])")));
    assert!(is_abstract_array(parse("(([15]))")));
    assert!(is_abstract_array(parse("[1 + 2 + 3]")));

    assert!(is_abstract_2d_array(parse("[][3]")));
    assert!(is_abstract_2d_array(parse("[0][4]")));
    assert!(is_abstract_2d_array(parse("[5][8]")));
    assert!(is_abstract_2d_array(parse("[100][200]")));
    assert!(is_abstract_2d_array(parse("[100 - 100][200 - 200]")));
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
            make_param_with_identifier("int", "age", SourceLocation::new(FilePosition::from(14), 3)),
            make_param_with_identifier("double", "salary", SourceLocation::new(FilePosition::from(26), 6)),
            make_param_with_identifier("float", "tax", SourceLocation::new(FilePosition::from(40), 3)),
        ]
    ));

    let pf =
        AstDeclarator { kind: AstDeclaratorKind::AbstractPointer, loc: SourceLocation::new(FilePosition::from(43), 1) };
    let ppf = AstDeclarator {
        kind: AstDeclaratorKind::Pointer(Box::new(pf)),
        loc: SourceLocation::new(FilePosition::from(42), 2),
    };
    let pppf = AstDeclarator {
        kind: AstDeclaratorKind::Pointer(Box::new(ppf)),
        loc: SourceLocation::new(FilePosition::from(41), 3),
    };

    assert!(is_function(
        parse("calculate(int age, double *salary, float ***)"),
        "calculate",
        &vec![
            make_param_with_identifier("int", "age", SourceLocation::new(FilePosition::from(14), 3)),
            make_declared_type(
                "double",
                None,
                Some(make_pointer_declarator(
                    "salary",
                    SourceLocation::new(FilePosition::from(27), 6),
                    SourceLocation::new(FilePosition::from(26), 7)
                ))
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
                Some(AstDeclarator {
                    kind: AstDeclaratorKind::AbstractPointer,
                    loc: SourceLocation::new(FilePosition::from(14), 1)
                })
            ),
            make_declared_type(
                "long double",
                None,
                Some(AstDeclarator {
                    kind: AstDeclaratorKind::AbstractPointer,
                    loc: SourceLocation::new(FilePosition::from(28), 1)
                })
            ),
            make_declared_type(
                "unsigned short",
                None,
                Some(AstDeclarator {
                    kind: AstDeclaratorKind::AbstractPointer,
                    loc: SourceLocation::new(FilePosition::from(45), 1)
                })
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
                Some(make_pointer_declarator(
                    "a",
                    SourceLocation::new(FilePosition::from(16), 1),
                    SourceLocation::new(FilePosition::from(15), 2)
                ))
            ),
            make_declared_type(
                "long double",
                None,
                Some(make_pointer_declarator(
                    "b",
                    SourceLocation::new(FilePosition::from(32), 1),
                    SourceLocation::new(FilePosition::from(31), 2)
                ))
            ),
            make_declared_type(
                "unsigned short",
                None,
                Some(make_pointer_declarator(
                    "c",
                    SourceLocation::new(FilePosition::from(51), 1),
                    SourceLocation::new(FilePosition::from(50), 2)
                ))
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
            Some(make_pointer_declarator(
                "p",
                SourceLocation::new(FilePosition::from(17), 1),
                SourceLocation::new(FilePosition::from(16), 2)
            ))
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

fn is_array(declarator: Option<AstDeclarator>) -> bool {
    let declarator = declarator.unwrap();
    matches!(declarator.kind, AstDeclaratorKind::Array { .. })
}

fn is_2d_array(declarator: Option<AstDeclarator>) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, .. } = declarator.kind { is_array(Some(*decl)) } else { false }
}

fn is_3d_array(declarator: Option<AstDeclarator>) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, .. } = declarator.kind { is_2d_array(Some(*decl)) } else { false }
}

fn is_abstract_array(declarator: Option<AstDeclarator>) -> bool {
    let declarator = declarator.unwrap();
    matches!(declarator.kind, AstDeclaratorKind::AbstractArray { .. })
}

fn is_abstract_2d_array(declarator: Option<AstDeclarator>) -> bool {
    let declarator = declarator.unwrap();

    if let AstDeclaratorKind::Array { decl, .. } = declarator.kind { is_abstract_array(Some(*decl)) } else { false }
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
    let mut driver = Driver::for_testing();
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
