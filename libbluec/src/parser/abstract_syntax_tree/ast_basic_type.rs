// Copyright 2025-2026 Neil Henderson
//
//! The `ast_basic_type` module defines [AstBasicType] and its related types.

use std::fmt;

use crate::core::SourceLocation;

use super::AstUniqueName;

/// The basic type of a declaration.
///
/// The term 'basic type' is not used in the C Standard, but it is used informally when discussing C parsers. The
/// basic type is the first type we see in a declaration before any of the declarators.
///
/// ```c
///  unsigned long int *age = 0, calculate(float salary);
///  ~~~~~~~~~~~~~~~~~
///
///  typedef int MyInt;
///  MyInt a = 0, b = 1, c = 2;
///  ~~~~~
/// ```
///
/// The subsequent declarators (e.g. `*age`, `calculate(float salary)`, `a`, `b`, `c`) augment the basic type to create
/// a derived type. The derived type always modifies the basic type. For example, `*age` augments the basic type
/// `unsigned long int` and creates a derived type of `unsigned long int *`.
///
/// See: [Reading C type declarations](http://unixwiz.net/techtips/reading-cdecl.html)
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AstBasicType(pub Vec<AstBasicTypeSpecifier>);

impl fmt::Display for AstBasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for spec in &self.0 {
            if first {
                write!(f, "{}", spec)?;
            } else {
                write!(f, " {}", spec)?;
            }
            first = false;
        }
        Ok(())
    }
}

/// A basic type specifier.
///
/// A basic type consists of one or more specifiers. In the example below the basic type is 'unsigned long int'
/// and consists of three built-in type specifiers.
///
/// ```c
///  unsigned long int salary = 0;
///  ~~~~~~~~ ~~~~ ~~~
/// ```
///
/// The following basic type has one alias specifier.
///
/// ```c
///  typedef float F32;
///  F32 temperature = 0.0f;
///  ~~~
/// ```
#[derive(Debug, Clone)]
pub enum AstBasicTypeSpecifier {
    BuiltinType { specifier: String, loc: SourceLocation },
    Alias { alias_name: AstUniqueName, loc: SourceLocation },
}

impl fmt::Display for AstBasicTypeSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstBasicTypeSpecifier::BuiltinType { specifier, .. } => write!(f, "{specifier}"),
            AstBasicTypeSpecifier::Alias { alias_name, .. } => write!(f, "{alias_name}"),
        }
    }
}

impl PartialEq for AstBasicTypeSpecifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                AstBasicTypeSpecifier::BuiltinType { specifier: sp1, .. },
                AstBasicTypeSpecifier::BuiltinType { specifier: sp2, .. },
            ) => sp1 == sp2,
            (
                AstBasicTypeSpecifier::Alias { alias_name: a1, .. },
                AstBasicTypeSpecifier::Alias { alias_name: a2, .. },
            ) => a1 == a2,

            _ => false,
        }
    }
}

impl Eq for AstBasicTypeSpecifier {}
