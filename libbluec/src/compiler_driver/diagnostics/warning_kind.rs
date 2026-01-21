// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `warning_kind` module defines the `WarningKind` enum.
//! 
//! An X-macro is used to generate the enum along with `WarningKind::as_str()` and `WarningKind::from_str()` to
//! convert a `WarningKind` variant to and from its string representation.

use std::collections::HashSet;
use std::fmt;

// An "X-macro" to generate the `WarningKind` enum and functions to convert to/from a string representation
// so that we don't need to duplicate strings.
macro_rules! define_warning_kind {
    ($($variant:ident => $string:expr),* $(,)?) => {
        /// The kind of warning.
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        pub enum WarningKind {
            $($variant),*
        }

        impl WarningKind {
            /// The string representation of the warning kind.
            pub const fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => $string),*
                }
            }

            /// A `HashSet` of all the warnings.
            pub fn all() -> HashSet<WarningKind> {
                HashSet::from([$(Self::$variant),*])
            }

            /// A sorted `Vec` of the string representations of all the warnings.
            pub fn all_strings() -> Vec<&'static str> {
                let mut all = vec![$($string),*];
                all.sort();
                all
            }
        }

        impl std::str::FromStr for WarningKind {
            type Err = String;

            /// Returns `Ok(WarningKind)` from its string representation, or returns an `Err`.
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($string => Ok(Self::$variant),)*
                    _ => Err(format!("Unknown WarningKind: {}", s)),
                }
            }
        }
    };
}

// Define the `WarningKind` enum variants and their string representations.
#[rustfmt::skip]
define_warning_kind! {
    //
    // Literals
    //
    Multichar                      => "multichar",
    UnknownEscapeSequence          => "unknown-escape-sequence",
    ConstantConversion             => "constant-conversion",
    ImplicitlyUnsignedLiteral      => "implicitly-unsigned-literal",
    //
    // Declarations and initializers
    //
    MissingDeclarations            => "missing-declarations",
    DuplicateDeclSpecifier         => "duplicate-decl-specifier",
    ExternInitializer              => "extern-initializer",
    Uninitialized                  => "uninitialized",
    UnusedVariable                 => "unused-variable",
    UnusedFunction                 => "unused-function",
    UnusedLocalTypedef             => "unused-local-typedef",
    ExcessInitializers             => "excess-initializers",
    MissingBraces                  => "missing-braces",
    ManyBracesAroundScalarInit     => "many-braces-around-scalar-init",
    //
    // Expressions
    //
    LogicalOpParentheses           => "logical-op-parentheses",
    BitwiseOpParentheses           => "bitwise-op-parentheses",
    Parentheses                    => "parentheses",
    ArrayBounds                    => "array-bounds",
    //
    // Conversions and casts
    //
    ImplicitConversion             => "implicit-conversion",
    ImplicitIntConversion          => "implicit-int-conversion",
    ImplicitFloatConversion        => "implicit-float-conversion",
    SignConversion                 => "sign-conversion",
    PointerToIntCast               => "pointer-to-int-cast",
    NonLiteralNullConversion       => "non-literal-null-conversion",
    //
    // Comparisons
    //
    CompareDistinctPointerTypes    => "compare-distinct-pointer-types",
    PointerIntegerCompare          => "pointer-integer-compare",
    //
    // Types
    //
    ConditionalTypeMismatch        => "conditional-type-mismatch",
    PointerTypeMismatch            => "pointer-type-mismatch",
}

impl fmt::Display for WarningKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl WarningKind {
    /// Returns a `HashSet` of compiler warnings that are enabled by default.
    pub fn enabled_by_default() -> HashSet<WarningKind> {
        HashSet::from([
            WarningKind::ArrayBounds,
            WarningKind::ConditionalTypeMismatch,
            WarningKind::ConstantConversion,
            WarningKind::CompareDistinctPointerTypes,
            WarningKind::DuplicateDeclSpecifier,
            WarningKind::ExcessInitializers,
            WarningKind::ExternInitializer,
            WarningKind::ImplicitlyUnsignedLiteral,
            WarningKind::ManyBracesAroundScalarInit,
            WarningKind::Multichar,
            WarningKind::NonLiteralNullConversion,
            WarningKind::Parentheses,
            WarningKind::PointerIntegerCompare,
            WarningKind::PointerToIntCast,
            WarningKind::PointerTypeMismatch,
            WarningKind::UnknownEscapeSequence,
        ])
    }
}
