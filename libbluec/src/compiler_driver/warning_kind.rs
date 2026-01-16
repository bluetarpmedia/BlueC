// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `warning_kind` module defines the `WarningKind` enum.

use std::fmt;
use std::collections::HashSet;

/// The kind of warning.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WarningKind {
    None,
    //
    // Literals
    //
    ConstantConversion,
    ImplicitlyUnsignedLiteral,
    //
    // Declarations and initializers
    //
    MissingDeclarations,
    DuplicateDeclSpecifier,
    ExternInitializer,
    Uninitialized,
    UnusedVariable,
    UnusedFunction,
    UnusedLocalTypedef,
    ExcessInitializers,
    MissingBraces,
    ManyBracesAroundScalarInit,
    //
    // Expressions
    //
    LogicalOpParentheses,
    BitwiseOpParentheses,
    Parentheses,
    ArrayBounds,
    //
    // Conversions and casts
    //
    ImplicitConversion,
    ImplicitIntConversion,
    ImplicitFloatConversion,
    SignConversion,
    PointerToIntCast,
    NonLiteralNullConversion,
    //
    // Comparisons
    //
    CompareDistinctPointerTypes,
    PointerIntegerCompare,
    //
    // Types
    //
    ConditionalTypeMismatch,
    PointerTypeMismatch,
}

impl fmt::Display for WarningKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl WarningKind {
    /// The string representation of the warning kind.
    #[rustfmt::skip]
    const fn as_str(&self) -> &'static str {
        match self {
            WarningKind::None                           => "",
            
            WarningKind::ConstantConversion             => "constant-conversion",
            WarningKind::ImplicitlyUnsignedLiteral      => "implicitly-unsigned-literal",

            WarningKind::MissingDeclarations            => "missing-declarations",
            WarningKind::DuplicateDeclSpecifier         => "duplicate-decl-specifier",
            WarningKind::ExternInitializer              => "extern-initializer",
            WarningKind::Uninitialized                  => "uninitialized",
            WarningKind::UnusedVariable                 => "unused-variable",
            WarningKind::UnusedFunction                 => "unused-function",
            WarningKind::UnusedLocalTypedef             => "unused-local-typedef",
            WarningKind::ExcessInitializers             => "excess-initializers",
            WarningKind::MissingBraces                  => "missing-braces",
            WarningKind::ManyBracesAroundScalarInit     => "many-braces-around-scalar-init",

            WarningKind::LogicalOpParentheses           => "logical-op-parentheses",
            WarningKind::BitwiseOpParentheses           => "bitwise-op-parentheses",
            WarningKind::Parentheses                    => "parentheses",
            WarningKind::ArrayBounds                    => "array-bounds",

            WarningKind::ImplicitConversion             => "implicit-conversion",
            WarningKind::ImplicitIntConversion          => "implicit-int-conversion",
            WarningKind::ImplicitFloatConversion        => "implicit-float-conversion",
            WarningKind::SignConversion                 => "sign-conversion",
            WarningKind::PointerToIntCast               => "pointer-to-int-cast",
            WarningKind::NonLiteralNullConversion       => "non-literal-null-conversion",

            WarningKind::CompareDistinctPointerTypes    => "compare-distinct-pointer-types",
            WarningKind::PointerIntegerCompare          => "pointer-integer-compare",

            WarningKind::ConditionalTypeMismatch        => "conditional-type-mismatch",
            WarningKind::PointerTypeMismatch            => "pointer-type-mismatch",
        }
    }

    /// Returns a `HashSet` of warnings that are enabled by default.
    pub fn enabled_by_default() -> HashSet<WarningKind> {
        [
            WarningKind::ConstantConversion,
            WarningKind::DuplicateDeclSpecifier,
            WarningKind::ExternInitializer,
            WarningKind::CompareDistinctPointerTypes,
            WarningKind::PointerIntegerCompare,
            WarningKind::PointerToIntCast,
            WarningKind::NonLiteralNullConversion,
            WarningKind::ConditionalTypeMismatch,
            WarningKind::PointerTypeMismatch,
        ].into_iter().collect()
    }
}
