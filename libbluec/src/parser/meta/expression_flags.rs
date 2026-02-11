// Copyright 2025-2026 Neil Henderson
//
//! The `expression_flags` module defines metadata flags that can be associated with an AST expression.

/// A flag which indicates a property of an AST expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AstExpressionFlag {
    /// The expression is a constant (literal).
    IsConstant = 1 << 0,

    /// The expression is an initializer for a static storage variable.
    IsStaticStorageInit = 1 << 1,

    /// The expression is enclosed in parentheses in the source code.
    HasParens = 1 << 2,

    /// The expression's type was promoted to 'int' due to integer promotion rules.
    PromotedToInt = 1 << 3,
}

/// Metadata flags associated with an `AstExpression`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AstExpressionFlags(u8);

impl AstExpressionFlags {
    /// Creates the flags with no flags set.
    pub fn none() -> Self {
        Self(0)
    }

    /// Is the given flag set?
    pub fn contains(&self, flag: AstExpressionFlag) -> bool {
        (self.0 & (flag as u8)) != 0
    }

    /// Sets a flag.
    pub fn insert(&mut self, flag: AstExpressionFlag) {
        self.0 |= flag as u8;
    }

    // Clears a flag.
    pub fn remove(&mut self, flag: AstExpressionFlag) {
        self.0 &= !(flag as u8);
    }
}

impl From<AstExpressionFlag> for AstExpressionFlags {
    fn from(flag: AstExpressionFlag) -> Self {
        AstExpressionFlags(flag as u8)
    }
}

impl std::ops::BitOr for AstExpressionFlags {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        AstExpressionFlags(self.0 | rhs.0)
    }
}

impl std::ops::BitOr for AstExpressionFlag {
    type Output = AstExpressionFlags;
    fn bitor(self, rhs: AstExpressionFlag) -> Self::Output {
        AstExpressionFlags(self as u8 | rhs as u8)
    }
}

impl std::ops::BitOr<AstExpressionFlag> for AstExpressionFlags {
    type Output = Self;
    fn bitor(self, rhs: AstExpressionFlag) -> Self {
        AstExpressionFlags(self.0 | rhs as u8)
    }
}

impl std::ops::BitAnd for AstExpressionFlags {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        AstExpressionFlags(self.0 & rhs.0)
    }
}

impl std::ops::BitOrAssign<AstExpressionFlag> for AstExpressionFlags {
    fn bitor_assign(&mut self, rhs: AstExpressionFlag) {
        self.0 |= rhs as u8;
    }
}
