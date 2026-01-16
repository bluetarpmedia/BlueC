// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The tokens module defines the tokens that the lexer produces.

use crate::lexer::SourceLocation;

use std::fmt;

/// Types of tokens produced by lexical analysis.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    // Single character tokens
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenSqBracket,
    CloseSqBracket,
    Semicolon,
    Colon,
    Comma,
    BitwiseAnd, // Or address-of
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    Plus,
    Minus,
    Multiply, // Or dereference
    Divide,
    Remainder,
    Assignment,
    LogicalNot,
    LessThan,
    GreaterThan,
    Ternary, // Aka conditional

    // Multi-character tokens
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
    Increment,
    Decrement,
    AdditionAssignment,
    SubtractionAssignment,
    MultiplyAssignment,
    DivideAssignment,
    RemainderAssignment,
    LeftShift,
    RightShift,
    LogicalAnd,
    LogicalOr,
    EqualTo,
    NotEqualTo,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,

    // Identifiers (which may be keywords)
    Identifier(String),

    // Char literal
    //      The `literal` includes the surrounding single quotes.
    //      The value is `i32` because in C a char literal has type of 'int'.
    CharLiteral { literal: String, value: i32 },
    
    // Numeric literals
    IntegerLiteral { literal: String, base: NumericLiteralBase, suffix: Option<IntegerLiteralSuffix> },
    FloatLiteral { literal: String, base: NumericLiteralBase, suffix: Option<FloatLiteralSuffix> },

    // String literal
    //      `literal` is the literal string as it appears in the source, including surrounding double quotes and
    //      unevaluated escape sequences.
    //      `ascii` represents the individual ascii values from the raw literal. Where possible, escape sequences
    //      from the literal are transformed into ascii values (e.g. hex/octal escape sequences). Other escape
    //      sequences remain in place. E.g. "hello\n" --> { "h", "e", "l", "l", "o", "\n" }.
    StringLiteral { literal: String, ascii: Vec<String> },
}

/// The base of an integer or floating-point literal.
///
/// Floating-point literals can only have a Decimal or Hex base.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumericLiteralBase {
    Decimal,
    Hex,
    Octal,
    Binary,
}

impl fmt::Display for NumericLiteralBase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumericLiteralBase::Decimal => write!(f, "decimal"),
            NumericLiteralBase::Hex => write!(f, "hex"),
            NumericLiteralBase::Octal => write!(f, "octal"),
            NumericLiteralBase::Binary => write!(f, "binary"),
        }
    }
}

impl NumericLiteralBase {
    /// The base as an integer, e.g. `10` for `Decimal`.
    pub fn as_int(&self) -> usize {
        match self {
            NumericLiteralBase::Decimal => 10,
            NumericLiteralBase::Hex => 16,
            NumericLiteralBase::Octal => 8,
            NumericLiteralBase::Binary => 2,
        }
    }
}

/// An integer literal suffix.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntegerLiteralSuffix {
    U,
    L,
    UL,
    LL,
    ULL,
}

impl fmt::Display for IntegerLiteralSuffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntegerLiteralSuffix::U => write!(f, "U"),
            IntegerLiteralSuffix::L => write!(f, "L"),
            IntegerLiteralSuffix::UL => write!(f, "UL"),
            IntegerLiteralSuffix::LL => write!(f, "LL"),
            IntegerLiteralSuffix::ULL => write!(f, "ULL"),
        }
    }
}

/// A floating-point literal suffix.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum FloatLiteralSuffix {
    F,
    L,
}

impl fmt::Display for FloatLiteralSuffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatLiteralSuffix::F => write!(f, "F"),
            FloatLiteralSuffix::L => write!(f, "L"),
        }
    }
}

impl TokenType {
    /// Makes an identifier token type with the given name.
    pub fn new_identifier(id: &str) -> TokenType {
        TokenType::Identifier(id.to_string())
    }
}

#[cfg(test)]
impl TokenType {
    /// Makes an integer literal token type. The given string should only contain digits.
    pub fn new_int_literal(lit: &str) -> TokenType {
        TokenType::IntegerLiteral { literal: lit.to_string(), base: NumericLiteralBase::Decimal, suffix: None }
    }

    /// Makes a long integer literal token type. The given string should only contain digits.
    pub fn new_long_int_literal(lit: &str) -> TokenType {
        TokenType::IntegerLiteral {
            literal: lit.to_string(),
            base: NumericLiteralBase::Decimal,
            suffix: Some(IntegerLiteralSuffix::L),
        }
    }

    /// Makes a long long integer literal token type. The given string should only contain digits.
    pub fn new_long_long_int_literal(lit: &str) -> TokenType {
        TokenType::IntegerLiteral {
            literal: lit.to_string(),
            base: NumericLiteralBase::Decimal,
            suffix: Some(IntegerLiteralSuffix::LL),
        }
    }
}

impl fmt::Display for TokenType {
    #[rustfmt::skip]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::OpenParen                    => write!(f, "("),
            TokenType::CloseParen                   => write!(f, ")"),
            TokenType::OpenBrace                    => write!(f, "{{"),
            TokenType::CloseBrace                   => write!(f, "}}"),
            TokenType::OpenSqBracket                => write!(f, "["),
            TokenType::CloseSqBracket               => write!(f, "]"),
            TokenType::Semicolon                    => write!(f, ";"),
            TokenType::Colon                        => write!(f, ":"),
            TokenType::Comma                        => write!(f, ","),
            TokenType::BitwiseAnd                   => write!(f, "&"),
            TokenType::BitwiseOr                    => write!(f, "|"),
            TokenType::BitwiseNot                   => write!(f, "~"),
            TokenType::BitwiseXor                   => write!(f, "^"),
            TokenType::Plus                         => write!(f, "+"),
            TokenType::Minus                        => write!(f, "-"),
            TokenType::Multiply                     => write!(f, "*"),
            TokenType::Divide                       => write!(f, "/"),
            TokenType::Remainder                    => write!(f, "%"),
            TokenType::Assignment                   => write!(f, "="),
            TokenType::LogicalNot                   => write!(f, "!"),
            TokenType::LessThan                     => write!(f, "<"),
            TokenType::GreaterThan                  => write!(f, ">"),
            TokenType::Ternary                      => write!(f, "?"),

            // Multi-character tokens
            TokenType::BitwiseAndAssignment         => write!(f, "&="),
            TokenType::BitwiseOrAssignment          => write!(f, "|="),
            TokenType::BitwiseXorAssignment         => write!(f, "^="),
            TokenType::LeftShiftAssignment          => write!(f, "<<="),
            TokenType::RightShiftAssignment         => write!(f, ">>="),
            TokenType::Increment                    => write!(f, "++"),
            TokenType::Decrement                    => write!(f, "--"),
            TokenType::AdditionAssignment           => write!(f, "+="),
            TokenType::SubtractionAssignment        => write!(f, "-="),
            TokenType::MultiplyAssignment           => write!(f, "*="),
            TokenType::DivideAssignment             => write!(f, "/="),
            TokenType::RemainderAssignment          => write!(f, "%="),
            TokenType::LogicalAnd                   => write!(f, "&&"),
            TokenType::LogicalOr                    => write!(f, "||"),
            TokenType::LeftShift                    => write!(f, "<<"),
            TokenType::RightShift                   => write!(f, ">>"),
            TokenType::EqualTo                      => write!(f, "=="),
            TokenType::NotEqualTo                   => write!(f, "!="),
            TokenType::LessThanOrEqualTo            => write!(f, "<="),
            TokenType::GreaterThanOrEqualTo         => write!(f, ">="),

            // Identifiers / keywords
            TokenType::Identifier(id)               => write!(f, "{}", id),

            // Literals
            TokenType::CharLiteral { literal, value } => {
                write!(f, "{literal} ({value})")
            }

            TokenType::IntegerLiteral { literal, suffix, .. } => {
                if let Some(suffix) = suffix {
                    write!(f, "{literal}{suffix}")
                } else {
                    write!(f, "{literal}")
                }
            }

            TokenType::FloatLiteral { literal, suffix, .. } => {
                if let Some(suffix) = suffix {
                    write!(f, "{literal}{suffix}")
                } else {
                    write!(f, "{literal}")
                }
            }

            TokenType::StringLiteral { literal, ascii } => {
                let ascii_joined = ascii.join("");
                write!(f, "{literal} (\"{ascii_joined}\")")
            }
        }
    }
}

/// A token produced by lexical analysis.
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub location: SourceLocation,
}

impl Token {
    /// Is the token an identifier?
    pub fn is_identifier(&self) -> bool {
        let TokenType::Identifier(_) = self.token_type else {
            return false;
        };
        true
    }

    /// Is the token an identifier with the given name?
    pub fn is_identifier_with_name(&self, expected_identifier: &str) -> bool {
        let TokenType::Identifier(ref id) = self.token_type else {
            return false;
        };
        id == expected_identifier
    }

    /// If the token is an identifier, returns its string. Otherwise returns `None`.
    pub fn get_identifier(&self) -> Option<&String> {
        let TokenType::Identifier(ref id) = self.token_type else {
            return None;
        };

        Some(id)
    }

    /// Does the token have the given type?
    pub fn has_type(&self, expected_type: TokenType) -> bool {
        self.token_type == expected_type
    }

    /// Does the token have one of the assignment token types?
    pub fn is_assignment(&self) -> bool {
        matches!(
            self.token_type,
            TokenType::Assignment
                | TokenType::AdditionAssignment
                | TokenType::SubtractionAssignment
                | TokenType::MultiplyAssignment
                | TokenType::DivideAssignment
                | TokenType::RemainderAssignment
                | TokenType::BitwiseAndAssignment
                | TokenType::BitwiseOrAssignment
                | TokenType::BitwiseXorAssignment
                | TokenType::LeftShiftAssignment
                | TokenType::RightShiftAssignment
        )
    }
}

#[cfg(test)]
impl Token {
    /// Creates a Token with a TokenType and a line/column source location.
    pub fn with_location(token_type: TokenType, line: usize, column: usize) -> Self {
        Self { token_type, location: SourceLocation { line, column, length: 1 } }
    }

    /// Creates a Token with a TokenType but a default/zero source location.
    pub fn without_location(token_type: TokenType) -> Self {
        Self { token_type, location: Default::default() }
    }
}
