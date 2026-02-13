// Copyright 2025-2026 Neil Henderson
//
//! The `ast_expression` module defines [AstExpression].

use super::{
    AstAssignmentOp, AstBinaryOp, AstDeclaredType, AstFloatLiteralKind, AstIntegerLiteralKind, AstNodeId, AstUnaryOp,
    AstUniqueName,
};

/// An expression.
#[derive(Debug, Clone, PartialEq)]
pub struct AstExpression {
    node_id: AstNodeId,
    kind: AstExpressionKind,
}

/// The kind of expression, which may in fact be a subexpression inside a tree of a larger expression.
#[derive(Debug, Clone, PartialEq)]
pub enum AstExpressionKind {
    Unary {
        op: AstUnaryOp,
        operand: Box<AstExpression>,
    },
    Binary {
        op: AstBinaryOp,
        lhs: Box<AstExpression>,
        rhs: Box<AstExpression>,
    },
    Assignment {
        computation_node_id: AstNodeId, // For a compound assignment; used by sema to annotate computation type
        op: AstAssignmentOp,
        lhs: Box<AstExpression>,
        rhs: Box<AstExpression>,
    },
    Conditional {
        condition: Box<AstExpression>,
        consequent: Box<AstExpression>,
        alternative: Box<AstExpression>,
    },
    FunctionCall {
        designator: Box<AstExpression>,
        args_node_id: AstNodeId,
        args: Vec<AstExpression>,
    },
    Deref {
        pointer: Box<AstExpression>,
    },
    AddressOf {
        target: Box<AstExpression>,
    },
    Subscript {
        expr1: Box<AstExpression>, // Pointer and Index sub-expressions can be swapped so we name them 1 & 2.
        expr2: Box<AstExpression>,
    },
    Cast {
        target_type: AstDeclaredType,
        inner: Box<AstExpression>,
        is_implicit: bool, // Was the cast expression added by sema/type checking
    },
    Ident {
        name: String,
        unique_name: AstUniqueName,
    },
    CharLiteral {
        literal: String,
        value: i32,
    },
    StringLiteral {
        literals: Vec<String>, // Adjacent string literal tokens are concatenated
        ascii: Vec<String>,
    },
    // Numeric literals are parsed as non-negative. A unary negate operator appears in the AST as a UnaryOperation,
    // and is later translated into a negative integer/float value after parsing.
    IntegerLiteral {
        literal: String,
        literal_base: usize,
        value: u64, // The evaluated literal
        kind: AstIntegerLiteralKind,
    },
    FloatLiteral {
        literal: String,
        literal_base: usize,
        value: f64, // The evaluated literal
        kind: AstFloatLiteralKind,
    },
}

impl AstExpression {
    /// Creates a new `AstExpression` with the given node ID and kind.
    pub fn new(node_id: AstNodeId, kind: AstExpressionKind) -> Self {
        Self { node_id, kind }
    }

    /// Creates a new `AstExpression:IntegerLiteral` with the given value, in base 10.
    pub fn new_int_literal(value: u64) -> Self {
        Self {
            node_id: AstNodeId::new(),
            kind: AstExpressionKind::IntegerLiteral {
                literal: value.to_string(),
                literal_base: 10,
                value,
                kind: AstIntegerLiteralKind::Int,
            },
        }
    }

    /// Deconstructs the expression to allow the caller to take ownership of the expression's fields.
    pub fn deconstruct(self) -> (AstNodeId, AstExpressionKind) {
        (self.node_id, self.kind)
    }

    /// The expression's unique node ID.
    pub fn id(&self) -> AstNodeId {
        self.node_id
    }

    /// The kind of expression.
    pub fn kind(&self) -> &AstExpressionKind {
        &self.kind
    }

    /// The kind of expression.
    pub fn kind_mut(&mut self) -> &mut AstExpressionKind {
        &mut self.kind
    }

    /// Is the AST expression an l-value?
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self.kind,
            AstExpressionKind::Ident { .. }
                | AstExpressionKind::Deref { .. }
                | AstExpressionKind::Subscript { .. }
                | AstExpressionKind::StringLiteral { .. }
        )
    }

    /// Is the AST expression a literal?
    pub fn is_literal(&self) -> bool {
        matches!(
            self.kind,
            AstExpressionKind::CharLiteral { .. }
                | AstExpressionKind::StringLiteral { .. }
                | AstExpressionKind::IntegerLiteral { .. }
                | AstExpressionKind::FloatLiteral { .. }
        )
    }

    /// Is the AST expression an arithmetic (character, integer or floating-point) literal?
    pub fn is_arithmetic_literal(&self) -> bool {
        matches!(
            self.kind,
            AstExpressionKind::CharLiteral { .. }
                | AstExpressionKind::IntegerLiteral { .. }
                | AstExpressionKind::FloatLiteral { .. }
        )
    }

    /// Is the AST expression a string literal?
    pub fn is_string_literal(&self) -> bool {
        matches!(self.kind, AstExpressionKind::StringLiteral { .. })
    }

    /// Is the AST expression an integer literal?
    pub fn is_integer_literal(&self) -> bool {
        matches!(self.kind, AstExpressionKind::IntegerLiteral { .. })
    }

    /// Is the AST expression an integer literal with the given value?
    pub fn is_integer_literal_with_value(&self, value: u64) -> bool {
        if let AstExpressionKind::IntegerLiteral { value: literal_value, .. } = self.kind
            && literal_value == value
        {
            true
        } else {
            false
        }
    }

    /// Is the AST expression an identifier of the given declared name?
    pub fn is_identifier_with_name(&self, declared_name: &str) -> bool {
        if let AstExpressionKind::Ident { name, .. } = &self.kind { name == declared_name } else { false }
    }

    /// Is the AST expression a binary operation?
    pub fn is_binary_expr(&self) -> bool {
        matches!(self.kind, AstExpressionKind::Binary { .. })
    }

    /// Is the AST expression a binary operation with the given operator?
    pub fn is_binary_expr_with_op(&self, op: AstBinaryOp) -> bool {
        if let AstExpressionKind::Binary { op: binary_op, .. } = self.kind
            && binary_op == op
        {
            true
        } else {
            false
        }
    }
}
