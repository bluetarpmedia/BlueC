// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `abstract_syntax_tree` module defines the C AST that the parser produces from the lexer's token stream.

mod ast_attributes;
mod ast_basic_type;
mod ast_constant_value;
mod ast_declarator;
mod ast_declared_type;
mod ast_identifier;
mod ast_literals;
mod ast_operators;
mod ast_type;
mod ast_unique_name;
mod ast_variable_declaration;

pub use self::ast_attributes::*;
pub use self::ast_basic_type::*;
pub use self::ast_constant_value::*;
pub use self::ast_declarator::*;
pub use self::ast_declared_type::*;
pub use self::ast_identifier::*;
pub use self::ast_literals::*;
pub use self::ast_operators::*;
pub use self::ast_type::*;
pub use self::ast_unique_name::*;
pub use self::ast_variable_declaration::*;

/// The root of the AST, representing the file scope of the translation unit.
#[derive(Debug)]
pub struct AstRoot(pub Vec<AstDeclaration>);

/// A declaration introduces a name and is either a variable or function declaration.
#[derive(Debug)]
pub enum AstDeclaration {
    Variable(AstVariableDeclaration),
    Function(AstFunction),
    TypeAlias(AstTypeAliasDeclaration),
}

impl AstDeclaration {
    /// Gets the declared type of the declaration.
    pub fn get_declared_type(&self) -> &AstDeclaredType {
        match &self {
            AstDeclaration::Variable(var_decl) => &var_decl.declared_type,
            AstDeclaration::Function(fn_decl) => &fn_decl.declared_type,
            AstDeclaration::TypeAlias(alias_decl) => alias_decl.decl.get_declared_type(),
        }
    }
}

/// A function declaration with optional definition.
#[derive(Debug)]
pub struct AstFunction {
    pub node_id: AstNodeId,
    pub declared_type: AstDeclaredType,
    pub ident: AstIdentifier,
    pub unique_name: AstUniqueName,
    pub param_names: Vec<(AstIdentifier, AstUniqueName)>,
    pub body: Option<AstBlock>,
    pub linkage: AstLinkage,
}

/// A `typedef` declaration.
#[derive(Debug)]
pub struct AstTypeAliasDeclaration {
    pub node_id: AstNodeId,
    pub decl: Box<AstDeclaration>,
}

/// A block is a list of statements and declarations wrapped in braces.
/// A function's body and a compound statement are both blocks.
#[derive(Debug)]
pub struct AstBlock(pub Vec<AstBlockItem>);

/// An item in a block can be a statement or a declaration.
///
/// In C, declarations (even with initializers) are not statements. E.g. this is invalid C code (but is valid C++):
/// ```c
/// if (condition)
///     int a = 1;
/// ```
/// So we treat declarations separately from statements. A declaration with an initializer expression will be translated
/// into IR so that the initializer expression becomes an assignment.
///
/// Note: Type alias (typedef) declarations do not appear in our AST. The Parser handles them during parsing but has no
/// need to construct them in the AST.
#[derive(Debug)]
pub enum AstBlockItem {
    Declaration(AstDeclaration),
    Statement(AstStatement),
}

/// A statement.
#[derive(Debug)]
pub enum AstStatement {
    Expression(AstFullExpression),
    Labeled {
        node_id: AstNodeId, // For the label declaration, not the inner statement.
        label_name: String,
        stmt: Box<AstStatement>,
    },
    Compound(AstBlock),
    Null, // An empty expression-statement, e.g. a single ';'
    If {
        controlling_expr: AstFullExpression,
        then_stmt: Box<AstStatement>,
        else_stmt: Option<Box<AstStatement>>,
    },
    Switch {
        node_id: AstNodeId,
        controlling_expr: AstFullExpression,
        body: Box<AstStatement>,
    },
    Case {
        switch_node_id: AstNodeId,
        constant_expr: AstFullExpression,
        stmt: Box<AstStatement>,
    },
    Default {
        switch_node_id: AstNodeId,
        stmt: Box<AstStatement>,
    },
    While {
        node_id: AstNodeId,
        controlling_expr: AstFullExpression,
        body: Box<AstStatement>,
    },
    DoWhile {
        node_id: AstNodeId,
        body: Box<AstStatement>,
        controlling_expr: AstFullExpression,
    },
    For {
        node_id: AstNodeId,
        init: Box<AstForInitializer>, // Boxed to reduce variant size
        controlling_expr: Option<AstFullExpression>,
        post: Option<AstFullExpression>,
        body: Box<AstStatement>,
    },
    Break {
        enclosing_stmt_node_id: AstNodeId, // Either a loop or switch statement node ID
    },
    Continue {
        loop_node_id: AstNodeId,
    },
    Goto {
        node_id: AstNodeId,
        label_name: String,
    },
    Return(AstFullExpression),
}

/// A for-statement initializer can either be a variable declaration or an expression, or nothing.
#[derive(Debug)]
pub enum AstForInitializer {
    Declaration(Vec<AstDeclaration>),
    Expression(Option<AstFullExpression>),
}

/// A full expression is an expression that is not a subexpression of another expression.
#[derive(Debug)]
pub struct AstFullExpression {
    pub node_id: AstNodeId,
    pub expr: AstExpression,
}

impl AstFullExpression {
    /// Creates a new full expression which takes ownership of the `expr` expression tree.
    pub fn new(expr: AstExpression) -> Self {
        AstFullExpression { node_id: AstNodeId::new(), expr }
    }
}

/// An expression, which may in fact be a subexpression inside a tree of a larger expression.
#[derive(Debug)]
pub enum AstExpression {
    UnaryOperation {
        node_id: AstNodeId,
        op: AstUnaryOp,
        expr: Box<AstExpression>,
    },
    BinaryOperation {
        node_id: AstNodeId,
        op: AstBinaryOp,
        left: Box<AstExpression>,
        right: Box<AstExpression>,
    },
    Assignment {
        node_id: AstNodeId,
        computation_node_id: AstNodeId,
        op: AstAssignmentOp,
        lhs: Box<AstExpression>,
        rhs: Box<AstExpression>,
    },
    Conditional {
        node_id: AstNodeId,
        expr: Box<AstExpression>,
        consequent: Box<AstExpression>,
        alternative: Box<AstExpression>,
    },
    FunctionCall {
        node_id: AstNodeId,
        designator: Box<AstExpression>,
        args_node_id: AstNodeId,
        args: Vec<AstExpression>,
    },
    Deref {
        node_id: AstNodeId,
        expr: Box<AstExpression>,
    },
    AddressOf {
        node_id: AstNodeId,
        expr: Box<AstExpression>,
    },
    Subscript {
        node_id: AstNodeId,
        expr1: Box<AstExpression>, // Pointer and Index sub-expressions can be swapped so we name them 1 & 2.
        expr2: Box<AstExpression>,
    },
    Cast {
        node_id: AstNodeId,
        target_type: AstDeclaredType,
        expr: Box<AstExpression>,
    },
    Identifier {
        node_id: AstNodeId,
        name: String,
        unique_name: AstUniqueName,
    },
    CharLiteral {
        node_id: AstNodeId,
        literal: String,
        value: i32,
    },
    StringLiteral {
        node_id: AstNodeId,
        literals: Vec<String>, // Adjacent string literal tokens are concatenated
        ascii: Vec<String>,
    },
    // Numeric literals are parsed as non-negative. A unary negate operator appears in the AST as a UnaryOperation,
    // and is later translated into a negative integer/float value after parsing.
    IntegerLiteral {
        node_id: AstNodeId,
        literal: String,
        literal_base: usize,
        value: u64, // The evaluated literal
        kind: AstIntegerLiteralKind,
    },
    FloatLiteral {
        node_id: AstNodeId,
        literal: String,
        literal_base: usize,
        value: f64, // The evaluated literal
        kind: AstFloatLiteralKind,
    },
}

impl AstExpression {
    /// Creates a new `AstExpression:IntegerLiteral` with the given value, in base 10.
    pub fn new_int_literal(value: u64) -> Self {
        AstExpression::IntegerLiteral {
            node_id: AstNodeId::new(),
            literal: value.to_string(),
            literal_base: 10,
            value,
            kind: AstIntegerLiteralKind::Int,
        }
    }

    /// Gets the node ID for the AST expression.
    pub fn node_id(&self) -> AstNodeId {
        match self {
            AstExpression::UnaryOperation { node_id, .. } => *node_id,
            AstExpression::BinaryOperation { node_id, .. } => *node_id,
            AstExpression::Assignment { node_id, .. } => *node_id,
            AstExpression::Conditional { node_id, .. } => *node_id,
            AstExpression::FunctionCall { node_id, .. } => *node_id,
            AstExpression::Deref { node_id, .. } => *node_id,
            AstExpression::AddressOf { node_id, .. } => *node_id,
            AstExpression::Subscript { node_id, .. } => *node_id,
            AstExpression::Cast { node_id, .. } => *node_id,
            AstExpression::Identifier { node_id, .. } => *node_id,
            AstExpression::CharLiteral { node_id, .. } => *node_id,
            AstExpression::StringLiteral { node_id, .. } => *node_id,
            AstExpression::IntegerLiteral { node_id, .. } => *node_id,
            AstExpression::FloatLiteral { node_id, .. } => *node_id,
        }
    }

    /// Is the AST expression an l-value?
    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            AstExpression::Identifier { .. }
                | AstExpression::Deref { .. }
                | AstExpression::Subscript { .. }
                | AstExpression::StringLiteral { .. }
        )
    }

    /// Is the AST expression a string literal?
    pub fn is_string_literal(&self) -> bool {
        matches!(self, AstExpression::StringLiteral { .. })
    }

    /// Is the AST expression an integer literal?
    pub fn is_integer_literal(&self) -> bool {
        matches!(self, AstExpression::IntegerLiteral { .. })
    }

    /// Is the AST expression an integer literal with the given value?
    pub fn is_integer_literal_with_value(&self, value: u64) -> bool {
        if let AstExpression::IntegerLiteral { value: literal_value, .. } = self
            && *literal_value == value
        {
            true
        } else {
            false
        }
    }

    /// Is the AST expression an arithmetic (integer or floating-point) literal?
    pub fn is_arithmetic_literal(&self) -> bool {
        matches!(
            self,
            AstExpression::CharLiteral { .. }
                | AstExpression::IntegerLiteral { .. }
                | AstExpression::FloatLiteral { .. }
        )
    }
}
