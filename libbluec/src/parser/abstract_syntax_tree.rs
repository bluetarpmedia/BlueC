// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `abstract_syntax_tree` module defines the C AST that the parser produces from the lexer's token stream.

mod ast_attributes;
mod ast_constant_value;
mod ast_declarator;
mod ast_identifier;
mod ast_literals;
mod ast_operators;
mod ast_type;
mod ast_unique_name;

pub use self::ast_attributes::*;
pub use self::ast_constant_value::*;
pub use self::ast_declarator::*;
pub use self::ast_identifier::*;
pub use self::ast_literals::*;
pub use self::ast_operators::*;
pub use self::ast_type::*;
pub use self::ast_unique_name::*;

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
            AstDeclaration::TypeAlias(alias_decl) => alias_decl.decl.get_declared_type()
        }
    }
}

/// A variable declaration with optional definition.
///
/// The `is_declaration_only` field specifies whether the variable is a declaration only, or if it's a declaration and
/// definition. A variable can be defined even if there is no `init_expr`, e.g. in file scope or a static variable
/// declaration in block scope. In both of those cases, the variable is a definition but has no initializer, and
/// will get a default value of zero.
///
/// If `is_declaration_only` is true, then the declaration only introduces the variable's name and type, and does not
/// allocate any storage for it. For example, `extern int i;` or `struct Foo;`.
///
/// Note that `extern int i = 3;` is a definition and the `extern` is ignored (and a warning is emitted).
#[derive(Debug)]
pub struct AstVariableDeclaration {
    pub node_id: AstNodeId,
    pub is_declaration_only: bool,
    pub is_file_scope: bool,
    pub declared_type: AstDeclaredType,
    pub ident: AstIdentifier,
    pub unique_name: AstUniqueName,
    pub init_expr: Option<AstFullExpression>,
    pub init_constant_value: Option<AstConstantValue>, // If `init_expr` can be evaluated at compile-time, this is the evaluated value.
    pub linkage: AstLinkage,
    pub storage: AstStorageDuration,
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
        fn_name: AstUniqueName,
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
    Cast {
        node_id: AstNodeId,
        target_type: AstDeclaredType,
        expr: Box<AstExpression>,
    },
    Variable {
        node_id: AstNodeId,
        name: String,
        unique_name: AstUniqueName,
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
            AstExpression::Cast { node_id, .. } => *node_id,
            AstExpression::Variable { node_id, .. } => *node_id,
            AstExpression::IntegerLiteral { node_id, .. } => *node_id,
            AstExpression::FloatLiteral { node_id, .. } => *node_id,
        }
    }

    /// Is the AST expression an l-value?
    pub fn is_lvalue(&self) -> bool {
        matches!(self, AstExpression::Variable { .. } | AstExpression::Deref { .. })
    }

    /// Is the AST expression an integer literal?
    pub fn is_integer_literal(&self) -> bool {
        matches!(self, AstExpression::IntegerLiteral { .. })
    }
}
