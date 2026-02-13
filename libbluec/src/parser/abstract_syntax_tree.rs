// Copyright 2025-2026 Neil Henderson
//
//! The `abstract_syntax_tree` module defines the C AST that the parser produces from the lexer's token stream.

mod ast_attributes;
mod ast_basic_type;
mod ast_constant_value;
mod ast_declarator;
mod ast_declared_type;
mod ast_expression;
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
pub use self::ast_expression::*;
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
        match self {
            AstDeclaration::Variable(var_decl) => &var_decl.declared_type,
            AstDeclaration::Function(fn_decl) => &fn_decl.declared_type,
            AstDeclaration::TypeAlias(alias_decl) => alias_decl.decl.get_declared_type(),
        }
    }

    /// Gets the unique name of the declaration.
    pub fn unique_name(&self) -> &AstUniqueName {
        match self {
            AstDeclaration::Variable(var_decl) => &var_decl.unique_name,
            AstDeclaration::Function(fn_decl) => &fn_decl.unique_name,
            AstDeclaration::TypeAlias(alias_decl) => alias_decl.decl.unique_name(),
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
    Expression(AstExpression),
    Labeled {
        node_id: AstNodeId, // For the label declaration, not the inner statement.
        label_name: String,
        stmt: Box<AstStatement>,
    },
    Compound(AstBlock),
    Null, // An empty expression-statement, e.g. a single ';'
    If {
        controlling_expr: AstExpression,
        then_stmt: Box<AstStatement>,
        else_stmt: Option<Box<AstStatement>>,
    },
    Switch {
        node_id: AstNodeId,
        controlling_expr: AstExpression,
        body: Box<AstStatement>,
    },
    Case {
        switch_node_id: AstNodeId,
        constant_expr: AstExpression,
        stmt: Box<AstStatement>,
    },
    Default {
        switch_node_id: AstNodeId,
        stmt: Box<AstStatement>,
    },
    While {
        node_id: AstNodeId,
        controlling_expr: AstExpression,
        body: Box<AstStatement>,
    },
    DoWhile {
        node_id: AstNodeId,
        body: Box<AstStatement>,
        controlling_expr: AstExpression,
    },
    For {
        node_id: AstNodeId,
        init: Box<AstForInitializer>, // Boxed to reduce variant size
        controlling_expr: Option<AstExpression>,
        post_expr: Option<AstExpression>,
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
    Return(AstExpression),
}

/// A for-statement initializer can either be a variable declaration or an expression, or nothing.
#[derive(Debug)]
pub enum AstForInitializer {
    Declaration(Vec<AstDeclaration>),
    Expression(Option<AstExpression>),
}
