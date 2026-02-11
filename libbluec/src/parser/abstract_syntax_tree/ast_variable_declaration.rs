// Copyright 2025-2026 Neil Henderson
//
//! The `ast_variable_declaration` module defines [AstVariableDeclaration] and its related types.

use std::fmt;

use super::{
    AstAddressConstant, AstConstantFp, AstConstantInteger, AstConstantValue, AstDeclaredType, AstFullExpression,
    AstIdentifier, AstLinkage, AstNodeId, AstStorageDuration, AstType, AstUniqueName,
};

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
///
/// If the `initializer` expression can be evaluated at compile-time (i.e. it's a constant expression) then the result
/// of its evaluation is stored in `init_constant_eval`. The sema stage is responsible for this evaluation. The
/// evaluation is a `Vec` because a static storage variable of array type may have more than one initializer value.
/// Scalar types will only have one value.
#[derive(Debug)]
pub struct AstVariableDeclaration {
    pub node_id: AstNodeId,
    pub is_declaration_only: bool,
    pub is_file_scope: bool,
    pub declared_type: AstDeclaredType,
    pub ident: AstIdentifier,
    pub unique_name: AstUniqueName,
    pub initializer: Option<AstVariableInitializer>,
    pub init_constant_eval: Vec<AstStaticStorageInitializer>,
    pub linkage: AstLinkage,
    pub storage: AstStorageDuration,
}

/// A variable initializer.
#[derive(Debug)]
pub enum AstVariableInitializer {
    Scalar(AstFullExpression),
    Aggregate { node_id: AstNodeId, init: Vec<AstVariableInitializer> },
}

impl AstVariableInitializer {
    /// Gets the initializer's node id.
    pub fn node_id(&self) -> AstNodeId {
        match self {
            AstVariableInitializer::Scalar(full_expr) => full_expr.node_id,
            AstVariableInitializer::Aggregate { node_id, .. } => *node_id,
        }
    }

    /// Is the initializer an aggregate initializer?
    pub fn is_aggregate(&self) -> bool {
        matches!(self, AstVariableInitializer::Aggregate { .. })
    }
}

/// A constant, compile-time initializer value for a static storage variable.
#[derive(Debug, Clone, PartialEq)]
pub enum AstStaticStorageInitializer {
    ZeroBytes(usize),
    Integer(AstConstantInteger),
    Fp(AstConstantFp),
    Pointer(AstType, AstAddressConstant),
    String { ascii: Vec<String> },
}

impl fmt::Display for AstStaticStorageInitializer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstStaticStorageInitializer::ZeroBytes(count) => write!(f, "ZeroBytes({count})"),
            AstStaticStorageInitializer::Integer(value) => write!(f, "{value}"),
            AstStaticStorageInitializer::Fp(value) => write!(f, "{value}"),
            AstStaticStorageInitializer::Pointer(ty, _) => write!(f, "{ty}"),
            AstStaticStorageInitializer::String { ascii } => {
                let ascii_joined = ascii.join("");
                write!(f, "String(\"{ascii_joined}\")")
            }
        }
    }
}

impl From<AstConstantValue> for AstStaticStorageInitializer {
    fn from(value: AstConstantValue) -> Self {
        match value {
            AstConstantValue::Integer(constant_int) => AstStaticStorageInitializer::Integer(constant_int),
            AstConstantValue::Fp(constant_fp) => AstStaticStorageInitializer::Fp(constant_fp),
            AstConstantValue::Pointer(ty, addr_constant) => AstStaticStorageInitializer::Pointer(ty, addr_constant),
            AstConstantValue::String { ascii } => AstStaticStorageInitializer::String { ascii },
        }
    }
}
