// Copyright 2025-2026 Neil Henderson
//
//! The `bluetac` module defines the types in the "BlueTac" three-address code intermediate representation.

mod bt_constant_value;
mod bt_static_storage_initializer;
mod bt_type;

use std::fmt;

use crate::ICE;
use crate::parser;
use crate::parser::AstConstantInteger;
use crate::sema::constant_table::ConstantIndex;
use crate::sema::symbol_table::SymbolTable;

pub use bt_constant_value::BtConstantValue;
pub use bt_static_storage_initializer::BtStaticStorageInitializer;
pub use bt_type::BtType;

/// The root of the BlueTac IR contains a list of definitions.
pub struct BtRoot(pub Vec<BtDefinition>);

/// A BlueTac IR definition is either a function definition, a variable with static storage duration, or a
/// read-only constant value with global lifetime (equivalent of a static storage duration).
#[derive(Debug)]
pub enum BtDefinition {
    /// A function definition
    Function(BtFunctionDefn),

    /// A variable with static storage duration.
    StaticVariable(BtStaticStorageVariable),

    /// A read-only constant value.
    StaticConstant(BtStaticConstant),
}

/// The IR for a function definition.
#[derive(Debug)]
pub struct BtFunctionDefn {
    pub name: String,
    pub is_global: bool,
    pub return_type: BtType,
    pub params: Vec<(BtType, String)>,
    pub instructions: Vec<BtInstruction>,
}

/// The IR for a variable with static storage duration.
#[derive(Debug, Clone)]
pub struct BtStaticStorageVariable {
    pub name: String,
    pub is_global: bool,
    pub data_type: BtType,
    pub init_value: Vec<BtStaticStorageInitializer>,
}

/// The IR for a global read-only constant value.
#[derive(Debug, Clone)]
pub struct BtStaticConstant {
    pub name: String,
    pub data_type: BtType,
    pub index: ConstantIndex,
}

/// A label identifier in IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtLabelIdentifier(pub String);

/// A BlueTac IR instruction.
#[derive(Debug)]
pub enum BtInstruction {
    Return(Option<BtValue>),
    SignExtend {
        src: BtValue,
        dst: BtValue,
    },
    ZeroExtend {
        src: BtValue,
        dst: BtValue,
    },
    Truncate {
        src: BtValue,
        dst: BtValue,
    },
    ConvertFp {
        src: BtValue,
        dst: BtValue,
    },
    FpToSignedInteger {
        src: BtValue,
        dst: BtValue,
    },
    FpToUnsignedInteger {
        src: BtValue,
        dst: BtValue,
    },
    SignedIntegerToFp {
        src: BtValue,
        dst: BtValue,
    },
    UnsignedIntegerToFp {
        src: BtValue,
        dst: BtValue,
    },
    Unary {
        op: BtUnaryOp,
        src: BtValue,
        dst: BtValue,
    },
    Binary {
        op: BtBinaryOp,
        src1: BtValue,
        src2: BtValue,
        dst: BtValue,
    },
    Copy {
        src: BtValue,
        dst: BtValue,
    },
    Load {
        src_ptr: BtValue,
        dst: BtValue,
    },
    Store {
        src: BtValue,
        dst_ptr: BtValue,
    },
    StoreAddress {
        src: BtValue,
        dst_ptr: BtValue,
    },
    StoreAtOffset {
        src: BtValue,
        dst_ptr: BtValue,
        dst_offset: usize,
    },
    AddPtr {
        src_ptr: BtValue,
        index: BtValue,
        scale: usize,
        dst_ptr: BtValue,
    },
    Jump {
        target: BtLabelIdentifier,
    },
    JumpIfZero {
        condition: BtValue,
        target: BtLabelIdentifier,
    },
    JumpIfNotZero {
        condition: BtValue,
        target: BtLabelIdentifier,
    },
    Label {
        id: BtLabelIdentifier,
    },
    Switch {
        controlling_value: BtValue,
        cases: Vec<BtSwitchCase>,
        default_label: Option<BtLabelIdentifier>,
        break_label: BtLabelIdentifier, // End of the switch statement
    },
    FunctionCall {
        designator: BtValue,
        args: Vec<BtValue>,
        dst: Option<BtValue>, // May be 'void'
    },
}

/// A case in a switch statement.
#[derive(Debug)]
pub struct BtSwitchCase {
    pub value: BtValue,
    pub label: BtLabelIdentifier,
}

/// Unary operator.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BtUnaryOp {
    Negate,
    Plus,
    BitwiseNot, // Complement
    LogicalNot,
}

impl fmt::Display for BtUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtUnaryOp::Negate => write!(f, "neg"),
            BtUnaryOp::Plus => write!(f, "plus"),
            BtUnaryOp::BitwiseNot => write!(f, "bitnot"),
            BtUnaryOp::LogicalNot => write!(f, "not"),
        }
    }
}

/// Binary operator.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BtBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LeftShift,
    RightShift,
    EqualTo,
    NotEqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
}

impl fmt::Display for BtBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtBinaryOp::Add => write!(f, "add"),
            BtBinaryOp::Subtract => write!(f, "sub"),
            BtBinaryOp::Multiply => write!(f, "mul"),
            BtBinaryOp::Divide => write!(f, "div"),
            BtBinaryOp::Remainder => write!(f, "rem"),
            BtBinaryOp::BitwiseAnd => write!(f, "bitand"),
            BtBinaryOp::BitwiseXor => write!(f, "bitxor"),
            BtBinaryOp::BitwiseOr => write!(f, "bitor"),
            BtBinaryOp::LeftShift => write!(f, "shl"),
            BtBinaryOp::RightShift => write!(f, "shr"),
            BtBinaryOp::EqualTo => write!(f, "eq"),
            BtBinaryOp::NotEqualTo => write!(f, "ne"),
            BtBinaryOp::LessThan => write!(f, "lt"),
            BtBinaryOp::GreaterThan => write!(f, "gt"),
            BtBinaryOp::LessThanOrEqualTo => write!(f, "le"),
            BtBinaryOp::GreaterThanOrEqualTo => write!(f, "ge"),
        }
    }
}

/// An IR value is either a constant or a variable.
#[derive(Debug, Clone, PartialEq)]
pub enum BtValue {
    Constant(BtConstantValue),
    Variable(String),
}

impl fmt::Display for BtValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtValue::Constant(bt_constant_value) => write!(f, "{}", bt_constant_value),
            BtValue::Variable(name) => write!(f, "{name}"),
        }
    }
}

impl From<AstConstantInteger> for BtValue {
    fn from(value: AstConstantInteger) -> Self {
        BtValue::Constant(value.into())
    }
}

impl BtValue {
    /// Creates a new constant Int8 value
    pub fn new_constant_i8(value: i8) -> Self {
        BtValue::Constant(BtConstantValue::Int8(value))
    }

    /// Creates a new constant Int16 value
    pub fn new_constant_i16(value: i16) -> Self {
        BtValue::Constant(BtConstantValue::Int16(value))
    }

    /// Creates a new constant Int32 value
    pub fn new_constant_i32(value: i32) -> Self {
        BtValue::Constant(BtConstantValue::Int32(value))
    }

    /// Creates a new constant Int64 value
    pub fn new_constant_i64(value: i64) -> Self {
        BtValue::Constant(BtConstantValue::Int64(value))
    }

    /// Creates a new constant UInt8 value
    pub fn new_constant_u8(value: u8) -> Self {
        BtValue::Constant(BtConstantValue::UInt8(value))
    }

    /// Creates a new constant UInt16 value
    pub fn new_constant_u16(value: u16) -> Self {
        BtValue::Constant(BtConstantValue::UInt16(value))
    }

    /// Creates a new constant UInt32 value
    pub fn new_constant_u32(value: u32) -> Self {
        BtValue::Constant(BtConstantValue::UInt32(value))
    }

    /// Creates a new constant UInt64 value
    pub fn new_constant_u64(value: u64) -> Self {
        BtValue::Constant(BtConstantValue::UInt64(value))
    }

    /// Creates a new constant Float32 value
    pub fn new_constant_f32(value: f32) -> Self {
        BtValue::Constant(BtConstantValue::Float32(value))
    }

    /// Creates a new constant Float64 value
    pub fn new_constant_f64(value: f64) -> Self {
        BtValue::Constant(BtConstantValue::Float64(value))
    }

    /// Is the value a constant value?
    pub fn is_constant(&self) -> bool {
        matches!(self, BtValue::Constant(_))
    }

    /// Gets the IR type of the value.
    pub fn get_type(&self, symbols: &SymbolTable) -> BtType {
        match self {
            BtValue::Constant(bt_constant_value) => bt_constant_value.get_bt_type(),

            BtValue::Variable(var) => {
                let Some(symbol) = symbols.get(parser::AstUniqueName::new(var)) else {
                    ICE!("Cannot find symbol for '{var}'");
                };

                BtType::from(&symbol.data_type)
            }
        }
    }
}
