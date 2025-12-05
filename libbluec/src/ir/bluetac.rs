// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `bluetac` module defines the types in the BlueTac intermediate representation (IR).

use std::fmt;

use crate::ICE;
use crate::parser;
use crate::parser::{AstConstantFp, AstConstantInteger, AstConstantValue, AstType};
use crate::sema::symbol_table::SymbolTable;

/// The root of the BlueTac IR contains a list of definitions.
pub struct BtRoot(pub Vec<BtDefinition>);

/// A BlueTac IR definition is either a function definition or a variable with static storage duration.
#[derive(Debug)]
pub enum BtDefinition {
    Function(BtFunctionDefn),
    StaticVariable(BtStaticStorageVariable),
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
    pub init_value: BtConstantValue,
}

/// A label identifier in IR.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtLabelIdentifier(pub String);

/// A BlueTac IR instruction.
#[derive(Debug)]
pub enum BtInstruction {
    Return(BtValue),
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
        dst: BtValue,
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

/// An IR data type.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BtType {
    Void,
    Int16,
    Int32,
    Int64,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Pointer, // Opaque pointer, effectively a UInt64 address.
    Function, // Opaque function
}

impl From<&AstType> for BtType {
    fn from(ast_type: &AstType) -> Self {
        match ast_type {
            AstType::Void => BtType::Void,
            AstType::Short => BtType::Int16,
            AstType::Int => BtType::Int32,
            AstType::Long => BtType::Int64,
            AstType::LongLong => BtType::Int64,
            AstType::UnsignedShort => BtType::UInt16,
            AstType::UnsignedInt => BtType::UInt32,
            AstType::UnsignedLong => BtType::UInt64,
            AstType::UnsignedLongLong => BtType::UInt64,
            AstType::Float => BtType::Float32,
            AstType::Double | AstType::LongDouble => BtType::Float64,
            AstType::Pointer(_) => BtType::Pointer,
            AstType::Function { .. } => BtType::Function,
        }
    }
}

impl From<AstType> for BtType {
    fn from(ast_type: AstType) -> Self {
        BtType::from(&ast_type)
    }
}

impl fmt::Display for BtType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtType::Void => write!(f, "Void"),
            BtType::Int16 => write!(f, "Int16"),
            BtType::Int32 => write!(f, "Int32"),
            BtType::Int64 => write!(f, "Int64"),
            BtType::UInt16 => write!(f, "UInt16"),
            BtType::UInt32 => write!(f, "UInt32"),
            BtType::UInt64 => write!(f, "UInt64"),
            BtType::Float32 => write!(f, "Float32"),
            BtType::Float64 => write!(f, "Float64"),
            BtType::Pointer => write!(f, "Pointer"),
            BtType::Function => write!(f, "Function"),
        }
    }
}

impl BtType {
    /// Creates a `BtConstantValue` for the current `BtType` with a default zero value.
    pub fn get_const_default_value(&self) -> BtConstantValue {
        match self {
            BtType::Void => ICE!("Cannot create BtConstantValue for BtType::Void"),
            BtType::Int16 => BtConstantValue::Int16(0),
            BtType::Int32 => BtConstantValue::Int32(0),
            BtType::Int64 => BtConstantValue::Int64(0),
            BtType::UInt16 => BtConstantValue::UInt16(0),
            BtType::UInt32 => BtConstantValue::UInt32(0),
            BtType::UInt64 => BtConstantValue::UInt64(0),
            BtType::Float32 => BtConstantValue::Float32(0.0),
            BtType::Float64 => BtConstantValue::Float64(0.0),
            BtType::Pointer => BtConstantValue::UInt64(0),
            BtType::Function => ICE!("Cannot get default value for BtType::Function"),
        }
    }

    /// The size of the type in bits.
    pub fn bits(&self) -> usize {
        match self {
            BtType::Void => 0,
            BtType::Int16 | BtType::UInt16 => 16,
            BtType::Int32 | BtType::UInt32 => 32,
            BtType::Int64 | BtType::UInt64 => 64,
            BtType::Float32 => 32,
            BtType::Float64 => 64,
            BtType::Pointer => 64,
            BtType::Function => 0,
        }
    }

    /// Is this type an integer (signed or unsigned) type?
    pub fn is_integer(&self) -> bool {
        matches!(self, BtType::Int16 | BtType::Int32 | BtType::Int64 | BtType::UInt16 | BtType::UInt32 | BtType::UInt64)
    }

    /// Is this type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, BtType::Float32 | BtType::Float64)
    }

    /// Is this type a signed integer?
    pub fn is_signed_integer(&self) -> bool {
        matches!(self, BtType::Int16 | BtType::Int32 | BtType::Int64)
    }

    /// Is this type an unsigned integer?
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(self, BtType::UInt16 | BtType::UInt32 | BtType::UInt64)
    }

    /// Is this type a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, BtType::Pointer)
    }

    /// Is this type a function type?
    pub fn is_function(&self) -> bool {
        matches!(self, BtType::Function)
    }

    /// Converts the `BtType` to a string suitable for the IR printer.
    pub fn to_printer(&self) -> String {
        match self {
            BtType::Void => "()".to_string(),
            BtType::Int16 => "i16".to_string(),
            BtType::Int32 => "i32".to_string(),
            BtType::Int64 => "i64".to_string(),
            BtType::UInt16 => "u16".to_string(),
            BtType::UInt32 => "u32".to_string(),
            BtType::UInt64 => "u64".to_string(),
            BtType::Float32 => "f32".to_string(),
            BtType::Float64 => "f64".to_string(),
            BtType::Pointer => "ptr".to_string(),
            BtType::Function => "fn".to_string(),
        }
    }
}

/// A constant integer or floating-point value.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum BtConstantValue {
    Int16(i16),
    Int32(i32),
    Int64(i64),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    Float32(f32),
    Float64(f64),
    AddressConstant { symbol: String },
}

impl From<AstConstantValue> for BtConstantValue {
    fn from(value: AstConstantValue) -> Self {
        match value {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Short(value) => BtConstantValue::Int16(value),
                AstConstantInteger::Int(value) => BtConstantValue::Int32(value),
                AstConstantInteger::LongLong(value) => BtConstantValue::Int64(value),
                AstConstantInteger::UnsignedShort(value) => BtConstantValue::UInt16(value),
                AstConstantInteger::UnsignedInt(value) => BtConstantValue::UInt32(value),
                AstConstantInteger::UnsignedLongLong(value) => BtConstantValue::UInt64(value),
            },

            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => BtConstantValue::Float32(value),
                AstConstantFp::Double(value) => BtConstantValue::Float64(value),
            },

            AstConstantValue::Pointer(_, init) => match init {
                parser::AstConstantPtrInitializer::NullPointerConstant => BtConstantValue::UInt64(0),
                parser::AstConstantPtrInitializer::CastExpression(val) => BtConstantValue::UInt64(val),
                parser::AstConstantPtrInitializer::AddressConstant { symbol: object } => {
                    BtConstantValue::AddressConstant { symbol: object.to_string() }
                }
            },
        }
    }
}

impl fmt::Display for BtConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtConstantValue::Int16(value) => write!(f, "{value}"),
            BtConstantValue::Int32(value) => write!(f, "{value}"),
            BtConstantValue::Int64(value) => write!(f, "{value}"),
            BtConstantValue::UInt16(value) => write!(f, "{value}"),
            BtConstantValue::UInt32(value) => write!(f, "{value}"),
            BtConstantValue::UInt64(value) => write!(f, "{value}"),
            BtConstantValue::Float32(value) => write!(f, "{value}"),
            BtConstantValue::Float64(value) => write!(f, "{value}"),
            BtConstantValue::AddressConstant { symbol: object } => write!(f, "@{object}"),
        }
    }
}

impl BtConstantValue {
    /// Makes a `BtConstantValue` for the given type with a constant value of `1`.
    pub fn one(bt_type: BtType) -> Self {
        match bt_type {
            BtType::Void => ICE!("Cannot create a BtConstantValue(1) for BtType::Void"),
            BtType::Int16 => BtConstantValue::Int16(1),
            BtType::Int32 => BtConstantValue::Int32(1),
            BtType::Int64 => BtConstantValue::Int64(1),
            BtType::UInt16 => BtConstantValue::UInt16(1),
            BtType::UInt32 => BtConstantValue::UInt32(1),
            BtType::UInt64 => BtConstantValue::UInt64(1),
            BtType::Float32 => BtConstantValue::Float32(1.0),
            BtType::Float64 => BtConstantValue::Float64(1.0),
            BtType::Pointer => ICE!("Cannot create a value of 1 for a pointer type"),
            BtType::Function => ICE!("Cannot create a value of 1 for a function type"),
        }
    }

    /// Gets the IR type of the constant value.
    pub fn get_bt_type(&self) -> BtType {
        match self {
            BtConstantValue::Int16(_) => BtType::Int16,
            BtConstantValue::Int32(_) => BtType::Int32,
            BtConstantValue::Int64(_) => BtType::Int64,
            BtConstantValue::UInt16(_) => BtType::UInt16,
            BtConstantValue::UInt32(_) => BtType::UInt32,
            BtConstantValue::UInt64(_) => BtType::UInt64,
            BtConstantValue::Float32(_) => BtType::Float32,
            BtConstantValue::Float64(_) => BtType::Float64,
            BtConstantValue::AddressConstant { .. } => BtType::Pointer,
        }
    }

    /// Is the constant value the default zero?
    pub fn has_default_value(&self) -> bool {
        match self {
            BtConstantValue::Int16(value) => *value == 0,
            BtConstantValue::Int32(value) => *value == 0,
            BtConstantValue::Int64(value) => *value == 0,
            BtConstantValue::UInt16(value) => *value == 0,
            BtConstantValue::UInt32(value) => *value == 0,
            BtConstantValue::UInt64(value) => *value == 0,
            BtConstantValue::Float32(value) => *value == 0.0,
            BtConstantValue::Float64(value) => *value == 0.0,
            _ => false,
        }
    }
}

/// An IR value is either a constant or a variable.
#[derive(Debug, Clone)]
pub enum BtValue {
    Constant(BtConstantValue),
    Variable(String),
}

impl From<AstConstantValue> for BtValue {
    fn from(value: AstConstantValue) -> Self {
        BtValue::Constant(value.into())
    }
}

impl fmt::Display for BtValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtValue::Constant(bt_constant_value) => write!(f, "{}", bt_constant_value),
            BtValue::Variable(name) => write!(f, "{name}"),
        }
    }
}

impl BtValue {
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
