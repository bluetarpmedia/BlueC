// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `bluetac` module defines the types in the "BlueTac" high-level intermediate representation (HIR).

mod bt_constant_value;
mod bt_static_storage_initializer;

use std::fmt;

use crate::ICE;
use crate::parser;
use crate::parser::{AstConstantInteger, AstType};
use crate::sema::constant_table::ConstantIndex;
use crate::sema::symbol_table::SymbolTable;

pub use bt_constant_value::BtConstantValue;
pub use bt_static_storage_initializer::BtStaticStorageInitializer;

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
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BtType {
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Array { element_type: Box<BtType>, count: usize },
    Pointer,  // Opaque pointer, effectively a UInt64 address.
    Function, // Opaque function type
}

impl From<&AstType> for BtType {
    fn from(ast_type: &AstType) -> Self {
        match ast_type {
            AstType::Void => BtType::Void,
            AstType::Char | AstType::SignedChar => BtType::Int8,
            AstType::Short => BtType::Int16,
            AstType::Int => BtType::Int32,
            AstType::Long => BtType::Int64,
            AstType::LongLong => BtType::Int64,
            AstType::UnsignedChar => BtType::UInt8,
            AstType::UnsignedShort => BtType::UInt16,
            AstType::UnsignedInt => BtType::UInt32,
            AstType::UnsignedLong => BtType::UInt64,
            AstType::UnsignedLongLong => BtType::UInt64,
            AstType::Float => BtType::Float32,
            AstType::Double | AstType::LongDouble => BtType::Float64,
            AstType::Pointer(_) => BtType::Pointer,
            AstType::Array { element_type: ast_elem_type, count } => {
                let element_type = Box::new(ast_elem_type.as_ref().into());
                BtType::Array { element_type, count: *count }
            }
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
            BtType::Int8 => write!(f, "Int8"),
            BtType::Int16 => write!(f, "Int16"),
            BtType::Int32 => write!(f, "Int32"),
            BtType::Int64 => write!(f, "Int64"),
            BtType::UInt8 => write!(f, "UInt8"),
            BtType::UInt16 => write!(f, "UInt16"),
            BtType::UInt32 => write!(f, "UInt32"),
            BtType::UInt64 => write!(f, "UInt64"),
            BtType::Float32 => write!(f, "Float32"),
            BtType::Float64 => write!(f, "Float64"),
            BtType::Pointer => write!(f, "Pointer"),
            BtType::Array { .. } => write!(f, "Array"),
            BtType::Function => write!(f, "Function"),
        }
    }
}

impl BtType {
    /// Creates a `BtConstantValue` for the current `BtType` with a default zero value.
    pub fn get_const_default_value(&self) -> BtConstantValue {
        match self {
            BtType::Int8 => BtConstantValue::Int8(0),
            BtType::Int16 => BtConstantValue::Int16(0),
            BtType::Int32 => BtConstantValue::Int32(0),
            BtType::Int64 => BtConstantValue::Int64(0),
            BtType::UInt8 => BtConstantValue::UInt8(0),
            BtType::UInt16 => BtConstantValue::UInt16(0),
            BtType::UInt32 => BtConstantValue::UInt32(0),
            BtType::UInt64 => BtConstantValue::UInt64(0),
            BtType::Float32 => BtConstantValue::Float32(0.0),
            BtType::Float64 => BtConstantValue::Float64(0.0),
            BtType::Pointer => BtConstantValue::UInt64(0),
            BtType::Void => ICE!("Cannot create BtConstantValue for BtType::Void"),
            BtType::Array { .. } => ICE!("Cannot get default value for BtType::Array"),
            BtType::Function => ICE!("Cannot get default value for BtType::Function"),
        }
    }

    /// The size of the type in bits.
    pub fn bits(&self) -> usize {
        match self {
            BtType::Void => 0,
            BtType::Int8 | BtType::UInt8 => 8,
            BtType::Int16 | BtType::UInt16 => 16,
            BtType::Int32 | BtType::UInt32 => 32,
            BtType::Int64 | BtType::UInt64 => 64,
            BtType::Float32 => 32,
            BtType::Float64 => 64,
            BtType::Pointer => 64,
            BtType::Array { element_type, count } => element_type.bits() * count,
            BtType::Function => 0,
        }
    }

    /// Is this type an integer (signed or unsigned) type?
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            BtType::Int8
                | BtType::Int16
                | BtType::Int32
                | BtType::Int64
                | BtType::UInt8
                | BtType::UInt16
                | BtType::UInt32
                | BtType::UInt64
        )
    }

    /// Is this type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, BtType::Float32 | BtType::Float64)
    }

    /// Is this type a signed integer?
    pub fn is_signed_integer(&self) -> bool {
        matches!(self, BtType::Int8 | BtType::Int16 | BtType::Int32 | BtType::Int64)
    }

    /// Is this type an unsigned integer?
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(self, BtType::UInt8 | BtType::UInt16 | BtType::UInt32 | BtType::UInt64)
    }

    /// Is this type a character (8-bit integer) type?
    pub fn is_character(&self) -> bool {
        matches!(self, BtType::Int8 | BtType::UInt8)
    }

    /// Is this type a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, BtType::Pointer)
    }

    /// Is this type an array type?
    pub fn is_array(&self) -> bool {
        matches!(self, BtType::Array { .. })
    }

    /// Is this type a character array type?
    pub fn is_character_array(&self) -> bool {
        if let BtType::Array { element_type, .. } = self { element_type.is_character() } else { false }
    }

    /// Is this type a function type?
    pub fn is_function(&self) -> bool {
        matches!(self, BtType::Function)
    }

    /// Gets the type's inner-most scalar type.
    ///
    /// For an aggregate type, recurses into the type until a scalar type is found.
    ///
    /// BtType::Array { BtType::Array { BtType::Int8 } }   --->  BtType::Int8
    pub fn get_innermost_scalar_type(&self) -> &BtType {
        match self {
            BtType::Array { element_type, .. } => element_type.get_innermost_scalar_type(),
            _ => self,
        }
    }

    /// Converts the `BtType` to a string suitable for the IR printer.
    pub fn to_printer(&self) -> String {
        match self {
            BtType::Void => "()".to_string(),
            BtType::Int8 => "i8".to_string(),
            BtType::Int16 => "i16".to_string(),
            BtType::Int32 => "i32".to_string(),
            BtType::Int64 => "i64".to_string(),
            BtType::UInt8 => "u8".to_string(),
            BtType::UInt16 => "u16".to_string(),
            BtType::UInt32 => "u32".to_string(),
            BtType::UInt64 => "u64".to_string(),
            BtType::Float32 => "f32".to_string(),
            BtType::Float64 => "f64".to_string(),
            BtType::Pointer => "ptr".to_string(),
            BtType::Array { element_type, count } => {
                let elem_type_str = element_type.to_printer();
                format!("[{count} x {elem_type_str}]")
            }
            BtType::Function => "fn".to_string(),
        }
    }
}

/// An IR value is either a constant or a variable.
#[derive(Debug, Clone)]
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
