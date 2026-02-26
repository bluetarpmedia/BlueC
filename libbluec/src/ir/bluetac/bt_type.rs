// Copyright 2025-2026 Neil Henderson
//
//! The `bt_type` module defines the [BtType] type.

use std::fmt;

use crate::ICE;
use crate::parser::AstType;

use super::BtConstantValue;

/// A data type in the BlueTac IR.
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
            AstType::Bool => BtType::Int8,
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

    /// Is this type an arithmetic type (integer or floating-point)?
    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_floating_point()
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
            BtType::Void => "void".to_string(),
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
