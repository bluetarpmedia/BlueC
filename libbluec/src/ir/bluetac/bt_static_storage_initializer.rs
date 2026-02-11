// Copyright 2025-2026 Neil Henderson
//
//! The `bt_static_storage_initializer` module defines the [BtStaticStorageInitializer] type.

use std::fmt;

use crate::parser::{AstAddressConstant, AstStaticStorageInitializer};

use super::BtConstantValue;

/// An initializer value for a static storage variable.
#[derive(Debug, Clone)]
pub enum BtStaticStorageInitializer {
    Constant(BtConstantValue),
    ZeroBytes(usize),
    String { ascii: Vec<String> },
    AddressOf { object: String, byte_offset: i32 },
}

impl fmt::Display for BtStaticStorageInitializer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BtStaticStorageInitializer::Constant(constant) => write!(f, "{constant}"),
            BtStaticStorageInitializer::ZeroBytes(size) => write!(f, "ZeroBytes({size})"),
            BtStaticStorageInitializer::String { ascii } => {
                let ascii_joined = ascii.join("");
                write!(f, "\"{ascii_joined}\"")
            }
            BtStaticStorageInitializer::AddressOf { object, byte_offset } => {
                if *byte_offset == 0 {
                    write!(f, "address-of @{object}")
                } else if *byte_offset > 0 {
                    write!(f, "address-of @{object} + {byte_offset}")
                } else {
                    write!(f, "address-of @{object} {byte_offset}")
                }
            }
        }
    }
}

impl From<AstStaticStorageInitializer> for BtStaticStorageInitializer {
    fn from(value: AstStaticStorageInitializer) -> Self {
        match value {
            AstStaticStorageInitializer::ZeroBytes(count) => BtStaticStorageInitializer::ZeroBytes(count),

            AstStaticStorageInitializer::Integer(constant_int) => {
                BtStaticStorageInitializer::Constant(constant_int.into())
            }

            AstStaticStorageInitializer::Fp(constant_fp) => BtStaticStorageInitializer::Constant(constant_fp.into()),

            AstStaticStorageInitializer::Pointer(_, address_constant) => match address_constant {
                AstAddressConstant::NullPointer => BtStaticStorageInitializer::Constant(BtConstantValue::UInt64(0)),

                AstAddressConstant::CastExpression(val) => {
                    BtStaticStorageInitializer::Constant(BtConstantValue::UInt64(val))
                }

                AstAddressConstant::AddressOfObject { object, byte_offset } => {
                    BtStaticStorageInitializer::AddressOf { object, byte_offset }
                }

                AstAddressConstant::AddressOfFunction(func) => {
                    BtStaticStorageInitializer::AddressOf { object: func, byte_offset: 0 }
                }
            },

            AstStaticStorageInitializer::String { ascii } => BtStaticStorageInitializer::String { ascii },
        }
    }
}

impl BtStaticStorageInitializer {
    /// Is the constant value the default zero?
    pub fn has_default_value(&self) -> bool {
        match self {
            BtStaticStorageInitializer::Constant(constant_value) => match constant_value {
                BtConstantValue::Int8(value) => *value == 0,
                BtConstantValue::Int16(value) => *value == 0,
                BtConstantValue::Int32(value) => *value == 0,
                BtConstantValue::Int64(value) => *value == 0,
                BtConstantValue::UInt8(value) => *value == 0,
                BtConstantValue::UInt16(value) => *value == 0,
                BtConstantValue::UInt32(value) => *value == 0,
                BtConstantValue::UInt64(value) => *value == 0,
                BtConstantValue::Float32(value) => *value == 0.0,
                BtConstantValue::Float64(value) => *value == 0.0,
            },
            BtStaticStorageInitializer::ZeroBytes(_) => true,
            _ => false,
        }
    }
}
