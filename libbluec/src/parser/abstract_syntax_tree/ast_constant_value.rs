// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_constant_value` module defines the `AstConstantValue` type.

use std::fmt;

use crate::ICE;
use crate::parser::{AstType, AstUniqueName};

/// A constant integer value.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstConstantInteger {
    Short(i16),
    Int(i32),
    LongLong(i64),
    UnsignedShort(u16),
    UnsignedInt(u32),
    UnsignedLongLong(u64),
    // We don't implement Long and UnsignedLong.
    //      On System V, they are 64-bit.
    //      On Windows x86_64, they are 32-bit.
    // So for simplicity we just map the Long/UnsignedLong to the relevant type.
}

/// A constant floating-point value.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum AstConstantFp {
    Float(f32),
    Double(f64),
}

/// A constant initializer expression for a pointer type.
#[derive(Debug, Clone, PartialEq)]
pub enum AstConstantPtrInitializer {
    NullPointerConstant,
    CastExpression(u64),
    AddressConstant { object: AstUniqueName },
}

/// A constant integer or floating-point value.
#[derive(Debug, Clone, PartialEq)]
pub enum AstConstantValue {
    Integer(AstConstantInteger),
    Fp(AstConstantFp),
    Pointer(AstType, AstConstantPtrInitializer),
    // Future: StringLiteral
}

impl fmt::Display for AstConstantInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstConstantInteger::Short(value) => write!(f, "{}", value),
            AstConstantInteger::Int(value) => write!(f, "{}", value),
            AstConstantInteger::LongLong(value) => write!(f, "{}", value),
            AstConstantInteger::UnsignedShort(value) => write!(f, "{}", value),
            AstConstantInteger::UnsignedInt(value) => write!(f, "{}", value),
            AstConstantInteger::UnsignedLongLong(value) => write!(f, "{}", value),
        }
    }
}

impl fmt::Display for AstConstantFp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstConstantFp::Float(value) => write!(f, "{}", value),
            AstConstantFp::Double(value) => write!(f, "{}", value),
        }
    }
}

impl fmt::Display for AstConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstConstantValue::Integer(value) => write!(f, "{}", value),
            AstConstantValue::Fp(value) => write!(f, "{}", value),
            AstConstantValue::Pointer(ty, _) => write!(f, "{ty}"),
        }
    }
}

impl AstConstantValue {
    /// Gets the AST type of the constant value.
    pub fn get_ast_type(&self) -> AstType {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Short(_) => AstType::Short,
                AstConstantInteger::Int(_) => AstType::Int,
                AstConstantInteger::LongLong(_) => AstType::LongLong,
                AstConstantInteger::UnsignedShort(_) => AstType::UnsignedShort,
                AstConstantInteger::UnsignedInt(_) => AstType::UnsignedInt,
                AstConstantInteger::UnsignedLongLong(_) => AstType::UnsignedLongLong,
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(_) => AstType::Float,
                AstConstantFp::Double(_) => AstType::Double,
            },
            AstConstantValue::Pointer(ty, _) => ty.clone(),
        }
    }

    /// Is the constant value equal to zero?
    pub fn is_zero(&self) -> bool {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Short(value) => *value == 0,
                AstConstantInteger::Int(value) => *value == 0,
                AstConstantInteger::LongLong(value) => *value == 0,
                AstConstantInteger::UnsignedShort(value) => *value == 0,
                AstConstantInteger::UnsignedInt(value) => *value == 0,
                AstConstantInteger::UnsignedLongLong(value) => *value == 0,
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => *value == 0.0,
                AstConstantFp::Double(value) => *value == 0.0,
            },
            AstConstantValue::Pointer(_, init) => matches!(init, AstConstantPtrInitializer::NullPointerConstant),
        }
    }

    /// Is the constant value below zero? Always false for unsigned types.
    pub fn has_negative_value(&self) -> bool {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Short(value) => *value < 0,
                AstConstantInteger::Int(value) => *value < 0,
                AstConstantInteger::LongLong(value) => *value < 0,
                _ => false,
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => *value < 0.0,
                AstConstantFp::Double(value) => *value < 0.0,
            },
            AstConstantValue::Pointer(..) => false,
        }
    }

    /// Converts the constant value to a different type, performing the appropriate cast for C.
    pub fn cast_to(self, cast_to_type: &AstType) -> Self {
        debug_assert!(cast_to_type.is_arithmetic());

        macro_rules! make_cast {
            ($value:expr) => {
                match cast_to_type {
                    AstType::Short => AstConstantValue::Integer(AstConstantInteger::from_value($value as i16)),
                    AstType::Int => AstConstantValue::Integer(AstConstantInteger::from_value($value as i32)),
                    AstType::Long | AstType::LongLong => {
                        AstConstantValue::Integer(AstConstantInteger::from_value($value as i64))
                    }
                    AstType::UnsignedShort => AstConstantValue::Integer(AstConstantInteger::from_value($value as u16)),
                    AstType::UnsignedInt => AstConstantValue::Integer(AstConstantInteger::from_value($value as u32)),
                    AstType::UnsignedLong | AstType::UnsignedLongLong => {
                        AstConstantValue::Integer(AstConstantInteger::from_value($value as u64))
                    }
                    AstType::Float => AstConstantValue::Fp(AstConstantFp::Float($value as f32)),
                    AstType::Double | AstType::LongDouble => AstConstantValue::Fp(AstConstantFp::Double($value as f64)),
                    _ => {
                        ICE!("Invalid compile-time type cast from {} to {}", self, cast_to_type)
                    }
                }
            };
        }

        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                // No-op from self to same type
                AstConstantInteger::Short(_)
                | AstConstantInteger::Int(_)
                | AstConstantInteger::LongLong(_)
                | AstConstantInteger::UnsignedShort(_)
                | AstConstantInteger::UnsignedInt(_)
                | AstConstantInteger::UnsignedLongLong(_)
                    if *cast_to_type == self.get_ast_type() =>
                {
                    self
                }

                AstConstantInteger::Short(value) => make_cast!(value),
                AstConstantInteger::Int(value) => make_cast!(value),
                AstConstantInteger::LongLong(value) => make_cast!(value),
                AstConstantInteger::UnsignedShort(value) => make_cast!(value),
                AstConstantInteger::UnsignedInt(value) => make_cast!(value),
                AstConstantInteger::UnsignedLongLong(value) => make_cast!(value),
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => make_cast!(value),
                AstConstantFp::Double(value) => make_cast!(value),
            },
            AstConstantValue::Pointer(..) => ICE!("Cannot cast AstConstantValue::Pointer"),
        }
    }
}

macro_rules! impl_from_trait {
    ($( $ty:ty => $variant:ident ),* $(,)?) => {
        $(
            impl From<$ty> for AstConstantInteger {
                fn from(v: $ty) -> Self {
                    AstConstantInteger::$variant(v)
                }
            }
        )*
    };
}

impl_from_trait! {
    i16 => Short,
    i32 => Int,
    i64 => LongLong,
    u16 => UnsignedShort,
    u32 => UnsignedInt,
    u64 => UnsignedLongLong,
}

impl AstConstantInteger {
    /// Makes an `AstConstantInteger` with the appropriate discriminant from the given value.
    pub fn from_value<T: Into<AstConstantInteger>>(value: T) -> Self {
        value.into()
    }

    /// Gets the AST type of the constant integer value.
    pub fn get_ast_type(&self) -> AstType {
        match self {
            AstConstantInteger::Short(_) => AstType::Short,
            AstConstantInteger::Int(_) => AstType::Int,
            AstConstantInteger::LongLong(_) => AstType::LongLong,
            AstConstantInteger::UnsignedShort(_) => AstType::UnsignedShort,
            AstConstantInteger::UnsignedInt(_) => AstType::UnsignedInt,
            AstConstantInteger::UnsignedLongLong(_) => AstType::UnsignedLongLong,
        }
    }
}

impl AstConstantFp {
    /// Gets the AST type of the constant floating-point value.
    pub fn get_ast_type(&self) -> AstType {
        match self {
            AstConstantFp::Float(_) => AstType::Float,
            AstConstantFp::Double(_) => AstType::Double,
        }
    }
}
