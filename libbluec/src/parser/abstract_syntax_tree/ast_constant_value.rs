// Copyright 2025-2026 Neil Henderson
//
//! The `ast_constant_value` module defines [AstConstantValue] and its related types.

use std::fmt;

use crate::ICE;
use crate::parser::AstType;

/// A constant value that was either parsed from a literal or was evaluated from a constant expression.
#[derive(Debug, Clone, PartialEq)]
pub enum AstConstantValue {
    /// A constant integer value.
    Integer(AstConstantInteger),

    // A constant floating-point value.
    Fp(AstConstantFp),

    /// A pointer address constant from evaluating a static storage pointer initializer. See [AstAddressConstant] for
    /// more examples.
    /// ```c
    /// static int *p = 0;
    /// static char *str = "hello";
    /// ```
    Pointer(AstType, AstAddressConstant),

    /// A constant string value from evaluating a static storage character array initializer.
    /// ```c
    /// static char text[4] = "abcd";
    /// ```
    String {
        ascii: Vec<String>,
    },
}

/// A constant integer value.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AstConstantInteger {
    Char(i8),
    Short(i16),
    Int(i32),
    LongLong(i64),
    UnsignedChar(u8),
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

/// An address constant is a null pointer, a pointer to an object of static storage duration, or a pointer to
/// a function designator. A string literal has static storage duration so its address can be used as an address
/// constant.
#[derive(Debug, Clone, PartialEq)]
pub enum AstAddressConstant {
    /// Null pointer constant. `int *p = 0;`.
    NullPointer,

    /// Initialize a pointer by casting from an integer value. `int *p = (int *)123;`.
    CastExpression(u64),

    /// Initialize a pointer with the address of an object of static storage duration.
    /// ```c
    /// static int a = 1;
    /// static int *p = &a;
    ///
    /// static int arr[4] = {1, 2, 3, 4};
    /// static int *p = &arr[2];
    /// static int *p = arr + 2;
    ///
    /// static char *ptr = "Hello";
    /// ```
    AddressOfObject { object: String, byte_offset: i32 },

    /// Initialize a function pointer with the address of a function.
    /// ```c
    /// int get(void) { return 1; }
    /// static int (*fn)(void) = &get;  // or `get` without `&`
    /// ```
    AddressOfFunction(String),
}

impl fmt::Display for AstConstantInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstConstantInteger::Char(value) => write!(f, "{}", value),
            AstConstantInteger::Short(value) => write!(f, "{}", value),
            AstConstantInteger::Int(value) => write!(f, "{}", value),
            AstConstantInteger::LongLong(value) => write!(f, "{}", value),
            AstConstantInteger::UnsignedChar(value) => write!(f, "{}", value),
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
            AstConstantValue::Integer(value) => write!(f, "{value}"),
            AstConstantValue::Fp(value) => write!(f, "{value}"),
            AstConstantValue::Pointer(ty, _) => write!(f, "{ty}"),
            AstConstantValue::String { ascii } => {
                let ascii_joined = ascii.join("");
                write!(f, "String(\"{ascii_joined}\")")
            }
        }
    }
}

impl AstConstantValue {
    /// Gets the AST type of the constant value.
    pub fn get_ast_type(&self) -> AstType {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Char(_) => AstType::Char,
                AstConstantInteger::Short(_) => AstType::Short,
                AstConstantInteger::Int(_) => AstType::Int,
                AstConstantInteger::LongLong(_) => AstType::LongLong,
                AstConstantInteger::UnsignedChar(_) => AstType::UnsignedChar,
                AstConstantInteger::UnsignedShort(_) => AstType::UnsignedShort,
                AstConstantInteger::UnsignedInt(_) => AstType::UnsignedInt,
                AstConstantInteger::UnsignedLongLong(_) => AstType::UnsignedLongLong,
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(_) => AstType::Float,
                AstConstantFp::Double(_) => AstType::Double,
            },
            AstConstantValue::Pointer(ty, _) => ty.clone(),
            AstConstantValue::String { ascii } => AstType::new_array(AstType::Char, ascii.len()),
        }
    }

    /// Is the constant value a pointer address constant?
    pub fn is_pointer_address_constant(&self) -> bool {
        matches!(self, AstConstantValue::Pointer(..))
    }

    /// Is the constant value equal to zero?
    pub fn is_zero(&self) -> bool {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Char(value) => *value == 0,
                AstConstantInteger::Short(value) => *value == 0,
                AstConstantInteger::Int(value) => *value == 0,
                AstConstantInteger::LongLong(value) => *value == 0,
                AstConstantInteger::UnsignedChar(value) => *value == 0,
                AstConstantInteger::UnsignedShort(value) => *value == 0,
                AstConstantInteger::UnsignedInt(value) => *value == 0,
                AstConstantInteger::UnsignedLongLong(value) => *value == 0,
            },
            // For floating-point values we only want to return true for positive zero '0.0', not '-0.0', because the
            // bit representation is different.
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => value.is_sign_positive() && *value == 0.0,
                AstConstantFp::Double(value) => value.is_sign_positive() && *value == 0.0,
            },
            AstConstantValue::Pointer(_, init) => matches!(init, AstAddressConstant::NullPointer),
            AstConstantValue::String { ascii, .. } => ascii.is_empty(),
        }
    }

    /// Is the constant value below zero? Always false for unsigned types.
    pub fn has_negative_value(&self) -> bool {
        match self {
            AstConstantValue::Integer(constant_integer) => match constant_integer {
                AstConstantInteger::Char(value) => *value < 0,
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
            AstConstantValue::String { .. } => false,
        }
    }

    /// Converts the constant value to a different type, performing the appropriate cast for C.
    pub fn cast_to(self, cast_to_type: &AstType) -> Self {
        debug_assert!(cast_to_type.is_arithmetic());

        macro_rules! make_cast {
            ($value:expr) => {
                match cast_to_type {
                    AstType::Char | AstType::SignedChar => {
                        AstConstantValue::Integer(AstConstantInteger::from_value($value as i8))
                    }
                    AstType::Short => AstConstantValue::Integer(AstConstantInteger::from_value($value as i16)),
                    AstType::Int => AstConstantValue::Integer(AstConstantInteger::from_value($value as i32)),
                    AstType::Long | AstType::LongLong => {
                        AstConstantValue::Integer(AstConstantInteger::from_value($value as i64))
                    }
                    AstType::UnsignedChar => AstConstantValue::Integer(AstConstantInteger::from_value($value as u8)),
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
                AstConstantInteger::Char(_)
                | AstConstantInteger::Short(_)
                | AstConstantInteger::Int(_)
                | AstConstantInteger::LongLong(_)
                | AstConstantInteger::UnsignedChar(_)
                | AstConstantInteger::UnsignedShort(_)
                | AstConstantInteger::UnsignedInt(_)
                | AstConstantInteger::UnsignedLongLong(_)
                    if *cast_to_type == self.get_ast_type() =>
                {
                    self
                }

                AstConstantInteger::Char(value) => make_cast!(value),
                AstConstantInteger::Short(value) => make_cast!(value),
                AstConstantInteger::Int(value) => make_cast!(value),
                AstConstantInteger::LongLong(value) => make_cast!(value),
                AstConstantInteger::UnsignedChar(value) => make_cast!(value),
                AstConstantInteger::UnsignedShort(value) => make_cast!(value),
                AstConstantInteger::UnsignedInt(value) => make_cast!(value),
                AstConstantInteger::UnsignedLongLong(value) => make_cast!(value),
            },
            AstConstantValue::Fp(constant_fp) => match constant_fp {
                AstConstantFp::Float(value) => make_cast!(value),
                AstConstantFp::Double(value) => make_cast!(value),
            },
            AstConstantValue::Pointer(..) => ICE!("Cannot cast AstConstantValue::Pointer"),
            AstConstantValue::String { .. } => ICE!("Cannot cast AstConstantValue::String"),
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
    i8 => Char,
    i16 => Short,
    i32 => Int,
    i64 => LongLong,
    u8 => UnsignedChar,
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
            AstConstantInteger::Char(_) => AstType::Char,
            AstConstantInteger::Short(_) => AstType::Short,
            AstConstantInteger::Int(_) => AstType::Int,
            AstConstantInteger::LongLong(_) => AstType::LongLong,
            AstConstantInteger::UnsignedChar(_) => AstType::UnsignedChar,
            AstConstantInteger::UnsignedShort(_) => AstType::UnsignedShort,
            AstConstantInteger::UnsignedInt(_) => AstType::UnsignedInt,
            AstConstantInteger::UnsignedLongLong(_) => AstType::UnsignedLongLong,
        }
    }

    /// Is the constant integer value equal to zero?
    pub fn is_zero(&self) -> bool {
        match self {
            AstConstantInteger::Char(value) => *value == 0,
            AstConstantInteger::Short(value) => *value == 0,
            AstConstantInteger::Int(value) => *value == 0,
            AstConstantInteger::LongLong(value) => *value == 0,
            AstConstantInteger::UnsignedChar(value) => *value == 0,
            AstConstantInteger::UnsignedShort(value) => *value == 0,
            AstConstantInteger::UnsignedInt(value) => *value == 0,
            AstConstantInteger::UnsignedLongLong(value) => *value == 0,
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
