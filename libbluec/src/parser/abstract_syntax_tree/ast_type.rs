// Copyright 2025-2026 Neil Henderson
//
//! The `ast_type` module defines the [AstType] enum.

use std::fmt;

use crate::ICE;

/// The canonical type of an expression.
///
/// The semantic analysis stage annotates every expression in the parsed AST with its `AstType`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstType {
    Void,
    Char, // Signed in our implementation
    SignedChar,
    Short,
    Int,
    Long,
    LongLong,
    UnsignedChar,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
    Float,
    Double,
    LongDouble, // 'long double' is an alias for 'double' (which is Standard-conforming)
    Pointer(Box<AstType>),
    Array { element_type: Box<AstType>, count: usize },
    Function { return_type: Box<AstType>, params: Vec<AstType> },
}

impl AstType {
    /// Creates a new pointer type to the given `AstType`.
    pub fn new_pointer_to(ty: AstType) -> Self {
        AstType::Pointer(Box::new(ty))
    }

    /// Creates a new array type.
    pub fn new_array(element_type: AstType, count: usize) -> Self {
        AstType::Array { element_type: Box::new(element_type), count }
    }

    /// Creates a new function type.
    pub fn new_fn(return_type: AstType, params: Vec<AstType>) -> Self {
        AstType::Function { return_type: Box::new(return_type), params }
    }

    /// A signed integer type that can hold the value of subtracting one pointer from another.
    /// The underlying type for `ptrdiff_t`.
    pub fn __ptrdiff_t() -> Self {
        AstType::Long
    }

    /// Is this type a 'basic type'?
    ///
    /// The term 'basic type' is not used in the C Standard, but it is used informally when discussing C parsers. The
    /// basic type is the first type we see in a declaration before any of the declarators.
    ///
    /// ```c
    ///  long double salary = 0.0;
    ///  ~~~~~~~~~~~
    ///
    ///  unsigned long int *age = 0, data[4], calculate(float salary);
    ///  ~~~~~~~~~~~~~~~~~
    /// ```
    ///
    /// This function returns true if the `AstType` is not a pointer, array or function type.
    pub fn is_basic_type(&self) -> bool {
        !matches!(self, AstType::Pointer(_) | AstType::Array { .. } | AstType::Function { .. })
    }

    /// Is this type a scalar type?
    pub fn is_scalar(&self) -> bool {
        !self.is_aggregate()
    }

    /// Is this type an aggregate type?
    pub fn is_aggregate(&self) -> bool {
        matches!(self, AstType::Array { .. }) // Future: Struct
    }

    /// Is this type a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, AstType::Pointer(_))
    }

    /// Is this type an array type?
    pub fn is_array(&self) -> bool {
        matches!(self, AstType::Array { .. })
    }

    /// Is this type an array of a character type?
    pub fn is_character_array(&self) -> bool {
        if let AstType::Array { element_type, .. } = self {
            matches!(element_type.as_ref(), AstType::Char | AstType::SignedChar | AstType::UnsignedChar)
        } else {
            false
        }
    }

    /// Is this type a function pointer type?
    ///
    /// A function pointer is a pointer whose referent is a function. In other words, there is only one level of
    /// indirection from the function.
    ///
    /// In C, a function pointer can be called without needing to dereference it first.
    ///
    /// ```c
    /// int get(void);
    ///
    /// int (*a)(void) = get;      // `a` is a function pointer.
    /// int (**b)(void) = &a;      // `b` is _NOT_ a function pointer; it points to the object `a`.
    ///
    /// int v1 = a();              // Can call `a` without dereferencing.
    /// int v2 = (*a)();           // Or can explicitly dereference.
    /// ```
    pub fn is_function_pointer(&self) -> bool {
        if let AstType::Pointer(referent) = self {
            matches!(referent.as_ref(), AstType::Function { .. })
        } else {
            false
        }
    }

    /// Is this type a function type? (Not a function pointer.)
    pub fn is_function(&self) -> bool {
        matches!(self, AstType::Function { .. })
    }

    /// Is this type an arithmetic type (integer, boolean, or floating-point)?
    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_floating_point()
    }

    /// Is this type an integral type?
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            AstType::Char
                | AstType::SignedChar
                | AstType::UnsignedChar
                | AstType::Short
                | AstType::UnsignedShort
                | AstType::Int
                | AstType::UnsignedInt
                | AstType::Long
                | AstType::UnsignedLong
                | AstType::LongLong
                | AstType::UnsignedLongLong
        )
    }

    /// Is this type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, AstType::Float | AstType::Double | AstType::LongDouble)
    }

    /// Is this type a character type?
    pub fn is_character(&self) -> bool {
        matches!(self, AstType::Char | AstType::SignedChar | AstType::UnsignedChar)
    }

    /// Gets the type's inner-most scalar type.
    ///
    /// For an aggregate type, recurses into the type until a scalar type is found.
    ///
    /// AstType::AstType { AstType::AstType { AstType::Char } }   --->  AstType::Char
    pub fn get_innermost_scalar_type(&self) -> &AstType {
        match self {
            AstType::Array { element_type, .. } => element_type.get_innermost_scalar_type(),
            _ => self,
        }
    }

    /// The size of this type in bits.
    pub fn bits(&self) -> usize {
        match self {
            AstType::Void => 0,
            AstType::Char | AstType::SignedChar | AstType::UnsignedChar => 8,
            AstType::Short | AstType::UnsignedShort => 16,
            AstType::Int | AstType::UnsignedInt => 32,
            AstType::Long | AstType::UnsignedLong => 64,
            AstType::LongLong | AstType::UnsignedLongLong => 64,
            AstType::Float => 32,
            AstType::Double | AstType::LongDouble => 64,
            AstType::Pointer(_) => 64, // We only support x86_64 for now, but in future will also support Arm64.
            AstType::Array { element_type, count } => element_type.bits() * count,
            _ => ICE!("Unexpected AstType"),
        }
    }

    /// Is this type a signed integer?
    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            AstType::Char | AstType::SignedChar | AstType::Short | AstType::Int | AstType::Long | AstType::LongLong
        )
    }

    /// Is this type an unsigned integer?
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            AstType::UnsignedChar
                | AstType::UnsignedShort
                | AstType::UnsignedInt
                | AstType::UnsignedLong
                | AstType::UnsignedLongLong
        )
    }

    /// Does this type have the same signedness as the `other` type?
    pub fn same_signedness(&self, other: &AstType) -> bool {
        (self.is_signed_integer() && other.is_signed_integer())
            || (self.is_unsigned_integer() && other.is_unsigned_integer())
    }

    /// Makes an unsigned integer version of the current integer type.
    ///
    /// Precondition: `self.is_integer() == true`
    pub fn to_unsigned(self) -> Self {
        match self {
            AstType::Char | AstType::SignedChar | AstType::UnsignedChar => AstType::UnsignedChar,
            AstType::Short | AstType::UnsignedShort => AstType::UnsignedShort,
            AstType::Int | AstType::UnsignedInt => AstType::UnsignedInt,
            AstType::Long | AstType::UnsignedLong => AstType::UnsignedLong,
            AstType::LongLong | AstType::UnsignedLongLong => AstType::UnsignedLongLong,
            _ => ICE!("Cannot make unsigned type for '{self}'"),
        }
    }

    /// Returns the type's valid range of values as a string slice, if the type has one. Otherwise returns `None`.
    ///
    /// # Examples
    /// ```
    /// # use libbluec::parser::AstType;
    /// assert_eq!(Some("[0, 255]"), AstType::UnsignedChar.valid_range_as_str());
    /// assert_eq!(Some("[-2147483648, 2147483647]"), AstType::Int.valid_range_as_str());
    /// assert_eq!(Some("+/- 3.4028235e38"), AstType::Float.valid_range_as_str());
    /// ```
    #[rustfmt::skip]
    pub fn valid_range_as_str(&self) -> Option<&'static str> {
        match self {
            AstType::Char | AstType::SignedChar               => Some("[-128, 127]"),
            AstType::Short                                    => Some("[-32768, 32767]"),
            AstType::Int                                      => Some("[-2147483648, 2147483647]"),
            AstType::Long | AstType::LongLong                 => Some("[-9223372036854775808, 9223372036854775807]"),
            AstType::UnsignedChar                             => Some("[0, 255]"),
            AstType::UnsignedShort                            => Some("[0, 65535]"),
            AstType::UnsignedInt                              => Some("[0, 4294967295]"),
            AstType::UnsignedLong | AstType::UnsignedLongLong => Some("[0, 18446744073709551615]"),
            AstType::Float                                    => Some("+/- 3.4028235e38"),
            AstType::Double | AstType::LongDouble             => Some("+/- 1.7976931348623157e308"),
            _ => None,
        }
    }

    /// Gets the common `AstType` for the given two types.
    ///
    /// This does not perform any integer promotions, e.g. the common type for `AstType::Short` and `AstType::Char`
    /// is `AstType::Short`.
    pub fn get_common_type(a: &AstType, b: &AstType) -> Self {
        debug_assert!(a != &AstType::Void);
        debug_assert!(b != &AstType::Void);

        if a == b {
            return a.clone();
        }

        // If either type is floating-point then the common type is floating-point.
        if a.is_floating_point() || b.is_floating_point() {
            return AstType::pick_largest_fp_from_types(a, b).clone();
        }

        // If both types have the same signedness then promote to the larger of the two.
        let both_signed = a.is_signed_integer() && b.is_signed_integer();
        let both_unsigned = a.is_unsigned_integer() && b.is_unsigned_integer();
        if both_signed || both_unsigned {
            return AstType::pick_larger_integer_rank(a, b).clone();
        }

        let (signed_type, unsigned_type) = if a.is_signed_integer() { (a, b) } else { (b, a) };

        // If the types have different signedness and their ranks are the same, or the unsigned one is higher, then
        // the common type is the unsigned one.
        if unsigned_type.integer_rank() >= signed_type.integer_rank() {
            return unsigned_type.clone();
        }

        // The remaining cases are when the signed type has the larger rank.
        debug_assert!(signed_type.integer_rank() > unsigned_type.integer_rank());

        // If all the values of the unsigned type can fit in the signed type then the common type is the signed one.
        if unsigned_type.fits_inside(signed_type) {
            return signed_type.clone();
        }

        // Otherwise, the common type is the unsigned version of the signed type.
        signed_type.clone().to_unsigned()
    }

    /// Can all the values of this type fit inside the given `other` type?
    ///
    /// # Examples
    /// ```
    /// # use libbluec::parser::AstType;
    /// assert_eq!(false, AstType::Int.fits_inside(&AstType::UnsignedInt));
    /// assert_eq!(false, AstType::Int.fits_inside(&AstType::Short));
    /// assert_eq!(true, AstType::Int.fits_inside(&AstType::Int));
    /// assert_eq!(true, AstType::Int.fits_inside(&AstType::Long));
    /// assert_eq!(false, AstType::UnsignedInt.fits_inside(&AstType::Int));
    /// assert_eq!(true, AstType::UnsignedInt.fits_inside(&AstType::Long));
    /// assert_eq!(true, AstType::Float.fits_inside(&AstType::Double));
    /// assert_eq!(true, AstType::Double.fits_inside(&AstType::LongDouble));
    /// ```
    pub fn fits_inside(&self, other: &AstType) -> bool {
        if self == other {
            return true;
        }

        match &self {
            t if t.is_signed_integer() => other.is_signed_integer() && other.bits() >= t.bits(),
            t if t.is_unsigned_integer() && other.is_unsigned_integer() => other.bits() >= t.bits(),
            t if t.is_unsigned_integer() && other.is_signed_integer() => other.bits() > t.bits(),
            t if t.is_floating_point() && other.is_floating_point() => other.bits() >= t.bits(),
            _ => false,
        }
    }

    /// Can the given `u64` value fit inside the range of values supported by the current type?
    ///
    /// # Examples
    /// ```
    /// # use libbluec::parser::AstType;
    /// assert_eq!(true, AstType::Short.can_hold_value(30_000_u64));
    /// assert_eq!(false, AstType::Short.can_hold_value(100_000_u64));
    /// ```
    pub fn can_hold_value(&self, value: u64) -> bool {
        match self {
            AstType::Void => false,
            AstType::Char | AstType::SignedChar => i8::try_from(value).is_ok(),
            AstType::Short => i16::try_from(value).is_ok(),
            AstType::Int => i32::try_from(value).is_ok(),
            AstType::Long | AstType::LongLong => i64::try_from(value).is_ok(),
            AstType::UnsignedChar => u8::try_from(value).is_ok(),
            AstType::UnsignedShort => u16::try_from(value).is_ok(),
            AstType::UnsignedInt => u32::try_from(value).is_ok(),
            AstType::UnsignedLong | AstType::UnsignedLongLong => true,
            AstType::Float => {
                // f32 exactly represents all unsigned integers in [0, 2^24]
                value <= (1u64 << 24)
            }
            AstType::Double | AstType::LongDouble => {
                // f64 exactly represents all unsigned integers in [0, 2^53]
                value <= (1u64 << 53)
            }
            _ => ICE!("Invalid type"),
        }
    }

    /// Promotes a small integer type ('_Bool', 'char', or 'short') to `AstType::Int`.
    ///
    /// If the type is an integral type with a rank lower than 'int' then the type is consumed and replaced with
    /// `AstType::Int`. Otherwise, the existing type is returned unchanged.
    ///
    /// Returns a tuple of the type and a boolean value indicating whether or not it was promoted.
    ///
    /// The C standard specifies that smaller integer types should be promoted to 'unsigned int' if their values
    /// cannot fit inside 'int', but in all mainstream implementations (including this one) all smaller integer types,
    /// both signed and unsigned, can fit into 'int'.
    pub fn promote_if_rank_lower_than_int(self) -> (Self, bool) {
        if !self.is_integer() || self.integer_rank() >= AstType::Int.integer_rank() {
            return (self, false);
        }

        (AstType::Int, true)
    }

    /// The rank of the integer type. Types that fit larger values have a higher rank.
    pub fn integer_rank(&self) -> usize {
        match self {
            AstType::Char | AstType::SignedChar | AstType::UnsignedChar => 2,
            AstType::Short | AstType::UnsignedShort => 3,
            AstType::Int | AstType::UnsignedInt => 4,
            AstType::Long | AstType::UnsignedLong => 5,
            AstType::LongLong | AstType::UnsignedLongLong => 6,
            _ => ICE!("Must be an integer type"),
        }
    }

    /// The rank of the floating-point type. Types that fit larger values have a higher rank.
    fn fp_rank(&self) -> usize {
        match self {
            AstType::Float => 3,
            AstType::Double => 4,
            AstType::LongDouble => 5,
            _ => ICE!("Must be a floating-point type"),
        }
    }

    /// Picks the larger of the given integer types based on their rank.
    ///
    /// Both types should be integer types.
    fn pick_larger_integer_rank<'a>(a: &'a AstType, b: &'a AstType) -> &'a AstType {
        if a.integer_rank() > b.integer_rank() { a } else { b }
    }

    /// Picks the larger of the given floating-point types based on their rank.
    ///
    /// Both types should be floating-point types.
    fn pick_larger_fp_rank<'a>(a: &'a AstType, b: &'a AstType) -> &'a AstType {
        if a.fp_rank() > b.fp_rank() { a } else { b }
    }

    /// Picks the largest floating-point type of the two given types.
    ///
    /// At least one of the types must be a floating-point type.
    fn pick_largest_fp_from_types<'a>(a: &'a AstType, b: &'a AstType) -> &'a AstType {
        match (a.is_floating_point(), b.is_floating_point()) {
            (true, true) => {
                if a == b {
                    a
                } else {
                    Self::pick_larger_fp_rank(a, b)
                }
            }
            (true, false) => a,
            (false, true) => b,
            _ => ICE!("At least one of the types should be floating-point"),
        }
    }
}

impl fmt::Display for AstType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", to_c_declarator(self))
    }
}

/// Creates a C declarator string representation of the given `AstType`.
#[rustfmt::skip]
pub fn to_c_declarator(ty: &AstType) -> String {
    to_c_declarator_recursive(ty, String::new())
}

#[rustfmt::skip]
fn to_c_declarator_recursive(ty: &AstType, current: String) -> String {
    // Is the given declarator string a pointer?
    let declarator_is_ptr = |decl: &str| -> bool {
        decl.starts_with('*')
    };

    // Prefixes the given non-empty string with a space, or returns an empty string.
    let prefix_with_space = |s: &str| -> String {
        if s.is_empty() { String::new() } else { format!(" {}", s) }
    };

    match ty {
        // Base cases
        //      If we have a current declarator buffer then append it to the base case with a space in between.
        //      `int <current declarator buffer>`
        //
        AstType::Void             => format!("void{}",               prefix_with_space(&current)),
        AstType::Char             => format!("char{}",               prefix_with_space(&current)),
        AstType::SignedChar       => format!("signed char{}",        prefix_with_space(&current)),
        AstType::Short            => format!("short{}",              prefix_with_space(&current)),
        AstType::Int              => format!("int{}",                prefix_with_space(&current)),
        AstType::Long             => format!("long{}",               prefix_with_space(&current)),
        AstType::LongLong         => format!("long long{}",          prefix_with_space(&current)),
        AstType::UnsignedChar     => format!("unsigned char{}",      prefix_with_space(&current)),
        AstType::UnsignedShort    => format!("unsigned short{}",     prefix_with_space(&current)),
        AstType::UnsignedInt      => format!("unsigned int{}",       prefix_with_space(&current)),
        AstType::UnsignedLong     => format!("unsigned long{}",      prefix_with_space(&current)),
        AstType::UnsignedLongLong => format!("unsigned long long{}", prefix_with_space(&current)),
        AstType::Float            => format!("float{}",              prefix_with_space(&current)),
        AstType::Double           => format!("double{}",             prefix_with_space(&current)),
        AstType::LongDouble       => format!("long double{}",        prefix_with_space(&current)),

        // Pointer: insert a '*' to the left of the current declarator buffer
        AstType::Pointer(referent) => {
            let new_declarator = format!("*{}", current);
            to_c_declarator_recursive(referent, new_declarator)
        }

        // Array: append '[N]' to the right of the current declarator buffer
        AstType::Array { element_type, count } => {
            let array_dim = format!("[{}]", count);
            
            // Remember: '[]' has a higher precedence than pointer '*'.
            //      If the current declarator is a pointer (buffer starts with '*') then wrap the buffer
            //      in parens before appending '[]'.
            //      `int *x[5]`   : x is an array of five pointers to int
            //      `int (*x)[5]` : x is a pointer to an array of five ints
            //
            let new_declarator = if declarator_is_ptr(&current) {
                format!("({}){}", current, array_dim)
            } else {
                format!("{}{}", current, array_dim)
            };

            to_c_declarator_recursive(element_type, new_declarator)
        }

        // Function: append '(params)'
        AstType::Function { return_type, params } => {
            let param_list = if params.is_empty() {
                "(void)".to_string()
            } else {
                let param_list = params.iter()
                    .map(|p| to_c_declarator_recursive(p, String::new()))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({param_list})")
            };

            // Remember: A function call '()' has a higher precedence than pointer '*'.
            //      If the current declarator is a pointer (buffer starts with '*') then wrap the buffer
            //      in parens before appending the params: `(*fn)(params)`
            //
            let new_declarator = if declarator_is_ptr(&current) {
                format!("({current}){param_list}")
            } else {
                format!("{current}{param_list}")
            };

            to_c_declarator_recursive(return_type, new_declarator)
        }
    }
}
