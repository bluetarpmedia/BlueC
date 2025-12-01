// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast_type` module defines the `AstType` enum and `AstBasicType` type.

use std::fmt;

use super::{AstDeclarator, AstIdentifier, AstStorageClassSpecifier, AstUniqueName};

use crate::ICE;
use crate::lexer::SourceLocation;

/// A basic type, optional storage class specifier, and optional declarator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstDeclaredType {
    /// The basic type of the declaration, from which the optional declarator's derived type is resolved.
    pub basic_type: AstBasicType,

    /// Optional storage class specifier.
    pub storage_class: Option<AstStorageClassSpecifier>,

    /// The optional declarator which augments the basic type (e.g. a pointer). The declarator may be ommitted
    /// in certain contexts, e.g.
    ///
    /// ```c
    /// int calc(int, float, double);   // Declare a function `calc` with parameter types but no names
    /// (float)                         // A cast expression to `float` type.
    /// ```
    pub declarator: Option<AstDeclarator>,

    /// The resolved, canonical `AstType`. The semantic analysis stage fills this out.
    pub resolved_type: Option<AstType>,
}

impl fmt::Display for AstDeclaredType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.basic_type)?;
        if let Some(declarator) = &self.declarator {
            write!(f, " {declarator}")?;
        }
        Ok(())
    }
}

impl AstDeclaredType {
    /// Creates a new, unresolved `AstDeclaredType`.
    pub fn unresolved(
        basic_type: AstBasicType,
        storage_class: Option<AstStorageClassSpecifier>,
        declarator: Option<AstDeclarator>,
    ) -> Self {
        AstDeclaredType { basic_type, storage_class, declarator, resolved_type: None }
    }

    /// Creates a new, resolved `AstDeclaredType`.
    pub fn resolved(ty: &AstType) -> Self {
        AstDeclaredType {
            basic_type: AstBasicType::default(),
            storage_class: None,
            declarator: None,
            resolved_type: Some(ty.clone()),
        }
    }

    /// If the declared type has a declarator, and if that declarator has an identifier, returns that identifier.
    /// Otherwise returns `None`.
    pub fn get_identifier(&self) -> Option<&AstIdentifier> {
        if let Some(declarator) = &self.declarator { declarator.get_identifier() } else { None }
    }

    /// Has the declared type been resolved?
    pub fn is_resolved(&self) -> bool {
        self.resolved_type.is_some()
    }
}

/// The basic type of a declaration.
///
/// The term 'basic type' is not used in the C Standard, but it is used informally when discussing C parsers. The
/// basic type is the first type we see in a declaration before any of the declarators.
///
/// ```c
///  unsigned long int *age = 0, calculate(float salary);
///  ~~~~~~~~~~~~~~~~~
///
///  typedef int MyInt;
///  MyInt a = 0, b = 1, c = 2;
///  ~~~~~
/// ```
///
/// The subsequent declarators (e.g. `*age`, `calculate(float salary)`, `a`, `b`, `c`) augment the basic type to create
/// a derived type. The derived type always modifies the basic type. For example, `*age` augments the basic type
/// `unsigned long int` and creates a derived type of `unsigned long int *`.
///
/// See: [Reading C type declarations](http://unixwiz.net/techtips/reading-cdecl.html)
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AstBasicType(pub Vec<AstBasicTypeSpecifier>);

impl fmt::Display for AstBasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for spec in &self.0 {
            if first {
                write!(f, "{}", spec)?;
            } else {
                write!(f, " {}", spec)?;
            }
            first = false;
        }
        Ok(())
    }
}

/// A basic type specifier.
#[derive(Debug, Clone)]
pub enum AstBasicTypeSpecifier {
    BuiltinType { specifier: String, loc: SourceLocation },
    Alias { alias_name: AstUniqueName, loc: SourceLocation },
}

impl fmt::Display for AstBasicTypeSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstBasicTypeSpecifier::BuiltinType { specifier, .. } => write!(f, "{specifier}"),
            AstBasicTypeSpecifier::Alias { alias_name, .. } => write!(f, "{alias_name}"),
        }
    }
}

impl PartialEq for AstBasicTypeSpecifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                AstBasicTypeSpecifier::BuiltinType { specifier: sp1, .. },
                AstBasicTypeSpecifier::BuiltinType { specifier: sp2, .. },
            ) => sp1 == sp2,
            (
                AstBasicTypeSpecifier::Alias { alias_name: a1, .. },
                AstBasicTypeSpecifier::Alias { alias_name: a2, .. },
            ) => a1 == a2,

            _ => false,
        }
    }
}

impl Eq for AstBasicTypeSpecifier {}

/// The canonical type of an identifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstType {
    Void,
    Short,
    Int,
    Long,
    LongLong,
    UnsignedShort,
    UnsignedInt,
    UnsignedLong,
    UnsignedLongLong,
    Float,
    Double,
    LongDouble, // 'long double' is an alias for 'double' (which is Standard-conforming)
    Pointer(Box<AstType>),
    Function { return_type: Box<AstType>, params: Vec<AstType> },
}

impl AstType {
    /// Creates a new pointer type to the given `AstType`.
    pub fn new_pointer_to(ty: AstType) -> Self {
        AstType::Pointer(Box::new(ty))
    }

    /// Creates a new function type.
    pub fn new_fn(return_type: AstType, params: Vec<AstType>) -> Self {
        AstType::Function { return_type: Box::new(return_type), params }
    }

    /// Is this type a 'basic type'?
    ///
    /// The term 'basic type' is not used in the C Standard, but it is used informally when discussing C parsers. The
    /// basic type is the first type we see in a declaration before any of the declarators.
    ///
    /// ```c
    ///  unsigned long int *age = 0, calculate(float salary);
    ///  ~~~~~~~~~~~~~~~~~
    /// ```
    pub fn is_basic_type(&self) -> bool {
        !matches!(self, AstType::Pointer(_) | AstType::Function { .. })
    }

    /// Is the type a pointer type?
    pub fn is_pointer(&self) -> bool {
        matches!(self, AstType::Pointer(_))
    }

    /// Is the type a function type? (Not a function pointer.)
    pub fn is_function(&self) -> bool {
        matches!(self, AstType::Function { .. })
    }

    /// Is the type an arithmetic type (integer, boolean, or floating-point)?
    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_floating_point()
    }

    /// Is the type an integral type?
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            AstType::Short
                | AstType::UnsignedShort
                | AstType::Int
                | AstType::UnsignedInt
                | AstType::Long
                | AstType::UnsignedLong
                | AstType::LongLong
                | AstType::UnsignedLongLong
        )
    }

    /// Is the type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, AstType::Float | AstType::Double | AstType::LongDouble)
    }

    /// The size of the type in bits.
    pub fn bits(&self) -> usize {
        match self {
            AstType::Void => 0,
            AstType::Short | AstType::UnsignedShort => 16,
            AstType::Int | AstType::UnsignedInt => 32,
            AstType::Long | AstType::UnsignedLong => 64,
            AstType::LongLong | AstType::UnsignedLongLong => 64,
            AstType::Float => 32,
            AstType::Double | AstType::LongDouble => 64,
            AstType::Pointer(_) => 64, // We only support x86_64 for now, but in future will also support Arm64.
            _ => ICE!("Unexpected AstType"),
        }
    }

    /// Is this type a signed integer?
    pub fn is_signed_integer(&self) -> bool {
        matches!(self, AstType::Short | AstType::Int | AstType::Long | AstType::LongLong)
    }

    /// Is this type an unsigned integer?
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            AstType::UnsignedShort | AstType::UnsignedInt | AstType::UnsignedLong | AstType::UnsignedLongLong
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
            AstType::Short | AstType::UnsignedShort => AstType::UnsignedShort,
            AstType::Int | AstType::UnsignedInt => AstType::UnsignedInt,
            AstType::Long | AstType::UnsignedLong => AstType::UnsignedLong,
            AstType::LongLong | AstType::UnsignedLongLong => AstType::UnsignedLongLong,
            _ => ICE!("Cannot make unsigned type for '{self}'"),
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
            AstType::Short => i16::try_from(value).is_ok(),
            AstType::Int => i32::try_from(value).is_ok(),
            AstType::Long | AstType::LongLong => i64::try_from(value).is_ok(),
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
    /// The C standard specifies that smaller integer types should be promoted to 'unsigned int' if their values
    /// cannot fit inside 'int', but in all mainstream implementations (including this one) all smaller integer types,
    /// both signed and unsigned, can fit into 'int'.
    pub fn promote_if_rank_lower_than_int(self) -> Self {
        if !self.is_integer() || self.integer_rank() >= AstType::Int.integer_rank() {
            return self;
        }

        AstType::Int
    }

    /// The rank of the integer type. Types that fit larger values have a higher rank.
    fn integer_rank(&self) -> usize {
        match self {
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
        match self {
            AstType::Void => write!(f, "void"),
            AstType::Short => write!(f, "short"),
            AstType::Int => write!(f, "int"),
            AstType::Long => write!(f, "long"),
            AstType::LongLong => write!(f, "long long"),
            AstType::UnsignedShort => write!(f, "unsigned short"),
            AstType::UnsignedInt => write!(f, "unsigned int"),
            AstType::UnsignedLong => write!(f, "unsigned long"),
            AstType::UnsignedLongLong => write!(f, "unsigned long long"),
            AstType::Float => write!(f, "float"),
            AstType::Double => write!(f, "double"),
            AstType::LongDouble => write!(f, "long double"),

            AstType::Pointer(referenced) => {
                write!(f, "{}*", referenced)
            }

            AstType::Function { return_type, params } => {
                write!(f, "{}(", return_type)?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    first = false;
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}
