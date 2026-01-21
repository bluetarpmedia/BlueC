// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast` module defines the x86_64 Assembly AST for the parent codegen module.

use std::fmt;

use crate::ICE;
use crate::ir::BtType;
use crate::sema::constant_table::ConstantIndex;

use super::registers::HwRegister;

/// The root of the assembly AST contains a list of definitions.
pub struct AsmRoot(pub Vec<AsmDefinition>);

/// Assembly data types.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AsmType {
    Byte,
    Word,
    DoubleWord, // Aka LongWord
    QuadWord,
    FpSingle,
    FpDouble,
    ByteArray { size: usize, align: usize },
}

impl From<&BtType> for AsmType {
    fn from(bt_type: &BtType) -> Self {
        AsmType::from(bt_type.clone())
    }
}

impl From<BtType> for AsmType {
    fn from(bt_type: BtType) -> Self {
        match bt_type {
            BtType::Int8 | BtType::UInt8 => AsmType::Byte,
            BtType::Int16 | BtType::UInt16 => AsmType::Word,
            BtType::Int32 | BtType::UInt32 => AsmType::DoubleWord,
            BtType::Int64 | BtType::UInt64 | BtType::Pointer => AsmType::QuadWord,
            BtType::Float32 => AsmType::FpSingle,
            BtType::Float64 => AsmType::FpDouble,
            BtType::Array { element_type, count } => {
                // If an array's total size in bytes is >= 16 then its alignment is 16. Otherwise its alignment is
                // the size of the array's scalar element (e.g. for multidimensional arrays).
                //
                let total_size_bytes = (element_type.bits() / 8) * count;

                let align = if total_size_bytes >= 16 {
                    16
                } else {
                    let scalar_type = element_type.get_innermost_scalar_type();
                    scalar_type.bits() / 8
                };

                AsmType::ByteArray { size: total_size_bytes, align }
            }
            BtType::Void => ICE!("No AsmType for BtType::Void"),
            BtType::Function => ICE!("No AsmType for BtType::Function"),
        }
    }
}

impl fmt::Display for AsmType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmType::Byte => write!(f, "Byte"),
            AsmType::Word => write!(f, "Word"),
            AsmType::DoubleWord => write!(f, "DoubleWord"),
            AsmType::QuadWord => write!(f, "QuadWord"),
            AsmType::FpSingle => write!(f, "Single"),
            AsmType::FpDouble => write!(f, "Double"),
            AsmType::ByteArray { size, align } => write!(f, "ByteArray[{size}, align={align}]"),
        }
    }
}

impl AsmType {
    /// The size in bytes of the type.
    pub fn size_bytes(&self) -> usize {
        match self {
            AsmType::Byte => 1,
            AsmType::Word => 2,
            AsmType::DoubleWord => 4,
            AsmType::QuadWord => 8,
            AsmType::FpSingle => 4,
            AsmType::FpDouble => 8,
            AsmType::ByteArray { size, .. } => *size,
        }
    }

    /// The natural alignment in bytes that the type requires is the same as its size.
    pub fn alignment_bytes(&self) -> usize {
        match self {
            AsmType::ByteArray { align, .. } => *align,
            _ => self.size_bytes(),
        }
    }

    /// Is this type a primitive scalar data type?
    pub fn is_primitive(&self) -> bool {
        matches!(self, AsmType::Byte | AsmType::Word | AsmType::DoubleWord | AsmType::QuadWord)
    }

    /// Is this type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, AsmType::FpSingle | AsmType::FpDouble)
    }

    /// Is this type a byte array?
    pub fn is_byte_array(&self) -> bool {
        matches!(self, AsmType::ByteArray { .. })
    }

    /// Returns the string representing the type that is used in an assembly instruction mnemonic.
    pub fn asm_operand_str(&self) -> &'static str {
        match self {
            AsmType::Byte => "b",
            AsmType::Word => "w",
            AsmType::DoubleWord => "l",
            AsmType::QuadWord => "q",
            AsmType::FpSingle => "ss",
            AsmType::FpDouble => "sd",
            AsmType::ByteArray { .. } => ICE!("No operand string for an AsmType::ByteArray"),
        }
    }
}

/// A definition is either a function definition, a variable with static storage duration, or a read-only constant.
pub enum AsmDefinition {
    Function(AsmFunction),
    StaticVariable(AsmStaticStorageVariable),
    StaticConstant(AsmConstant),
}

/// A function definition.
pub struct AsmFunction {
    pub name: String,
    pub is_global: bool,
    pub instructions: Vec<AsmInstruction>,
}

/// A variable with static storage duration.
#[derive(Debug)]
pub struct AsmStaticStorageVariable {
    pub name: String,
    pub is_global: bool,
    pub alignment: usize,
    pub init_value: Vec<AsmConstantInitializer>,
}

/// A read-only constant.
pub struct AsmConstant {
    pub label: AsmLabelName,
    pub alignment: usize,
    pub value: AsmConstantInitializer,
}

impl AsmConstant {
    /// Is this a string constant?
    pub fn is_string(&self) -> bool {
        matches!(self.value, AsmConstantInitializer::AsciiString { .. })
    }
}

/// A constant/static initializer value for an `AsmStaticStorageVariable` or `AsmConstant`.
///
/// The assembler doesn't care whether the bits are meant to be interpreted as signed or not, but we track that
/// in order to print a signed or unsigned value in the '.s' file.
///
/// Float32 and 64 values are converted to their bit representation as `Imm32` and `Imm64` unsigned values.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AsmConstantInitializer {
    ZeroBytes(usize),
    Imm8 { value: u8, signed: bool },
    Imm16 { value: u16, signed: bool },
    Imm32 { value: u32, signed: bool },
    Imm64 { value: u64, signed: bool },
    AddressOf { object: String, byte_offset: i32 },
    AsciiString { value: String, byte_count: usize },
    AsciiStringArray { values: Vec<String> },
}

impl From<f64> for AsmConstantInitializer {
    /// Creates an `AsmStaticInitializer` from the given `f64` value.
    fn from(value: f64) -> Self {
        let bits = value.to_bits();
        Self::Imm64 { value: bits, signed: false }
    }
}

impl AsmConstantInitializer {
    /// Is the initializer value zero?
    pub fn is_zero(&self) -> bool {
        match self {
            AsmConstantInitializer::ZeroBytes(_) => true,
            AsmConstantInitializer::Imm8 { value, .. } => *value == 0,
            AsmConstantInitializer::Imm16 { value, .. } => *value == 0,
            AsmConstantInitializer::Imm32 { value, .. } => *value == 0,
            AsmConstantInitializer::Imm64 { value, .. } => *value == 0,
            _ => false,
        }
    }

    /// The size in bytes for the type of the initializer value.
    pub fn size_bytes(&self) -> usize {
        match self {
            AsmConstantInitializer::ZeroBytes(size) => *size,
            AsmConstantInitializer::Imm8 { .. } => 1,
            AsmConstantInitializer::Imm16 { .. } => 2,
            AsmConstantInitializer::Imm32 { .. } => 4,
            AsmConstantInitializer::Imm64 { .. } => 8,
            AsmConstantInitializer::AddressOf { .. } => 8,
            AsmConstantInitializer::AsciiString { byte_count, .. } => *byte_count,
            AsmConstantInitializer::AsciiStringArray { .. } => {
                ICE!("No size for AsmConstantInitializer::AsciiStringArray")
            }
        }
    }

    /// Gets initializer value's `AsmType`.
    pub fn asm_type(&self) -> AsmType {
        match self {
            AsmConstantInitializer::Imm8 { .. } => AsmType::Byte,
            AsmConstantInitializer::Imm16 { .. } => AsmType::Word,
            AsmConstantInitializer::Imm32 { .. } => AsmType::DoubleWord,
            AsmConstantInitializer::Imm64 { .. } => AsmType::QuadWord,
            AsmConstantInitializer::AddressOf { .. } => AsmType::QuadWord,
            AsmConstantInitializer::ZeroBytes(_) => ICE!("No AsmType for AsmConstantInitializer::ZeroBytes"),
            AsmConstantInitializer::AsciiString { .. } => ICE!("No AsmType for AsmConstantInitializer::AsciiString"),
            AsmConstantInitializer::AsciiStringArray { .. } => {
                ICE!("No AsmType for AsmConstantInitializer::AsciiStringArray")
            }
        }
    }
}

/// A label.
#[derive(Debug, Clone)]
pub struct AsmLabelName(pub String);

impl fmt::Display for AsmLabelName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A constant, represented by an index in the `ConstantTable`.
#[derive(Debug, Copy, Clone)]
pub struct AsmConstantId(pub usize);

impl From<ConstantIndex> for AsmConstantId {
    fn from(value: ConstantIndex) -> Self {
        AsmConstantId(value.0)
    }
}

/// Instructions.
#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Mov { asm_type: AsmType, src: AsmOperand, dst: AsmOperand },

    // Move and sign extend
    MovSx { src_type: AsmType, dst_type: AsmType, src: AsmOperand, dst: AsmOperand },

    // Move and zero extend
    MovZx { src_type: AsmType, dst_type: AsmType, src: AsmOperand, dst: AsmOperand },

    // Load effective address (dst is a QuadWord)
    Lea { src: AsmOperand, dst: AsmOperand },

    // Alias for `cvtss2sd` and `cvtsd2ss`
    // Convert scalar single/double to scalar single/double
    Cvtfp2fp { src_type: AsmType, dst_type: AsmType, src: AsmOperand, dst: AsmOperand },

    // Alias for `cvttss2si` and `cvttsd2si`
    // Convert scalar single/double to signed integer (dword or quad) and always truncate (round toward zero) the fractional part.
    Cvtfp2si { src_type: AsmType, dst_type: AsmType, src: AsmOperand, dst: AsmOperand },

    // Alias for `Cvtsi2ss` and `Cvtsi2sd`
    // Convert signed integer (dword or quad) to scalar double.
    Cvtsi2fp { src_type: AsmType, dst_type: AsmType, src: AsmOperand, dst: AsmOperand },

    Unary { op: AsmUnaryOp, asm_type: AsmType, operand: AsmOperand },
    Binary { op: AsmBinaryOp, asm_type: AsmType, src: AsmOperand, dst: AsmOperand },
    Cmp { asm_type: AsmType, op1: AsmOperand, op2: AsmOperand }, // Calculates `op2 - op1`
    IDiv { asm_type: AsmType, operand: AsmOperand },
    Div { asm_type: AsmType, operand: AsmOperand },
    Cdq { asm_type: AsmType },
    Jmp { target: AsmLabelName },
    JmpCC { cond_code: ConditionalCode, target: AsmLabelName },
    SetCC { cond_code: ConditionalCode, operand: AsmOperand },
    Label { id: AsmLabelName },
    Push(AsmOperand),
    Call(AsmOperand),
    Ret,
}

/// Unary operations
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AsmUnaryOp {
    Neg,
    Not,
    Shr, // Shift right 1
}

/// Binary operations.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Mul,
    DivFp,
    And,
    Or,
    Xor,
    Shl, // Same as Sal
    Shr, // Shift Right (logical), fills leftmost bit with 0.
    Sar, // Shift Arithmetic Right, preserves sign bit
}

/// An instruction operand.
#[derive(Debug, Clone)]
pub enum AsmOperand {
    /// An immediate value (holds signed/unsigned i16, u16, i32, u32, i64 and u64 values)
    Imm { value: u64, bits: usize, signed: bool },

    /// A hardware register
    Reg(HwRegister),

    /// A memory operand with base-relative addressing. AT&T `address = relative(%base)` (Intel `[base] + relative`).
    Memory { base: HwRegister, relative: i32 },

    /// An indexed operand. AT&T `address = (%base, %index, scale)` (or Intel `[base + scale*index]`).
    Indexed { base: HwRegister, index: HwRegister, scale: usize },

    /// A global value stored in the Data or Read-Only Data section that is accessed with RIP-relative addressing.
    ///
    /// AT&T: `address = symbol(%rip)` where `symbol` is a .data or .rodata symbol.
    /// Can have an optional relative offset into the symbol, e.g. `symbol+4(%rip)`.
    Data { symbol: String, relative: i32 },

    /// A function name
    Function(String),

    /// A pseudo-variable for a scalar object, before we've allocated a register/memory location for it.
    Pseudo(String),

    /// A pseudo-variable for an aggregate object, before we've allocated a memory location for it.
    PseudoMemory { name: String, offset: usize },
}

impl AsmOperand {
    /// Creates an Immediate operand from an `i8` value.
    pub fn from_i8(value: i8) -> Self {
        Self::Imm { value: value as u64, bits: 8, signed: true }
    }

    /// Creates an Immediate operand from an `i16` value.
    pub fn from_i16(value: i16) -> Self {
        Self::Imm { value: value as u64, bits: 16, signed: true }
    }

    /// Creates an Immediate operand from an `i32` value.
    pub fn from_i32(value: i32) -> Self {
        Self::Imm { value: value as u64, bits: 32, signed: true }
    }

    /// Creates an Immediate operand from an `i64` value.
    pub fn from_i64(value: i64) -> Self {
        Self::Imm { value: value as u64, bits: 64, signed: true }
    }

    /// Creates an Immediate operand from a `u8` value.
    pub fn from_u8(value: u8) -> Self {
        Self::Imm { value: value as u64, bits: 8, signed: false }
    }

    /// Creates an Immediate operand from a `u16` value.
    pub fn from_u16(value: u16) -> Self {
        Self::Imm { value: value as u64, bits: 16, signed: false }
    }

    /// Creates an Immediate operand from a `u32` value.
    pub fn from_u32(value: u32) -> Self {
        Self::Imm { value: value as u64, bits: 32, signed: false }
    }

    /// Creates an Immediate operand from a `u64` value.
    pub fn from_u64(value: u64) -> Self {
        Self::Imm { value, bits: 64, signed: false }
    }

    /// Creates a stack address memory operand.
    pub fn stack_address(offset_from_rbp: i32) -> Self {
        AsmOperand::Memory { base: HwRegister::RBP, relative: offset_from_rbp }
    }

    /// Creates a register operand for the given 64-bit hardware register to fit the given `AsmType`.
    ///
    /// The type must be `Byte`, `Word`, `DoubleWord`, or `QuadWord`.
    pub fn hw_reg(reg: HwRegister, asm_type: AsmType) -> Self {
        match reg {
            HwRegister::RAX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::AL),
                AsmType::Word => AsmOperand::Reg(HwRegister::AX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EAX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RAX),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::RBX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::BL),
                AsmType::Word => AsmOperand::Reg(HwRegister::BX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EBX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RBX),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::RCX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::CL),
                AsmType::Word => AsmOperand::Reg(HwRegister::CX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::ECX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RCX),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::RDX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::DL),
                AsmType::Word => AsmOperand::Reg(HwRegister::DX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EDX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RDX),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::RSI => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::SIL),
                AsmType::Word => AsmOperand::Reg(HwRegister::SI),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::ESI),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RSI),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::RDI => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::DIL),
                AsmType::Word => AsmOperand::Reg(HwRegister::DI),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EDI),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RDI),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::R8 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R8b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R8w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R8d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R8),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::R9 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R9b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R9w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R9d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R9),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::R10 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R10b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R10w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R10d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R10),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            HwRegister::R11 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R11b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R11w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R11d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R11),
                _ => ICE!("Invalid asm_type: '{asm_type}'"),
            },

            // XMM registers have no aliases
            HwRegister::XMM0
            | HwRegister::XMM1
            | HwRegister::XMM2
            | HwRegister::XMM3
            | HwRegister::XMM4
            | HwRegister::XMM5
            | HwRegister::XMM6
            | HwRegister::XMM7
            | HwRegister::XMM8
            | HwRegister::XMM9
            | HwRegister::XMM10
            | HwRegister::XMM11
            | HwRegister::XMM12
            | HwRegister::XMM13
            | HwRegister::XMM14
            | HwRegister::XMM15 => AsmOperand::Reg(reg),

            _ => ICE!("Must pass a 64-bit register, not '{reg}'"),
        }
    }

    /// Is this operand an immediate value?
    pub fn is_immediate(&self) -> bool {
        matches!(self, AsmOperand::Imm { .. })
    }

    /// Is this operand a memory address?
    pub fn is_memory_address(&self) -> bool {
        matches!(
            self,
            AsmOperand::Memory { .. } | AsmOperand::Indexed { .. } | AsmOperand::Data { .. } | AsmOperand::Function(_)
        )
    }

    /// Is this a data operand? (RIP-relative addressing.)
    pub fn is_data_operand(&self) -> bool {
        matches!(self, AsmOperand::Data { .. })
    }

    /// Is this operand a HW (general or XMM) register?
    pub fn is_hw_register(&self) -> bool {
        matches!(self, AsmOperand::Reg(_))
    }

    /// Is this operand an XMM HW register?
    pub fn is_xmm_hw_register(&self) -> bool {
        if let AsmOperand::Reg(reg) = &self { reg.is_xmm() } else { false }
    }
}

/// Conditional codes for `JmpCC` and `SetCC` instructions.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ConditionalCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
    A,
    AE,
    B,
    BE,
    P, // Is Parity Flag set, e.g. true for quiet NaNs
}
