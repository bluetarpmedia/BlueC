// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `ast` module defines the x86_64 Assembly AST for the parent codegen module.

use super::registers::HwRegister;
use crate::internal_error;
use crate::ir::BtType;

use std::fmt;

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
}

impl From<&BtType> for AsmType {
    fn from(bt_type: &BtType) -> Self {
        AsmType::from(*bt_type)
    }
}

impl From<BtType> for AsmType {
    fn from(bt_type: BtType) -> Self {
        match bt_type {
            BtType::Void => internal_error::ICE("No AsmType for BtType::Void"),
            BtType::Int16 | BtType::UInt16 => AsmType::Word,
            BtType::Int32 | BtType::UInt32 => AsmType::DoubleWord,
            BtType::Int64 | BtType::UInt64 | BtType::Pointer => AsmType::QuadWord,
            BtType::Float32 => AsmType::FpSingle,
            BtType::Float64 => AsmType::FpDouble,
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
        }
    }

    /// The natural alignment in bytes that the type requires is the same as its size.
    pub fn alignment_bytes(&self) -> usize {
        self.size_bytes()
    }

    /// Is this type a primitive scalar data type?
    pub fn is_primitive(&self) -> bool {
        matches!(self, AsmType::Byte | AsmType::Word | AsmType::DoubleWord | AsmType::QuadWord)
    }

    /// Is this type a floating-point type?
    pub fn is_floating_point(&self) -> bool {
        matches!(self, AsmType::FpSingle | AsmType::FpDouble)
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
    pub init_value: AsmConstantInitializer,
}

/// A read-only constant.
pub struct AsmConstant {
    pub label: AsmLabelName,
    pub value: AsmConstantInitializer,
}

/// A constant/static initializer value for an `AsmStaticStorageVariable` or `AsmConstant`.
///
/// The assembler doesn't care whether the bits are meant to be interpreted as signed or not, but we track that
/// in order to print a signed or unsigned value in the '.s' file.
///
/// Float32 and 64 values are converted to their bit representation as `Imm32` and `Imm64` unsigned values.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AsmConstantInitializer {
    Imm16 { value: u16, align: usize, signed: bool },
    Imm32 { value: u32, align: usize, signed: bool },
    Imm64 { value: u64, align: usize, signed: bool },
    AddressConstant { object: String }
}

impl From<f64> for AsmConstantInitializer {
    /// Creates an `AsmStaticInitializer` from the given `f64` value.
    fn from(value: f64) -> Self {
        let bits = value.to_bits();
        Self::Imm64 { value: bits, align: 8, signed: false }
    }
}

impl AsmConstantInitializer {
    /// Is the initializer value zero?
    pub fn is_zero(&self) -> bool {
        match self {
            AsmConstantInitializer::Imm16 { value, .. } => *value == 0,
            AsmConstantInitializer::Imm32 { value, .. } => *value == 0,
            AsmConstantInitializer::Imm64 { value, .. } => *value == 0,
            _ => false,
        }
    }

    /// The size in bytes for the type of the initializer value.
    pub fn size_bytes(&self) -> usize {
        match self {
            AsmConstantInitializer::Imm16 { .. } => 2,
            AsmConstantInitializer::Imm32 { .. } => 4,
            AsmConstantInitializer::Imm64 { .. } => 8,
            AsmConstantInitializer::AddressConstant { .. } => 8,
        }
    }

    /// The byte alignment requirement of the initializer value.
    ///
    /// This should be >= its `size_bytes`. For example, in some cases it may be necessary to align
    /// a value to a 16-byte alignment in order to use a certain instruction.
    pub fn alignment_bytes(&self) -> usize {
        let alignment = match self {
            AsmConstantInitializer::Imm16 { align, .. } => *align,
            AsmConstantInitializer::Imm32 { align, .. } => *align,
            AsmConstantInitializer::Imm64 { align, .. } => *align,
            AsmConstantInitializer::AddressConstant { .. } => 8,
        };

        debug_assert!(alignment >= self.size_bytes());
        alignment
    }

    /// Gets initializer value's `AsmType`.
    pub fn asm_type(&self) -> AsmType {
        match self {
            AsmConstantInitializer::Imm16 { .. } => AsmType::Word,
            AsmConstantInitializer::Imm32 { .. } => AsmType::DoubleWord,
            AsmConstantInitializer::Imm64 { .. } => AsmType::QuadWord,
            AsmConstantInitializer::AddressConstant { .. } => AsmType::QuadWord,
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
    Call(String),
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
    /// An immediate value (fits i16, u16, i32, u32, i64 and u64 values)
    Imm(u64),

    /// A hardware register
    Reg(HwRegister),

    /// A pseudo-variable; operands that could live in HW registers, as well as for static-storage duration variables
    Pseudo(String),

    /// A memory operand with base-relative addressing. `address = relative(%base)` in AT&T (or `[base] + relative`)
    Memory { base: HwRegister, relative: i32 },

    /// A global value stored in the Data section.
    Data(String),
}

impl AsmOperand {
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
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::RBX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::BL),
                AsmType::Word => AsmOperand::Reg(HwRegister::BX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EBX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RBX),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::RCX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::CL),
                AsmType::Word => AsmOperand::Reg(HwRegister::CX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::ECX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RCX),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::RDX => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::DL),
                AsmType::Word => AsmOperand::Reg(HwRegister::DX),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EDX),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RDX),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::RSI => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::SIL),
                AsmType::Word => AsmOperand::Reg(HwRegister::SI),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::ESI),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RSI),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::RDI => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::DIL),
                AsmType::Word => AsmOperand::Reg(HwRegister::DI),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::EDI),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::RDI),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::R8 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R8b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R8w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R8d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R8),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::R9 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R9b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R9w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R9d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R9),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::R10 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R10b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R10w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R10d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R10),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
            },

            HwRegister::R11 => match asm_type {
                AsmType::Byte => AsmOperand::Reg(HwRegister::R11b),
                AsmType::Word => AsmOperand::Reg(HwRegister::R11w),
                AsmType::DoubleWord => AsmOperand::Reg(HwRegister::R11d),
                AsmType::QuadWord => AsmOperand::Reg(HwRegister::R11),
                _ => internal_error::ICE(format!("Invalid asm_type: {asm_type}")),
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

            _ => internal_error::ICE(format!("Must pass a 64-bit register, not '{reg}'")),
        }
    }

    /// Is this operand an immediate value?
    pub fn is_immediate(&self) -> bool {
        matches!(self, AsmOperand::Imm(_))
    }

    /// Is this operand a memory address?
    pub fn is_memory_address(&self) -> bool {
        matches!(self, AsmOperand::Memory { .. } | AsmOperand::Data(_))
    }

    /// Is this operand a HW (general or XMM) register?
    pub fn is_hw_register(&self) -> bool {
        matches!(self, AsmOperand::Reg(_))
    }

    /// Is this operand an XMM HW register?
    pub fn is_xmm_hw_register(&self) -> bool {
        if let AsmOperand::Reg(reg) = &self {
            reg.is_xmm()
        } else {
            false
        }
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
