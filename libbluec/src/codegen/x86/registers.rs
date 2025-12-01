// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `registers` module defines the x86_64 hardware registers.

use std::fmt;

/// 64-bit hardware registers and their 32-bit and 8-bit aliases.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
#[expect(unused)]
pub enum HwRegister {
    RAX,
    EAX,
    AX,
    AL,

    RBX,
    EBX,
    BX,
    BL,

    RCX,
    ECX,
    CX,
    CL,

    RDX,
    EDX,
    DX,
    DL,

    RSI,
    ESI,
    SI,
    SIL,

    RDI,
    EDI,
    DI,
    DIL,

    R8,
    R8d,
    R8w,
    R8b,

    R9,
    R9d,
    R9w,
    R9b,

    R10,
    R10d,
    R10w,
    R10b,

    R11,
    R11d,
    R11w,
    R11b,

    RBP,
    RSP,

    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

#[rustfmt::skip]
impl fmt::Display for HwRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HwRegister::RAX   => write!(f, "%rax"),
            HwRegister::EAX   => write!(f, "%eax"),
            HwRegister::AX    => write!(f, "%ax"),
            HwRegister::AL    => write!(f, "%al"),

            HwRegister::RBX   => write!(f, "%rbx"),
            HwRegister::EBX   => write!(f, "%ebx"),
            HwRegister::BX    => write!(f, "%bx"),
            HwRegister::BL    => write!(f, "%bl"),

            HwRegister::RCX   => write!(f, "%rcx"),
            HwRegister::ECX   => write!(f, "%ecx"),
            HwRegister::CX    => write!(f, "%cx"),
            HwRegister::CL    => write!(f, "%cl"),

            HwRegister::RDX   => write!(f, "%rdx"),
            HwRegister::EDX   => write!(f, "%edx"),
            HwRegister::DX    => write!(f, "%dx"),
            HwRegister::DL    => write!(f, "%dl"),

            HwRegister::RSI   => write!(f, "%rsi"),
            HwRegister::ESI   => write!(f, "%esi"),
            HwRegister::SI    => write!(f, "%si"),
            HwRegister::SIL   => write!(f, "%sil"),

            HwRegister::RDI   => write!(f, "%rdi"),
            HwRegister::EDI   => write!(f, "%edi"),
            HwRegister::DI    => write!(f, "%di"),
            HwRegister::DIL   => write!(f, "%dil"),

            HwRegister::R8    => write!(f, "%r8"),
            HwRegister::R8d   => write!(f, "%r8d"),
            HwRegister::R8w   => write!(f, "%r8w"),
            HwRegister::R8b   => write!(f, "%r8b"),

            HwRegister::R9    => write!(f, "%r9"),
            HwRegister::R9d   => write!(f, "%r9d"),
            HwRegister::R9w   => write!(f, "%r9w"),
            HwRegister::R9b   => write!(f, "%r9b"),

            HwRegister::R10   => write!(f, "%r10"),
            HwRegister::R10d  => write!(f, "%r10d"),
            HwRegister::R10w  => write!(f, "%r10w"),
            HwRegister::R10b  => write!(f, "%r10b"),

            HwRegister::R11   => write!(f, "%r11"),
            HwRegister::R11d  => write!(f, "%r11d"),
            HwRegister::R11w  => write!(f, "%r11w"),
            HwRegister::R11b  => write!(f, "%r11b"),

            HwRegister::RBP   => write!(f, "%rbp"),
            HwRegister::RSP   => write!(f, "%rsp"),

            HwRegister::XMM0  => write!(f, "%xmm0"),
            HwRegister::XMM1  => write!(f, "%xmm1"),
            HwRegister::XMM2  => write!(f, "%xmm2"),
            HwRegister::XMM3  => write!(f, "%xmm3"),
            HwRegister::XMM4  => write!(f, "%xmm4"),
            HwRegister::XMM5  => write!(f, "%xmm5"),
            HwRegister::XMM6  => write!(f, "%xmm6"),
            HwRegister::XMM7  => write!(f, "%xmm7"),
            HwRegister::XMM8  => write!(f, "%xmm8"),
            HwRegister::XMM9  => write!(f, "%xmm9"),
            HwRegister::XMM10 => write!(f, "%xmm10"),
            HwRegister::XMM11 => write!(f, "%xmm11"),
            HwRegister::XMM12 => write!(f, "%xmm12"),
            HwRegister::XMM13 => write!(f, "%xmm13"),
            HwRegister::XMM14 => write!(f, "%xmm14"),
            HwRegister::XMM15 => write!(f, "%xmm15"),
        }
    }
}

impl HwRegister {
    /// The size in bits of the register.
    #[rustfmt::skip]
    pub fn size_bits(&self) -> usize {
        match self {
            HwRegister::RAX   => 64,
            HwRegister::EAX   => 32,
            HwRegister::AX    => 16,
            HwRegister::AL    => 8,

            HwRegister::RBX   => 64,
            HwRegister::EBX   => 32,
            HwRegister::BX    => 16,
            HwRegister::BL    => 8,

            HwRegister::RCX   => 64,
            HwRegister::ECX   => 32,
            HwRegister::CX    => 16,
            HwRegister::CL    => 8,

            HwRegister::RDX   => 64,
            HwRegister::EDX   => 32,
            HwRegister::DX    => 16,
            HwRegister::DL    => 8,

            HwRegister::RSI   => 64,
            HwRegister::ESI   => 32,
            HwRegister::SI    => 16,
            HwRegister::SIL   => 8,

            HwRegister::RDI   => 64,
            HwRegister::EDI   => 32,
            HwRegister::DI    => 16,
            HwRegister::DIL   => 8,

            HwRegister::R8    => 64,
            HwRegister::R8d   => 32,
            HwRegister::R8w   => 16,
            HwRegister::R8b   => 8,

            HwRegister::R9    => 64,
            HwRegister::R9d   => 32,
            HwRegister::R9w   => 16,
            HwRegister::R9b   => 8,

            HwRegister::R10   => 64,
            HwRegister::R10d  => 32,
            HwRegister::R10w  => 16,
            HwRegister::R10b  => 8,

            HwRegister::R11   => 64,
            HwRegister::R11d  => 32,
            HwRegister::R11w  => 16,
            HwRegister::R11b  => 8,

            HwRegister::RBP   => 64,
            HwRegister::RSP   => 64,

            HwRegister::XMM0  => 128,
            HwRegister::XMM1  => 128,
            HwRegister::XMM2  => 128,
            HwRegister::XMM3  => 128,
            HwRegister::XMM4  => 128,
            HwRegister::XMM5  => 128,
            HwRegister::XMM6  => 128,
            HwRegister::XMM7  => 128,
            HwRegister::XMM8  => 128,
            HwRegister::XMM9  => 128,
            HwRegister::XMM10 => 128,
            HwRegister::XMM11 => 128,
            HwRegister::XMM12 => 128,
            HwRegister::XMM13 => 128,
            HwRegister::XMM14 => 128,
            HwRegister::XMM15 => 128,
        }
    }

    /// Is this an XMM register?
    pub fn is_xmm(&self) -> bool {
        matches!(
            self,
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
                | HwRegister::XMM15
        )
    }
}
