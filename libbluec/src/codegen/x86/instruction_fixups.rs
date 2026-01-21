// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `instruction_fixups` module contains the functionality to rewrite instructions based on their semantic
//! requirements.
//! R10, R11, XMM14 and XMM15 are used as temp registers for fixups and rewrites.

use crate::ICE;

use super::ast::{AsmBinaryOp, AsmFunction, AsmInstruction, AsmOperand, AsmType};
use super::registers::HwRegister;

/// Rewrites instructions based on their semantic requirements.
pub fn rewrite_instructions(function: &mut AsmFunction) {
    // First step is to rewrite specific instructions that have certain operand requirements.
    //
    //      We use `R11` for instruction-specific fixups and we use `R10` when we need to MOV an operand into
    //      a temporary register.
    //      For floating-point instructions, we use XMM14 if the source operand needs a temp register, and
    //      XMM15 if the dest operand needs a temp register.

    // Take ownership of the function's instructions, since we'll almost certainly have to rewrite some of them.
    let src_instructions = std::mem::take(&mut function.instructions);

    let mut out = Vec::with_capacity(src_instructions.len());

    for instr in src_instructions {
        match instr {
            AsmInstruction::Mov { ref asm_type, ref src, ref dst, .. }
                if !asm_type.is_floating_point() && operand_is_imm64(src) && dst.is_memory_address() =>
            {
                out.extend(fixup_mov_quad_immediate_to_address(instr))
            }

            AsmInstruction::MovSx { .. } => out.extend(fixup_movsx(instr)),

            AsmInstruction::MovZx { .. } => out.extend(fixup_movzx(instr)),

            AsmInstruction::Lea { .. } => out.extend(fixup_lea(instr)),

            AsmInstruction::Cvtfp2si { .. } => out.extend(fixup_cvtfp2si(instr)),

            AsmInstruction::Cvtsi2fp { .. } => out.extend(fixup_cvtsi2fp(instr)),

            AsmInstruction::Cmp { .. } => out.extend(fixup_cmp(instr)),

            AsmInstruction::Push(ref op) if operand_is_imm64(op) || op.is_xmm_hw_register() => {
                out.extend(fixup_push(instr))
            }

            AsmInstruction::IDiv { ref operand, .. } if operand.is_immediate() => {
                let is_signed = true;
                out.extend(fixup_division_immediate(is_signed, instr))
            }

            AsmInstruction::Div { ref operand, .. } if operand.is_immediate() => {
                let is_signed = false;
                out.extend(fixup_division_immediate(is_signed, instr))
            }

            AsmInstruction::Binary { op: AsmBinaryOp::Shl | AsmBinaryOp::Shr | AsmBinaryOp::Sar, ref src, .. }
                if !src.is_immediate() =>
            {
                out.extend(fixup_shift_count(instr))
            }

            AsmInstruction::Binary {
                asm_type: AsmType::FpSingle | AsmType::FpDouble,
                op: AsmBinaryOp::Add | AsmBinaryOp::Sub | AsmBinaryOp::Mul | AsmBinaryOp::DivFp | AsmBinaryOp::Xor,
                ..
            } => out.extend(fixup_fp_binary_operation(instr)),

            AsmInstruction::Binary {
                op:
                    AsmBinaryOp::Add
                    | AsmBinaryOp::Sub
                    | AsmBinaryOp::Mul
                    | AsmBinaryOp::And
                    | AsmBinaryOp::Or
                    | AsmBinaryOp::Xor,
                ..
            } => out.extend(fixup_integer_binary_operation(instr)),

            _ => {
                out.push(instr.clone());
            }
        }
    }

    // As a final step, fixup instructions with 2 operands that are both memory addresses by moving the first operand
    // into a temporary register.
    //
    function.instructions = fixup_instructions_with_two_operand_memory_address(out);
}

macro_rules! fixup_movsz_movzx {
    ($movx:ident, $instr:ident) => {{
        let mut out = Vec::with_capacity(3); // At most we'll need 3 instructions

        let AsmInstruction::$movx { src_type, dst_type, src, dst } = $instr else {
            ICE!("Expected $movx instruction");
        };

        // Rewrite the MovSx/Zx to a straight Mov if the src and dst have the same types
        if src_type == dst_type {
            out.push(AsmInstruction::Mov { asm_type: src_type, src, dst });
            return out;
        }

        // Safe to use both `R10` and `R11` here because `fixup_instructions_with_two_operand_memory_address` doesn't
        // handle MovSx/MovZx. We use `R10` for the MOV into a temp register.
        let tmp_src = AsmOperand::hw_reg(HwRegister::R10, src_type);
        let tmp_dst = AsmOperand::hw_reg(HwRegister::R11, dst_type);

        if src.is_immediate() && !dst.is_hw_register() {
            out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: tmp_src.clone() });
            out.push(AsmInstruction::$movx { src_type, dst_type, src: tmp_src, dst: tmp_dst.clone() });
            out.push(AsmInstruction::Mov { asm_type: dst_type, src: tmp_dst, dst });
        } else if src.is_immediate() {
            out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: tmp_src.clone() });
            out.push(AsmInstruction::$movx { src_type, dst_type, src: tmp_src, dst });
        } else if !dst.is_hw_register() {
            out.push(AsmInstruction::$movx { src_type, dst_type, src, dst: tmp_dst.clone() });
            out.push(AsmInstruction::Mov { asm_type: dst_type, src: tmp_dst, dst });
        } else {
            out.push(AsmInstruction::$movx { src_type, dst_type, src, dst }); // Keep original
        }

        out
    }};
}

/// `movsx`: Move with sign-extension
///
/// Constraints
/// - The src operand cannot be an immediate
/// - The dst operand must be a register
fn fixup_movsx(instr: AsmInstruction) -> Vec<AsmInstruction> {
    fixup_movsz_movzx!(MovSx, instr)
}

/// `movzx`: Move with zero-extend
///
/// Constraints
/// - The src operand cannot be an immediate, and must be a Byte or Word
/// - The dst operand must be a register
fn fixup_movzx(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let AsmInstruction::MovZx { src_type, dst_type, .. } = &instr else {
        ICE!("Expected MovZx instruction");
    };

    // `MOVZX` only handles Byte or Word source operands, so if we have a DoubleWord source then we transform
    // the operation into a `MOV`, since that clears out the upper 32-bits of the dest QuadWord for us.
    //
    if *src_type == AsmType::DoubleWord && dst_type != src_type {
        let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

        let AsmInstruction::MovZx { src_type, dst_type, src, dst } = instr else {
            ICE!("Expected MovZx instruction");
        };

        debug_assert!(dst_type == AsmType::QuadWord);

        if let AsmOperand::Reg(register) = dst {
            // Make the dest register operand the same size as the DoubleWord source (e.g. `movl src, %eax`).
            let tmp_reg = AsmOperand::hw_reg(register, src_type);
            out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: tmp_reg });
        } else {
            // Move the DoubleWord into R11d, which clears the upper 32-bits of R11, and then move R11 into dst.
            let r11d = AsmOperand::Reg(HwRegister::R11d);
            let r11q = AsmOperand::Reg(HwRegister::R11);
            out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: r11d });
            out.push(AsmInstruction::Mov { asm_type: dst_type, src: r11q, dst });
        }

        out
    }
    // The `MOVZX` instruction cannot use an immediate as `src` or a memory address as `dst`.
    //
    else {
        fixup_movsz_movzx!(MovZx, instr)
    }
}

/// `lea`: Load effective address
///
/// Constraints
/// - The src operand must be a memory address (we only assert this; the generator must provide this)
/// - The dst operand must be a register
fn fixup_lea(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let AsmInstruction::Lea { src, dst } = instr else {
        ICE!("Expected Lea instruction");
    };

    let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

    if let AsmOperand::Reg(reg) = dst {
        debug_assert!(reg.size_bits() == 64);
        out.push(AsmInstruction::Lea { src, dst });
    } else {
        let r11 = AsmOperand::Reg(HwRegister::R11);
        out.push(AsmInstruction::Lea { src, dst: r11.clone() });
        out.push(AsmInstruction::Mov { asm_type: AsmType::QuadWord, src: r11, dst });
    }

    out
}

/// `cvttss2si` and `cvttsd2si`: convert scalar single/double to signed integer (with truncation, round towards zero).
///
/// Constraints:
/// - Dest operand must be a register
fn fixup_cvtfp2si(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

    let AsmInstruction::Cvtfp2si { src_type, dst_type, src, dst } = instr else {
        ICE!("Expected Cvtfp2si instruction");
    };

    if dst.is_hw_register() {
        out.push(AsmInstruction::Cvtfp2si { src_type, dst_type, src, dst });
    } else {
        let tmp_dst = AsmOperand::hw_reg(HwRegister::R11, dst_type);
        out.push(AsmInstruction::Cvtfp2si { src_type, dst_type, src, dst: tmp_dst.clone() });
        out.push(AsmInstruction::Mov { asm_type: dst_type, src: tmp_dst, dst });
    }

    out
}

/// `cvtsi2ss` and `cvtsi2sd`: convert signed integer to scalar single/double
///
/// Constraints:
/// - Source operand cannot be an immediate
/// - Dest operand must be a register
fn fixup_cvtsi2fp(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(3); // At most we'll need 3 instructions

    let AsmInstruction::Cvtsi2fp { src_type, dst_type, src, dst } = instr else {
        ICE!("Expected Cvtsi2fp instruction");
    };

    let tmp_src = AsmOperand::hw_reg(HwRegister::R10, src_type);
    let tmp_dst = AsmOperand::Reg(HwRegister::XMM15);

    if src.is_immediate() && dst.is_hw_register() {
        out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: tmp_src.clone() });
        out.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src: tmp_src, dst });
    } else if src.is_immediate() && !dst.is_hw_register() {
        out.push(AsmInstruction::Mov { asm_type: src_type, src, dst: tmp_src.clone() });
        out.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src: tmp_src, dst: tmp_dst.clone() });
        out.push(AsmInstruction::Mov { asm_type: dst_type, src: tmp_dst, dst });
    } else if !dst.is_hw_register() {
        out.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src, dst: tmp_dst.clone() });
        out.push(AsmInstruction::Mov { asm_type: dst_type, src: tmp_dst, dst });
    } else {
        out.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src, dst });
    }

    out
}

/// `mov`
///
/// Constraints
/// - If source operand is an immediate, it must fit inside a DoubleWord
///
/// The `mov` instruction supports moving an immediate to a memory address but the immediate must fit inside a
/// DoubleWord. Otherwise, we need to move the immediate into a temp register and then MOV the temp register to
/// the memory address.
fn fixup_mov_quad_immediate_to_address(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

    let AsmInstruction::Mov { asm_type, src, dst } = instr else {
        ICE!("Expected Mov instruction");
    };

    // If `src` is an immediate and doesn't fit in a DoubleWord then copy it to `R10`.
    let src = if operand_is_imm64(&src) {
        let r10_operand = AsmOperand::hw_reg(HwRegister::R10, asm_type);
        out.push(AsmInstruction::Mov { asm_type, src, dst: r10_operand.clone() });
        r10_operand
    } else {
        src
    };

    out.push(AsmInstruction::Mov { asm_type, src, dst });

    out
}

/// `cmp`
///
/// Constraints (in AT&T syntax)
/// - See `fixup_cmp_immediate` and `fixup_cmp_fp`
fn fixup_cmp(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let AsmInstruction::Cmp { asm_type, op1, op2 } = &instr else {
        ICE!("Expected Cmp instruction");
    };

    if asm_type.is_floating_point() {
        let valid_operands = (op1.is_memory_address() || op1.is_hw_register()) && op2.is_hw_register();

        if valid_operands { vec![instr.clone()] } else { fixup_cmp_fp(instr) }
    } else if op1.is_immediate() || op2.is_immediate() {
        fixup_cmp_immediate(instr)
    } else {
        vec![instr.clone()]
    }
}

/// `cmp` for integers.
///
/// Constraints (in AT&T syntax)
/// - If the first operand is an immediate, it must fit inside a DoubleWord ('int' / 'unsigned int')
/// - The second operand cannot be an immediate
///
/// E.g. in AT&T syntax: `cmp $5, %rax` is fine, but `cmp %rax, $5` is not.
fn fixup_cmp_immediate(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(3); // At most we'll need 3 instructions

    let AsmInstruction::Cmp { asm_type, op1, op2 } = instr else {
        ICE!("Expected Cmp instruction");
    };

    debug_assert!(op1.is_immediate() || op2.is_immediate());

    // If operand1 is an immediate and doesn't fit in a DoubleWord then copy it to `R10`.
    let op1 = if operand_is_imm64(&op1) {
        let r10_operand = AsmOperand::hw_reg(HwRegister::R10, asm_type);
        out.push(AsmInstruction::Mov { asm_type, src: op1, dst: r10_operand.clone() });
        r10_operand
    } else {
        op1
    };

    // If operand2 is an immediate then copy it to `R11`.
    let op2 = if op2.is_immediate() {
        let r11_operand = AsmOperand::hw_reg(HwRegister::R11, asm_type);
        out.push(AsmInstruction::Mov { asm_type, src: op2, dst: r11_operand.clone() });
        r11_operand
    } else {
        op2
    };

    out.push(AsmInstruction::Cmp { asm_type, op1, op2 });

    out
}

/// `cmp` for floating-point.
///
/// Constraints (in AT&T syntax)
/// - The first operand must be a register or memory address.
/// - The second operand must be a register.
fn fixup_cmp_fp(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(3); // At most we'll need 3 instructions

    let AsmInstruction::Cmp { asm_type, op1, op2 } = instr else {
        ICE!("Expected Cmp instruction");
    };

    let valid_op1 = op1.is_hw_register() || op1.is_memory_address();
    let valid_op2 = op2.is_hw_register();

    let op1 = if valid_op1 {
        op1
    } else {
        let xmm14 = AsmOperand::Reg(HwRegister::XMM14);
        out.push(AsmInstruction::Mov { asm_type, src: op1, dst: xmm14.clone() });
        xmm14
    };

    let op2 = if valid_op2 {
        op2
    } else {
        let xmm15 = AsmOperand::Reg(HwRegister::XMM15);
        out.push(AsmInstruction::Mov { asm_type, src: op2, dst: xmm15.clone() });
        xmm15
    };

    out.push(AsmInstruction::Cmp { asm_type, op1, op2 });

    out
}

/// `push`
///
/// Constraints
/// - If the operand is an immediate, it must fit inside a DoubleWord ('int' / 'unsigned int')
/// - Cannot push an XMM register
fn fixup_push(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

    let AsmInstruction::Push(operand) = instr else {
        ICE!("Expected Cmp instruction");
    };

    if operand_is_imm64(&operand) {
        let r10_operand = AsmOperand::hw_reg(HwRegister::R10, AsmType::QuadWord);
        out.push(AsmInstruction::Mov { asm_type: AsmType::QuadWord, src: operand, dst: r10_operand.clone() });
        out.push(AsmInstruction::Push(r10_operand));
    } else if operand.is_xmm_hw_register() {
        // subq 8, %rsp
        out.push(AsmInstruction::Binary {
            op: AsmBinaryOp::Sub,
            asm_type: AsmType::QuadWord,
            src: AsmOperand::from_u64(8),
            dst: AsmOperand::hw_reg(HwRegister::RSP, AsmType::QuadWord),
        });
        // movsd xmm, (%rsp)  ; Copy the low 64 bits from the xmm register onto the stack
        out.push(AsmInstruction::Mov {
            asm_type: AsmType::FpDouble,
            src: operand,
            dst: AsmOperand::Memory { base: HwRegister::RSP, relative: 0 },
        });
    } else {
        out.push(AsmInstruction::Push(operand));
    }

    out
}

/// `idiv` and `div`: Signed and unsigned integer division
///
/// Constraints
/// - The operand cannot be an immediate
fn fixup_division_immediate(is_signed: bool, instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(2); // At most we'll need 2 instructions

    let (asm_type, operand) = if is_signed {
        let AsmInstruction::IDiv { asm_type, operand } = instr else {
            ICE!("Expected IDiv instruction");
        };
        (asm_type, operand)
    } else {
        let AsmInstruction::Div { asm_type, operand } = instr else {
            ICE!("Expected Div instruction");
        };
        (asm_type, operand)
    };

    debug_assert!(operand.is_immediate());

    // We use `R10` for the MOV into a temp registger.
    let r10_operand = AsmOperand::hw_reg(HwRegister::R10, asm_type);

    out.push(AsmInstruction::Mov { asm_type, src: operand, dst: r10_operand.clone() });

    if is_signed {
        out.push(AsmInstruction::IDiv { asm_type, operand: r10_operand });
    } else {
        out.push(AsmInstruction::Div { asm_type, operand: r10_operand });
    }

    out
}

/// `sal`, `shl`, `sar`, `shr`: Shift left/right
///
/// Constraints
/// - The 'shift count' operand must be an immediate value, or stored in `%cl` (lowest 8-bits of `%rcx`).
fn fixup_shift_count(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(2); // We need 2 instructions

    let AsmInstruction::Binary { op, asm_type, src, dst } = instr else {
        ICE!("Expected Binary instruction");
    };

    debug_assert!(is_shift_left_or_right(&op));

    let rcx_operand = AsmOperand::hw_reg(HwRegister::RCX, asm_type);
    let cl_reg = AsmOperand::Reg(HwRegister::CL);

    out.push(AsmInstruction::Mov { asm_type, src, dst: rcx_operand });
    out.push(AsmInstruction::Binary { op, asm_type, src: cl_reg, dst });

    out
}

/// Floating-point binary operations
///
/// Constraints
/// - Destination operand of add,sub,mul,div,xor [sd/ss] must be a register.
fn fixup_fp_binary_operation(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(3); // At most we'll need 3 instructions

    let AsmInstruction::Binary { op, asm_type, src, dst } = instr else {
        ICE!("Expected Binary instruction");
    };

    assert!(asm_type.is_floating_point());

    if dst.is_hw_register() {
        out.push(AsmInstruction::Binary { op, asm_type, src, dst });
    } else {
        let tmp_dst = AsmOperand::Reg(HwRegister::XMM15);

        out.push(AsmInstruction::Mov { asm_type, src: dst.clone(), dst: tmp_dst.clone() });
        out.push(AsmInstruction::Binary { op, asm_type, src, dst: tmp_dst.clone() });
        out.push(AsmInstruction::Mov { asm_type, src: tmp_dst, dst });
    }

    out
}

/// Integer binary operations
///
/// Constraints
/// - If the source operand is an immediate, it must fit inside a DoubleWord ('int' / 'unsigned int').
/// - `imul`'s destination operand must be a register.
fn fixup_integer_binary_operation(instr: AsmInstruction) -> Vec<AsmInstruction> {
    let mut out = Vec::with_capacity(4); // At most we'll need 4 instructions 

    let AsmInstruction::Binary { op, asm_type, src, dst } = instr else {
        ICE!("Expected Binary instruction");
    };

    assert!(!asm_type.is_floating_point());

    // If `src` is an immediate and doesn't fit in a DoubleWord then copy it to `R10`.
    let src = if operand_is_imm64(&src) {
        let r10_operand = AsmOperand::hw_reg(HwRegister::R10, asm_type);
        out.push(AsmInstruction::Mov { asm_type, src, dst: r10_operand.clone() });
        r10_operand
    } else {
        src
    };

    // The dest operand for `IMUL` must be a register.
    //
    if operator_dst_must_be_register(&op) && !dst.is_hw_register() {
        let r11_operand = AsmOperand::hw_reg(HwRegister::R11, asm_type);

        out.push(AsmInstruction::Mov { asm_type, src: dst.clone(), dst: r11_operand.clone() });
        out.push(AsmInstruction::Binary { op, asm_type, src, dst: r11_operand.clone() });
        out.push(AsmInstruction::Mov { asm_type, src: r11_operand, dst });
    } else {
        out.push(AsmInstruction::Binary { op, asm_type, src, dst });
    }

    out
}

fn fixup_instructions_with_two_operand_memory_address(instructions: Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    // We use the `R10` or `XMM14` register for all temporary moves when rewriting instructions with two operands.

    // Creates an instruction to MOV (copy) an operand to the temp register, and returns the temp register operand
    // and the instruction as a tuple.
    //
    let mov_operand_to_tmp = |asm_type: &AsmType, operand: AsmOperand| -> (AsmOperand, AsmInstruction) {
        let hw_reg = if asm_type.is_floating_point() { HwRegister::XMM14 } else { HwRegister::R10 };
        let tmp_operand = AsmOperand::hw_reg(hw_reg, *asm_type);

        let mov = AsmInstruction::Mov { asm_type: *asm_type, src: operand, dst: tmp_operand.clone() };

        (tmp_operand, mov)
    };

    // For each instructions with two operands that are both memory addresses, move the first operand into the temp
    // register and replace the original instruction with the temp register as its new first operand.
    //
    //      Note: `MovSx` and `MovZx` are handled above, because their `dst` cannot be a memory address.
    //
    let mut out = Vec::with_capacity(instructions.len());

    for instr in instructions {
        match instr {
            // Copy a byte array from RIP-relative data address
            AsmInstruction::Mov { asm_type, src, dst } if src.is_data_operand() && asm_type.is_byte_array() => {
                out.extend(copy_byte_array(asm_type, src, dst));
            }

            AsmInstruction::Mov { asm_type, src, dst } if both_operands_are_memory_addresses(&src, &dst) => {
                let (tmp_operand, mov_to_tmp) = mov_operand_to_tmp(&asm_type, src);
                out.push(mov_to_tmp);
                out.push(AsmInstruction::Mov { asm_type, src: tmp_operand, dst });
            }

            AsmInstruction::Cmp { asm_type, op1, op2 } if both_operands_are_memory_addresses(&op1, &op2) => {
                let (tmp_operand, mov_to_tmp) = mov_operand_to_tmp(&asm_type, op1);
                out.push(mov_to_tmp);
                out.push(AsmInstruction::Cmp { asm_type, op1: tmp_operand, op2 });
            }

            AsmInstruction::Binary { op, asm_type, src, dst } if both_operands_are_memory_addresses(&src, &dst) => {
                let (tmp_operand, mov_to_tmp) = mov_operand_to_tmp(&asm_type, src);
                out.push(mov_to_tmp);
                out.push(AsmInstruction::Binary { op, asm_type, src: tmp_operand, dst });
            }

            _ => {
                out.push(instr);
            }
        }
    }

    out
}

fn copy_byte_array(array_type: AsmType, src: AsmOperand, dst: AsmOperand) -> Vec<AsmInstruction> {
    let AsmType::ByteArray { size, .. } = array_type else {
        ICE!("Expected AsmType::ByteArray");
    };

    let AsmOperand::Data { symbol, .. } = src else {
        ICE!("Expected AsmOperand::Data for src");
    };

    let AsmOperand::Memory { base, relative: mut dst_relative } = dst else {
        ICE!("Expected AsmOperand::Memory for dst");
    };

    let mut out = Vec::with_capacity(10);

    let mut remaining_bytes = size;
    let mut src_relative = 0;

    while remaining_bytes > 0 {
        let bytes_to_copy = if remaining_bytes >= 8 {
            8
        } else if remaining_bytes >= 4 {
            4
        } else if remaining_bytes >= 2 {
            2
        } else {
            1
        };

        let asm_type = match bytes_to_copy {
            1 => AsmType::Byte,
            2 => AsmType::Word,
            4 => AsmType::DoubleWord,
            8 => AsmType::QuadWord,
            _ => ICE!("Invalid byte count"),
        };

        let src = AsmOperand::Data { symbol: symbol.clone(), relative: src_relative };
        let rax_reg = AsmOperand::hw_reg(HwRegister::RAX, asm_type);
        out.push(AsmInstruction::Mov { asm_type, src, dst: rax_reg.clone() });

        let dst = AsmOperand::Memory { base, relative: dst_relative };
        out.push(AsmInstruction::Mov { asm_type, src: rax_reg.clone(), dst });

        src_relative += bytes_to_copy as i32;
        dst_relative += bytes_to_copy as i32;
        remaining_bytes -= bytes_to_copy;
    }

    out
}

fn is_shift_left_or_right(op: &AsmBinaryOp) -> bool {
    matches!(op, AsmBinaryOp::Shl | AsmBinaryOp::Shr | AsmBinaryOp::Sar)
}

fn operator_dst_must_be_register(op: &AsmBinaryOp) -> bool {
    matches!(op, AsmBinaryOp::Mul)
}

fn operand_is_imm64(operand: &AsmOperand) -> bool {
    if let AsmOperand::Imm { value, .. } = operand
        && *value > i32::MAX as u64
    {
        true
    } else {
        false
    }
}

fn both_operands_are_memory_addresses(operand1: &AsmOperand, operand2: &AsmOperand) -> bool {
    operand1.is_memory_address() && operand2.is_memory_address()
}
