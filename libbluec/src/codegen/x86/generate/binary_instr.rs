// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `binary_instr` module provides functionality to generate instructions for an operation that has two operands.

use super::Generator;
use super::compare;
use super::{AsmBinaryOp, AsmInstruction, AsmOperand, AsmType, ConditionalCode, HwRegister};

use crate::internal_error;
use crate::ir;

/// Generates the x86_64 instructions for a binary operation.
pub fn generate_instruction(
    op: &ir::BtBinaryOp,
    src1: &ir::BtValue,
    src2: &ir::BtValue,
    dst: &ir::BtValue,
    asm: &mut Vec<AsmInstruction>,
    generator: &mut Generator,
) {
    let src1_bt_type = src1.get_type(&generator.symbols);
    let src2_bt_type = src2.get_type(&generator.symbols);
    debug_assert!(
        src1_bt_type == src2_bt_type,
        "Binary operation '{op}' sources have different types '{src1_bt_type}' and '{src2_bt_type}'"
    );

    let is_signed = src1_bt_type.is_signed_integer();

    let src_asm_type = src1_bt_type.into();
    let dst_asm_type = dst.get_type(&generator.symbols).into();

    let src1_operand = generator.translate_bt_value_to_asm_operand(src1);
    let src2_operand = generator.translate_bt_value_to_asm_operand(src2);
    let dst_operand = generator.translate_bt_value_to_asm_operand(dst);

    match op {
        ir::BtBinaryOp::Add
        | ir::BtBinaryOp::Subtract
        | ir::BtBinaryOp::Multiply
        | ir::BtBinaryOp::BitwiseAnd
        | ir::BtBinaryOp::BitwiseXor
        | ir::BtBinaryOp::BitwiseOr
        | ir::BtBinaryOp::LeftShift
        | ir::BtBinaryOp::RightShift => {
            let asm_type = src_asm_type;

            // MOV src1 into dst
            asm.push(AsmInstruction::Mov { asm_type, src: src1_operand, dst: dst_operand.clone() });

            // dst = dst <op> src2
            let op = translate_ir_binary_op_to_asm(op, is_signed);
            asm.push(AsmInstruction::Binary { op, asm_type, src: src2_operand, dst: dst_operand });
        }

        ir::BtBinaryOp::EqualTo
        | ir::BtBinaryOp::NotEqualTo
        | ir::BtBinaryOp::LessThan
        | ir::BtBinaryOp::GreaterThan
        | ir::BtBinaryOp::LessThanOrEqualTo
        | ir::BtBinaryOp::GreaterThanOrEqualTo => {
            let cond_code = translate_ir_relational_op_to_condition_code(op, is_signed);

            let and_then = || {
                let al_reg = AsmOperand::Reg(HwRegister::AL);
                let r11_reg = AsmOperand::hw_reg(HwRegister::R11, dst_asm_type);

                vec![
                    // sete %al
                    AsmInstruction::SetCC { cond_code, operand: al_reg.clone() },
                    //
                    // MovZx %al, %r11[x]
                    AsmInstruction::MovZx {
                        src_type: AsmType::Byte,
                        dst_type: dst_asm_type,
                        src: al_reg,
                        dst: r11_reg.clone(),
                    },
                    //
                    // Mov %r11[x], dst
                    AsmInstruction::Mov { asm_type: dst_asm_type, src: r11_reg, dst: dst_operand.clone() },
                ]
            };

            let nan_handler = || {
                let cmp_nan_result = if *op == ir::BtBinaryOp::NotEqualTo { 1 } else { 0 };

                vec![AsmInstruction::Mov {
                    asm_type: dst_asm_type,
                    src: AsmOperand::Imm(cmp_nan_result),
                    dst: dst_operand.clone(),
                }]
            };

            // Note that we flip the operands for the `cmp` instruction.
            //      The `cmp` instruction works by subtracting its second operand from its first, and then sets status
            //      flags base on the result. Our IR has operands in the same order as the source code. E.g.
            //
            //          C               BlueTac IR
            //          `(a > b)`  -->  `dst = gt a, b`
            //
            //      And we need to generate (AT&T syntax):
            //
            //          movl a, eax   ; eax = a
            //          cmpl b, eax   ; eax - b
            //
            //      Floating-point works similar.
            //
            if src_asm_type.is_floating_point() {
                compare::compare_2_operands_fp(
                    src_asm_type,
                    &src2_operand,
                    &src1_operand,
                    and_then,
                    nan_handler,
                    asm,
                    generator,
                );
            } else {
                compare::compare_2_operands_integer(src_asm_type, &src2_operand, &src1_operand, and_then, asm);
            }
        }

        ir::BtBinaryOp::Divide => translate_division(
            src_asm_type,
            is_signed,
            src1_operand,
            src2_operand,
            dst_operand,
            false, // Return division result
            asm,
        ),

        ir::BtBinaryOp::Remainder => translate_division(
            src_asm_type,
            is_signed,
            src1_operand,
            src2_operand,
            dst_operand,
            true, // Return remainder
            asm,
        ),

        #[allow(unreachable_patterns)]
        _ => {
            internal_error::ICE("Unexpected binary operator");
        }
    }
}

fn translate_division(
    asm_type: AsmType,
    is_signed: bool,
    dividend: AsmOperand,
    divisor: AsmOperand,
    dst: AsmOperand,
    return_remainder: bool,
    asm_instructions: &mut Vec<AsmInstruction>,
) {
    if asm_type.is_floating_point() {
        // MOV dividend into dst
        asm_instructions.push(AsmInstruction::Mov { asm_type, src: dividend, dst: dst.clone() });

        // dst = dst / divisor
        asm_instructions.push(AsmInstruction::Binary {
            op: AsmBinaryOp::DivFp,
            asm_type,
            src: divisor,
            dst: dst.clone(),
        });

        return;
    }

    let rax_reg = AsmOperand::hw_reg(HwRegister::RAX, asm_type);
    let rdx_reg = AsmOperand::hw_reg(HwRegister::RDX, asm_type);

    // Move the dividend into RAX
    asm_instructions.push(AsmInstruction::Mov { asm_type, src: dividend, dst: rax_reg.clone() });

    if is_signed {
        // Sign extend into RDX:RAX (or EDX:EAX or DX:AX)
        asm_instructions.push(AsmInstruction::Cdq { asm_type });

        asm_instructions.push(AsmInstruction::IDiv { asm_type, operand: divisor });
    } else {
        // Zero out RDX/EDX/DX
        asm_instructions.push(AsmInstruction::Mov { asm_type, src: AsmOperand::Imm(0), dst: rdx_reg.clone() });

        asm_instructions.push(AsmInstruction::Div { asm_type, operand: divisor });
    }

    // Move result into `dst`
    //      The remainder is in RDX and the division result is in RAX.
    //
    if return_remainder {
        asm_instructions.push(AsmInstruction::Mov { asm_type, src: rdx_reg, dst });
    } else {
        asm_instructions.push(AsmInstruction::Mov { asm_type, src: rax_reg, dst });
    }
}

#[rustfmt::skip]
fn translate_ir_relational_op_to_condition_code(op: &ir::BtBinaryOp, is_signed: bool) -> ConditionalCode {
    match op {
        ir::BtBinaryOp::EqualTo                           => ConditionalCode::E,
        ir::BtBinaryOp::NotEqualTo                        => ConditionalCode::NE,

        // Signed
        ir::BtBinaryOp::LessThan if is_signed             => ConditionalCode::L,
        ir::BtBinaryOp::LessThanOrEqualTo if is_signed    => ConditionalCode::LE,
        ir::BtBinaryOp::GreaterThan if is_signed          => ConditionalCode::G,
        ir::BtBinaryOp::GreaterThanOrEqualTo if is_signed => ConditionalCode::GE,

        // Unsigned
        ir::BtBinaryOp::LessThan                          => ConditionalCode::B,
        ir::BtBinaryOp::LessThanOrEqualTo                 => ConditionalCode::BE,
        ir::BtBinaryOp::GreaterThan                       => ConditionalCode::A,
        ir::BtBinaryOp::GreaterThanOrEqualTo              => ConditionalCode::AE,

        _ => { internal_error::ICE("Unexpected relational operator")}
    }
}

#[rustfmt::skip]
fn translate_ir_binary_op_to_asm(op: &ir::BtBinaryOp, is_signed: bool) -> AsmBinaryOp {
    match op {
        ir::BtBinaryOp::Add        => AsmBinaryOp::Add,
        ir::BtBinaryOp::Subtract   => AsmBinaryOp::Sub,
        ir::BtBinaryOp::Multiply   => AsmBinaryOp::Mul,
        ir::BtBinaryOp::BitwiseAnd => AsmBinaryOp::And,
        ir::BtBinaryOp::BitwiseXor => AsmBinaryOp::Xor,
        ir::BtBinaryOp::BitwiseOr  => AsmBinaryOp::Or,
        ir::BtBinaryOp::LeftShift  => AsmBinaryOp::Shl,

        ir::BtBinaryOp::RightShift if is_signed => AsmBinaryOp::Sar,
        ir::BtBinaryOp::RightShift              => AsmBinaryOp::Shr,

        #[allow(unreachable_patterns)]
        _ => { internal_error::ICE("Unexpected binary operator")}
    }
}
