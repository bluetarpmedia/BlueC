// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `unary_instr` module provides functionality to generate instructions for an operation that has one operand.

use super::Generator;
use super::compare;
use super::{AsmBinaryOp, AsmConstantId, AsmInstruction, AsmOperand, AsmType, AsmUnaryOp, ConditionalCode, HwRegister};

use crate::internal_error;
use crate::ir;

/// Generates the x86_64 instructions for a unary operation.
pub fn generate_instruction(
    op: &ir::BtUnaryOp,
    src: &ir::BtValue,
    dst: &ir::BtValue,
    asm: &mut Vec<AsmInstruction>,
    generator: &mut Generator,
) {
    let asm_type: AsmType = src.get_type(&generator.symbols).into();

    let src_operand = generator.translate_bt_value_to_asm_operand(src);
    let dst_operand = generator.translate_bt_value_to_asm_operand(dst);

    if *op == ir::BtUnaryOp::LogicalNot {
        // LogicalNot dst type is 'int', potentially different from src type.
        let dst_asm_type: AsmType = dst.get_type(&generator.symbols).into();
        debug_assert!(dst_asm_type.is_primitive());

        let and_then = || {
            let al_reg = AsmOperand::Reg(HwRegister::AL);
            let r11_reg = AsmOperand::hw_reg(HwRegister::R11, dst_asm_type);

            vec![
                // sete %al
                AsmInstruction::SetCC { cond_code: ConditionalCode::E, operand: al_reg.clone() },
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
            vec![AsmInstruction::Mov { asm_type: dst_asm_type, src: AsmOperand::from_u64(0), dst: dst_operand.clone() }]
        };

        // cmp 0, src
        if asm_type.is_floating_point() {
            compare::compare_with_zero_fp(asm_type, &src_operand, and_then, nan_handler, asm, generator);
        } else {
            compare::compare_with_zero_integer(asm_type, &src_operand, and_then, asm);
        }
    }
    //
    // Unary Plus is silently dropped here; just MOV the src into dst.
    //
    else if *op == ir::BtUnaryOp::Plus {
        asm.push(AsmInstruction::Mov { asm_type, src: src_operand, dst: dst_operand.clone() });
    }
    //
    // Unary Negate for floating-point types is handled differently than for integers
    //
    else if *op == ir::BtUnaryOp::Negate && asm_type.is_floating_point() {
        // We need to XOR the the float value with -0.0 to flip the sign bit.
        // The `xorps` / `xorpd` instruction requires its memory operand to be 16-byte aligned.
        //
        const ALIGNMENT: usize = 16;
        let neg_zero_id = if asm_type == AsmType::FpSingle {
            AsmConstantId::from(generator.constants.add_f32(-0.0, ALIGNMENT))
        } else {
            AsmConstantId::from(generator.constants.add_f64(-0.0, ALIGNMENT))
        };

        let neg_zero_lbl = generator.labels.make_constant_label(neg_zero_id);
        let neg_zero = AsmOperand::Data { symbol: neg_zero_lbl.to_string(), relative: 0 };

        asm.push(AsmInstruction::Mov { asm_type, src: src_operand, dst: dst_operand.clone() });

        asm.push(AsmInstruction::Binary { op: AsmBinaryOp::Xor, asm_type, src: neg_zero, dst: dst_operand.clone() });
    }
    //
    // Remaining unary ops
    //
    else {
        asm.push(AsmInstruction::Mov { asm_type, src: src_operand, dst: dst_operand.clone() });

        let asm_op = translate_ir_unary_op_to_asm(op);
        asm.push(AsmInstruction::Unary { op: asm_op, asm_type, operand: dst_operand });
    }
}

fn translate_ir_unary_op_to_asm(op: &ir::BtUnaryOp) -> AsmUnaryOp {
    match op {
        ir::BtUnaryOp::BitwiseNot => AsmUnaryOp::Not,
        ir::BtUnaryOp::Negate => AsmUnaryOp::Neg,

        _ => internal_error::ICE("Unexpected unary operator"),
    }
}
