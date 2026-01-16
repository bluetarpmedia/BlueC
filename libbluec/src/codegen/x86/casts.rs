// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `casts` module provides functionality to generate the instructions for casts between various data types.
//! R8, R9 and XMM1 are used for casts between floating-point and integer types.

use super::ast::*;
use super::generate::Generator;
use super::registers::HwRegister;
use crate::internal_error;
use crate::ir::BtValue;

pub fn fp_to_fp(src: &BtValue, dst: &BtValue, asm: &mut Vec<AsmInstruction>, generator: &mut Generator) {
    let src_type: AsmType = src.get_type(&generator.symbols).into();
    let dst_type: AsmType = dst.get_type(&generator.symbols).into();

    debug_assert!(src_type.is_floating_point());
    debug_assert!(dst_type.is_floating_point());
    debug_assert!(src_type != dst_type);

    let src = generator.translate_bt_value_to_asm_operand(src);
    let dst = generator.translate_bt_value_to_asm_operand(dst);

    // `Cvtfp2fp` is an alias for `cvtss2sd` / `cvtsd2ss`.
    //      Src operand must be a register or memory address.
    //      Dst operand must be a register.
    //      Src and Dst can be the same register, e.g. move a float value into XMM1 and then convert it to double.

    let valid_src = src.is_hw_register() || src.is_memory_address();
    let dst_is_reg = dst.is_hw_register();

    let xmm1 = AsmOperand::Reg(HwRegister::XMM1);

    let src = if valid_src {
        src
    } else {
        // xmm1 = src
        asm.push(AsmInstruction::Mov { asm_type: src_type, src, dst: xmm1.clone() });

        xmm1.clone()
    };

    if dst_is_reg {
        asm.push(AsmInstruction::Cvtfp2fp { src_type, dst_type, src, dst });
    } else {
        asm.push(AsmInstruction::Cvtfp2fp { src_type, dst_type, src, dst: xmm1.clone() });
        
        // dst = xmm1
        asm.push(AsmInstruction::Mov { asm_type: dst_type, src: xmm1, dst });
    }
}

/// Generates the instructions for a cast from a floating-point type to a signed integer type.
pub fn fp_to_signed_integer(src: &BtValue, dst: &BtValue, asm: &mut Vec<AsmInstruction>, generator: &mut Generator) {
    let src_type: AsmType = src.get_type(&generator.symbols).into();
    let dst_type: AsmType = dst.get_type(&generator.symbols).into();

    debug_assert!(src_type.is_floating_point());
    debug_assert!(!dst_type.is_floating_point());

    let src = generator.translate_bt_value_to_asm_operand(src);
    let dst = generator.translate_bt_value_to_asm_operand(dst);

    // Future: Add a compiler driver flag to check if the value was out-of-range for the signed/unsigned integer.
    //      See `stmxcsr` and the Invalid Operation flag (first bit).

    match dst_type {
        AsmType::Byte | AsmType::Word => {
            let r9_quad = AsmOperand::Reg(HwRegister::R9);
            let r9_dest = AsmOperand::hw_reg(HwRegister::R9, dst_type); // R9b / R9w

            asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type: AsmType::QuadWord, src, dst: r9_quad });
            asm.push(AsmInstruction::Mov { asm_type: dst_type, src: r9_dest, dst });
        }

        AsmType::DoubleWord | AsmType::QuadWord => asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type, src, dst }),

        _ => internal_error::ICE(format!("Unsupported dst_type '{dst_type}' for fp_to_signed_integer")),
    }
}

/// Generates the instructions for a cast from a floating-point type to an unsigned integer type.
pub fn fp_to_unsigned_integer(src: &BtValue, dst: &BtValue, asm: &mut Vec<AsmInstruction>, generator: &mut Generator) {
    let src_type: AsmType = src.get_type(&generator.symbols).into();
    let dst_type: AsmType = dst.get_type(&generator.symbols).into();

    debug_assert!(src_type.is_floating_point());
    debug_assert!(!dst_type.is_floating_point());

    let src = generator.translate_bt_value_to_asm_operand(src);
    let dst = generator.translate_bt_value_to_asm_operand(dst);

    let r9_quad = AsmOperand::Reg(HwRegister::R9);

    match dst_type {
        AsmType::Byte | AsmType::Word => {
            let r9_dest = AsmOperand::hw_reg(HwRegister::R9, dst_type); // R9b / R9w
            asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type: AsmType::QuadWord, src, dst: r9_quad });
            asm.push(AsmInstruction::Mov { asm_type: dst_type, src: r9_dest, dst });
        }

        AsmType::DoubleWord => {
            let r9_dword = AsmOperand::Reg(HwRegister::R9d);
            asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type: AsmType::QuadWord, src, dst: r9_quad });
            asm.push(AsmInstruction::Mov { asm_type: AsmType::DoubleWord, src: r9_dword, dst });
        }

        AsmType::QuadWord => {
            let two_pow_63_id = if src_type == AsmType::FpSingle {
                const TWO_POW_63: f32 = 9_223_372_036_854_775_808.0;
                const ALIGNMENT: usize = 4;

                AsmConstantId::from(generator.constants.add_f32(TWO_POW_63, ALIGNMENT))
            } else if src_type == AsmType::FpDouble {
                const TWO_POW_63: f64 = 9_223_372_036_854_775_808.0;
                const ALIGNMENT: usize = 8;

                AsmConstantId::from(generator.constants.add_f64(TWO_POW_63, ALIGNMENT))
            } else {
                unreachable!();
            };
            
            let two_pow_63_lbl = generator.labels.make_constant_label(two_pow_63_id);
            let two_pow_63_fp = AsmOperand::Data { symbol: two_pow_63_lbl.to_string(), relative: 0 };

            let out_of_range_label = generator.labels.make_function_local_label();
            let end_label = generator.labels.make_function_local_label();

            // If the floating-point source value is < 2^63 then we can use 'Cvtfp2si' to convert it to a signed
            // 64-bit integer and then interpret the value as unsigned.
            //      If not, jump to the 'out_of_range' label.
            //
            asm.push(AsmInstruction::Cmp { asm_type: src_type, op1: two_pow_63_fp.clone(), op2: src.clone() });
            asm.push(AsmInstruction::JmpCC { cond_code: ConditionalCode::AE, target: out_of_range_label.clone() });

            // Convert to signed integer and then move into `dst`. The bits will then be interpreted as unsigned.
            asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type, src: src.clone(), dst: r9_quad.clone() });
            asm.push(AsmInstruction::Mov { asm_type: dst_type, src: r9_quad.clone(), dst: dst.clone() });
            asm.push(AsmInstruction::Jmp { target: end_label.clone() });

            // Too big for a signed 64-bit integer.
            let xmm1 = AsmOperand::Reg(HwRegister::XMM1);
            let r8_quad = AsmOperand::Reg(HwRegister::R8);
            asm.push(AsmInstruction::Label { id: out_of_range_label });

            // xmm1 = src
            asm.push(AsmInstruction::Mov { asm_type: src_type, src, dst: xmm1.clone() });

            // xmm1 -= 2^63
            asm.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Sub,
                asm_type: src_type,
                src: two_pow_63_fp,
                dst: xmm1.clone(),
            });

            // r9 = cvttsd2si(xmm1)
            asm.push(AsmInstruction::Cvtfp2si { src_type, dst_type, src: xmm1, dst: r9_quad.clone() });

            // r9 += 2^63
            let two_pow_63_int = AsmOperand::from_u64(9_223_372_036_854_775_808);
            asm.push(AsmInstruction::Mov { asm_type: AsmType::QuadWord, src: two_pow_63_int, dst: r8_quad.clone() });
            asm.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                asm_type: AsmType::QuadWord,
                src: r8_quad,
                dst: r9_quad.clone(),
            });

            // dst = r9
            asm.push(AsmInstruction::Mov { asm_type: dst_type, src: r9_quad, dst });

            asm.push(AsmInstruction::Label { id: end_label });
        }

        _ => internal_error::ICE(format!("Unsupported dst_type '{dst_type}' for fp_to_unsigned_integer")),
    }
}

/// Generates the instructions for a cast from a signed integer type to a floating-point type.
pub fn signed_integer_to_fp(src: &BtValue, dst: &BtValue, asm: &mut Vec<AsmInstruction>, generator: &mut Generator) {
    let src_type: AsmType = src.get_type(&generator.symbols).into();
    let dst_type: AsmType = dst.get_type(&generator.symbols).into();

    debug_assert!(!src_type.is_floating_point());
    debug_assert!(dst_type.is_floating_point());

    let src = generator.translate_bt_value_to_asm_operand(src);
    let dst = generator.translate_bt_value_to_asm_operand(dst);

    match src_type {
        AsmType::Byte | AsmType::Word => {
            // Sign-extend the signed byte/word value into a quad.
            let r9_quad = AsmOperand::Reg(HwRegister::R9);
            asm.push(AsmInstruction::MovSx { src_type, dst_type: AsmType::QuadWord, src, dst: r9_quad.clone() });

            // Convert to floating point.
            asm.push(AsmInstruction::Cvtsi2fp { src_type: AsmType::QuadWord, dst_type, src: r9_quad, dst });
        }

        AsmType::DoubleWord | AsmType::QuadWord => asm.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src, dst }),

        _ => internal_error::ICE(format!("Unsupported src_type '{src_type}' for signed_integer_to_fp")),
    }
}

/// Generates the instructions for a cast from an unsigned integer type to a floating-point type.
pub fn unsigned_integer_to_fp(src: &BtValue, dst: &BtValue, asm: &mut Vec<AsmInstruction>, generator: &mut Generator) {
    let src_type: AsmType = src.get_type(&generator.symbols).into();
    let dst_type: AsmType = dst.get_type(&generator.symbols).into();

    debug_assert!(!src_type.is_floating_point());
    debug_assert!(dst_type.is_floating_point());

    let src = generator.translate_bt_value_to_asm_operand(src);
    let dst = generator.translate_bt_value_to_asm_operand(dst);

    match src_type {
        AsmType::Byte | AsmType::Word | AsmType::DoubleWord => {
            // Zero-extend the unsigned byte/word/dword into a quad (which we then treat as a signed 64-bit value).
            let r9_quad = AsmOperand::Reg(HwRegister::R9);
            asm.push(AsmInstruction::MovZx { src_type, dst_type: AsmType::QuadWord, src, dst: r9_quad.clone() });

            // Convert to floating point.
            asm.push(AsmInstruction::Cvtsi2fp { src_type: AsmType::QuadWord, dst_type, src: r9_quad, dst });
        }

        AsmType::QuadWord => {
            let out_of_range_label = generator.labels.make_function_local_label();
            let end_label = generator.labels.make_function_local_label();

            // Perform a signed comparison with 0: is the value <= INT64_MAX (2^63 - 1) ?
            asm.push(AsmInstruction::Cmp { asm_type: src_type, op1: AsmOperand::from_u64(0), op2: src.clone() });
            asm.push(AsmInstruction::JmpCC { cond_code: ConditionalCode::L, target: out_of_range_label.clone() });

            // If the unsigned src value is <= INT64_MAX  then we can use `cvtsi2sd/ss` directly.
            asm.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src: src.clone(), dst: dst.clone() });
            asm.push(AsmInstruction::Jmp { target: end_label.clone() });

            // Otherwise we have a bit of work to do.
            //      We divide the unsigned src value by 2 so that it fits inside the range of a signed 64-bit
            //      value, meaning we can then use the `cvtsi2sd/ss` instruction, and then double the result.
            //      However, after dividing the unsigned value by 2, we also need to ensure it is rounded up
            //      or down to the nearest odd value, if the original value was odd.
            //      E.g. we want:
            //          12 / 2 --> 6
            //          10 / 2 --> 5
            //           9 / 2 --> 5  (9 / 2 = 4, rounded up because 9 is odd)
            //           7 / 2 --> 3  (7 / 2 = 3, rounded down because 7 is odd)
            //
            //      We need to round-to-odd because of the potential subsequent floating point rounding error if the
            //      integer value cannot be represented exactly as floating point (e.g. if it's > 2^53). By rounding
            //      the halved integer to odd, it means when we use `cvtsi2sd/ss` we'll get the correctly rounded-to
            //      -nearest floating point value, and then we can multiply it by 2 to restore the equivalent of the
            //      original unsigned value.
            //
            let r8_quad = AsmOperand::Reg(HwRegister::R8);
            let r9_quad = AsmOperand::Reg(HwRegister::R9);
            asm.push(AsmInstruction::Label { id: out_of_range_label });

            // r8 = src
            asm.push(AsmInstruction::Mov { asm_type: src_type, src: src.clone(), dst: r8_quad.clone() });

            // r9 = r8
            asm.push(AsmInstruction::Mov { asm_type: src_type, src: r8_quad.clone(), dst: r9_quad.clone() });

            // Divide the unsigned src value in 'r9' by 2.
            // r9 >> 1
            asm.push(AsmInstruction::Unary { op: AsmUnaryOp::Shr, asm_type: src_type, operand: r9_quad.clone() });

            // If the LSB is set on the original unsigned value (meaning its odd) then set it on the value in R9.
            // r8 &= 1
            // r9 |= r8
            asm.push(AsmInstruction::Binary {
                op: AsmBinaryOp::And,
                asm_type: src_type,
                src: AsmOperand::from_u64(1),
                dst: r8_quad.clone(),
            });
            asm.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Or,
                asm_type: src_type,
                src: r8_quad.clone(),
                dst: r9_quad.clone(),
            });

            // Convert to floating point.
            // xmm1 = cvtsi2sd(r9)
            let xmm1 = AsmOperand::Reg(HwRegister::XMM1);
            asm.push(AsmInstruction::Cvtsi2fp { src_type, dst_type, src: r9_quad, dst: xmm1.clone() });

            // Multiply the floating point value by 2.
            // xmm1 += xmm1
            asm.push(AsmInstruction::Binary {
                op: AsmBinaryOp::Add,
                asm_type: dst_type,
                src: xmm1.clone(),
                dst: xmm1.clone(),
            });

            // dst = xmm1
            asm.push(AsmInstruction::Mov { asm_type: dst_type, src: xmm1.clone(), dst });

            asm.push(AsmInstruction::Label { id: end_label });
        }

        _ => internal_error::ICE(format!("Unsupported src_type '{src_type}' for signed_integer_to_fp")),
    }
}
