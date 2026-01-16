// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `functions` module provides functionality to handle x86_64 function parameters and arguments.

use super::super::ast::{AsmBinaryOp, AsmInstruction, AsmOperand, AsmType};
use super::super::registers::HwRegister;
use super::Generator;

use crate::ICE;
use crate::ir;

/// Copies function parameters from calling-convention HW registers or the stack into pseudo-registers.
pub fn copy_params_into_pseudo_registers(bt_func: &ir::BtFunctionDefn, asm_instructions: &mut Vec<AsmInstruction>) {
    // Get the source operands where the function parameters are located
    let src_operands = classify_params_iter(bt_func.params.iter().map(|(bt_type, _)| bt_type.clone()));

    // Copy the parameters into pseudo-registers
    //
    for ((bt_type, unique_name), src) in bt_func.params.iter().zip(src_operands) {
        let asm_type = AsmType::from(bt_type);

        asm_instructions.push(AsmInstruction::Mov { asm_type, src, dst: AsmOperand::Pseudo(unique_name.clone()) });
    }
}

/// Generates the instructions for a call to the given function.
pub fn generate_function_call(
    designator: &ir::BtValue,
    args: &[ir::BtValue],
    dst: &ir::BtValue,
    asm: &mut Vec<AsmInstruction>,
    generator: &mut Generator,
) {
    // Get the dest operands where we need to copy the function arguments into
    let dst_operands = classify_params_iter(args.iter().map(|arg| arg.get_type(&generator.symbols)));

    assert!(args.len() == dst_operands.len());

    // Count the args that need to be passed on the stack
    let stack_arg_count = dst_operands.iter().filter(|op| op.is_memory_address()).count();

    // We need to ensure the stack pointer is always 16-byte aligned. All args pushed onto the
    // stack are 8 bytes (even if the parameter type itself is smaller), so if we push an even
    // number of args then we'll always maintain the 16-byte alignment. If we know we're going to
    // push an odd number of args then we'll first increase the stack pointer by 8 bytes so that
    // afterwards it will still be 16-byte aligned.
    let stack_padding = if stack_arg_count % 2 == 0 { 0 } else { 8 };

    if stack_padding != 0 {
        // rsp -= stack_padding
        asm.push(AsmInstruction::Binary {
            op: AsmBinaryOp::Sub,
            asm_type: AsmType::QuadWord,
            src: AsmOperand::from_u64(stack_padding),
            dst: AsmOperand::Reg(HwRegister::RSP),
        });
    }

    // Split the arguments into those that are passed by register and those passed on the stack.
    //
    let mut args_reg: Vec<(&ir::BtValue, AsmOperand)> = Vec::with_capacity(args.len());
    let mut args_stack: Vec<(&ir::BtValue, AsmOperand)> = Vec::with_capacity(args.len());

    for (arg, dst) in args.iter().zip(dst_operands.into_iter()) {
        if dst.is_memory_address() {
            args_stack.push((arg, dst));
        } else {
            args_reg.push((arg, dst));
        }
    }

    // Copy the args into the appropriate registers
    for (value, dst) in args_reg {
        let asm_type = value.get_type(&generator.symbols).into();
        let arg_operand = generator.translate_bt_value_to_asm_operand(value);

        asm.push(AsmInstruction::Mov { asm_type, src: arg_operand, dst });
    }

    // Pass any remaining args on the stack as 8-byte values by pushing them in reverse order
    for (value, _) in args_stack.iter().rev() {
        let asm_type = value.get_type(&generator.symbols).into();
        let arg_operand = generator.translate_bt_value_to_asm_operand(value);

        match arg_operand {
            // Immediate and HW registers can be pushed
            AsmOperand::Imm { .. } | AsmOperand::Reg(_) => asm.push(AsmInstruction::Push(arg_operand)),

            // Pseudo-registers of type QuadWord or FpDouble can be pushed (since they're 8 bytes long)
            AsmOperand::Pseudo(_) if matches!(asm_type, AsmType::QuadWord | AsmType::FpDouble) => {
                asm.push(AsmInstruction::Push(arg_operand))
            }

            _ => {
                // For other pseudo-registers and stack variables, we move the operand value into the RAX
                // register and then push the 64-bit RAX register so that the argument takes up 8 bytes.
                // We can MOV floating-point types into EAX/RAX too.
                //
                let asm_type = match asm_type {
                    AsmType::FpSingle => AsmType::DoubleWord,
                    AsmType::FpDouble => AsmType::QuadWord,
                    _ => asm_type,
                };

                let dst = AsmOperand::hw_reg(HwRegister::RAX, asm_type);
                asm.push(AsmInstruction::Mov { asm_type, src: arg_operand, dst });
                asm.push(AsmInstruction::Push(AsmOperand::Reg(HwRegister::RAX)));
            }
        }
    }

    // Call the function
    let designator_operand = generator.translate_bt_value_to_asm_operand(designator);
    asm.push(AsmInstruction::Call(designator_operand));

    // Deallocate any stack space we created to restore the stack pointer
    let bytes_to_free = (stack_arg_count * 8) as u64 + stack_padding;
    if bytes_to_free != 0 {
        // Add `bytes_to_free` to RSP.
        asm.push(AsmInstruction::Binary {
            op: AsmBinaryOp::Add,
            asm_type: AsmType::QuadWord,
            src: AsmOperand::from_u64(bytes_to_free),
            dst: AsmOperand::Reg(HwRegister::RSP),
        });
    }

    // Copy the function's return value into RAX or XMM0.
    let asm_type: AsmType = dst.get_type(&generator.symbols).into();
    let dst_reg = if asm_type.is_floating_point() { HwRegister::XMM0 } else { HwRegister::RAX };

    let asm_dest = generator.translate_bt_value_to_asm_operand(dst);
    asm.push(AsmInstruction::Mov { asm_type, src: AsmOperand::hw_reg(dst_reg, asm_type), dst: asm_dest });
}

/// Classifies the given parameter types into the appropriate operands.
fn classify_params_iter<I>(param_types_iter: I) -> Vec<AsmOperand>
where
    I: Iterator<Item = ir::BtType>,
{
    const FIRST_STACK_PARAM_BYTE_OFFSET: usize = 16;
    const PARAM_BYTE_SIZE: usize = 8;

    let mut int_count = 0;
    let mut fp_count = 0;
    let mut next_relative_addr = FIRST_STACK_PARAM_BYTE_OFFSET;

    param_types_iter
        .map(|bt_type| {
            let asm_type = AsmType::from(bt_type);

            match asm_type {
                AsmType::Byte | AsmType::Word | AsmType::DoubleWord | AsmType::QuadWord => {
                    if int_count < 6 {
                        let index = int_count;
                        int_count += 1;
                        AsmOperand::hw_reg(get_integer_parameter_hw_register(index), asm_type)
                    } else {
                        let relative_addr = next_relative_addr;
                        next_relative_addr += PARAM_BYTE_SIZE;
                        AsmOperand::stack_address(relative_addr as i32)
                    }
                }

                AsmType::FpSingle | AsmType::FpDouble => {
                    if fp_count < 8 {
                        let index = fp_count;
                        fp_count += 1;
                        AsmOperand::Reg(get_fp_parameter_hw_register(index))
                    } else {
                        let relative_addr = next_relative_addr;
                        next_relative_addr += PARAM_BYTE_SIZE;
                        AsmOperand::stack_address(relative_addr as i32)
                    }
                }

                AsmType::ByteArray { .. } => ICE!("Unexpected AsmType::ByteArray in function params"),
            }
        })
        .collect()
}

fn get_integer_parameter_hw_register(arg_index: usize) -> HwRegister {
    match arg_index {
        0 => HwRegister::RDI,
        1 => HwRegister::RSI,
        2 => HwRegister::RDX,
        3 => HwRegister::RCX,
        4 => HwRegister::R8,
        5 => HwRegister::R9,
        n => ICE!("Invalid function argument index {n}; must be in range [0, 6)."),
    }
}

fn get_fp_parameter_hw_register(arg_index: usize) -> HwRegister {
    match arg_index {
        0 => HwRegister::XMM0,
        1 => HwRegister::XMM1,
        2 => HwRegister::XMM2,
        3 => HwRegister::XMM3,
        4 => HwRegister::XMM4,
        5 => HwRegister::XMM5,
        6 => HwRegister::XMM6,
        7 => HwRegister::XMM7,
        n => ICE!("Invalid function argument index {n}; must be in range [0, 8)."),
    }
}
