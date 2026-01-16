// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `generate` module provides x86_64 assembly generation functionality to the parent codegen module.
//!
//! R10, R11, XMM14 and XMM15 are used for fixups and rewrites.
//! R8, R9 and XMM1 are used for casts between floating-point and integer types.
//! XMM0 is used for comparisons with '0.0'.

mod binary_instr;
mod compare;
mod functions;
mod unary_instr;

use super::ast::*;
use super::casts;
use super::instruction_fixups;
use super::label_maker::AsmLabelMaker;
use super::registers::HwRegister;
use super::symbols::AsmSymbol;
use super::symbols::AsmSymbolTable;

use crate::ICE;
use crate::ir;
use crate::sema::constant_table::{ConstantTable, ConstantValue, UnsignedValue};
use crate::sema::symbol_table::SymbolTable;
use crate::sema::type_conversion;

use std::collections::HashMap;

/// The x86_64 code generator which lowers the BlueTac IR into an x86_64 AST.
pub struct Generator {
    pub symbols: SymbolTable,
    pub constants: ConstantTable,
    pub labels: AsmLabelMaker,
}

impl Generator {
    /// Creates a new generator and takes ownership of the Parser's symbol table.
    pub fn new(symbols: SymbolTable, constants: ConstantTable) -> Self {
        Self { symbols, constants, labels: AsmLabelMaker::new() }
    }

    /// Transforms an IR `BtValue` into an `AsmOperand`.
    pub fn translate_bt_value_to_asm_operand(&mut self, value: &ir::BtValue) -> AsmOperand {
        match value {
            ir::BtValue::Constant(const_value) => match const_value {
                ir::BtConstantValue::Int8(value) => AsmOperand::from_i8(*value),
                ir::BtConstantValue::Int16(value) => AsmOperand::from_i16(*value),
                ir::BtConstantValue::Int32(value) => AsmOperand::from_i32(*value),
                ir::BtConstantValue::Int64(value) => AsmOperand::from_i64(*value),
                ir::BtConstantValue::UInt8(value) => AsmOperand::from_u8(*value),
                ir::BtConstantValue::UInt16(value) => AsmOperand::from_u16(*value),
                ir::BtConstantValue::UInt32(value) => AsmOperand::from_u32(*value),
                ir::BtConstantValue::UInt64(value) => AsmOperand::from_u64(*value),

                ir::BtConstantValue::Float32(value) => {
                    const ALIGNMENT: usize = 4;
                    let constant_id = AsmConstantId::from(self.constants.add_f32(*value, ALIGNMENT));
                    let constant_lbl = self.labels.make_constant_label(constant_id);
                    AsmOperand::Data { symbol: constant_lbl.to_string(), relative: 0 }
                }

                ir::BtConstantValue::Float64(value) => {
                    const ALIGNMENT: usize = 8;
                    let constant_id = AsmConstantId::from(self.constants.add_f64(*value, ALIGNMENT));
                    let constant_lbl = self.labels.make_constant_label(constant_id);
                    AsmOperand::Data { symbol: constant_lbl.to_string(), relative: 0 }
                }
            },
            ir::BtValue::Variable(name) => AsmOperand::Pseudo(name.clone()),
        }
    }
}

/// Generates an x86_64 assembly AST of the given IR.
pub fn generate_asm(bt_root: ir::BtRoot, symbols: SymbolTable, constants: ConstantTable) -> (AsmRoot, AsmSymbolTable) {
    // Transform the IR definitions into Asm definitions.
    //
    let mut generator = Generator::new(symbols, constants);

    let bt_definitions = bt_root.0;
    let mut asm_definitions = bt_definitions
        .into_iter()
        .map(|bt_defn| match bt_defn {
            ir::BtDefinition::Function(bt_func) => {
                AsmDefinition::Function(generate_asm_function(bt_func, &mut generator))
            }

            ir::BtDefinition::StaticVariable(bt_static_var) => {
                AsmDefinition::StaticVariable(generate_asm_static_storage_variable(bt_static_var))
            }

            ir::BtDefinition::StaticConstant(bt_static_constant) => {
                AsmDefinition::StaticConstant(generate_asm_static_constant(bt_static_constant, &generator.constants))
            }
        })
        .collect::<Vec<AsmDefinition>>();

    // Transform the parser's symbol table into the back-end `AsmSymbolTable`.
    //
    let mut asm_symbols = AsmSymbolTable::from_frontend_symbols(generator.symbols);

    // Add the floating-point constant table entries to the back-end symbol table and to the list of definitions.
    //
    let constants = generator.constants.get_float_constants();
    asm_definitions.reserve(constants.len());

    for constant_entry in constants {
        let imm = match constant_entry.value {
            UnsignedValue::U32(value) => AsmConstantInitializer::Imm32 { value, signed: false },
            UnsignedValue::U64(value) => AsmConstantInitializer::Imm64 { value, signed: false },
        };

        let constant = AsmConstant {
            label: generator.labels.make_constant_label(AsmConstantId::from(constant_entry.index)),
            alignment: constant_entry.alignment,
            value: imm,
        };

        asm_symbols.add_constant(&constant.label.0, constant.value.asm_type());
        asm_definitions.push(AsmDefinition::StaticConstant(constant));
    }

    // Post-process the generated Asm functions to perform some fixups.
    //
    for defn in &mut asm_definitions {
        if let AsmDefinition::Function(function) = defn {
            // Replace pseudo operands with stack-allocated variables or, for static storage duration variables,
            // replace them with Data operands. Also calculate the total stack space we need to allocate.
            //
            let stack_space = replace_pseudo_operands(function, &asm_symbols);

            // Insert instruction to allocate necessary stack space for the variables.
            if stack_space > 0 {
                allocate_stack_space(function, stack_space);
            }

            // Rewrite instructions in case we generated invalid semantics
            //      E.g. most instructions cannot take 2 memory addresses as operands.
            instruction_fixups::rewrite_instructions(function);
        }
    }

    // Pass the generated assembly AST and the new symbol table to the next stage
    //
    (AsmRoot(asm_definitions), asm_symbols)
}

fn generate_asm_function(bt_func: ir::BtFunctionDefn, generator: &mut Generator) -> AsmFunction {
    let mut asm_instructions = Vec::new();

    generator.labels.reset_for_new_function();

    // Copy function parameters from calling-convention HW registers into pseudo-registers
    //
    functions::copy_params_into_pseudo_registers(&bt_func, &mut asm_instructions);

    generate_asm_instructions(&bt_func.instructions, &mut asm_instructions, generator);
    AsmFunction { name: bt_func.name.clone(), is_global: bt_func.is_global, instructions: asm_instructions }
}

fn generate_asm_static_storage_variable(bt_static_var: ir::BtStaticStorageVariable) -> AsmStaticStorageVariable {
    let init_value = bt_static_var
        .init_value
        .into_iter()
        .map(|init| match init {
            ir::BtStaticStorageInitializer::Constant(constant_value) => match constant_value {
                ir::BtConstantValue::Int8(value) => AsmConstantInitializer::Imm8 { value: value as u8, signed: true },
                ir::BtConstantValue::Int16(value) => {
                    AsmConstantInitializer::Imm16 { value: value as u16, signed: true }
                }
                ir::BtConstantValue::Int32(value) => {
                    AsmConstantInitializer::Imm32 { value: value as u32, signed: true }
                }
                ir::BtConstantValue::Int64(value) => {
                    AsmConstantInitializer::Imm64 { value: value as u64, signed: true }
                }
                ir::BtConstantValue::UInt8(value) => AsmConstantInitializer::Imm8 { value, signed: false },
                ir::BtConstantValue::UInt16(value) => AsmConstantInitializer::Imm16 { value, signed: false },
                ir::BtConstantValue::UInt32(value) => AsmConstantInitializer::Imm32 { value, signed: false },
                ir::BtConstantValue::UInt64(value) => AsmConstantInitializer::Imm64 { value, signed: false },
                ir::BtConstantValue::Float32(value) => {
                    let value = value.to_bits();
                    AsmConstantInitializer::Imm32 { value, signed: false }
                }
                ir::BtConstantValue::Float64(value) => {
                    let value = value.to_bits();
                    AsmConstantInitializer::Imm64 { value, signed: false }
                }
            },
            ir::BtStaticStorageInitializer::ZeroBytes(count) => AsmConstantInitializer::ZeroBytes(count),
            ir::BtStaticStorageInitializer::String { ascii } => {
                let byte_count = ascii.len();
                let value = ascii.join("");
                AsmConstantInitializer::AsciiString { value, byte_count }
            }
            ir::BtStaticStorageInitializer::AddressOf { object, byte_offset } => {
                AsmConstantInitializer::AddressOf { object, byte_offset }
            }
        })
        .collect();

    let asm_type = AsmType::from(bt_static_var.data_type.clone());
    let alignment = asm_type.alignment_bytes();

    AsmStaticStorageVariable {
        name: bt_static_var.name.clone(),
        is_global: bt_static_var.is_global,
        alignment,
        init_value,
    }
}

fn generate_asm_static_constant(bt_static_constant: ir::BtStaticConstant, constants: &ConstantTable) -> AsmConstant {
    let (value, alignment) = match constants.get_constant_value_by_index(bt_static_constant.index) {
        ConstantValue::String { value } => {
            let ir::BtType::Array { element_type, count } = &bt_static_constant.data_type else {
                ICE!("Invalid BtType for constant string");
            };

            debug_assert!(matches!(element_type.as_ref(), ir::BtType::Int8 | ir::BtType::UInt8));

            let alignment = if *count >= 16 { 16 } else { 1 };

            (AsmConstantInitializer::AsciiString { value: value.clone(), byte_count: *count }, alignment)
        }

        ConstantValue::StringArray { values, .. } => {
            (AsmConstantInitializer::AsciiStringArray { values: values.clone() }, 1)
        }

        ConstantValue::F32 { value, alignment } => {
            (AsmConstantInitializer::Imm32 { value: value.to_bits(), signed: false }, *alignment)
        }

        ConstantValue::F64 { value, alignment } => {
            (AsmConstantInitializer::Imm64 { value: value.to_bits(), signed: false }, *alignment)
        }
    };

    let label = AsmLabelName(bt_static_constant.name);

    AsmConstant { label, alignment, value }
}

fn generate_asm_instructions(
    bluetac_instructions: &Vec<ir::BtInstruction>,
    asm_instructions: &mut Vec<AsmInstruction>,
    generator: &mut Generator,
) {
    for ir in bluetac_instructions {
        match ir {
            ir::BtInstruction::Return(val) => {
                let asm_type: AsmType = val.get_type(&generator.symbols).into();
                let dst_reg = if asm_type.is_floating_point() { HwRegister::XMM0 } else { HwRegister::RAX };

                let src_operand = generator.translate_bt_value_to_asm_operand(val);
                let dst_operand = AsmOperand::hw_reg(dst_reg, asm_type);

                asm_instructions.push(AsmInstruction::Mov { asm_type, src: src_operand, dst: dst_operand });
                asm_instructions.push(AsmInstruction::Ret);
            }

            ir::BtInstruction::SignExtend { src, dst } => {
                let src_type = src.get_type(&generator.symbols).into();
                let dst_type = dst.get_type(&generator.symbols).into();

                if src_type == dst_type {
                    ICE!("SignExtend has same types '{src_type}' '{dst_type}'");
                }

                let src_operand = generator.translate_bt_value_to_asm_operand(src);
                let dst_operand = generator.translate_bt_value_to_asm_operand(dst);

                asm_instructions.push(AsmInstruction::MovSx { src_type, dst_type, src: src_operand, dst: dst_operand });
            }

            ir::BtInstruction::ZeroExtend { src, dst } => {
                let src_type = src.get_type(&generator.symbols).into();
                let dst_type = dst.get_type(&generator.symbols).into();

                if src_type == dst_type {
                    ICE!("ZeroExtend has same types '{src_type}' '{dst_type}'");
                } else if src_type == AsmType::QuadWord {
                    ICE!("ZeroExtend cannot be used on QuadWord source operand");
                }

                let src_operand = generator.translate_bt_value_to_asm_operand(src);
                let dst_operand = generator.translate_bt_value_to_asm_operand(dst);

                asm_instructions.push(AsmInstruction::MovZx { src_type, dst_type, src: src_operand, dst: dst_operand });
            }

            ir::BtInstruction::Truncate { src, dst } => {
                let src_operand = generator.translate_bt_value_to_asm_operand(src);
                let dst_operand = generator.translate_bt_value_to_asm_operand(dst);

                // The truncation asm type is the `dst` operand type, i.e. what we're copying into.
                let asm_type = dst.get_type(&generator.symbols).into();

                // If the source operand is an immediate value then we'll truncate it ourselves first.
                //      Some assemblers will warn about using immediate values that are too large, e.g. using
                //      an 8-byte immediate with a `movl` to a 32-bit DWORD register.
                //
                let src_operand = match asm_type {
                    AsmType::Byte => {
                        if let AsmOperand::Imm { value, bits, .. } = src_operand
                            && bits > 8
                        {
                            AsmOperand::from_i8(type_conversion::convert_u64_to_i8(value))
                        } else {
                            src_operand
                        }
                    }
                    AsmType::Word => {
                        if let AsmOperand::Imm { value, bits, .. } = src_operand
                            && bits > 16
                        {
                            AsmOperand::from_i16(type_conversion::convert_u64_to_i16(value))
                        } else {
                            src_operand
                        }
                    }
                    AsmType::DoubleWord => {
                        if let AsmOperand::Imm { value, bits, .. } = src_operand
                            && bits > 32
                        {
                            AsmOperand::from_i32(type_conversion::convert_u64_to_i32(value))
                        } else {
                            src_operand
                        }
                    }
                    _ => src_operand,
                };

                asm_instructions.push(AsmInstruction::Mov { asm_type, src: src_operand, dst: dst_operand });
            }

            ir::BtInstruction::ConvertFp { src, dst } => {
                casts::fp_to_fp(src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::FpToSignedInteger { src, dst } => {
                casts::fp_to_signed_integer(src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::FpToUnsignedInteger { src, dst } => {
                casts::fp_to_unsigned_integer(src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::SignedIntegerToFp { src, dst } => {
                casts::signed_integer_to_fp(src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::UnsignedIntegerToFp { src, dst } => {
                casts::unsigned_integer_to_fp(src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::Unary { op, src, dst } => {
                unary_instr::generate_instruction(op, src, dst, asm_instructions, generator);
            }

            ir::BtInstruction::Binary { op, src1, src2, dst } => {
                binary_instr::generate_instruction(op, src1, src2, dst, asm_instructions, generator);
            }

            ir::BtInstruction::Jump { target } => {
                let target = translate_ir_label_to_target(target, generator);
                asm_instructions.push(AsmInstruction::Jmp { target });
            }

            ir::BtInstruction::JumpIfZero { condition, target } => {
                let asm_type: AsmType = condition.get_type(&generator.symbols).into();
                let condition = generator.translate_bt_value_to_asm_operand(condition);
                let target = translate_ir_label_to_target(target, generator);

                let and_then = || vec![AsmInstruction::JmpCC { cond_code: ConditionalCode::E, target }];
                let nan_handler = || vec![]; // NaN should always evaluate to nonzero, and the 'E' condition code tests for ZF.

                // cmp 0, src
                if asm_type.is_floating_point() {
                    compare::compare_with_zero_fp(
                        asm_type,
                        &condition,
                        and_then,
                        nan_handler,
                        asm_instructions,
                        generator,
                    );
                } else {
                    compare::compare_with_zero_integer(asm_type, &condition, and_then, asm_instructions);
                }
            }

            ir::BtInstruction::JumpIfNotZero { condition, target } => {
                let asm_type: AsmType = condition.get_type(&generator.symbols).into();
                let condition = generator.translate_bt_value_to_asm_operand(condition);
                let target = translate_ir_label_to_target(target, generator);

                let and_then =
                    || vec![AsmInstruction::JmpCC { cond_code: ConditionalCode::NE, target: target.clone() }];

                // NaN should always evaluate to nonzero, meaning NE is true. So if the PF is set then we jmp to the target.
                let nan_handler = || vec![AsmInstruction::Jmp { target: target.clone() }];

                // cmp 0, src
                if asm_type.is_floating_point() {
                    compare::compare_with_zero_fp(
                        asm_type,
                        &condition,
                        and_then,
                        nan_handler,
                        asm_instructions,
                        generator,
                    );
                } else {
                    compare::compare_with_zero_integer(asm_type, &condition, and_then, asm_instructions);
                }
            }

            // For now, we only implement a sequence of if-else comparisons.
            // Future: Jump table and binary tree, depending on heuristics about the density and number of cases.
            ir::BtInstruction::Switch { controlling_value, cases, default_label, break_label } => {
                let asm_type = controlling_value.get_type(&generator.symbols).into();
                let controlling_value = generator.translate_bt_value_to_asm_operand(controlling_value);

                // Compare each case and, if equal, jump to it.
                for case in cases {
                    let case_value = generator.translate_bt_value_to_asm_operand(&case.value);
                    let case_target = translate_ir_label_to_target(&case.label, generator);

                    asm_instructions.push(AsmInstruction::Cmp {
                        asm_type,
                        op1: case_value,
                        op2: controlling_value.clone(),
                    });
                    asm_instructions.push(AsmInstruction::JmpCC { cond_code: ConditionalCode::E, target: case_target });
                }

                // If there's a default label then unconditionally jump to it. If not, then unconditionally
                // jump to the end of the switch statement (the break) so that none of its body is executed.
                if default_label.is_some() {
                    let default_target = translate_ir_label_to_target(default_label.as_ref().unwrap(), generator);
                    asm_instructions.push(AsmInstruction::Jmp { target: default_target });
                } else {
                    let break_target = translate_ir_label_to_target(break_label, generator);
                    asm_instructions.push(AsmInstruction::Jmp { target: break_target });
                }
            }

            ir::BtInstruction::Copy { src, dst } => {
                let asm_type = src.get_type(&generator.symbols).into();
                let src = generator.translate_bt_value_to_asm_operand(src);
                let dst = generator.translate_bt_value_to_asm_operand(dst);
                asm_instructions.push(AsmInstruction::Mov { asm_type, src, dst });
            }

            ir::BtInstruction::Load { src_ptr, dst } => {
                let asm_type = dst.get_type(&generator.symbols).into();
                let src_ptr = generator.translate_bt_value_to_asm_operand(src_ptr);
                let dst = generator.translate_bt_value_to_asm_operand(dst);

                asm_instructions.push(AsmInstruction::Mov {
                    asm_type: AsmType::QuadWord,
                    src: src_ptr,
                    dst: AsmOperand::Reg(HwRegister::RAX),
                });

                asm_instructions.push(AsmInstruction::Mov {
                    asm_type,
                    src: AsmOperand::Memory { base: HwRegister::RAX, relative: 0 },
                    dst,
                });
            }

            ir::BtInstruction::Store { src, dst_ptr } => {
                let asm_type = src.get_type(&generator.symbols).into();
                let src = generator.translate_bt_value_to_asm_operand(src);
                let dst_ptr = generator.translate_bt_value_to_asm_operand(dst_ptr);

                asm_instructions.push(AsmInstruction::Mov {
                    asm_type: AsmType::QuadWord,
                    src: dst_ptr,
                    dst: AsmOperand::Reg(HwRegister::RAX),
                });

                asm_instructions.push(AsmInstruction::Mov {
                    asm_type,
                    src,
                    dst: AsmOperand::Memory { base: HwRegister::RAX, relative: 0 },
                });
            }

            ir::BtInstruction::StoreAddress { src, dst_ptr } => {
                let src = if src.get_type(&generator.symbols).is_array() {
                    make_pseudo_memory_operand(src, 0)
                } else {
                    generator.translate_bt_value_to_asm_operand(src)
                };

                let dst = generator.translate_bt_value_to_asm_operand(dst_ptr);
                asm_instructions.push(AsmInstruction::Lea { src, dst });
            }

            ir::BtInstruction::StoreAtOffset { src, dst_ptr, dst_offset } => {
                let asm_type = src.get_type(&generator.symbols).into();
                let src = generator.translate_bt_value_to_asm_operand(src);
                let dst = make_pseudo_memory_operand(dst_ptr, *dst_offset);
                asm_instructions.push(AsmInstruction::Mov { asm_type, src, dst });
            }

            ir::BtInstruction::AddPtr { src_ptr, index, scale, dst_ptr } => {
                let index_asm_type = index.get_type(&generator.symbols).into();

                let base = generator.translate_bt_value_to_asm_operand(src_ptr);
                let index = generator.translate_bt_value_to_asm_operand(index);
                let dst = generator.translate_bt_value_to_asm_operand(dst_ptr);

                // Move the base src_ptr into RAX
                let asm_type = AsmType::QuadWord;
                let rax_reg = AsmOperand::hw_reg(HwRegister::RAX, asm_type);
                asm_instructions.push(AsmInstruction::Mov { asm_type, src: base, dst: rax_reg });

                // If the index is a constant (immediate value) then we can calculate `index * scale` at compile-time
                // and use base-relative addressing.
                if let AsmOperand::Imm { value, .. } = &index {
                    let constant_index = *value as i32;
                    let scale = *scale as i32;
                    let relative = constant_index * scale;
                    let memory = AsmOperand::Memory { base: HwRegister::RAX, relative };

                    asm_instructions.push(AsmInstruction::Lea { src: memory, dst });
                } else {
                    // Move and sign-extend the index into RBX
                    asm_instructions.push(AsmInstruction::MovSx {
                        src_type: index_asm_type,
                        dst_type: AsmType::QuadWord,
                        src: index,
                        dst: AsmOperand::hw_reg(HwRegister::RBX, AsmType::QuadWord),
                    });

                    // LEA supports a scale of 1,2,4,8. For anything else we'll have to generate a Mul instruction.
                    if is_supported_lea_scale(*scale) {
                        let idx = AsmOperand::Indexed { base: HwRegister::RAX, index: HwRegister::RBX, scale: *scale };
                        asm_instructions.push(AsmInstruction::Lea { src: idx, dst });
                    } else {
                        asm_instructions.push(AsmInstruction::Binary {
                            op: AsmBinaryOp::Mul,
                            asm_type: AsmType::QuadWord,
                            src: AsmOperand::from_u64(*scale as u64),
                            dst: AsmOperand::hw_reg(HwRegister::RBX, AsmType::QuadWord),
                        });

                        let idx = AsmOperand::Indexed { base: HwRegister::RAX, index: HwRegister::RBX, scale: 1 };
                        asm_instructions.push(AsmInstruction::Lea { src: idx, dst });
                    }
                }
            }

            ir::BtInstruction::Label { id } => {
                let id = translate_ir_label_to_target(id, generator);
                asm_instructions.push(AsmInstruction::Label { id });
            }

            ir::BtInstruction::FunctionCall { designator, args, dst } => {
                functions::generate_function_call(designator, args, dst, asm_instructions, generator);
            }
        }
    }
}

fn translate_ir_label_to_target(target: &ir::BtLabelIdentifier, generator: &mut Generator) -> AsmLabelName {
    // Future: Add asm comment with the original IR label name (target.0)
    generator.labels.translate_ir_label_into_function_local_label(target)
}

fn make_pseudo_memory_operand(value: &ir::BtValue, offset: usize) -> AsmOperand {
    let ir::BtValue::Variable(name) = value else {
        ICE!("Expected a BtValue::Variable");
    };

    AsmOperand::PseudoMemory { name: name.clone(), offset }
}

/// Replaces the pseudo operands with either stack-allocated variables or Data operands, and returns the required
/// amount of stack space that the function needs to allocate.
fn replace_pseudo_operands(function: &mut AsmFunction, symbols: &AsmSymbolTable) -> usize {
    let mut pseudo_stack_map = HashMap::new();
    let mut stack_addr = 0;

    let instructions = &mut function.instructions;
    for instr in instructions {
        match instr {
            AsmInstruction::Mov { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::MovSx { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::MovZx { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Lea { src, dst } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Cvtfp2si { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Cvtsi2fp { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Cvtfp2fp { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Unary { operand, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Binary { src, dst, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, src, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, dst, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Cmp { op1, op2, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, op1, symbols, &mut pseudo_stack_map);
                stack_addr = replace_pseudo_operand(stack_addr, op2, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::IDiv { operand, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Div { operand, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::SetCC { operand, .. } => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Call(operand) => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            AsmInstruction::Push(operand) => {
                stack_addr = replace_pseudo_operand(stack_addr, operand, symbols, &mut pseudo_stack_map);
            }

            // These instructions have no operands
            AsmInstruction::Cdq { .. } => (),
            AsmInstruction::Jmp { .. } => (),
            AsmInstruction::JmpCC { .. } => (),
            AsmInstruction::Label { .. } => (),
            AsmInstruction::Ret => (),
        }
    }

    stack_addr.unsigned_abs() as usize
}

fn replace_pseudo_operand(
    mut stack_addr: i32,
    operand: &mut AsmOperand,
    symbols: &AsmSymbolTable,
    pseudo_stack_map: &mut HashMap<String, i32>,
) -> i32 {
    if !matches!(operand, AsmOperand::Pseudo(_) | AsmOperand::PseudoMemory { .. }) {
        return stack_addr;
    }

    let (name, offset) = match operand {
        AsmOperand::Pseudo(name) => (name, None),
        AsmOperand::PseudoMemory { name, offset } => (name, Some(*offset)),
        _ => unreachable!("Checked above for Pseudo or PseudoMemory operands"),
    };

    // If this pseudo operand holds a function name then replace it with the appropriate operand
    if let Some(AsmSymbol::Function { .. }) = symbols.get(name) {
        *operand = AsmOperand::Function(name.clone());
        return stack_addr;
    }

    let Some(AsmSymbol::Object { asm_type, is_static, .. }) = symbols.get(name) else {
        ICE!("AsmSymbolTable does not contain entry for '{name}'");
    };

    // If this pseudo operand is for a variable with a static storage duration then replace it with a Data operand.
    // Don't add it to the `pseudo_stack_map`; that's only for pseudo operands we replace with stack variables.
    //
    if *is_static {
        *operand = AsmOperand::Data { symbol: name.clone(), relative: 0 };
        return stack_addr;
    };

    // If we have an offset from a PseudoMemory operand (for an aggregate type) then add the offset to the
    // given stack address.
    let add_offset = |addr: i32| -> i32 { if let Some(offset) = offset { addr + offset as i32 } else { addr } };

    // Do we already have a stack variable for the pseudo operand? If so, return it's existing address.
    if let Some(existing_stack_addr) = pseudo_stack_map.get(name) {
        *operand = AsmOperand::stack_address(add_offset(*existing_stack_addr));
        return stack_addr;
    }

    let variable_size = asm_type.size_bytes() as i32;
    let variable_alignment = asm_type.alignment_bytes() as i32;

    // First make space for the variable
    stack_addr -= variable_size;

    // Next ensure the stack address is aligned for the variable.
    stack_addr = align_stack_addr(stack_addr, variable_alignment);

    // Keep track of the variable's location
    pseudo_stack_map.insert(name.clone(), stack_addr);

    // Return the variable's location with optional offset for an aggregate type.
    *operand = AsmOperand::stack_address(add_offset(stack_addr));

    stack_addr
}

/// Aligns the relative stack address (a value <= 0) to the given alignment.
pub(super) fn align_stack_addr(stack_addr: i32, alignment: i32) -> i32 {
    assert!(stack_addr <= 0);
    assert!(alignment > 0);

    if stack_addr % alignment == 0 {
        return stack_addr;
    }

    (stack_addr - alignment + 1) / alignment * alignment
}

fn allocate_stack_space(function: &mut AsmFunction, stack_space: usize) {
    // The stack pointer needs to be 16-byte aligned, so we'll round up the stack space if necessary.
    let bytes_to_allocate = round_up_to_multiple_of_16(stack_space) as u64;

    // Insert the allocation at the very beginning.
    function.instructions.insert(
        0,
        AsmInstruction::Binary {
            op: AsmBinaryOp::Sub,
            asm_type: AsmType::QuadWord,
            src: AsmOperand::from_u64(bytes_to_allocate),
            dst: AsmOperand::Reg(HwRegister::RSP),
        },
    );
}

fn is_supported_lea_scale(scale: usize) -> bool {
    matches!(scale, 1 | 2 | 4 | 8)
}

pub(super) fn round_up_to_multiple_of_16(x: usize) -> usize {
    (x + 15) & !15
}
