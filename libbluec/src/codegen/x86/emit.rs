// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `emit` module provides x86_64 assembly emission functionality to the parent codegen module.

use crate::ICE;

use super::ast::*;
use super::file_writer::{
    AsmDataDefinitionDirective, AsmFileWriter, AsmSectionDirective, AsmStorageReservationDirective,
};
use super::symbols::AsmSymbolTable;

use std::io::Result;

/// Emits the x86_64 assembly code for the given program and writes it to an '.s' file.
pub fn emit_asm_to_file(asm_root: &AsmRoot, asm_file: &str, symbol_table: AsmSymbolTable) {
    let mut writer = AsmFileWriter::new(asm_file, symbol_table);

    if let Err(e) = emit_asm_root(&mut writer, asm_root) {
        eprintln!("{}", e);
    }

    writer.flush().expect("Did not flush writes to .s file")
}

fn emit_asm_root(writer: &mut AsmFileWriter, asm_root: &AsmRoot) -> Result<()> {
    let asm_definitions = &asm_root.0;
    for defn in asm_definitions {
        match defn {
            AsmDefinition::Function(function) => emit_asm_function(writer, function)?,
            AsmDefinition::StaticVariable(static_var) => emit_asm_static_variable(writer, static_var)?,
            AsmDefinition::StaticConstant(constant) => emit_asm_constant(writer, constant)?,
        }
    }

    #[cfg(all(target_os = "linux", not(target_os = "macos")))]
    emit_asm_nonexecutable_stack(writer)?;

    Ok(())
}

fn emit_asm_function(writer: &mut AsmFileWriter, function: &AsmFunction) -> Result<()> {
    let function_name = &function.name;

    writer.write_section_directive(AsmSectionDirective::Text)?;

    if function.is_global {
        writer.write_global_directive(function_name)?;
    }

    writer.write_symbol_label(function_name)?;

    // Prologue
    writer.writeln_with_indent("pushq %rbp")?;
    writer.writeln_with_indent("movq %rsp, %rbp")?;

    for instr in &function.instructions {
        writer.write_instruction(instr)?;
    }

    writer.write_blank_line()?;

    Ok(())
}

fn emit_asm_static_variable(writer: &mut AsmFileWriter, static_var: &AsmStaticStorageVariable) -> Result<()> {
    let variable_name = &static_var.name;

    if static_var.is_global {
        writer.write_global_directive(variable_name)?;
    }

    // Section is either .bss or .data depending on whether the static variable's value is zero.
    let bss = static_var.init_value.is_zero();
    if bss {
        writer.write_section_directive(AsmSectionDirective::Bss)?;
    } else {
        writer.write_section_directive(AsmSectionDirective::Data)?;
    }

    // Alignment
    writer.write_byte_alignment_directive(static_var.init_value.alignment_bytes())?;

    // Name
    writer.write_symbol_label(variable_name)?;

    // Value
    if bss {
        let byte_count = static_var.init_value.size_bytes();
        writer.write_storage_directive(AsmStorageReservationDirective::Zero { byte_count })?;
    } else {
        match static_var.init_value {
            AsmConstantInitializer::Imm16 { value, signed, .. } => {
                writer.write_data_definition_directive(AsmDataDefinitionDirective::Word(value), signed)?
            }
            AsmConstantInitializer::Imm32 { value, signed, .. } => {
                writer.write_data_definition_directive(AsmDataDefinitionDirective::DoubleWord(value), signed)?
            }
            AsmConstantInitializer::Imm64 { value, signed, .. } => {
                writer.write_data_definition_directive(AsmDataDefinitionDirective::QuadWord(value), signed)?
            }
            AsmConstantInitializer::AddressConstant { ref object } => {
                writer.write_data_definition_directive(AsmDataDefinitionDirective::AddressConstant { object: object.clone() }, false)?
            }
        };
    }

    writer.write_blank_line()?;

    Ok(())
}

fn emit_asm_constant(writer: &mut AsmFileWriter, constant: &AsmConstant) -> Result<()> {
    let align = match constant.value {
        AsmConstantInitializer::Imm16 { align, .. } => align,
        AsmConstantInitializer::Imm32 { align, .. } => align,
        AsmConstantInitializer::Imm64 { align, .. } => align,
        AsmConstantInitializer::AddressConstant { .. } => ICE!("AddressConstant used for AsmConstant")
    };

    // Readonly data section (e.g. `.rodata` or `.literal8/16` on macOS)
    writer.write_section_directive(AsmSectionDirective::ReadOnlyData { align })?;

    // Alignment
    writer.write_byte_alignment_directive(align)?;

    // Label
    writer.write_local_symbol_label(&constant.label)?;

    // Value
    let value_byte_size;
    match constant.value {
        AsmConstantInitializer::Imm16 { value, signed, .. } => {
            value_byte_size = 2;
            writer.write_data_definition_directive(AsmDataDefinitionDirective::Word(value), signed)?
        }
        AsmConstantInitializer::Imm32 { value, signed, .. } => {
            value_byte_size = 4;
            writer.write_data_definition_directive(AsmDataDefinitionDirective::DoubleWord(value), signed)?
        }
        AsmConstantInitializer::Imm64 { value, signed, .. } => {
            value_byte_size = 8;
            writer.write_data_definition_directive(AsmDataDefinitionDirective::QuadWord(value), signed)?
        }
        AsmConstantInitializer::AddressConstant { .. } => ICE!("AddressConstant used for AsmConstant")
    }

    // Padding on macOS
    //      E.g. If we have a `.literal16` section but only write an 8-byte value, we need to pad out the remainder.
    if cfg!(target_os = "macos") && value_byte_size < align {
        let padding = align - value_byte_size;
        match padding {
            8 => writer.write_data_definition_directive(AsmDataDefinitionDirective::QuadWord(0), false)?,
            n if n % 4 == 0 => {
                for _ in 0..padding / 4 {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::DoubleWord(0), false)?;
                }
            }
            n if n % 2 == 0 => {
                for _ in 0..padding / 2 {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::Word(0), false)?;
                }
            }
            _ => ICE!("Invalid padding {padding} for macOS constant data"),
        }
    }

    writer.write_blank_line()?;

    Ok(())
}

#[cfg(all(target_os = "linux", not(target_os = "macos")))]
fn emit_asm_nonexecutable_stack(writer: &mut AsmFileWriter) -> Result<()> {
    writer.write_blank_line()?;
    writer.writeln_with_indent(".section .note.GNU-stack,\"\",@progbits")?;

    Ok(())
}
