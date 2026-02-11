// Copyright 2025-2026 Neil Henderson
//
//! The `emit` module provides x86_64 assembly emission functionality to the parent codegen module.

use std::io::Result;

use crate::ICE;

use super::ast::*;
use super::file_writer::{
    AsmDataDefinitionDirective, AsmFileWriter, AsmSectionDirective, AsmStorageReservationDirective,
};
use super::symbols::AsmSymbolTable;

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
    let bss = static_var.init_value.len() == 1 && static_var.init_value[0].is_zero();
    if bss {
        writer.write_section_directive(AsmSectionDirective::Bss)?;
    } else {
        writer.write_section_directive(AsmSectionDirective::Data)?;
    }

    // Alignment
    if static_var.alignment > 1 {
        writer.write_byte_alignment_directive(static_var.alignment)?;
    }

    // Name
    writer.write_symbol_label(variable_name)?;

    // Value
    if bss {
        let byte_count = static_var.init_value[0].size_bytes();
        writer.write_storage_directive(AsmStorageReservationDirective::Zero { byte_count })?;
    } else {
        for ini in &static_var.init_value {
            match ini {
                AsmConstantInitializer::ZeroBytes(size) => {
                    writer.write_storage_directive(AsmStorageReservationDirective::Zero { byte_count: *size })?;
                }
                AsmConstantInitializer::Imm8 { value, signed, .. } => {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::Byte(*value), *signed)?
                }
                AsmConstantInitializer::Imm16 { value, signed, .. } => {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::Word(*value), *signed)?
                }
                AsmConstantInitializer::Imm32 { value, signed, .. } => {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::DoubleWord(*value), *signed)?
                }
                AsmConstantInitializer::Imm64 { value, signed, .. } => {
                    writer.write_data_definition_directive(AsmDataDefinitionDirective::QuadWord(*value), *signed)?
                }
                AsmConstantInitializer::AddressOf { object, byte_offset } => writer.write_data_definition_directive(
                    AsmDataDefinitionDirective::AddressConstant { object: object.clone(), byte_offset: *byte_offset },
                    false,
                )?,
                AsmConstantInitializer::AsciiString { value, .. } => {
                    let (string, implicit_null) = strip_null_terminator_if_present(value);
                    let directive = AsmDataDefinitionDirective::Ascii { string, implicit_null };
                    writer.write_data_definition_directive(directive, false)?
                }
                AsmConstantInitializer::AsciiStringArray { .. } => ICE!("AsciiStringArray used for static variable"),
            }
        }
    }

    writer.write_blank_line()?;

    Ok(())
}

fn emit_asm_constant(writer: &mut AsmFileWriter, constant: &AsmConstant) -> Result<()> {
    // Readonly data section
    //
    if constant.is_string() {
        let is_null_terminated = constant.is_null_terminated_string();
        writer.write_section_directive(AsmSectionDirective::ReadOnlyString { is_null_terminated })?;
    } else {
        writer.write_section_directive(AsmSectionDirective::ReadOnlyData { align: constant.alignment })?;
    }

    // Alignment
    if constant.alignment > 1 {
        writer.write_byte_alignment_directive(constant.alignment)?;
    }

    // Label
    writer.write_local_symbol_label(&constant.label)?;

    // Value
    let (value_byte_size, add_padding) = match &constant.value {
        AsmConstantInitializer::Imm8 { value, signed, .. } => {
            writer.write_data_definition_directive(AsmDataDefinitionDirective::Byte(*value), *signed)?;
            (1, true)
        }
        AsmConstantInitializer::Imm16 { value, signed, .. } => {
            writer.write_data_definition_directive(AsmDataDefinitionDirective::Word(*value), *signed)?;
            (2, true)
        }
        AsmConstantInitializer::Imm32 { value, signed, .. } => {
            writer.write_data_definition_directive(AsmDataDefinitionDirective::DoubleWord(*value), *signed)?;
            (4, true)
        }
        AsmConstantInitializer::Imm64 { value, signed, .. } => {
            writer.write_data_definition_directive(AsmDataDefinitionDirective::QuadWord(*value), *signed)?;
            (8, true)
        }
        AsmConstantInitializer::AsciiString { value, .. } => {
            let (string, implicit_null) = strip_null_terminator_if_present(value);
            let directive = AsmDataDefinitionDirective::Ascii { string, implicit_null };
            writer.write_data_definition_directive(directive, false)?;
            (0, false)
        }
        AsmConstantInitializer::AsciiStringArray { values } => {
            for v in values {
                let (string, implicit_null) = strip_null_terminator_if_present(v);
                let directive = AsmDataDefinitionDirective::Ascii { string, implicit_null };
                writer.write_data_definition_directive(directive, false)?;
            }
            (0, false)
        }
        AsmConstantInitializer::AddressOf { .. } => ICE!("AddressConstant used for AsmConstant"),
        AsmConstantInitializer::ZeroBytes { .. } => ICE!("ZeroBytes used for AsmConstant"),
    };

    // Padding on macOS
    //      E.g. If we have a `.literal16` section but only write an 8-byte value, we need to pad out the remainder.
    //
    if cfg!(target_os = "macos") && add_padding && value_byte_size < constant.alignment {
        let padding = constant.alignment - value_byte_size;
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

fn strip_null_terminator_if_present(value: &str) -> (String, bool) {
    if value.ends_with("\\0") {
        return (value.strip_suffix("\\0").unwrap().to_owned(), true);
    }

    if value.ends_with("\\00") {
        return (value.strip_suffix("\\00").unwrap().to_owned(), true);
    }

    if value.ends_with("\\000") {
        return (value.strip_suffix("\\000").unwrap().to_owned(), true);
    }

    (value.to_string(), false)
}
