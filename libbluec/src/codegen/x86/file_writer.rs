// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `file_writer` module provides functionality to write x86_64 assembly to a file.

use crate::ICE;
use super::ast::{AsmBinaryOp, AsmInstruction, AsmLabelName, AsmOperand, AsmType, AsmUnaryOp, ConditionalCode};
use super::symbols::{AsmSymbol, AsmSymbolTable};

use std::fs::File;
use std::io::{BufWriter, Result, Write};

const INDENT: &str = "    ";

/// Section directives.
pub enum AsmSectionDirective {
    Bss,
    Data,
    ReadOnlyData { align: usize },
    Text,
}

/// Data definition directives.
pub enum AsmDataDefinitionDirective {
    Word(u16),
    DoubleWord(u32), // Aka LongWord
    QuadWord(u64),
    AddressConstant { object: String }
}

/// Space filling / storage reservation directives.
pub enum AsmStorageReservationDirective {
    Zero { byte_count: usize },
}

/// An x86_64 assembly code file writer.
pub struct AsmFileWriter {
    writer: BufWriter<File>,
    symbols: AsmSymbolTable,
}

impl AsmFileWriter {
    /// Creates a new file writer.
    pub fn new(asm_file: &str, symbol_table: AsmSymbolTable) -> Self {
        let file = File::create(asm_file).expect("Cannot open .s file for writing");
        let writer = BufWriter::new(file);

        Self { writer, symbols: symbol_table }
    }

    /// Writes a section directive.
    pub fn write_section_directive(&mut self, directive: AsmSectionDirective) -> Result<()> {
        match directive {
            AsmSectionDirective::Bss => self.writeln_with_indent(".bss"),
            AsmSectionDirective::Data => self.writeln_with_indent(".data"),
            AsmSectionDirective::ReadOnlyData { align } => {
                if cfg!(target_os = "macos") {
                    match align {
                        4 | 8 | 16 => self.writeln_with_indent(&format!(".literal{align}")),
                        _ => ICE!("Invalid ReadOnlyData alignment {align}"),
                    }
                } else {
                    self.writeln_with_indent(".section .rodata")
                }
            }
            AsmSectionDirective::Text => self.writeln_with_indent(".text"),
        }
    }

    /// Writes a `.balign` directive to align on the given byte `alignment` boundary.
    pub fn write_byte_alignment_directive(&mut self, alignment: usize) -> Result<()> {
        writeln!(self.writer, "{INDENT}.balign {alignment}")
    }

    /// Writes a storage reservation directive.
    pub fn write_storage_directive(&mut self, directive: AsmStorageReservationDirective) -> Result<()> {
        match directive {
            AsmStorageReservationDirective::Zero { byte_count } => writeln!(self.writer, "{INDENT}.zero {byte_count}"),
        }
    }

    /// Writes a data definition directive.
    pub fn write_data_definition_directive(
        &mut self,
        directive: AsmDataDefinitionDirective,
        print_signed: bool,
    ) -> Result<()> {
        match directive {
            AsmDataDefinitionDirective::Word(value) => {
                if print_signed {
                    writeln!(self.writer, "{INDENT}.word {}", value as i16)
                } else {
                    writeln!(self.writer, "{INDENT}.word {value}")
                }
            }
            AsmDataDefinitionDirective::DoubleWord(value) => {
                if print_signed {
                    writeln!(self.writer, "{INDENT}.long {}", value as i32)
                } else {
                    let value_hex = format!("{:#x}", value);
                    let value_f32 = f32::from_bits(value);
                    writeln!(self.writer, "{INDENT}.long {value}    # {value_hex} [{:.}_f32]", value_f32)
                }
            }
            AsmDataDefinitionDirective::QuadWord(value) => {
                if print_signed {
                    writeln!(self.writer, "{INDENT}.quad {}", value as i64)
                } else {
                    let value_hex = format!("{:#x}", value);
                    let value_f64 = f64::from_bits(value);
                    writeln!(self.writer, "{INDENT}.quad {value}    # {value_hex} [{:.}_f64]", value_f64)
                }
            }
            AsmDataDefinitionDirective::AddressConstant { object } => {
                writeln!(self.writer, "{INDENT}.quad {}", make_asm_identifier(&object))
            }
        }
    }

    /// Writes a global directive for the given symbol name.
    pub fn write_global_directive(&mut self, symbol_name: &str) -> Result<()> {
        writeln!(self.writer, "{INDENT}.globl {}", make_asm_identifier(symbol_name))
    }

    /// Writes a label for a symbol.
    ///
    /// The `symbol_name` argument should _NOT_ include the `:` character.
    ///
    /// If the symbol should be global then call `write_global_directive` before calling this function.
    pub fn write_symbol_label(&mut self, symbol_name: &str) -> Result<()> {
        debug_assert!(!symbol_name.contains(':'));
        writeln!(self.writer, "{}:", make_asm_identifier(symbol_name))
    }

    /// Writes a label for a local symbol.
    ///
    /// A local label has an "L" (macOS) or ".L" (Linux) prefix.
    /// Local symbols are defined and used within the assembler, but they are normally not saved in object files.
    /// Note: This does not refer to local/global as in visibility to the linker, but whether the symbol is saved
    /// in the object file and hence usable by the debugger.
    pub fn write_local_symbol_label(&mut self, label: &AsmLabelName) -> Result<()> {
        writeln!(self.writer, "{}:", make_asm_local_symbol_label(&label.0))
    }

    /// Writes an instruction.
    pub fn write_instruction(&mut self, instr: &AsmInstruction) -> Result<()> {
        match instr {
            AsmInstruction::Mov { asm_type, src, dst } => {
                self.write_binary_instruction(&make_asm_instr("mov", asm_type), src, dst)
            }

            AsmInstruction::MovSx { src_type, dst_type, src, dst } => {
                self.write_binary_instruction(&make_two_operand_asm_instr("movs", src_type, dst_type), src, dst)
            }

            AsmInstruction::MovZx { src_type, dst_type, src, dst } => {
                self.write_binary_instruction(&make_two_operand_asm_instr("movz", src_type, dst_type), src, dst)
            }

            AsmInstruction::Lea { src, dst } => {
                debug_assert!(src.is_memory_address());
                debug_assert!(dst.is_hw_register());

                self.write_binary_instruction(&make_asm_instr("lea", &AsmType::QuadWord), src, dst)
            }

            AsmInstruction::Cvtfp2fp { src_type, dst_type, src, dst } => {
                let mnemonic = match src_type {
                    AsmType::FpSingle => "cvtss2",
                    AsmType::FpDouble => "cvtsd2",
                    _ => ICE!("Invalid src type '{src_type}' for AsmInstruction::Cvtfp2fp"),
                };
                self.write_binary_instruction(&make_asm_instr(mnemonic, dst_type), src, dst)
            }

            AsmInstruction::Cvtfp2si { src_type, dst_type, src, dst } => {
                let mnemonic = match src_type {
                    AsmType::FpSingle => "cvttss2si",
                    AsmType::FpDouble => "cvttsd2si",
                    _ => ICE!("Invalid src type '{src_type}' for AsmInstruction::Cvtfp2si"),
                };
                self.write_binary_instruction(&make_asm_instr(mnemonic, dst_type), src, dst)
            }

            AsmInstruction::Cvtsi2fp { src_type, dst_type, src, dst } => {
                let mnemonic = match dst_type {
                    AsmType::FpSingle => "cvtsi2ss",
                    AsmType::FpDouble => "cvtsi2sd",
                    _ => ICE!("Invalid dst type '{dst_type}' for AsmInstruction::Cvtsi2fp"),
                };
                self.write_binary_instruction(&make_asm_instr(mnemonic, src_type), src, dst)
            }

            AsmInstruction::Cmp { asm_type: AsmType::FpSingle, op1, op2 } => {
                self.write_binary_instruction("ucomiss", op1, op2)
            }

            AsmInstruction::Cmp { asm_type: AsmType::FpDouble, op1, op2 } => {
                self.write_binary_instruction("ucomisd", op1, op2)
            }

            AsmInstruction::Cmp { asm_type, op1, op2 } => {
                self.write_binary_instruction(&make_asm_instr("cmp", asm_type), op1, op2)
            }

            AsmInstruction::IDiv { asm_type, operand } => {
                self.write_unary_instruction(&make_asm_instr("idiv", asm_type), operand)
            }

            AsmInstruction::Div { asm_type, operand } => {
                self.write_unary_instruction(&make_asm_instr("div", asm_type), operand)
            }

            AsmInstruction::Unary { op, asm_type, operand } => {
                self.write_unary_instruction(&asm_instr_for_unary_op(op, asm_type), operand)
            }

            AsmInstruction::Binary { op, asm_type, src, dst } => {
                self.write_binary_instruction(&asm_instr_for_binary_op(op, asm_type), src, dst)
            }

            AsmInstruction::Push(operand) => self.write_unary_instruction("pushq", operand),

            AsmInstruction::Cdq { asm_type } => match asm_type {
                AsmType::Word => self.write_nullary_instruction("cwd"),
                AsmType::DoubleWord => self.write_nullary_instruction("cdq"),
                AsmType::QuadWord => self.write_nullary_instruction("cqo"),
                _ => ICE!("Wrong type '{asm_type}' for Cdq"),
            },

            AsmInstruction::Jmp { target } => {
                self.writeln_with_indent(&format!("jmp {}", make_asm_local_symbol_label(&target.0)))
            }

            AsmInstruction::JmpCC { cond_code, target } => self.writeln_with_indent(&format!(
                "{} {}",
                make_asm_conditional_jmp(cond_code),
                make_asm_local_symbol_label(&target.0)
            )),

            AsmInstruction::SetCC { cond_code, operand } => {
                // If the operand is a register then we have to transform it to an 8-bit register alias, since
                // conditional SET instructions only support 8-bit register operands.
                let operand = if let AsmOperand::Reg(hw_reg) = operand
                    && hw_reg.size_bits() != 8
                {
                    AsmOperand::hw_reg(*hw_reg, AsmType::Byte)
                } else {
                    operand.clone()
                };

                self.writeln_with_indent(&format!(
                    "{} {}",
                    make_asm_conditional_set(cond_code),
                    self.asm_operand_to_string(&operand)
                ))
            }

            AsmInstruction::Label { id } => self.write_local_symbol_label(id),

            AsmInstruction::Call(identifier) => {
                let function_name = if cfg!(target_os = "macos") {
                    // This saves us calling `make_asm_identifier`.
                    format!("_{}", identifier)
                } else if cfg!(target_os = "linux") {
                    // On Linux, if the function is not defined in this translation unit then we append "@PLT"
                    // to the function name to signal that the linker may need to lookup the function in the
                    // Procedure Linkage Table, if it's defined in a shared library. For static libraries or
                    // other object files linked with this one, "@PLT" is harmless and has no effect.
                    //
                    let Some(AsmSymbol::Function { is_defined }) = self.symbols.get(identifier) else {
                        ICE!("Missing function symbol");
                    };

                    if *is_defined { identifier.to_string() } else { format!("{}@PLT", &identifier) }
                } else {
                    ICE!("Unsupported platform");
                };

                self.writeln_with_indent(&format!("call {function_name}"))
            }

            AsmInstruction::Ret => {
                self.writeln_with_indent("movq %rbp, %rsp")?;
                self.writeln_with_indent("popq %rbp")?;
                self.write_nullary_instruction("ret")?;
                Ok(())
            }
        }
    }

    /// Writes the given string as a line with indentation.
    pub fn writeln_with_indent(&mut self, line: &str) -> Result<()> {
        writeln!(self.writer, "{INDENT}{line}")
    }

    /// Writes a blank line.
    pub fn write_blank_line(&mut self) -> Result<()> {
        writeln!(self.writer)
    }

    /// Flushes the output stream.
    pub fn flush(&mut self) -> Result<()> {
        self.writer.flush()
    }

    fn write_nullary_instruction(&mut self, instr: &str) -> Result<()> {
        writeln!(self.writer, "{INDENT}{instr}")
    }

    fn write_unary_instruction(&mut self, instr: &str, operand: &AsmOperand) -> Result<()> {
        writeln!(self.writer, "{INDENT}{instr} {}", self.asm_operand_to_string(operand))
    }

    fn write_binary_instruction(&mut self, instr: &str, operand1: &AsmOperand, operand2: &AsmOperand) -> Result<()> {
        writeln!(
            self.writer,
            "{INDENT}{instr} {}, {}",
            self.asm_operand_to_string(operand1),
            self.asm_operand_to_string(operand2)
        )
    }

    fn asm_operand_to_string(&self, operand: &AsmOperand) -> String {
        match operand {
            AsmOperand::Imm(int) => format!("${}", int),

            AsmOperand::Reg(hw_register) => hw_register.to_string(),

            AsmOperand::Memory { base, relative } => {
                if *relative == 0 {
                    format!("({base})")
                } else {
                    format!("{relative}({base})")
                }
            }

            AsmOperand::Data(name) => {
                let Some(symbol) = self.symbols.get(name) else {
                    ICE!("Symbol '{name}' not found in back-end symbol table");
                };

                let AsmSymbol::Object { is_constant, .. } = symbol else {
                    ICE!("Symbol '{name}' not an AsmSymbol::Object");
                };

                if *is_constant {
                    format!("{}(%rip)", make_asm_local_symbol_label(name))
                } else {
                    format!("{}(%rip)", make_asm_identifier(name))
                }
            }

            AsmOperand::Pseudo(name) => {
                ICE!("AsmOperand::Pseudo {} should have been replaced", name);
            }
        }
    }
}

fn make_asm_instr(instr_base: &str, asm_type: &AsmType) -> String {
    format!("{instr_base}{}", asm_type.asm_operand_str())
}

fn make_two_operand_asm_instr(instr_base: &str, operand1_type: &AsmType, operand2_type: &AsmType) -> String {
    format!("{instr_base}{}{}", operand1_type.asm_operand_str(), operand2_type.asm_operand_str())
}

fn make_asm_identifier(identifier: &str) -> String {
    if cfg!(target_os = "macos") { format!("_{}", identifier) } else { identifier.to_string() }
}

fn make_asm_local_symbol_label(identifier: &str) -> String {
    if cfg!(target_os = "macos") { format!("L{}", identifier) } else { format!(".L{}", identifier) }
}

fn asm_instr_for_unary_op(op: &AsmUnaryOp, asm_type: &AsmType) -> String {
    match op {
        AsmUnaryOp::Neg => make_asm_instr("neg", asm_type),
        AsmUnaryOp::Not => make_asm_instr("not", asm_type),
        AsmUnaryOp::Shr => make_asm_instr("shr", asm_type),
    }
}

fn asm_instr_for_binary_op(op: &AsmBinaryOp, asm_type: &AsmType) -> String {
    match op {
        // Floating-point
        AsmBinaryOp::Mul if asm_type.is_floating_point() => make_asm_instr("mul", asm_type),
        AsmBinaryOp::Xor if asm_type.is_floating_point() => {
            if asm_type == &AsmType::FpSingle {
                "xorps".to_string()
            } else if asm_type == &AsmType::FpDouble {
                "xorpd".to_string()
            } else {
                ICE!("No fp xor instruction for '{asm_type}'");
            }
        }
        AsmBinaryOp::DivFp => make_asm_instr("div", asm_type),

        // Integer
        AsmBinaryOp::Add => make_asm_instr("add", asm_type),
        AsmBinaryOp::Sub => make_asm_instr("sub", asm_type),
        AsmBinaryOp::Mul => make_asm_instr("imul", asm_type),
        AsmBinaryOp::And => make_asm_instr("and", asm_type),
        AsmBinaryOp::Or => make_asm_instr("or", asm_type),
        AsmBinaryOp::Xor => make_asm_instr("xor", asm_type),
        AsmBinaryOp::Shl => make_asm_instr("shl", asm_type),
        AsmBinaryOp::Shr => make_asm_instr("shr", asm_type),
        AsmBinaryOp::Sar => make_asm_instr("sar", asm_type),
    }
}

#[rustfmt::skip]
fn make_asm_conditional_jmp(cond_code: &ConditionalCode) -> &'static str {
    match cond_code {
        ConditionalCode::E   => "je",
        ConditionalCode::NE  => "jne",
        ConditionalCode::L   => "jl",
        ConditionalCode::LE  => "jle",
        ConditionalCode::G   => "jg",
        ConditionalCode::GE  => "jge",
        ConditionalCode::A   => "ja",
        ConditionalCode::AE  => "jae",
        ConditionalCode::B   => "jb",
        ConditionalCode::BE  => "jbe",
        ConditionalCode::P   => "jp",
    }
}

#[rustfmt::skip]
fn make_asm_conditional_set(cond_code: &ConditionalCode) -> &'static str {
    match cond_code {
        ConditionalCode::E   => "sete",
        ConditionalCode::NE  => "setne",
        ConditionalCode::L   => "setl",
        ConditionalCode::LE  => "setle",
        ConditionalCode::G   => "setg",
        ConditionalCode::GE  => "setge",
        ConditionalCode::A   => "seta",
        ConditionalCode::AE  => "setae",
        ConditionalCode::B   => "setb",
        ConditionalCode::BE  => "setbe",
        ConditionalCode::P   => "setp",
    }
}
