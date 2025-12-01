// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `printer` module provides functionality to print the BlueTac IR to stdout.

use crate::ICE;
use crate::parser::AstUniqueName;
use crate::sema::symbol_table::SymbolTable;

use super::*;

/// Prints the BlueTac IR to stdout.
pub fn print(bt_root: &BtRoot, symbols: &SymbolTable) {
    let bt_definitions = &bt_root.0;
    for defn in bt_definitions {
        match defn {
            BtDefinition::Function(function_defn) => print_function(function_defn, symbols),
            BtDefinition::StaticVariable(static_variable) => print_static_storage_variable(static_variable),
        }
    }
}

fn print_function(function: &BtFunctionDefn, symbols: &SymbolTable) {
    if function.is_global {
        print!("define pub symbol ");
    } else {
        print!("define internal symbol ");
    }

    print!("@{}", function.name);

    // Params
    let mut first = true;
    print!("(");
    for param in &function.params {
        if !first {
            print!(", ");
        }
        print!("{}: {}", param.1, bt_type_to_string(&param.0));
        first = false;
    }
    print!(")");

    // Return type
    println!(" -> {} {{", bt_type_to_string(&function.return_type));

    // Instructions
    for instr in &function.instructions {
        print_instruction(instr, symbols);
    }

    println!("}}");
    println!();
}

fn print_static_storage_variable(variable: &BtStaticStorageVariable) {
    if variable.is_global {
        print!("define pub symbol ");
    } else {
        print!("define internal symbol ");
    }

    println!("@{}: {} = {}", variable.name, bt_type_to_string(&variable.data_type), variable.init_value);
}

fn print_instruction(instr: &BtInstruction, symbols: &SymbolTable) {
    match instr {
        BtInstruction::Return(bt_value) => print_return(bt_value, symbols),
        BtInstruction::SignExtend { src, dst } => print_src_dst_instr("sign-ex", src, dst, symbols),
        BtInstruction::ZeroExtend { src, dst } => print_src_dst_instr("zero-ex", src, dst, symbols),
        BtInstruction::Truncate { src, dst } => print_src_dst_instr("trunc", src, dst, symbols),
        BtInstruction::ConvertFp { src, dst } => print_src_dst_instr("cast-fp", src, dst, symbols),
        BtInstruction::FpToSignedInteger { src, dst } => print_src_dst_instr("cast-fp-si", src, dst, symbols),
        BtInstruction::FpToUnsignedInteger { src, dst } => print_src_dst_instr("cast-fp-ui", src, dst, symbols),
        BtInstruction::SignedIntegerToFp { src, dst } => print_src_dst_instr("cast-si-fp", src, dst, symbols),
        BtInstruction::UnsignedIntegerToFp { src, dst } => print_src_dst_instr("cast-ui-fp", src, dst, symbols),
        BtInstruction::Unary { op, src, dst } => print_unary_instr(op, src, dst, symbols),
        BtInstruction::Binary { op, src1, src2, dst } => print_binary_instr(op, src1, src2, dst, symbols),
        BtInstruction::Copy { src, dst } => print_src_dst_instr("", src, dst, symbols),
        BtInstruction::GetAddress { src, dst } => print_src_dst_instr("addr", src, dst, symbols),
        BtInstruction::Load { src_ptr, dst } => print_src_dst_instr("ld", src_ptr, dst, symbols),
        BtInstruction::Store { src, dst_ptr } => print_src_dst_instr("st", src, dst_ptr, symbols),
        BtInstruction::Jump { target } => print_jump(target),
        BtInstruction::JumpIfZero { condition, target } => print_jump_if_condition(condition, false, target, symbols),
        BtInstruction::JumpIfNotZero { condition, target } => print_jump_if_condition(condition, true, target, symbols),
        BtInstruction::Label { id } => print_label(id),
        BtInstruction::Switch { controlling_value, cases, default_label, .. } => {
            print_switch(controlling_value, cases, default_label, symbols)
        }
        BtInstruction::FunctionCall { identifier, args, dst } => print_function_call(identifier, args, dst, symbols),
    }
}

fn print_return(value: &BtValue, symbols: &SymbolTable) {
    let bt_type = get_ir_type(value, symbols);
    println!("  ret {value} {bt_type}");
}

fn print_src_dst_instr(instr: &str, src: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src_type = get_ir_type(src, symbols);
    let dst_type = get_ir_type(dst, symbols);

    if instr.is_empty() {
        println!("  {dst} {dst_type} = {src} {src_type}");
    } else {
        println!("  {dst} {dst_type} = {instr} {src} {src_type}");
    }
}

fn print_unary_instr(op: &BtUnaryOp, src: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src_type = get_ir_type(src, symbols);
    let dst_type = get_ir_type(dst, symbols);
    println!("  {dst} {dst_type} = {op} {src} {src_type}");
}

fn print_binary_instr(op: &BtBinaryOp, src1: &BtValue, src2: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src1_type = get_ir_type(src1, symbols);
    let src2_type = get_ir_type(src2, symbols);
    let dst_type = get_ir_type(dst, symbols);
    println!("  {dst} {dst_type} = {op} {src1} {src1_type}, {src2} {src2_type}");
}

fn print_jump(target: &BtLabelIdentifier) {
    println!("  jmp {}", target.0);
}

fn print_jump_if_condition(
    cond: &BtValue,
    evaluates_to: bool,
    target: &BtLabelIdentifier,
    symbols: &SymbolTable,
) {
    let cond_type = get_ir_type(cond, symbols);
    println!("  if ({cond} {cond_type} == {evaluates_to}) jmp {}", target.0);
}

fn print_label(target: &BtLabelIdentifier) {
    println!("{}:", target.0);
}

fn print_switch(
    controlling_value: &BtValue,
    cases: &Vec<BtSwitchCase>,
    default_label: &Option<BtLabelIdentifier>,
    symbols: &SymbolTable,
) {
    let controlling_value_type = get_ir_type(controlling_value, symbols);

    println!("  switch ({controlling_value} {controlling_value_type}) {{");

    // Cases
    for case in cases {
        let case_type = get_ir_type(&case.value, symbols);
        println!("    case {} {}: jmp {}", case.value, case_type, case.label.0);
    }

    // Default
    if let Some(default_label) = default_label {
        println!("    default: jmp {}", default_label.0);
    }
    
    println!("  }}");
}

fn print_function_call(identifier: &str, args: &Vec<BtValue>, dst: &BtValue, symbols: &SymbolTable) {
    let dst_type = get_ir_type(dst, symbols);

    print!("  {dst} {dst_type} = call @{identifier} (");

    // Args
    let mut first = true;
    for arg in args {
        let arg_type = get_ir_type(arg, symbols);
        if !first {
            print!(", ");
        }
        print!("{arg} {arg_type}");
        first = false;
    }

    println!(")");
}

fn get_ir_type(value: &BtValue, symbols: &SymbolTable) -> String {
    let bt_type = match value {
        BtValue::Constant(bt_constant_value) => bt_constant_value.get_bt_type(),
        BtValue::Variable(name) => {
            let Some(symbol) = symbols.get(AstUniqueName::new(name)) else {
                ICE!("Symbol table is missing entry for '{name}'");
            };

            BtType::from(&symbol.data_type)
        }
    };

    bt_type_to_string(&bt_type)
}

fn bt_type_to_string(bt_type: &BtType) -> String {
    match bt_type {
        BtType::Void => "()".to_string(),
        BtType::Int16 => "i16".to_string(),
        BtType::Int32 => "i32".to_string(),
        BtType::Int64 => "i64".to_string(),
        BtType::UInt16 => "u16".to_string(),
        BtType::UInt32 => "u32".to_string(),
        BtType::UInt64 => "u64".to_string(),
        BtType::Float32 => "f32".to_string(),
        BtType::Float64 => "f64".to_string(),
    }
}
