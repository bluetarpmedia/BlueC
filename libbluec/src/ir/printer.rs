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
        print!("{}: {}", param.1, param.0.to_printer());
        first = false;
    }
    print!(")");

    // Return type
    println!(" -> {} {{", function.return_type.to_printer());

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

    println!("@{}: {} = {}", variable.name, variable.data_type.to_printer(), variable.init_value);
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
        BtInstruction::StoreAddress { src, dst_ptr } => print_storeaddress_instr(src, dst_ptr, symbols),
        BtInstruction::Load { src_ptr, dst } => print_load_instr(src_ptr, dst, symbols),
        BtInstruction::Store { src, dst_ptr } => print_store_instr(src, dst_ptr, symbols),
        BtInstruction::Jump { target } => print_jump(target),
        BtInstruction::JumpIfZero { condition, target } => print_jump_if_condition(condition, false, target, symbols),
        BtInstruction::JumpIfNotZero { condition, target } => print_jump_if_condition(condition, true, target, symbols),
        BtInstruction::Label { id } => print_label(id),
        BtInstruction::Switch { controlling_value, cases, default_label, .. } => {
            print_switch(controlling_value, cases, default_label, symbols)
        }
        BtInstruction::FunctionCall { designator, args, dst } => print_function_call(designator, args, dst, symbols),
    }
}

fn print_return(value: &BtValue, symbols: &SymbolTable) {
    let bt_type = get_bt_type(value, symbols).to_printer();
    println!("  ret {bt_type} {value}");
}

fn print_src_dst_instr(instr: &str, src: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src_type = get_bt_type(src, symbols).to_printer();
    let dst_type = get_bt_type(dst, symbols).to_printer();

    if instr.is_empty() {
        println!("  {dst} = {src_type} {src}");
    } else {
        println!("  {dst} = {instr} {dst_type}, {src_type} {src}");
    }
}

fn print_load_instr(src_ptr: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src_type = get_bt_type(src_ptr, symbols).to_printer();
    let dst_type = get_bt_type(dst, symbols).to_printer();
    println!("  {dst} = load {dst_type}, {src_type} {src_ptr}");
}

fn print_storeaddress_instr(src: &BtValue, dst_ptr: &BtValue, symbols: &SymbolTable) {
    let dst_type = get_bt_type(dst_ptr, symbols).to_printer();
    println!("  store address-of @{src}, {dst_type} {dst_ptr}");
}

fn print_store_instr(src: &BtValue, dst_ptr: &BtValue, symbols: &SymbolTable) {
    let src_type = get_bt_type(src, symbols).to_printer();
    let dst_type = get_bt_type(dst_ptr, symbols).to_printer();
    println!("  store {src_type} {src}, {dst_type} {dst_ptr}");
}

fn print_unary_instr(op: &BtUnaryOp, src: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src_type = get_bt_type(src, symbols).to_printer();
    println!("  {dst} = {op} {src} {src_type}");
}

fn print_binary_instr(op: &BtBinaryOp, src1: &BtValue, src2: &BtValue, dst: &BtValue, symbols: &SymbolTable) {
    let src1_type = get_bt_type(src1, symbols).to_printer();
    let src2_type = get_bt_type(src2, symbols).to_printer();
    println!("  {dst} = {op} {src1_type} {src1}, {src2_type} {src2}");
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
    let cond_type = get_bt_type(cond, symbols).to_printer();
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
    let controlling_value_type = get_bt_type(controlling_value, symbols).to_printer();

    println!("  switch ({controlling_value} {controlling_value_type}) {{");

    // Cases
    for case in cases {
        let case_type = get_bt_type(&case.value, symbols).to_printer();
        println!("    case {} {}: jmp {}", case.value, case_type, case.label.0);
    }

    // Default
    if let Some(default_label) = default_label {
        println!("    default: jmp {}", default_label.0);
    }
    
    println!("  }}");
}

fn print_function_call(designator: &BtValue, args: &Vec<BtValue>, dst: &BtValue, symbols: &SymbolTable) {
    let dst_type = get_bt_type(dst, symbols).to_printer();
    let designator_type = get_bt_type(designator, symbols);

    if designator_type.is_function() {
        print!("  {dst} {dst_type} = call @{designator} (");
    } else if designator_type.is_pointer() {
        print!("  {dst} {dst_type} = call ptr {designator} (");
    } else {
        ICE!("Invalid function designator type '{designator_type}' for '{designator}'");
    }

    // Args
    let mut first = true;
    for arg in args {
        let arg_type = get_bt_type(arg, symbols).to_printer();
        if !first {
            print!(", ");
        }
        print!("{arg} {arg_type}");
        first = false;
    }

    println!(")");
}

fn get_bt_type(value: &BtValue, symbols: &SymbolTable) -> BtType {
    match value {
        BtValue::Constant(bt_constant_value) => bt_constant_value.get_bt_type(),
        BtValue::Variable(name) => {
            let Some(symbol) = symbols.get(AstUniqueName::new(name)) else {
                ICE!("Symbol table is missing entry for '{name}'");
            };

            BtType::from(&symbol.data_type)
        }
    }
}
