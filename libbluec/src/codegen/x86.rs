// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `x86` module contains the codegen functionality for the `x86_64` System V ABI.

mod ast;
mod casts;
mod emit;
mod file_writer;
mod generate;
mod instruction_fixups;
mod label_maker;
mod registers;
mod symbols;

#[cfg(test)]
mod tests;

pub use emit::emit_asm_to_file;
pub use generate::generate_asm;
