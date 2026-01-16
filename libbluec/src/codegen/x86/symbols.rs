// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `symbols` module defines the `AsmSymbolTable` type, which is a symbol table for the back-end
//! assembly code generation.

use std::collections::HashMap;

use super::ast::AsmType;
use crate::parser;
use crate::parser::AstStorageDuration;
use crate::ir::BtType;
use crate::sema::symbol_table::SymbolTable;

/// A symbol in the table.
#[derive(Debug, Clone)]
pub enum AsmSymbol {
    Object { asm_type: AsmType, is_static: bool, is_constant: bool },
    Function { is_defined: bool },
}

/// A back-end table for assembly code symbols.
#[derive(Debug, Clone)]
pub struct AsmSymbolTable {
    symbols: HashMap<String, AsmSymbol>,
}

impl AsmSymbolTable {
    /// Creates a back-end assembly symbol table by consuming the given front end's symbol table.
    pub fn from_frontend_symbols(sema_symbols: SymbolTable) -> Self {
        let symbols = sema_symbols
            .into_iter()
            .filter_map(|(unique_name, symbol)| match symbol.kind() {
                parser::symbol::SymbolKind::Variable => {
                    let is_static = symbol.storage_duration() == AstStorageDuration::Static;
                    let is_constant = false;
                    let bt_type = BtType::from(symbol.data_type);
                    let asm_type = AsmType::from(bt_type);
                    Some((unique_name.to_string(), AsmSymbol::Object { asm_type, is_static, is_constant }))
                }
                parser::symbol::SymbolKind::Constant => {
                    let is_static = true;
                    let is_constant = true;
                    let bt_type = BtType::from(symbol.data_type);
                    let asm_type = AsmType::from(bt_type);
                    Some((unique_name.to_string(), AsmSymbol::Object { asm_type, is_static, is_constant }))
                }
                parser::symbol::SymbolKind::Function => {
                    let is_defined = symbol.is_defined();
                    Some((unique_name.to_string(), AsmSymbol::Function { is_defined }))
                }
                parser::symbol::SymbolKind::TypeAlias => None,
            })
            .collect();

        Self { symbols }
    }

    /// Adds a constant to the symbol table.
    pub fn add_constant(&mut self, name: &str, asm_type: AsmType) {
        self.symbols.insert(name.to_string(), AsmSymbol::Object { asm_type, is_static: true, is_constant: true });
    }

    /// Gets a symbol from the table by its name.
    pub fn get(&self, name: &str) -> Option<&AsmSymbol> {
        self.symbols.get(name)
    }
}
