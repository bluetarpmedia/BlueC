// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `symbol_table` module defines the [SymbolTable] type and its related types.

use std::collections::{HashMap, HashSet};

use crate::core::{SourceIdentifier, SourceLocation, SymbolKind};
use crate::parser::{AstLinkage, AstStorageDuration, AstType, AstUniqueName};

/// Metadata about an identifier's definition.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Definition {
    None,      // No definition.
    Tentative, // A variable declared at global scope with no initializer, which may be repeated.
    Defined,   // A variable or function with a definition. (A variable definition may be implicit.)
}

/// Symbol attributes.
#[derive(Debug, Clone)]
pub struct SymbolAttributes {
    pub declared_name: String,  // The declared name of the symbol, as it appears in the source.
    pub kind: SymbolKind,       // The kind of symbol (variable/function/type alias)
    pub definition: Definition, // For variables and functions, is the symbol defined?
    pub linkage: AstLinkage,    // The symbol's linkage
    pub storage_duration: AstStorageDuration, // The symbol's storage duration
    pub decl_loc: SourceLocation, // The source location where the symbol is declared
    pub is_file_scope_typedef: bool, // Is the typedef alias declared at file scope?
}

impl SymbolAttributes {
    /// Creates symbol attributes for a local variable.
    ///
    /// This includes automatic local variables, static local variables, and extern local variables.
    pub fn local_var(
        var_source_id: SourceIdentifier,
        definition: Definition,
        linkage: AstLinkage,
        storage_duration: AstStorageDuration,
    ) -> Self {
        Self {
            declared_name: var_source_id.0.into(),
            kind: SymbolKind::Variable,
            definition,
            linkage,
            storage_duration,
            decl_loc: var_source_id.1,
            is_file_scope_typedef: false,
        }
    }

    /// Creates symbol attributes for a file scope variable.
    pub fn file_scope_var(var_source_id: SourceIdentifier, definition: Definition, linkage: AstLinkage) -> Self {
        Self {
            declared_name: var_source_id.0.into(),
            kind: SymbolKind::Variable,
            definition,
            linkage,
            storage_duration: AstStorageDuration::Static, // File scope variable always has static storage duration
            decl_loc: var_source_id.1,
            is_file_scope_typedef: false,
        }
    }

    /// Creates symbol attributes for a function.
    pub fn function(definition: Definition, linkage: AstLinkage, loc: SourceLocation) -> Self {
        Self {
            declared_name: String::new(), // The declared name is the same as its unique name
            kind: SymbolKind::Function,
            definition,
            linkage,
            storage_duration: AstStorageDuration::None, // Functions do not have a storage duration
            decl_loc: loc,
            is_file_scope_typedef: false,
        }
    }

    /// Creates symbol attributes for a type alias.
    pub fn type_alias(alias_source_id: SourceIdentifier, is_at_file_scope: bool) -> Self {
        Self {
            declared_name: alias_source_id.0.into(),
            kind: SymbolKind::TypeAlias,
            definition: Definition::None,
            linkage: AstLinkage::None,
            storage_duration: AstStorageDuration::None,
            decl_loc: alias_source_id.1,
            is_file_scope_typedef: is_at_file_scope,
        }
    }

    /// Creates symbol attributes for a constant.
    pub fn constant(loc: SourceLocation) -> Self {
        Self {
            declared_name: String::new(), // The declared name is the same as its unique name
            kind: SymbolKind::Constant,
            definition: Definition::None,
            linkage: AstLinkage::Internal,
            storage_duration: AstStorageDuration::Static,
            decl_loc: loc,
            is_file_scope_typedef: false,
        }
    }
}

/// Metadata about a symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub data_type: AstType, // The symbol's type (e.g. 'int')
    attrs: SymbolAttributes,
}

impl Symbol {
    /// The kind of symbol.
    pub fn kind(&self) -> SymbolKind {
        self.attrs.kind
    }

    /// The name of the symbol as it was declared in the source code.
    pub fn declared_name(&self) -> String {
        self.attrs.declared_name.clone()
    }

    /// Does the symbol have internal or external linkage?
    pub fn has_linkage(&self) -> bool {
        self.attrs.linkage.has_linkage()
    }

    /// The symbol's linkage.
    pub fn linkage(&self) -> AstLinkage {
        self.attrs.linkage
    }

    /// The symbol's storage duration.
    pub fn storage_duration(&self) -> AstStorageDuration {
        self.attrs.storage_duration
    }

    /// Is the symbol defined?
    pub fn is_defined(&self) -> bool {
        self.attrs.definition == Definition::Defined
    }

    /// The source location of the symbol's declaration.
    pub fn location(&self) -> SourceLocation {
        self.attrs.decl_loc
    }
}

/// Errors returned by `SymbolTable::set_definition`.
pub enum SetDefinitionError {
    DefinitionRequired,    // A new definition is required; it cannot be `Definition::None`.
    SymbolCannotBeDefined, // The symbol does not support definition (type alias).
    SymbolNotFound,        // A symbol does not exist for the given unique name.
    AlreadyDefined,        // The symbol already has a definition.
}

/// The Symbol Table records metadata about identifiers.
///
/// The parser and/or sema stage can query the Symbol Table and emit diagnostics such as
/// when a variable, function or type alias is incorrectly redeclared.
///
/// In addition, it helps us solve the "typedef-name: identifier" problem when there is
/// grammar ambiguity between a type alias and a variable identifier. One example is:
///
/// ```C
/// typedef int AA;
/// AA aa;              // Declare variable 'aa' of type AA (alias of int).
/// float AA;           // Declare variable 'AA' of type float.
/// ```
#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<AstUniqueName, Symbol>,
    used: HashSet<AstUniqueName>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    /// Creates a new symbol table.
    pub fn new() -> Self {
        Self { symbols: HashMap::new(), used: HashSet::new() }
    }

    /// Adds a symbol to the table.
    ///
    /// Returns `Ok` if the symbol was added, or `Err(&Symbol)` with the existing symbol if a symbol already exists
    /// for the given `unique_name`.
    pub fn add<T: AsRef<AstUniqueName>>(
        &mut self,
        unique_name: T,
        data_type: AstType,
        attrs: SymbolAttributes,
    ) -> Result<(), &Symbol> {
        if self.symbols.contains_key(unique_name.as_ref()) {
            return Err(&self.symbols[unique_name.as_ref()]);
        }

        self.symbols.insert(unique_name.as_ref().clone(), Symbol { data_type, attrs });

        Ok(())
    }

    /// Sets that a symbol is defined.
    ///
    /// A symbol's definition can be changed from undefined to either `Definition::Tentative` or `Definition::Defined`,
    /// but it's not possible to change a defined symbol back to undefined, or to change a defined symbol to tentative,
    /// or vice versa.
    ///
    /// Returns `Ok` if a symbol exists for the given `unique_name` and if its previous definition was undefined, or
    /// `Err` otherwise.
    pub fn set_definition<T: AsRef<AstUniqueName>>(
        &mut self,
        unique_name: T,
        definition: Definition,
    ) -> Result<(), SetDefinitionError> {
        if definition == Definition::None {
            return Err(SetDefinitionError::DefinitionRequired);
        }

        match self.symbols.get_mut(unique_name.as_ref()) {
            Some(symbol) if symbol.attrs.kind == SymbolKind::TypeAlias => {
                Err(SetDefinitionError::SymbolCannotBeDefined)
            }
            Some(symbol) if symbol.attrs.definition == Definition::None => {
                symbol.attrs.definition = definition;
                Ok(())
            }
            Some(_) => Err(SetDefinitionError::AlreadyDefined),
            None => Err(SetDefinitionError::SymbolNotFound),
        }
    }

    /// Returns the symbol for the given unique identifier name, or returns `None`.
    pub fn get<T: AsRef<AstUniqueName>>(&self, unique_name: T) -> Option<&Symbol> {
        self.symbols.get(unique_name.as_ref())
    }

    /// Sets that the symbol has been used.
    pub fn set_symbol_used(&mut self, unique_name: &AstUniqueName) {
        self.used.insert(unique_name.clone());
    }

    /// Returns a vector of all unused variable and function symbols, excluding ones with external linkage, and
    /// unused local typedefs. (Do not warn for unused file-scope typedefs, since they could come from included headers.)
    pub fn get_unused_symbols(&self) -> Vec<(String, SymbolKind, SourceLocation)> {
        let mut unused: Vec<(String, SymbolKind, SourceLocation)> = self
            .symbols
            .iter()
            .filter_map(|(unique_name, s)| {
                let is_global_typedef = s.kind() == SymbolKind::TypeAlias && s.attrs.is_file_scope_typedef;

                if !is_global_typedef && s.linkage() != AstLinkage::External && !self.used.contains(unique_name) {
                    let name =
                        if s.kind() == SymbolKind::Function { unique_name.to_string() } else { s.declared_name() };
                    Some((name, s.kind(), s.location()))
                } else {
                    None
                }
            })
            .collect();

        // Sort by file pos
        unused.sort_by_key(|symbol| symbol.2.file_pos);

        unused
    }
}

impl IntoIterator for SymbolTable {
    type Item = (AstUniqueName, Symbol);
    type IntoIter = std::collections::hash_map::IntoIter<AstUniqueName, Symbol>;

    fn into_iter(self) -> Self::IntoIter {
        self.symbols.into_iter()
    }
}
