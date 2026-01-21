// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::core::{FilePosition, SourceIdentifier, SourceLocation, SymbolKind};
use crate::parser::{AstLinkage, AstType, AstUniqueName};

use super::super::symbol_table::{Definition, SymbolAttributes, SymbolTable};

#[test]
fn add_symbols() {
    let mut table = SymbolTable::new();

    assert!(table.get(AstUniqueName::new("global_var")).is_none());
    assert!(table.get(AstUniqueName::new("my_alias")).is_none());
    assert!(table.get(AstUniqueName::new("calculate")).is_none());

    add_global_variable_declaration(&mut table, "global_var");
    add_type_alias_declaration(&mut table, "my_alias", true);
    add_function_declaration(&mut table, "calculate", Definition::None, AstLinkage::External);

    assert!(table.get(AstUniqueName::new("global_var")).is_some_and(|symbol| symbol.kind() == SymbolKind::Variable));
    assert!(table.get(AstUniqueName::new("my_alias")).is_some_and(|symbol| symbol.kind() == SymbolKind::TypeAlias));
    assert!(table.get(AstUniqueName::new("calculate")).is_some_and(|symbol| symbol.kind() == SymbolKind::Function));

    // Cannot add duplicate symbol with same unique name
    let global_var_source_id = SourceIdentifier("global_var", make_location(1));
    assert!(
        table
            .add(
                AstUniqueName::new("global_var"),
                AstType::Int,
                SymbolAttributes::file_scope_var(global_var_source_id, Definition::Defined, AstLinkage::External),
            )
            .is_err()
    );

    // Cannot redefine it as a function
    assert!(
        table
            .add(
                AstUniqueName::new("global_var"),
                AstType::Function { return_type: Box::new(AstType::Int), params: Vec::new() },
                SymbolAttributes::function(Definition::Defined, AstLinkage::External, make_location(1))
            )
            .is_err()
    );

    // Cannot redefine it as a type alias
    assert!(
        table
            .add(
                AstUniqueName::new("global_var"),
                AstType::Int,
                SymbolAttributes::type_alias(global_var_source_id, true)
            )
            .is_err()
    );
}

#[test]
fn set_defined() {
    let mut table = SymbolTable::new();

    // Cannot set definition for a symbol that doesn't exist
    assert!(table.set_definition(AstUniqueName::new("not_a_symbol"), Definition::Defined).is_err());

    add_type_alias_declaration(&mut table, "my_alias", true);

    // Cannot set definition on an alias
    assert!(table.set_definition(AstUniqueName::new("my_alias"), Definition::Defined).is_err());

    add_function_declaration(&mut table, "calculate", Definition::None, AstLinkage::External);
    assert!(
        table
            .get(AstUniqueName::new("calculate"))
            .is_some_and(|symbol| symbol.kind() == SymbolKind::Function && !symbol.is_defined())
    );

    // Cannot set definition to None
    assert!(table.set_definition(AstUniqueName::new("calculate"), Definition::None).is_err());

    // Mark the function has defined
    assert!(table.set_definition(AstUniqueName::new("calculate"), Definition::Defined).is_ok());
    assert!(
        table
            .get(AstUniqueName::new("calculate"))
            .is_some_and(|symbol| symbol.kind() == SymbolKind::Function && symbol.is_defined())
    );

    // Cannot change definition
    assert!(table.set_definition(AstUniqueName::new("calculate"), Definition::None).is_err());
    assert!(table.set_definition(AstUniqueName::new("calculate"), Definition::Tentative).is_err());
}

#[test]
fn unused_symbols() {
    let mut table = SymbolTable::new();

    add_global_variable_declaration(&mut table, "global_var");
    add_local_variable_declaration(&mut table, "local_var");

    add_type_alias_declaration(&mut table, "global_alias", true);
    add_type_alias_declaration(&mut table, "local_alias", false);

    add_function_declaration(&mut table, "func_extern", Definition::Defined, AstLinkage::External);
    add_function_declaration(&mut table, "func_intern", Definition::Defined, AstLinkage::Internal);

    let unused = table.get_unused_symbols();
    assert_eq!(unused.len(), 3);
    let unused_names = unused.into_iter().map(|s| s.0).collect::<Vec<String>>();
    assert_eq!(unused_names, ["local_var".to_string(), "local_alias".to_string(), "func_intern".to_string()]);

    table.set_symbol_used(&AstUniqueName::new("local_alias"));
    table.set_symbol_used(&AstUniqueName::new("local_var"));
    table.set_symbol_used(&AstUniqueName::new("func_intern"));

    let unused = table.get_unused_symbols();
    assert_eq!(unused.len(), 0);
}

fn add_global_variable_declaration(table: &mut SymbolTable, name: &str) {
    let var_source_id = SourceIdentifier(name, make_location(1));
    assert!(
        table
            .add(
                AstUniqueName::new(name),
                AstType::Int,
                SymbolAttributes::file_scope_var(var_source_id, Definition::Defined, AstLinkage::External),
            )
            .is_ok()
    );
}

fn add_local_variable_declaration(table: &mut SymbolTable, name: &str) {
    let var_source_id = SourceIdentifier(name, make_location(2));
    assert!(
        table
            .add(
                AstUniqueName::new(name),
                AstType::Int,
                SymbolAttributes::file_scope_var(var_source_id, Definition::Defined, AstLinkage::None),
            )
            .is_ok()
    );
}

fn add_type_alias_declaration(table: &mut SymbolTable, name: &str, file_scope: bool) {
    let alias_source_id = SourceIdentifier(name, make_location(3));
    assert!(
        table
            .add(AstUniqueName::new(name), AstType::Int, SymbolAttributes::type_alias(alias_source_id, file_scope))
            .is_ok()
    );
}

fn add_function_declaration(table: &mut SymbolTable, name: &str, definition: Definition, linkage: AstLinkage) {
    assert!(
        table
            .add(
                AstUniqueName::new(name),
                AstType::Function { return_type: Box::new(AstType::Int), params: Vec::new() },
                SymbolAttributes::function(definition, linkage, make_location(4))
            )
            .is_ok()
    );
}

fn make_location(pos: i32) -> SourceLocation {
    SourceLocation::new(FilePosition::from(pos), 1)
}
