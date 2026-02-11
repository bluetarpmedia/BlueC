// Copyright 2025-2026 Neil Henderson
//
//! The `symbols` module provides functionality to type check a symbol and then, if valid, add the symbol to the
//! Symbol Table.

use crate::ICE;
use crate::compiler_driver::{Diagnostic, Driver, Error, Warning};
use crate::core::SymbolKind;
use crate::parser::{
    AstDeclaration, AstDeclaredType, AstExpression, AstFunction, AstIdentifier, AstLinkage, AstMetadata, AstNodeId,
    AstStorageClassSpecifierOption, AstStorageDuration, AstType, AstTypeAliasDeclaration, AstUniqueName,
    AstVariableDeclaration,
};

use super::super::symbol_table::{Definition, SymbolAttributes, SymbolTable};
use super::checker::{TypeCheckError, TypeChecker};
use super::type_resolution;

/// Verifies that the type alias declaration is semantically valid.
pub fn verify_type_alias_declaration(
    alias_decl: &mut AstTypeAliasDeclaration,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let inner_decl = alias_decl.decl.as_mut();

    let (declared_type, alias_ident, alias_unique_name, is_file_scope) = match inner_decl {
        AstDeclaration::Variable(var_decl) => {
            (&var_decl.declared_type, &var_decl.ident, &var_decl.unique_name, var_decl.is_file_scope)
        }
        AstDeclaration::Function(fn_decl) => (&fn_decl.declared_type, &fn_decl.ident, &fn_decl.unique_name, true),
        _ => ICE!("Unexpected declaration"),
    };

    let resolved_type =
        type_resolution::resolve_declared_type(declared_type, chk, driver).map_err(|_| TypeCheckError)?;

    // Is the alias identifier already declared in the current scope?
    //      It's valid to repeat the `typedef` declaration in the same scope multiple times, as long as it
    //      remains consistent with the underlying type.
    //
    if let Some((existing_alias_type, decl_loc)) = chk.current_declaration_scope().type_alias_map.get(&alias_ident.name)
        && resolved_type != *existing_alias_type
    {
        let old_t = existing_alias_type;
        let new_t = &resolved_type;
        Error::redefine_type_alias_type(alias_ident.into(), old_t, new_t, *decl_loc, driver);
        return Err(TypeCheckError);
    }

    // Add the type alias to the symbol table
    //      Ignore the error if a duplicate already exists, since we've already validated the potential duplicate.
    //
    _ = chk.symbols.add(
        alias_unique_name,
        resolved_type.clone(),
        SymbolAttributes::type_alias(alias_ident.into(), is_file_scope),
    );

    // Add the type alias to the current scope.
    chk.current_declaration_scope()
        .type_alias_map
        .insert(alias_ident.name.clone(), (resolved_type.clone(), alias_ident.loc));

    // Set the resolved type on the type alias' inner declaration.
    match inner_decl {
        AstDeclaration::Variable(var_decl) => var_decl.declared_type.resolved_type = Some(resolved_type),
        AstDeclaration::Function(fn_decl) => fn_decl.declared_type.resolved_type = Some(resolved_type),
        _ => ICE!("Unexpected declaration"),
    };

    Ok(())
}

/// Verifies that the file scope variable declaration is semantically valid.
pub fn verify_file_scope_variable_declaration(
    var_decl: &mut AstVariableDeclaration,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let var_unique_name = &var_decl.unique_name;
    let var_ident = &var_decl.ident;
    let var_declared_type = &var_decl.declared_type;
    let var_type =
        type_resolution::resolve_declared_type(var_declared_type, chk, driver).map_err(|_| TypeCheckError)?;

    let has_initializer = var_decl.initializer.is_some();

    let is_declared_extern = var_declared_type.storage_class.is_extern();

    // A tentative definition is a variable declaration at file scope, with no initializer, and either 'static'
    // or no storage specifier.
    //
    //      If later we find a variable definition for this identifer, then this tentative definition turns into
    //      a declaration. Otherwise, if there is no subsequent definition, then the tentative definition will be
    //      treated as the variable being defined to zero.
    //
    let is_tentative_definition = !has_initializer && !is_declared_extern;

    // At file scope, an extern declaration can have an initializer, but we emit a warning because really this is a
    // definition and extern is ignored.
    if is_declared_extern && has_initializer {
        Warning::extern_initializer(var_declared_type.storage_class.as_ref().unwrap().loc, driver)
    }

    if let Some(existing_symbol) = chk.symbols.get(var_unique_name) {
        let symbol_kind = existing_symbol.kind();
        let symbol_loc = existing_symbol.location();

        // Cannot redefine as a different kind of symbol
        if existing_symbol.kind() != SymbolKind::Variable {
            Error::redefine_as_different_symbol(var_ident.into(), symbol_kind, symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Cannot redefine with a different type.
        if existing_symbol.data_type != var_type {
            let old_t = &existing_symbol.data_type;
            let new_t = &var_type;
            Error::redefine_variable_type(var_ident.into(), old_t, new_t, symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Cannot redefine with a different value.
        if existing_symbol.is_defined() && has_initializer {
            Error::redefine_variable_value(var_ident.into(), symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Cannot change variable's linkage
        if !is_declared_extern && existing_symbol.linkage() != var_decl.linkage {
            Error::redefine_variable_linkage(var_ident.into(), var_decl.linkage, symbol_loc, driver);
            return Err(TypeCheckError);
        }
    }

    let definition = if is_tentative_definition {
        Definition::Tentative
    } else if has_initializer {
        Definition::Defined
    } else {
        Definition::None
    };

    if chk
        .symbols
        .add(
            var_unique_name,
            var_type.clone(),
            SymbolAttributes::file_scope_var(var_ident.into(), definition, var_decl.linkage),
        )
        .is_err()
    {
        let _ = chk.symbols.set_definition(var_unique_name, definition);
    }

    var_decl.declared_type.resolved_type = Some(var_type);

    Ok(())
}

/// Verifies that the block scope variable declaration is semantically valid.
pub fn verify_local_variable_declaration(
    var_decl: &mut AstVariableDeclaration,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let var_unique_name = &var_decl.unique_name;
    let var_ident = &var_decl.ident;
    let var_declared_type = &var_decl.declared_type;

    let var_type = if var_declared_type.is_resolved() {
        var_declared_type.resolved_type.clone().unwrap()
    } else {
        type_resolution::resolve_declared_type(var_declared_type, chk, driver).map_err(|_| TypeCheckError)?
    };

    let has_initializer = var_decl.initializer.is_some();

    let is_declared_extern = var_declared_type.storage_class.is_extern();

    if is_declared_extern {
        // Cannot declare 'extern' local variable with initializer.
        if has_initializer {
            Error::local_extern_variable_with_initializer(
                var_declared_type.storage_class.as_ref().unwrap().loc,
                driver,
            );
            return Err(TypeCheckError);
        }

        if let Some(existing_symbol) = chk.symbols.get(var_unique_name) {
            let symbol_kind = existing_symbol.kind();
            let symbol_loc = existing_symbol.location();

            // Cannot redefine as a different kind of symbol
            if existing_symbol.kind() != SymbolKind::Variable {
                Error::redefine_as_different_symbol(var_ident.into(), symbol_kind, symbol_loc, driver);
                return Err(TypeCheckError);
            }

            // Cannot redefine with a different type.
            if existing_symbol.data_type != var_type {
                let old_t = &existing_symbol.data_type;
                let new_t = &var_type;
                Error::redefine_variable_type(var_ident.into(), old_t, new_t, symbol_loc, driver);
                return Err(TypeCheckError);
            }
        }
    }

    let definition = if has_initializer { Definition::Defined } else { Definition::None };

    if chk
        .symbols
        .add(
            var_unique_name,
            var_type.clone(),
            SymbolAttributes::local_var(var_ident.into(), definition, var_decl.linkage, var_decl.storage),
        )
        .is_err()
    {
        let _ = chk.symbols.set_definition(var_unique_name, definition);
    }

    var_decl.declared_type.resolved_type = Some(var_type);

    Ok(())
}

/// Verifies that the function parameter declaration is semantically valid.
pub fn verify_function_parameter_declaration(
    param_unique_name: &AstUniqueName,
    param_ident: &AstIdentifier,
    param_type: &AstType,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let declared_type = AstDeclaredType::resolved(param_type);

    let mut param_decl = AstVariableDeclaration {
        node_id: AstNodeId::new(),
        is_declaration_only: true,
        is_file_scope: false,
        declared_type,
        ident: param_ident.clone(),
        unique_name: param_unique_name.clone(),
        initializer: None,
        init_constant_eval: Vec::new(),
        linkage: AstLinkage::None,
        storage: AstStorageDuration::Automatic,
    };

    verify_local_variable_declaration(&mut param_decl, chk, driver)
}

/// Verifies that the function declaration is semantically valid.
pub fn verify_function_declaration(
    function: &mut AstFunction,
    chk: &mut TypeChecker,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let fn_unique_name = &function.unique_name;
    let fn_ident = &function.ident;
    let fn_declared_type = &function.declared_type;

    // Resolve the function type
    let fn_type = type_resolution::resolve_declared_type(fn_declared_type, chk, driver).map_err(|_| TypeCheckError)?;

    // If any of the parameter types are array types then transform them to a pointer to the array element type.
    //
    let AstType::Function { return_type, params } = fn_type else {
        ICE!("Expected an AstType::Function");
    };

    let fn_type = if params.iter().any(|param_t| param_t.is_array()) {
        let new_params = params
            .into_iter()
            .map(|param_t| {
                if let AstType::Array { element_type, .. } = param_t {
                    AstType::new_pointer_to(*element_type.clone())
                } else {
                    param_t
                }
            })
            .collect();

        AstType::Function { return_type, params: new_params }
    } else {
        AstType::Function { return_type, params }
    };

    let declared_linkage = function.linkage;
    let has_body = function.body.is_some();

    // If there's an existing symbol for this function's name then make sure the declarations match.
    //
    if let Some(existing_symbol) = chk.symbols.get(fn_unique_name) {
        let symbol_kind = existing_symbol.kind();
        let symbol_loc = existing_symbol.location();

        // Cannot redefine as a different kind of symbol
        if existing_symbol.kind() != SymbolKind::Function {
            Error::redefine_as_different_symbol(fn_ident.into(), symbol_kind, symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Function signature must match existing declaration
        if existing_symbol.data_type != fn_type {
            let old_type = &existing_symbol.data_type;
            let new_type = fn_type;
            Error::redefine_function_type(fn_ident.into(), old_type, &new_type, symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Cannot declare function with internal linkage if previous declaration has external linkage.
        if declared_linkage == AstLinkage::Internal && existing_symbol.linkage() == AstLinkage::External {
            Error::redefine_external_linkage_function_with_internal_linkage(fn_ident.into(), symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Cannot redefine with a different body.
        if existing_symbol.is_defined() && has_body {
            Error::redefine_function_body(fn_ident.into(), symbol_loc, driver);
            return Err(TypeCheckError);
        }

        // Ensure this function declaration's linkage matches the previous declaration.
        function.linkage = existing_symbol.linkage();
    }

    let definition = if has_body { Definition::Defined } else { Definition::None };

    if chk
        .symbols
        .add(fn_unique_name, fn_type.clone(), SymbolAttributes::function(definition, declared_linkage, fn_ident.loc))
        .is_err()
    {
        let _ = chk.symbols.set_definition(fn_unique_name, definition);
    }

    function.declared_type.resolved_type = Some(fn_type);

    Ok(())
}

/// Verifies that the arguments are valid for a function's parameters.
pub fn verify_function_arguments(
    fn_unique_name: Option<AstUniqueName>,
    args_node_id: AstNodeId,
    params: &[AstType],
    args: &[AstExpression],
    metadata: &AstMetadata,
    symbols: &SymbolTable,
    driver: &mut Driver,
) -> Result<(), TypeCheckError> {
    let args_count = args.len();
    let params_count = params.len();

    if args_count != params_count {
        let err = if args_count > params_count {
            format!("Too many arguments passed to function (expected {}, have {})", params_count, args_count)
        } else {
            format!("Too few arguments passed to function (expected {}, have {})", params_count, args_count)
        };

        let args_source_loc = metadata.get_source_location(args_node_id);
        let mut diag = Diagnostic::error_at_location(err, args_source_loc);

        if let Some(ref fn_name) = fn_unique_name {
            let fn_symbol = symbols.get(fn_name).unwrap();
            diag.add_note(format!("Function '{}' was previously declared here:", fn_name), Some(fn_symbol.location()));
        }

        driver.add_diagnostic(diag);
    }

    Ok(())
}
