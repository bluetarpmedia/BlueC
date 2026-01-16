// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `type_resolution` module provides functionality to resolve a declared type into a canonical `AstType`.

use super::symbol_table::SymbolTable;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::compiler_driver::diagnostics::{Diagnostic, SourceIdentifier};
use crate::compiler_driver::errors::Error;
use crate::compiler_driver::Warning;
use crate::lexer::SourceLocation;
use crate::parser::{AstBasicTypeSpecifier, AstDeclarator, AstDeclaratorKind, AstDeclaredType, AstType};

/// An error returned by `resolve_type`. Error diagnostics are emitted to the compiler driver.
pub enum ResolutionError {
    NoBasicType,
    SymbolTableRequired,
    SemanticError,
}

/// Resolves the given `AstDeclaredType` to its canonical `AstType`.
pub fn resolve_declared_type(
    declared_type: &AstDeclaredType,
    mut symbols: Option<&mut SymbolTable>,
    mut driver: Option<&mut Driver>,
) -> Result<AstType, ResolutionError> {
    if let Some(resolved_type) = &declared_type.resolved_type {
        return Ok(resolved_type.clone());
    }

    if declared_type.basic_type.0.is_empty() {
        return Err(ResolutionError::NoBasicType);
    }

    // Resolve the basic type.
    //      If there's a single specifier then it may be an alias.
    //
    let type_specifiers = &declared_type.basic_type.0;
    let resolved_type = if type_specifiers.len() == 1
        && let AstBasicTypeSpecifier::Alias { alias_name, .. } = &type_specifiers[0]
    {
        if let Some(ref mut symbols) = symbols {
            // Mark the alias as being used.
            symbols.set_symbol_used(alias_name);

            let symbol = symbols.get(alias_name).expect("Symbol should exist for alias");
            symbol.data_type.clone()
        } else {
            return Err(ResolutionError::SymbolTableRequired);
        }
    } else {
        resolve_builtin_type_specifiers(declared_type, &mut driver)?
    };

    // If there's a declarator then it can augment the basic type, e.g. a function or pointer.
    let resolved_type = if let Some(declarator) = &declared_type.declarator {
        resolve_declarator(resolved_type, declarator, &mut symbols, &mut driver)?
    } else {
        resolved_type
    };

    Ok(resolved_type)
}

fn resolve_builtin_type_specifiers(
    declared_type: &AstDeclaredType,
    driver: &mut Option<&mut Driver>,
) -> Result<AstType, ResolutionError> {
    let type_specifiers = &declared_type.basic_type.0;

    let mut void_count = 0;
    let mut char_count = 0;
    let mut short_count = 0;
    let mut int_count = 0;
    let mut long_count = 0;
    let mut signed_count = 0;
    let mut unsigned_count = 0;
    let mut float_count = 0;
    let mut double_count = 0;

    for type_specifier in type_specifiers {
        match type_specifier {
            AstBasicTypeSpecifier::BuiltinType { specifier, loc } => {
                match specifier.as_str() {
                    "void" => void_count += 1,
                    "char" => char_count += 1,
                    "short" => short_count += 1,
                    "int" => int_count += 1,
                    "long" => long_count += 1,
                    "signed" => signed_count += 1,
                    "unsigned" => unsigned_count += 1,
                    "float" => float_count += 1,
                    "double" => double_count += 1,
                    _ => ICE!("Unexpected specifier '{specifier}'"),
                }

                let mut sema_err = false; // We want to emit multiple diagnostics before returning from the function

                // These type specifiers cannot be repeated.
                if void_count > 1
                    || char_count > 1
                    || short_count > 1
                    || int_count > 1
                    || long_count > 2
                    || float_count > 1
                    || double_count > 1
                {
                    if let Some(driver) = driver {
                        let specifier = SourceIdentifier(specifier, *loc);
                        Error::cannot_combine_type_specifier(specifier, driver);
                    }
                    sema_err = true;
                }

                // 'double' can be combined with 'long' but nothing else.
                if double_count == 1 && type_specifiers.len() > 1 {
                    let invalid_combination = char_count > 0
                        || short_count > 0
                        || int_count > 0
                        || long_count > 1
                        || signed_count > 0
                        || unsigned_count > 0
                        || float_count > 0;

                    if invalid_combination {
                        if let Some(driver) = driver {
                            let specifier = SourceIdentifier(specifier, *loc);
                            Error::cannot_combine_type_specifier(specifier, driver);
                        }
                        sema_err = true;
                    }
                }

                // 'char' can be combined with 'signed' or 'unsigned' but nothing else.
                //      Allow signed/unsigned to be repeated; we'll warn below.
                if char_count == 1 && type_specifiers.len() > 1 {
                    let invalid_combination = short_count > 0
                        || int_count > 0
                        || long_count > 0
                        || float_count > 0
                        || double_count > 0;

                    if invalid_combination {
                        if let Some(driver) = driver {
                            let specifier = SourceIdentifier(specifier, *loc);
                            Error::cannot_combine_type_specifier(specifier, driver);
                        }
                        sema_err = true;
                    }
                }

                // Cannot combine 'short' with 'long'.
                // Cannot combine 'signed' with 'unsigned'.
                // Cannot combine anything with 'float'.
                // Cannot combine anything with 'void'.
                let combine_short_long = short_count > 0 && long_count > 0;
                let combine_signed_unsigned = signed_count > 0 && unsigned_count > 0;
                let combine_with_float = float_count == 1 && type_specifiers.len() > 1;
                let combine_with_void = void_count == 1 && type_specifiers.len() > 1;

                if combine_short_long || combine_signed_unsigned || combine_with_float || combine_with_void {
                    if let Some(driver) = driver {
                        let specifier = SourceIdentifier(specifier, *loc);
                        Error::cannot_combine_type_specifier(specifier, driver);
                    }
                    sema_err = true;
                }

                // Warn if 'signed' or 'unsigned' is repeated.
                if (signed_count > 1 || unsigned_count > 1)
                    && let Some(driver) = driver
                {
                    let specifier = SourceIdentifier(specifier, *loc);
                    Warning::duplicate_decl_specifier(specifier, driver);
                }

                if sema_err {
                    return Err(ResolutionError::SemanticError);
                }
            }

            AstBasicTypeSpecifier::Alias { alias_name, .. } => {
                ICE!("Should not have alias '{alias_name}' with multiple specifiers")
            }
        }
    }

    let is_void = void_count == 1;
    let is_fp = float_count > 0 || double_count > 0;
    let is_char = char_count == 1;

    let ty = if is_void {
        AstType::Void
    } else if is_fp {
        resolve_fp_data_type(float_count, double_count, long_count)
    } else if is_char {
        resolve_char_data_type(char_count, signed_count, unsigned_count)
    } else {
        resolve_integer_data_type(short_count, int_count, long_count, signed_count, unsigned_count)
    };

    Ok(ty)
}

fn resolve_fp_data_type(float_count: i32, double_count: i32, long_count: i32) -> AstType {
    match (float_count, double_count, long_count) {
        (1, 0, 0) => AstType::Float,
        (0, 1, 0) => AstType::Double,
        (0, 1, 1) => AstType::LongDouble,
        _ => {
            ICE!("Sema: Invalid floating-point type specifiers should have been handled");
        }
    }
}

fn resolve_char_data_type(char_count: i32, signed_count: i32, unsigned_count: i32) -> AstType {
    match (char_count, signed_count, unsigned_count) {
        (1, 0, 0) => AstType::Char,
        (1, 1, 0) => AstType::SignedChar,
        (1, 0, 1) => AstType::UnsignedChar,
        _ => {
            ICE!("Sema: Invalid 'char' type specifiers should have been handled");
        }
    }
}

fn resolve_integer_data_type(
    short_count: i32,
    int_count: i32,
    long_count: i32,
    signed_count: i32,
    unsigned_count: i32,
) -> AstType {
    // Determine the type name string version and its `AstType` from the specifiers.
    //
    let ty = match (short_count, int_count, long_count) {
        (1, 0, 0) => AstType::Short,
        (1, 1, 0) => AstType::Short,
        (0, 1, 0) => AstType::Int,
        (0, 0, 1) => AstType::Long,
        (0, 1, 1) => AstType::Long,
        (0, 0, 2) => AstType::LongLong,
        (0, 1, 2) => AstType::LongLong,
        (0, 0, 0) if signed_count == 1 => AstType::Int,
        (0, 0, 0) if unsigned_count == 1 => AstType::UnsignedInt,
        _ => {
            ICE!("Sema: Invalid integer type specifiers should have been handled");
        }
    };

    // Make unsigned if necessary.
    if unsigned_count > 0 && ty.is_signed_integer() { ty.to_unsigned() } else { ty }
}

// Resolves the derived type for the given basic type and declarator.
fn resolve_declarator(
    ast_type: AstType,
    declarator: &AstDeclarator,
    symbols: &mut Option<&mut SymbolTable>,
    driver: &mut Option<&mut Driver>,
) -> Result<AstType, ResolutionError> {
    resolve_declarator_recursively(ast_type, declarator, symbols, driver)
}

fn resolve_declarator_recursively(
    ast_type: AstType,
    declarator: &AstDeclarator,
    symbols: &mut Option<&mut SymbolTable>,
    driver: &mut Option<&mut Driver>,
) -> Result<AstType, ResolutionError> {
    match &declarator.kind {
        AstDeclaratorKind::Ident(_) => Ok(ast_type),

        AstDeclaratorKind::AbstractPointer => {
            let derived_type = AstType::new_pointer_to(ast_type);
            Ok(derived_type)
        }

        AstDeclaratorKind::AbstractArray { size } => {
            if ast_type.is_function() {
                emit_cannot_declare_array_of_function_error(declarator.loc, &ast_type, driver);
                return Err(ResolutionError::SemanticError);
            }

            let derived_type = AstType::new_array(ast_type, *size);
            Ok(derived_type)
        }

        AstDeclaratorKind::Pointer(referent) => {
            let derived_type = AstType::new_pointer_to(ast_type);
            resolve_declarator_recursively(derived_type, referent, symbols, driver)
        }

        AstDeclaratorKind::Array { decl, size } => {
            if ast_type.is_function() {
                emit_cannot_declare_array_of_function_error(decl.loc, &ast_type, driver);
                return Err(ResolutionError::SemanticError);
            }

            let derived_type = AstType::new_array(ast_type, *size);
            resolve_declarator_recursively(derived_type, decl, symbols, driver)
        }

        AstDeclaratorKind::AbstractFunction { params } => {
            let param_types = resolve_function_param_types(params, symbols, driver)?;
            let fn_derived_type = AstType::new_fn(ast_type, param_types);
            Ok(fn_derived_type)
        }

        AstDeclaratorKind::Function { decl, params } => {
            if ast_type.is_function() {
                emit_cannot_return_function_error(decl.loc, driver);
                return Err(ResolutionError::SemanticError);
            }

            match decl.kind {
                AstDeclaratorKind::Ident(_) => {
                    let param_types = resolve_function_param_types(params, symbols, driver)?;
                    let fn_derived_type = AstType::Function { return_type: Box::new(ast_type), params: param_types };
                    Ok(fn_derived_type)
                }

                AstDeclaratorKind::AbstractPointer
                | AstDeclaratorKind::Pointer(_)
                | AstDeclaratorKind::AbstractArray { .. }
                | AstDeclaratorKind::Array { .. } => {
                    let param_types = resolve_function_param_types(params, symbols, driver)?;
                    let fn_type = AstType::Function { return_type: Box::new(ast_type), params: param_types };
                    resolve_declarator_recursively(fn_type, decl, symbols, driver)
                }

                AstDeclaratorKind::AbstractFunction { .. } | AstDeclaratorKind::Function { .. } => {
                    // This case detects ill-formed declaration like this: `int (foo(void))(void);`
                    // (The `ast_type` at this point is 'int'.)

                    emit_cannot_return_function_error(decl.loc, driver);
                    Err(ResolutionError::SemanticError)
                }
            }
        }
    }
}

fn resolve_function_param_types(
    params: &[AstDeclaredType],
    symbols: &mut Option<&mut SymbolTable>,
    driver: &mut Option<&mut Driver>,
) -> Result<Vec<AstType>, ResolutionError> {
    let symbols = symbols.as_mut().expect("Symbols is required");
    let driver = driver.as_mut().expect("Driver is required");

    params
        .iter()
        .map(|param_declared_type| {
            let ty = resolve_declared_type(param_declared_type, Some(symbols), Some(driver))?;

            // Function type decays to function pointer
            let ty = if ty.is_function() { AstType::new_pointer_to(ty) } else { ty };

            Ok(ty)
        })
        .collect()
}

fn emit_cannot_return_function_error(loc: SourceLocation, driver: &mut Option<&mut Driver>) {
    if let Some(driver) = driver {
        let err = "Function cannot return a function".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
    }
}

fn emit_cannot_declare_array_of_function_error(
    loc: SourceLocation,
    fn_type: &AstType,
    driver: &mut Option<&mut Driver>,
) {
    if let Some(driver) = driver {
        let err = format!("Cannot declare an array of function type '{fn_type}'");
        let mut diag = Diagnostic::error_at_location(err, loc);
        diag.add_note("Did you mean to declare an array of function pointers instead?".to_string(), None);
        driver.add_diagnostic(diag);
    }
}
