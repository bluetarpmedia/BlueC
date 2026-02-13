// Copyright 2025-2026 Neil Henderson
//
//! The `bluetac_translation` module defines the various functions to translate from the parser's AST into
//! BlueTac intermediate representation.

mod binary_expr;
mod expr;
mod unary_expr;
pub(super) mod utils;

use std::collections::HashMap;

use crate::ICE;
use crate::compiler_driver::Driver;
use crate::core::{SourceIdentifier, SourceLocation};
use crate::parser;
use crate::sema::constant_table::{ConstantIndex, ConstantTable};
use crate::sema::symbol_table::{Definition, SymbolAttributes, SymbolTable};

use super::label_maker::LabelMaker;
use super::{
    BtConstantValue, BtDefinition, BtFunctionDefn, BtInstruction, BtRoot, BtStaticConstant, BtStaticStorageInitializer,
    BtStaticStorageVariable, BtSwitchCase, BtType, BtValue,
};

#[derive(Debug)]
enum EvalExpr {
    Value(BtValue),
    Dereferenced(BtValue),
}

/// The translator manages state for translation from the AST to BlueTac IR.
pub struct BlueTacTranslator {
    metadata: parser::AstMetadata,
    pub symbols: SymbolTable,
    pub constants: ConstantTable,
    static_variables: Vec<BtStaticStorageVariable>,
    next_temp_variable_id: (usize, usize),
    label_maker: LabelMaker,
}

impl BlueTacTranslator {
    /// Creates a new BlueTac IR translator.
    pub fn new(metadata: parser::AstMetadata, symbols: SymbolTable, constants: ConstantTable) -> Self {
        Self {
            metadata,
            symbols,
            constants,
            static_variables: Vec::new(),
            next_temp_variable_id: (0, 0),
            label_maker: LabelMaker::new(),
        }
    }

    /// Gets the `AstType` from the symbol table for the given AST expression.
    pub fn get_expression_type(&self, expr: &parser::AstExpression) -> &parser::AstType {
        self.get_ast_type_from_node(expr.id())
    }

    /// Gets the `AstType` from the symbol table for the given AST node.
    pub fn get_ast_type_from_node(&self, node_id: parser::AstNodeId) -> &parser::AstType {
        self.metadata.get_node_type(node_id)
    }

    /// Adds a static storage duration variable (may be global or local).
    pub fn add_static_storage_duration_variable(
        &mut self,
        name: &parser::AstUniqueName,
        is_global: bool,
        data_type: BtType,
        init_value: Vec<BtStaticStorageInitializer>,
    ) {
        self.static_variables.push(BtStaticStorageVariable {
            name: name.to_string(),
            is_global,
            data_type,
            init_value,
        });
    }

    /// Merges duplicate static-storage variable definitions.
    ///
    /// It's valid to have multiple tentative definitions in file scope. In this case, a definition with a non-zero
    /// value takes precedence, and all other tentative definitions are removed. If all the tentative definitions have
    /// a value of zero then they are merged into one.
    pub fn deduplicate_static_variables(&mut self) {
        let mut deduped = HashMap::new();

        for var in self.static_variables.drain(..) {
            deduped
                .entry(var.name.clone())
                .and_modify(|existing: &mut BtStaticStorageVariable| {
                    debug_assert!(!var.init_value.is_empty());

                    // Take the new declaration's value if it's non-zero.
                    let new_is_nonzero = var.init_value.len() > 1 || !var.init_value[0].has_default_value();
                    let existing_is_zero = existing.init_value.len() == 1 && existing.init_value[0].has_default_value();

                    if new_is_nonzero && existing_is_zero {
                        *existing = var.clone();
                    }
                })
                .or_insert(var);
        }

        self.static_variables.extend(deduped.into_values());
    }

    /// Creates a temp variable.
    pub fn make_temp_variable(&mut self, data_type: parser::AstType) -> BtValue {
        let id = self.next_temp_variable_id;
        if id.0 == usize::MAX || id.1 == usize::MAX {
            ICE!("Exhausted Bt translator temp variables"); // Technically we have 1 more available
        }
        self.next_temp_variable_id = (id.0, id.1 + 1);

        let temp_var_name = format!("ir.{}.{}", id.0, id.1);

        // Add the temp variable and its data type to our symbol table.
        // The codegen stage needs to query the type of every variable, including temporary ones we create.
        let unique_name = parser::AstUniqueName::new(temp_var_name.clone());
        if self
            .symbols
            .add(
                unique_name,
                data_type,
                SymbolAttributes::local_var(
                    SourceIdentifier(&temp_var_name, SourceLocation::default()),
                    Definition::Defined,
                    parser::AstLinkage::None,
                    parser::AstStorageDuration::Automatic,
                ),
            )
            .is_err()
        {
            ICE!("IR temp variable '{temp_var_name}' already exists in symbol table");
        }

        BtValue::Variable(temp_var_name)
    }

    /// Resets state for a new function definition.
    pub fn reset_for_new_function(&mut self, function_name: &str) {
        let id = self.next_temp_variable_id;
        self.next_temp_variable_id = (id.0 + 1, 0);
        self.label_maker.reset_for_new_function(function_name);
    }
}

/// Translates the parser's AST into BlueTac IR.
pub fn translate_ast_to_ir(
    ast: parser::AstRoot,
    metadata: parser::AstMetadata,
    symbols: SymbolTable,
    constants: ConstantTable,
    driver: &mut Driver,
) -> (BtRoot, SymbolTable, ConstantTable) {
    let mut translator = BlueTacTranslator::new(metadata, symbols, constants);

    let mut bt_definitions = ast
        .0
        .into_iter()
        .filter_map(|decl| match decl {
            parser::AstDeclaration::Function(func) => {
                if func.body.is_none() {
                    None
                } else {
                    Some(BtDefinition::Function(translate_function(func, &mut translator, driver)))
                }
            }

            parser::AstDeclaration::Variable(var_decl) => {
                if var_decl.is_declaration_only {
                    return None;
                }

                let is_global = var_decl.linkage == parser::AstLinkage::External;
                translate_static_storage_duration_variable(var_decl, is_global, &mut translator);

                None
            }

            parser::AstDeclaration::TypeAlias(_) => None, // Ignore type aliases
        })
        .collect::<Vec<BtDefinition>>();

    // Merge the static variable definitions in case we have multiple tentative definitions.
    translator.deduplicate_static_variables();

    let BlueTacTranslator { static_variables, symbols, constants, .. } = translator;

    // Insert the static variable definitions at the front
    bt_definitions.splice(0..0, static_variables.into_iter().map(BtDefinition::StaticVariable));

    // Insert the static constants at the front
    bt_definitions.splice(0..0, make_constants(&symbols, &constants).into_iter().map(BtDefinition::StaticConstant));

    (BtRoot(bt_definitions), symbols, constants)
}

fn make_constants(symbols: &SymbolTable, constants: &ConstantTable) -> Vec<BtStaticConstant> {
    (0..constants.len())
        .map(|idx| {
            let constant_index = ConstantIndex(idx);
            let name = constants.make_const_symbol_name(constant_index);
            let Some(symbol) = symbols.get(parser::AstUniqueName::new(name.clone())) else {
                ICE!("Symbol table is missing entry for '{name}'");
            };

            let data_type = BtType::from(&symbol.data_type);

            BtStaticConstant { name, data_type, index: constant_index }
        })
        .collect()
}

fn translate_function(
    function: parser::AstFunction,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) -> BtFunctionDefn {
    if function.body.is_none() {
        ICE!("Function '{}' has no body", function.ident);
    }

    translator.reset_for_new_function(function.unique_name.as_str());

    let mut instructions = Vec::new();

    let block = function.body.unwrap();
    translate_block(block, &mut instructions, translator, driver);

    // Place a Return(0) at the very end of every function.
    //
    //      The C standard allows `main` to omit its return statement, in which case an implicit `return 0` is added.
    //      But it's undefined behaviour for any other function to reach the end of its body without a return (unless
    //      the return type is `void`, which we haven't implemented yet). So, for now, we always insert a return
    //      statement with `return 0` (of type int), since if there's an existing one then the extra one is harmless.
    //      It doesn't matter if the function's return type is, for example, `long` instead of `int` because we're not
    //      obligated to return a specific value, we're just ensuring that control returns to the caller.
    //
    //      Future: The optimization stage will remove the extra return.
    //
    //      TODO: Sema should find functions (except `main`) whose return type is not `void` and is missing a return.
    //
    instructions.push(BtInstruction::Return(BtValue::Constant(BtConstantValue::Int32(0))));

    let fn_type = function.declared_type.resolved_type.as_ref().unwrap();
    let parser::AstType::Function { return_type, params } = fn_type else {
        ICE!("Function '{}' has wrong AstType", function.ident);
    };

    let bt_return_type: BtType = return_type.as_ref().into();

    let bt_params = params
        .iter()
        .zip(&function.param_names)
        .map(|(data_type, (_, unique_name))| (BtType::from(data_type), unique_name.to_string()))
        .collect();

    BtFunctionDefn {
        name: function.unique_name.to_string(),
        is_global: function.linkage == parser::AstLinkage::External,
        return_type: bt_return_type,
        params: bt_params,
        instructions,
    }
}

fn translate_block(
    block: parser::AstBlock,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let block_items = block.0;
    for block_item in block_items {
        match block_item {
            parser::AstBlockItem::Statement(stmt) => translate_statement(stmt, instructions, translator, driver),

            parser::AstBlockItem::Declaration(decl) => {
                if let parser::AstDeclaration::Variable(var_decl) = decl {
                    // Add static storage duration variables to the translator, so that they get inserted as
                    // root-level definitions at the end of translation.
                    //
                    // Remember: in block scope, a static storage duration variable has no linkage.
                    //
                    if !var_decl.is_declaration_only && var_decl.storage == parser::AstStorageDuration::Static {
                        if var_decl.linkage == parser::AstLinkage::None {
                            const IS_GLOBAL: bool = false;
                            translate_static_storage_duration_variable(var_decl, IS_GLOBAL, translator);
                        } else {
                            ICE!("Static variable '{}' at block scope should have no linkage", var_decl.ident);
                        }
                    }
                    // Non-static variables with initializers are translated by `translate_variable_declaration` and
                    // their initializer expression is added to the function's instructions.
                    //
                    else if var_decl.initializer.is_some() {
                        translate_var_declaration(
                            var_decl.unique_name,
                            var_decl.initializer,
                            instructions,
                            translator,
                            driver,
                        );
                    }
                }
            }
        }
    }
}

fn translate_statement(
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    match stmt {
        parser::AstStatement::Expression(expr) => {
            translate_expression_statement(expr, instructions, translator, driver)
        }
        parser::AstStatement::Labeled { node_id: _, label_name, stmt } => {
            translate_labeled_statement(&label_name, *stmt, instructions, translator, driver)
        }
        parser::AstStatement::Compound(block) => translate_block(block, instructions, translator, driver),
        parser::AstStatement::Null => (),

        // Control statements: Conditional
        parser::AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            translate_if_statement(controlling_expr, *then_stmt, else_stmt, instructions, translator, driver)
        }

        parser::AstStatement::Switch { node_id, controlling_expr, body } => {
            translate_switch_statement(node_id, controlling_expr, *body, instructions, translator, driver)
        }

        parser::AstStatement::Case { switch_node_id, constant_expr, stmt, .. } => {
            let case_node_id = constant_expr.id();
            translate_switch_case_statement(switch_node_id, case_node_id, *stmt, instructions, translator, driver)
        }

        parser::AstStatement::Default { switch_node_id, stmt } => {
            translate_switch_default_statement(switch_node_id, *stmt, instructions, translator, driver)
        }

        // Control statements: Loops
        parser::AstStatement::While { node_id, controlling_expr, body } => {
            translate_while_statement(node_id, controlling_expr, *body, instructions, translator, driver)
        }
        parser::AstStatement::DoWhile { node_id, body, controlling_expr } => {
            translate_do_while_statement(node_id, *body, controlling_expr, instructions, translator, driver)
        }
        parser::AstStatement::For { .. } => translate_for_statement(stmt, instructions, translator, driver),

        // Control statements: Jumps
        parser::AstStatement::Break { enclosing_stmt_node_id } => {
            translate_break_statement(enclosing_stmt_node_id, instructions, translator, driver)
        }
        parser::AstStatement::Continue { loop_node_id } => {
            translate_continue_statement(loop_node_id, instructions, translator, driver)
        }
        parser::AstStatement::Goto { node_id: _, label_name } => {
            translate_goto_statement(&label_name, instructions, translator, driver)
        }
        parser::AstStatement::Return(expr) => translate_return_statement(expr, instructions, translator, driver),
    }
}

fn translate_return_statement(
    expr: parser::AstExpression,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    let return_value = expr::translate_full_expression(translator, &expr, instructions);
    instructions.push(BtInstruction::Return(return_value));
}

fn translate_expression_statement(
    expr: parser::AstExpression,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    expr::translate_full_expression_without_result(translator, &expr, instructions);
}

fn translate_goto_statement(
    label_name: &str,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    let label = translator.label_maker.make_user_label(label_name);
    instructions.push(BtInstruction::Jump { target: label });
}

fn translate_labeled_statement(
    label_name: &str,
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let label = translator.label_maker.make_user_label(label_name);
    instructions.push(BtInstruction::Label { id: label });
    translate_statement(stmt, instructions, translator, driver);
}

fn translate_if_statement(
    controlling_expr: parser::AstExpression,
    then_stmt: parser::AstStatement,
    else_stmt: Option<Box<parser::AstStatement>>,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let has_else_stmt = else_stmt.is_some();

    let else_label = translator.label_maker.make_unique_label("if_else");
    let end_label = translator.label_maker.make_unique_label("if_end");

    // Evaluate the controlling expression
    let controlling_value = expr::translate_full_expression(translator, &controlling_expr, instructions);

    if has_else_stmt {
        instructions.push(BtInstruction::JumpIfZero { condition: controlling_value, target: else_label.clone() });
    } else {
        instructions.push(BtInstruction::JumpIfZero { condition: controlling_value, target: end_label.clone() });
    }

    // Then clause
    translate_statement(then_stmt, instructions, translator, driver);

    if let Some(else_stmt) = else_stmt {
        instructions.push(BtInstruction::Jump { target: end_label.clone() });

        // Else label
        instructions.push(BtInstruction::Label { id: else_label });

        // Else clause
        translate_statement(*else_stmt, instructions, translator, driver);
    }

    // End label
    instructions.push(BtInstruction::Label { id: end_label });
}

fn translate_switch_statement(
    switch_node_id: parser::AstNodeId,
    controlling_expr: parser::AstExpression,
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    // Evaluate the controlling expression.
    //
    //      We always evaluate the controlling expression, even if there are no case/default labels,
    //      because the expression can have side effects (e.g. assignment).
    //
    let controlling_value = expr::translate_full_expression(translator, &controlling_expr, instructions);

    let has_default_case = translator.metadata.switch_has_default(switch_node_id);
    let switch_cases = translator.metadata.get_switch_cases(switch_node_id);

    // If there is no default case and also no case labels then we can ignore the remainder of the switch statement.
    if !has_default_case && switch_cases.is_empty() {
        return;
    }

    let cases: Vec<_> = switch_cases
        .into_iter()
        .map(|(case_value, case_node_id)| BtSwitchCase {
            value: BtValue::from(case_value),
            label: translator.label_maker.make_switch_case_label(switch_node_id, case_node_id),
        })
        .collect();

    let default_label =
        if has_default_case { Some(translator.label_maker.make_switch_label("default", switch_node_id)) } else { None };

    let break_label = translator.label_maker.make_control_label("break", switch_node_id);

    // We use a special IR instruction for the switch. Depending on the target, the codegen may choose
    // to use a jump table or a binary tree or a simple series of if-else comparisons. But we can't decide
    // that here in our IR, since we're not sure what capabilities the target has.
    instructions.push(BtInstruction::Switch {
        controlling_value,
        cases,
        default_label,
        break_label: break_label.clone(),
    });

    // Switch statement body
    translate_statement(stmt, instructions, translator, driver);

    // Break/end label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_switch_case_statement(
    switch_node_id: parser::AstNodeId,
    case_node_id: parser::AstNodeId,
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let case_label = translator.label_maker.make_switch_case_label(switch_node_id, case_node_id);
    instructions.push(BtInstruction::Label { id: case_label });
    translate_statement(stmt, instructions, translator, driver);
}

fn translate_switch_default_statement(
    switch_node_id: parser::AstNodeId,
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let default_label = translator.label_maker.make_switch_label("default", switch_node_id);
    instructions.push(BtInstruction::Label { id: default_label });
    translate_statement(stmt, instructions, translator, driver);
}

fn translate_while_statement(
    node_id: parser::AstNodeId,
    controlling_expr: parser::AstExpression,
    body: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Start of the loop / Continue label
    instructions.push(BtInstruction::Label { id: continue_label.clone() });

    // Loop controlling expression
    let condition_value = expr::translate_full_expression(translator, &controlling_expr, instructions);

    // If false, jump to end / break label
    instructions.push(BtInstruction::JumpIfZero { condition: condition_value, target: break_label.clone() });

    // Loop body
    translate_statement(body, instructions, translator, driver);

    // Unconditional jump to start of loop (aka continue label)
    instructions.push(BtInstruction::Jump { target: continue_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_do_while_statement(
    node_id: parser::AstNodeId,
    body: parser::AstStatement,
    controlling_expr: parser::AstExpression,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let start_label = translator.label_maker.make_control_label("start", node_id);
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Start of the loop
    instructions.push(BtInstruction::Label { id: start_label.clone() });

    // Loop body
    translate_statement(body, instructions, translator, driver);

    // Continue label
    instructions.push(BtInstruction::Label { id: continue_label });

    // Loop controlling expression
    let condition_value = expr::translate_full_expression(translator, &controlling_expr, instructions);

    // If true, jump to start of loop
    instructions.push(BtInstruction::JumpIfNotZero { condition: condition_value, target: start_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_for_statement(
    stmt: parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    driver: &mut Driver,
) {
    let parser::AstStatement::For { node_id, init, controlling_expr, post_expr: post, body } = stmt else {
        ICE!("Expected AstStatement::For");
    };

    let start_label = translator.label_maker.make_control_label("start", node_id);
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Initializer
    match *init {
        parser::AstForInitializer::Declaration(declarations) => {
            for decl in declarations {
                if let parser::AstDeclaration::Variable(var_decl) = decl {
                    translate_var_declaration(
                        var_decl.unique_name,
                        var_decl.initializer,
                        instructions,
                        translator,
                        driver,
                    );
                }
            }
        }
        parser::AstForInitializer::Expression(full_expr) => {
            if let Some(full_expr) = full_expr {
                expr::translate_full_expression_without_result(translator, &full_expr, instructions);
            }
        }
    }

    // Start of the loop
    instructions.push(BtInstruction::Label { id: start_label.clone() });

    // Loop controlling expression
    if let Some(controlling_expr) = controlling_expr {
        let condition_value = expr::translate_full_expression(translator, &controlling_expr, instructions);

        // If false, jump to end / break label
        instructions.push(BtInstruction::JumpIfZero { condition: condition_value, target: break_label.clone() });
    }

    // Loop body
    translate_statement(*body, instructions, translator, driver);

    // Continue label
    instructions.push(BtInstruction::Label { id: continue_label });

    // Post/update expression
    if let Some(post) = post {
        expr::translate_full_expression_without_result(translator, &post, instructions);
    }

    // Unconditional jump to start of loop
    instructions.push(BtInstruction::Jump { target: start_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_break_statement(
    enclosing_stmt_node_id: parser::AstNodeId,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    // Remember: This can break either a switch or a loop.
    let break_label = translator.label_maker.make_control_label("break", enclosing_stmt_node_id);
    instructions.push(BtInstruction::Jump { target: break_label });
}

fn translate_continue_statement(
    loop_node_id: parser::AstNodeId,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    let continue_label = translator.label_maker.make_control_label("continue", loop_node_id);
    instructions.push(BtInstruction::Jump { target: continue_label });
}

fn translate_var_declaration(
    unique_variable_name: parser::AstUniqueName,
    initializer: Option<parser::AstVariableInitializer>,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
    _driver: &mut Driver,
) {
    if let Some(initializer) = initializer {
        match initializer {
            parser::AstVariableInitializer::Scalar(full_expr) => {
                let init_result = expr::translate_full_expression(translator, &full_expr, instructions);
                let variable = BtValue::Variable(unique_variable_name.to_string());
                instructions.push(BtInstruction::Copy { src: init_result, dst: variable });
            }

            parser::AstVariableInitializer::Aggregate { .. } => {
                translate_aggregate_initializer(&unique_variable_name, 0, initializer, instructions, translator);
            }
        }
    }
}

fn translate_aggregate_initializer(
    unique_variable_name: &parser::AstUniqueName,
    offset: usize,
    initializer: parser::AstVariableInitializer,
    instructions: &mut Vec<BtInstruction>,
    translator: &mut BlueTacTranslator,
) -> usize {
    match initializer {
        parser::AstVariableInitializer::Scalar(full_expr) => {
            let src = expr::translate_full_expression(translator, &full_expr, instructions);
            let src_type = src.get_type(&translator.symbols);
            debug_assert!(src_type.bits() > 0 && src_type.bits().is_multiple_of(8));

            let dst_ptr = BtValue::Variable(unique_variable_name.to_string());
            instructions.push(BtInstruction::StoreAtOffset { src, dst_ptr, dst_offset: offset });

            offset + src_type.bits() / 8
        }

        parser::AstVariableInitializer::Aggregate { init, .. } => {
            let mut next_offset = offset;
            for ini in init {
                next_offset =
                    translate_aggregate_initializer(unique_variable_name, next_offset, ini, instructions, translator);
            }

            next_offset
        }
    }
}

fn translate_static_storage_duration_variable(
    var_decl: parser::AstVariableDeclaration,
    is_global: bool,
    translator: &mut BlueTacTranslator,
) {
    let mut init_value = var_decl
        .init_constant_eval
        .into_iter()
        .map(BtStaticStorageInitializer::from)
        .collect::<Vec<BtStaticStorageInitializer>>();

    let data_type: BtType = var_decl.declared_type.resolved_type.as_ref().unwrap().into();

    if init_value.is_empty() {
        if let BtType::Array { element_type, count } = &data_type {
            let zero_count = element_type.bits() / 8 * count;
            init_value = vec![BtStaticStorageInitializer::ZeroBytes(zero_count)];
        } else {
            init_value = vec![BtStaticStorageInitializer::Constant(data_type.get_const_default_value())];
        }
    }

    translator.add_static_storage_duration_variable(&var_decl.unique_name, is_global, data_type, init_value);
}
