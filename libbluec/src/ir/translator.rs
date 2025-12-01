// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `bluetac_translation` module defines the various functions to translate from the parser's AST into
//! BlueTac intermediate representation.

mod binary_expr;
mod expr;
mod unary_expr;

use super::{
    BtConstantValue, BtDefinition, BtFunctionDefn, BtInstruction, BtRoot, BtStaticStorageVariable, BtSwitchCase,
    BtType, BtValue,
};

use super::label_maker::LabelMaker;
use crate::ICE;
use crate::compiler_driver::diagnostics::SourceIdentifier;
use crate::lexer::SourceLocation;
use crate::parser;
use crate::sema::symbol_table::{Definition, SymbolAttributes, SymbolTable};

use std::collections::HashMap;

enum EvalExpr {
    Value(BtValue),
    Dereferenced(BtValue),
}

/// The translator manages state for translation from the AST to BlueTac IR.
pub struct BlueTacTranslator {
    metadata: parser::AstMetadata,
    pub symbols: SymbolTable,
    static_variables: Vec<BtStaticStorageVariable>,
    next_temp_variable_id: (usize, usize),
    label_maker: LabelMaker,
}

impl BlueTacTranslator {
    /// Creates a new BlueTac IR translator.
    pub fn new(metadata: parser::AstMetadata, symbols: SymbolTable) -> Self {
        Self {
            metadata,
            symbols,
            static_variables: Vec::new(),
            next_temp_variable_id: (0, 0),
            label_maker: LabelMaker::new(),
        }
    }

    /// Gets the `AstType` from the symbol table for the given AST expression.
    pub fn get_expression_type(&self, expr: &parser::AstExpression) -> &parser::AstType {
        self.get_ast_type_from_node(&expr.node_id())
    }

    /// Gets the `AstType` from the symbol table for the given AST node.
    pub fn get_ast_type_from_node(&self, node_id: &parser::AstNodeId) -> &parser::AstType {
        let Some(data_type) = self.metadata.get_node_type(node_id) else {
            ICE!("Symbol should exist for node {node_id}");
        };
        data_type
    }

    /// Adds a static storage duration variable (may be global or local).
    pub fn add_static_storage_duration_variable(
        &mut self,
        name: &parser::AstUniqueName,
        is_global: bool,
        data_type: BtType,
        init_value: BtConstantValue,
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
    /// It's valid to have multiple tentative definitions in file scope. In this case, a definition with a non-zero value
    /// takes precedence, and all other tentative definitions are removed. If all the tentative definitions have a value
    /// of zero then they are merged into one.
    pub fn deduplicate_static_variables(&mut self) {
        let mut deduped = HashMap::new();

        for var in self.static_variables.drain(..) {
            deduped
                .entry(var.name.clone())
                .and_modify(|existing: &mut BtStaticStorageVariable| {
                    // Take the new declaration's value if it's non-zero.
                    if !var.init_value.has_default_value() && existing.init_value.has_default_value() {
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
pub fn translate_ast_to_ir(translator: &mut BlueTacTranslator, ast: parser::AstRoot) -> BtRoot {
    let mut bt_definitions = ast
        .0
        .into_iter()
        .filter_map(|decl| match decl {
            parser::AstDeclaration::Function(func) => {
                if func.body.is_none() {
                    None
                } else {
                    Some(BtDefinition::Function(translate_function(translator, &func)))
                }
            }

            parser::AstDeclaration::Variable(var_decl) => {
                if var_decl.is_declaration_only {
                    return None;
                }

                let is_global = var_decl.linkage == parser::AstLinkage::External;
                let data_type: BtType = var_decl.declared_type.resolved_type.unwrap().into();

                let init_value = match var_decl.init_constant_value {
                    Some(const_value) => const_value.into(),
                    None => data_type.get_const_default_value(),
                };

                translator.add_static_storage_duration_variable(
                    &var_decl.unique_name,
                    is_global,
                    data_type,
                    init_value,
                );

                None
            }

            parser::AstDeclaration::TypeAlias(_) => None, // Ignore type aliases
        })
        .collect::<Vec<BtDefinition>>();

    // Merge the static variable definitions in case we have multiple tentative definitions.
    translator.deduplicate_static_variables();

    // Append the static variables
    bt_definitions.extend(translator.static_variables.iter().map(|var| BtDefinition::StaticVariable(var.clone())));

    BtRoot(bt_definitions)
}

fn translate_function(translator: &mut BlueTacTranslator, function: &parser::AstFunction) -> BtFunctionDefn {
    if function.body.is_none() {
        ICE!("Function '{}' has no body", function.ident);
    }

    translator.reset_for_new_function(function.unique_name.as_str());

    let mut instructions = Vec::new();

    let block = function.body.as_ref().unwrap();
    translate_block(translator, block, &mut instructions);

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
    translator: &mut BlueTacTranslator,
    block: &parser::AstBlock,
    instructions: &mut Vec<BtInstruction>,
) {
    let block_items = &block.0;
    for block_item in block_items {
        match block_item {
            parser::AstBlockItem::Statement(stmt) => translate_statement(translator, stmt, instructions),

            parser::AstBlockItem::Declaration(decl) => {
                if let parser::AstDeclaration::Variable(var_decl) = decl {
                    // Add static storage duration variables to the translator, so that they get inserted as
                    // root-level definitions at the end of translation.
                    //
                    // Remember: in block scope, a static storage duration variable has no linkage.
                    //
                    if !var_decl.is_declaration_only && var_decl.storage == parser::AstStorageDuration::Static {
                        if var_decl.linkage == parser::AstLinkage::None {
                            let data_type: BtType = var_decl.declared_type.resolved_type.clone().unwrap().into();

                            let init_value = match &var_decl.init_constant_value {
                                Some(const_value) => BtConstantValue::from(const_value.clone()),
                                None => data_type.get_const_default_value(),
                            };

                            translator.add_static_storage_duration_variable(
                                &var_decl.unique_name,
                                false, // is_global
                                data_type,
                                init_value,
                            );
                        } else {
                            ICE!("Static variable '{}' at block scope should have no linkage", var_decl.ident);
                        }
                    }
                    // Non-static variables with initializers are translated by `translate_variable_declaration` and
                    // their initializer expression is added to the function's instructions.
                    //
                    else if var_decl.init_expr.is_some() {
                        translate_var_declaration(translator, &var_decl.unique_name, &var_decl.init_expr, instructions);
                    }
                }
            }
        }
    }
}

fn translate_statement(
    translator: &mut BlueTacTranslator,
    stmt: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    match stmt {
        parser::AstStatement::Expression(expr) => translate_expression_statement(translator, expr, instructions),
        parser::AstStatement::Labeled { node_id: _, label_name, stmt } => {
            translate_labeled_statement(translator, label_name, stmt, instructions)
        }
        parser::AstStatement::Compound(block) => translate_block(translator, block, instructions),
        parser::AstStatement::Null => (),

        // Control statements: Conditional
        parser::AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            translate_if_statement(translator, controlling_expr, then_stmt, else_stmt, instructions)
        }

        parser::AstStatement::Switch { node_id, controlling_expr, body } => {
            translate_switch_statement(translator, node_id, controlling_expr, body, instructions)
        }

        parser::AstStatement::Case { switch_node_id, constant_expr, stmt, .. } => {
            let case_node_id = constant_expr.node_id;
            translate_switch_case_statement(translator, switch_node_id, &case_node_id, stmt, instructions)
        }

        parser::AstStatement::Default { switch_node_id, stmt } => {
            translate_switch_default_statement(translator, switch_node_id, stmt, instructions)
        }

        // Control statements: Loops
        parser::AstStatement::While { node_id, controlling_expr, body } => {
            translate_while_statement(translator, node_id, controlling_expr, body, instructions)
        }
        parser::AstStatement::DoWhile { node_id, body, controlling_expr } => {
            translate_do_while_statement(translator, node_id, body, controlling_expr, instructions)
        }
        parser::AstStatement::For { node_id, init, controlling_expr, post, body } => {
            translate_for_statement(translator, node_id, init, controlling_expr, post, body, instructions)
        }

        // Control statements: Jumps
        parser::AstStatement::Break { enclosing_stmt_node_id } => {
            translate_break_statement(translator, enclosing_stmt_node_id, instructions)
        }
        parser::AstStatement::Continue { loop_node_id } => {
            translate_continue_statement(translator, loop_node_id, instructions)
        }
        parser::AstStatement::Goto { node_id: _, label_name } => {
            translate_goto_statement(translator, label_name, instructions)
        }
        parser::AstStatement::Return(expr) => translate_return_statement(translator, expr, instructions),
    }
}

fn translate_return_statement(
    translator: &mut BlueTacTranslator,
    expr: &parser::AstFullExpression,
    instructions: &mut Vec<BtInstruction>,
) {
    let return_value = expr::translate_full_expression(translator, expr, instructions);
    instructions.push(BtInstruction::Return(return_value));
}

fn translate_expression_statement(
    translator: &mut BlueTacTranslator,
    expr: &parser::AstFullExpression,
    instructions: &mut Vec<BtInstruction>,
) {
    expr::translate_full_expression_without_result(translator, expr, instructions);
}

fn translate_goto_statement(
    translator: &mut BlueTacTranslator,
    label_name: &str,
    instructions: &mut Vec<BtInstruction>,
) {
    let label = translator.label_maker.make_user_label(label_name);
    instructions.push(BtInstruction::Jump { target: label });
}

fn translate_labeled_statement(
    translator: &mut BlueTacTranslator,
    label_name: &str,
    stmt: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    let label = translator.label_maker.make_user_label(label_name);
    instructions.push(BtInstruction::Label { id: label });
    translate_statement(translator, stmt, instructions);
}

fn translate_if_statement(
    translator: &mut BlueTacTranslator,
    controlling_expr: &parser::AstFullExpression,
    then_stmt: &parser::AstStatement,
    else_stmt: &Option<Box<parser::AstStatement>>,
    instructions: &mut Vec<BtInstruction>,
) {
    let has_else_stmt = else_stmt.is_some();

    let else_label = translator.label_maker.make_unique_label("if_else");
    let end_label = translator.label_maker.make_unique_label("if_end");

    // Evaluate the controlling expression
    let controlling_value = expr::translate_full_expression(translator, controlling_expr, instructions);

    if has_else_stmt {
        instructions.push(BtInstruction::JumpIfZero { condition: controlling_value, target: else_label.clone() });
    } else {
        instructions.push(BtInstruction::JumpIfZero { condition: controlling_value, target: end_label.clone() });
    }

    // Then clause
    translate_statement(translator, then_stmt, instructions);

    if let Some(else_stmt) = else_stmt {
        instructions.push(BtInstruction::Jump { target: end_label.clone() });

        // Else label
        instructions.push(BtInstruction::Label { id: else_label });

        // Else clause
        translate_statement(translator, else_stmt, instructions);
    }

    // End label
    instructions.push(BtInstruction::Label { id: end_label });
}

fn translate_switch_statement(
    translator: &mut BlueTacTranslator,
    switch_node_id: &parser::AstNodeId,
    controlling_expr: &parser::AstFullExpression,
    stmt: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    // Evaluate the controlling expression.
    //
    //      We always evaluate the controlling expression, even if there are no case/default labels,
    //      because the expression can have side effects (e.g. assignment).
    //
    let controlling_value = expr::translate_full_expression(translator, controlling_expr, instructions);

    let has_default_case = translator.metadata.switch_has_default(*switch_node_id);
    let switch_cases = translator.metadata.get_switch_cases(*switch_node_id);

    // If there is no default case and also no case labels then we can ignore the remainder of the switch statement.
    if !has_default_case && switch_cases.is_empty() {
        return;
    }

    let cases: Vec<_> = switch_cases
        .into_iter()
        .map(|(case_value, case_node_id)| BtSwitchCase {
            value: BtValue::from(parser::AstConstantValue::Integer(case_value)),
            label: translator.label_maker.make_switch_case_label(switch_node_id, &case_node_id),
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
    translate_statement(translator, stmt, instructions);

    // Break/end label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_switch_case_statement(
    translator: &mut BlueTacTranslator,
    switch_node_id: &parser::AstNodeId,
    case_node_id: &parser::AstNodeId,
    stmt: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    let case_label = translator.label_maker.make_switch_case_label(switch_node_id, case_node_id);
    instructions.push(BtInstruction::Label { id: case_label });
    translate_statement(translator, stmt, instructions);
}

fn translate_switch_default_statement(
    translator: &mut BlueTacTranslator,
    switch_node_id: &parser::AstNodeId,
    stmt: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    let default_label = translator.label_maker.make_switch_label("default", switch_node_id);
    instructions.push(BtInstruction::Label { id: default_label });
    translate_statement(translator, stmt, instructions);
}

fn translate_while_statement(
    translator: &mut BlueTacTranslator,
    node_id: &parser::AstNodeId,
    controlling_expr: &parser::AstFullExpression,
    body: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Start of the loop / Continue label
    instructions.push(BtInstruction::Label { id: continue_label.clone() });

    // Loop controlling expression
    let condition_value = expr::translate_full_expression(translator, controlling_expr, instructions);

    // If false, jump to end / break label
    instructions.push(BtInstruction::JumpIfZero { condition: condition_value, target: break_label.clone() });

    // Loop body
    translate_statement(translator, body, instructions);

    // Unconditional jump to start of loop (aka continue label)
    instructions.push(BtInstruction::Jump { target: continue_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_do_while_statement(
    translator: &mut BlueTacTranslator,
    node_id: &parser::AstNodeId,
    body: &parser::AstStatement,
    controlling_expr: &parser::AstFullExpression,
    instructions: &mut Vec<BtInstruction>,
) {
    let start_label = translator.label_maker.make_control_label("start", node_id);
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Start of the loop
    instructions.push(BtInstruction::Label { id: start_label.clone() });

    // Loop body
    translate_statement(translator, body, instructions);

    // Continue label
    instructions.push(BtInstruction::Label { id: continue_label });

    // Loop controlling expression
    let condition_value = expr::translate_full_expression(translator, controlling_expr, instructions);

    // If true, jump to start of loop
    instructions.push(BtInstruction::JumpIfNotZero { condition: condition_value, target: start_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_for_statement(
    translator: &mut BlueTacTranslator,
    node_id: &parser::AstNodeId,
    init: &parser::AstForInitializer,
    controlling_expr: &Option<parser::AstFullExpression>,
    post: &Option<parser::AstFullExpression>,
    body: &parser::AstStatement,
    instructions: &mut Vec<BtInstruction>,
) {
    let start_label = translator.label_maker.make_control_label("start", node_id);
    let continue_label = translator.label_maker.make_control_label("continue", node_id);
    let break_label = translator.label_maker.make_control_label("break", node_id);

    // Initializer
    match init {
        parser::AstForInitializer::Declaration(declarations) => {
            for decl in declarations {
                if let parser::AstDeclaration::Variable(var_decl) = decl {
                    translate_var_declaration(translator, &var_decl.unique_name, &var_decl.init_expr, instructions);
                }
            }
        }
        parser::AstForInitializer::Expression(expr) if expr.is_some() => {
            expr::translate_full_expression_without_result(translator, expr.as_ref().unwrap(), instructions);
        }
        _ => (),
    }

    // Start of the loop
    instructions.push(BtInstruction::Label { id: start_label.clone() });

    // Loop controlling expression
    if controlling_expr.is_some() {
        let condition_value =
            expr::translate_full_expression(translator, controlling_expr.as_ref().unwrap(), instructions);

        // If false, jump to end / break label
        instructions.push(BtInstruction::JumpIfZero { condition: condition_value, target: break_label.clone() });
    }

    // Loop body
    translate_statement(translator, body, instructions);

    // Continue label
    instructions.push(BtInstruction::Label { id: continue_label });

    // Post/update expression
    if post.is_some() {
        expr::translate_full_expression_without_result(translator, post.as_ref().unwrap(), instructions);
    }

    // Unconditional jump to start of loop
    instructions.push(BtInstruction::Jump { target: start_label });

    // Break label
    instructions.push(BtInstruction::Label { id: break_label });
}

fn translate_break_statement(
    translator: &mut BlueTacTranslator,
    enclosing_stmt_node_id: &parser::AstNodeId,
    instructions: &mut Vec<BtInstruction>,
) {
    // Remember: This can break either a switch or a loop.
    let break_label = translator.label_maker.make_control_label("break", enclosing_stmt_node_id);
    instructions.push(BtInstruction::Jump { target: break_label });
}

fn translate_continue_statement(
    translator: &mut BlueTacTranslator,
    loop_node_id: &parser::AstNodeId,
    instructions: &mut Vec<BtInstruction>,
) {
    let continue_label = translator.label_maker.make_control_label("continue", loop_node_id);
    instructions.push(BtInstruction::Jump { target: continue_label });
}

fn translate_var_declaration(
    translator: &mut BlueTacTranslator,
    unique_variable_name: &parser::AstUniqueName,
    init_expr: &Option<parser::AstFullExpression>,
    instructions: &mut Vec<BtInstruction>,
) {
    if let Some(expr) = init_expr {
        let init_result = expr::translate_full_expression(translator, expr, instructions);
        let variable = BtValue::Variable(unique_variable_name.to_string());
        instructions.push(BtInstruction::Copy { src: init_result, dst: variable });
    }
}
