// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `printer` module is used by the parent parser module to print a tree of the C AST to stdout.

use crate::parser::meta::AstMetadata;
use crate::parser::*;

/// Prints the AST with indentation
pub fn print(ast_root: &AstRoot, metadata: Option<&AstMetadata>) {
    println!("TranslationUnit");
    for decl in &ast_root.0 {
        print_declaration(decl, metadata, 1);
    }
    println!("{}|", make_close_indent(1));
}

fn print_declaration(decl: &AstDeclaration, metadata: Option<&AstMetadata>, level: usize) {
    match decl {
        AstDeclaration::Variable(var_decl) => print_variable_declaration(var_decl, metadata, level),
        AstDeclaration::Function(func_decl) => print_function(func_decl, metadata, level),
        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(alias_decl, metadata, level),
    }
}

fn print_variable_declaration(var_decl: &AstVariableDeclaration, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    print!("{}", indent);

    let storage = match var_decl.storage {
        AstStorageDuration::None => "none",
        AstStorageDuration::Automatic => "automatic",
        AstStorageDuration::Static => "static",
    };

    let linkage = match var_decl.linkage {
        AstLinkage::None => "none",
        AstLinkage::Internal => "internal",
        AstLinkage::External => "external",
    };

    if var_decl.is_declaration_only {
        print!("VariableDecl(");
    } else {
        print!("VariableDefn(");
    }

    print!(
        "name=\"{}\", unique=\"{}\", declared=\"{}\")",
        var_decl.ident.name, var_decl.unique_name, var_decl.declared_type
    );
    print_resolved_type(&var_decl.declared_type);
    println!();

    {
        let indent = make_indent(level + 1);

        println!("{indent}linkage={linkage}");

        if !var_decl.is_declaration_only {
            println!("{indent}storage={storage}");
        }

        // Initializer expression
        if let Some(init) = &var_decl.initializer {
            println!("{indent}init:");
            print_variable_initializer(init, metadata, level + 2);
            println!("{}|", make_close_indent(level + 2));
        }

        // Evaluated constant initializer
        if !var_decl.init_constant_eval.is_empty() {
            print!("{indent}init-constant-eval: ");

            let is_aggregate = var_decl.init_constant_eval.len() > 1;
            if is_aggregate {
                print!("{{ ");
            }

            const VALUES_PER_LINE: usize = 32;
            if var_decl.init_constant_eval.len() > VALUES_PER_LINE {
                print!("\n{indent}    ");
            }

            let mut first = true;
            for (idx, val) in var_decl.init_constant_eval.iter().enumerate() {
                if !first {
                    print!(", ");
                }

                if idx > 0 && idx.is_multiple_of(VALUES_PER_LINE) {
                    print!("\n{indent}    ");
                }
                
                print!("{val}");
                first = false;
            }

            if is_aggregate {
                print!(" }}");
            }

            println!();
        }

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_variable_initializer(init: &AstVariableInitializer, metadata: Option<&AstMetadata>, level: usize) {
    match init {
        AstVariableInitializer::Scalar(full_expr) => print_full_expression(full_expr, metadata, level),

        AstVariableInitializer::Aggregate { node_id, init } => {
            let indent = make_indent(level + 1);
            print!("{indent}Aggregate");
            print_node_type(node_id, metadata);
            println!();

            for i in init {
                print_variable_initializer(i, metadata, level + 2);
            }

            println!("{}|", make_close_indent(level + 1));
        }
    }
}

fn print_function(function: &AstFunction, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);

    let is_decl = function.body.is_none();

    let linkage = match function.linkage {
        AstLinkage::None => "none",
        AstLinkage::Internal => "internal",
        AstLinkage::External => "external",
    };

    print!("{}", indent);
    if is_decl {
        print!("FunctionDecl");
    } else {
        print!("Function");
    }

    print!("(name=\"{}\", signature=\"{}\", linkage={linkage})", function.unique_name, function.declared_type);
    print_resolved_type(&function.declared_type);
    println!();

    {
        let indent = make_indent(level + 1);

        if let Some(func_body) = &function.body {
            print!("{indent}params=");

            if function.param_names.is_empty() {
                print!("void");
            } else {
                let mut sep = "";
                for (param_ident, _) in &function.param_names {
                    let name = &param_ident.name;
                    print!("{sep}\"{name}\"");
                    sep = ", ";
                }

                print!(", unique=");
                sep = "";
                for (_, unique) in &function.param_names {
                    print!("{sep}\"{unique}\"");
                    sep = ", ";
                }
            }

            println!();

            println!("{}body:", indent);
            print_block(func_body, metadata, level + 2);
            println!("{}|", make_close_indent(level + 2));
        }

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_type_alias_declaration(alias_decl: &AstTypeAliasDeclaration, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    println!("{indent}TypeAliasDecl");

    {
        let indent = make_indent(level + 1);
        println!("{indent}decl=");

        print_declaration(&alias_decl.decl, metadata, level + 2);

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_block(block: &AstBlock, metadata: Option<&AstMetadata>, level: usize) {
    let block_items = &block.0;
    for item in block_items {
        match item {
            AstBlockItem::Declaration(decl) => print_declaration(decl, metadata, level),
            AstBlockItem::Statement(stmt) => print_statement(stmt, metadata, level),
        }
    }
}

fn print_statement(stmt: &AstStatement, metadata: Option<&AstMetadata>, level: usize) {
    match stmt {
        AstStatement::Expression(expr) => print_expression_statement(expr, metadata, level),
        AstStatement::Labeled { node_id: _, label_name, stmt } => {
            print_labeled_statement(label_name, stmt, metadata, level)
        }
        AstStatement::Compound(block) => print_compound_statement(block, metadata, level),
        AstStatement::Null => print_null_statement(level),

        // Control statements: Conditional
        AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            print_if_statement(controlling_expr, then_stmt, else_stmt, metadata, level)
        }
        AstStatement::Switch { node_id, controlling_expr, body } => {
            print_switch_statement(node_id, controlling_expr, body, metadata, level)
        }

        // Control statements: Loops
        AstStatement::While { node_id, controlling_expr, body } => {
            print_while_statement(node_id, controlling_expr, body, metadata, level)
        }
        AstStatement::DoWhile { node_id, body, controlling_expr } => {
            print_do_while_statement(node_id, body, controlling_expr, metadata, level)
        }
        AstStatement::For { node_id, init, controlling_expr, post, body } => {
            print_for_statement(node_id, init, controlling_expr, post, body, metadata, level)
        }

        // Control statements: Jumps
        AstStatement::Break { enclosing_stmt_node_id } => print_break_statement(enclosing_stmt_node_id, level),
        AstStatement::Continue { loop_node_id } => print_continue_statement(loop_node_id, level),
        AstStatement::Goto { node_id: _, label_name } => print_goto_statement(label_name, level),
        AstStatement::Case { switch_node_id, constant_expr, stmt } => {
            print_switch_case_statement(switch_node_id, constant_expr, stmt, metadata, level)
        }
        AstStatement::Default { switch_node_id, stmt } => {
            print_switch_default_statement(switch_node_id, stmt, metadata, level)
        }
        AstStatement::Return(expr) => print_return_statement(expr, metadata, level),
    }
}

fn print_expression_statement(expr: &AstFullExpression, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    println!("{indent}ExprStatement");
    print_full_expression(expr, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_labeled_statement(label_name: &String, stmt: &AstStatement, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Labeled(name=\"{}\")", label_name);

    print_statement(stmt, metadata, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_compound_statement(block: &AstBlock, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Compound");
    print_block(block, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_null_statement(level: usize) {
    let indent = make_indent(level);
    println!("{indent}NullStatement()");
    println!("{}|", make_close_indent(level + 1));
}

fn print_if_statement(
    controlling_expr: &AstFullExpression,
    then_stmt: &AstStatement,
    else_stmt: &Option<Box<AstStatement>>,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}If()");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}then:");
        print_statement(then_stmt, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    if else_stmt.is_some() {
        let indent = make_indent(level + 1);
        println!("{indent}else:");
        let else_stmt = else_stmt.as_ref().unwrap();
        print_statement(else_stmt, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_statement(
    node_id: &AstNodeId,
    controlling_expr: &AstFullExpression,
    body: &AstStatement,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}Switch(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_while_statement(
    node_id: &AstNodeId,
    controlling_expr: &AstFullExpression,
    body: &AstStatement,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}While(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_do_while_statement(
    node_id: &AstNodeId,
    body: &AstStatement,
    controlling_expr: &AstFullExpression,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}Do(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}while:");
        print_full_expression(controlling_expr, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_for_statement(
    node_id: &AstNodeId,
    init: &AstForInitializer,
    controlling_expr: &Option<AstFullExpression>,
    post_expr: &Option<AstFullExpression>,
    body: &AstStatement,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}For(id={node_id})");

    // Init
    {
        let indent = make_indent(level + 1);
        println!("{indent}init:");

        match init {
            AstForInitializer::Declaration(var_decls) => {
                for decl in var_decls {
                    print_declaration(decl, metadata, level + 2);
                }
            }
            AstForInitializer::Expression(expr) => match expr {
                Some(expr) => {
                    print_full_expression(expr, metadata, level + 2);
                }
                None => {
                    let indent = make_indent(level + 2);
                    println!("{indent}None");
                }
            },
        }

        println!("{}|", make_close_indent(level + 2));
    }

    // Condition
    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");

        match controlling_expr {
            Some(controlling_expr) => {
                print_full_expression(controlling_expr, metadata, level + 2);
            }
            None => {
                let indent = make_indent(level + 2);
                println!("{indent}None");
            }
        }

        println!("{}|", make_close_indent(level + 2));
    }

    // Post
    {
        let indent = make_indent(level + 1);
        println!("{indent}post:");

        match post_expr {
            Some(post_expr) => {
                print_full_expression(post_expr, metadata, level + 2);
            }
            None => {
                let indent = make_indent(level + 2);
                println!("{indent}None");
            }
        }

        println!("{}|", make_close_indent(level + 2));
    }

    // Body
    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_break_statement(enclosing_stmt_node_id: &AstNodeId, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Break(enclosing_stmt_id={})", enclosing_stmt_node_id);
    println!("{}|", make_close_indent(level + 1));
}

fn print_continue_statement(loop_node_id: &AstNodeId, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Continue(loop_id={})", loop_node_id);
    println!("{}|", make_close_indent(level + 1));
}

fn print_goto_statement(label: &String, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Goto(label=\"{}\")", label);
    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_case_statement(
    switch_node_id: &AstNodeId,
    constant_expr: &AstFullExpression,
    stmt: &AstStatement,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}Case(switch_id={})", switch_node_id);

    print!("{}", indent);
    println!("  condition=");
    print_full_expression(constant_expr, metadata, level + 1);

    print!("{}", indent);
    println!("  body=");
    print_statement(stmt, metadata, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_default_statement(
    switch_node_id: &AstNodeId,
    stmt: &AstStatement,
    metadata: Option<&AstMetadata>,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}Default(switch_id={})", switch_node_id);
    print_statement(stmt, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_return_statement(expr: &AstFullExpression, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Return");
    print_full_expression(expr, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_full_expression(full_expr: &AstFullExpression, metadata: Option<&AstMetadata>, level: usize) {
    print_expression(&full_expr.expr, metadata, level);
}

fn print_expression(expr: &AstExpression, metadata: Option<&AstMetadata>, level: usize) {
    let indent = make_indent(level);
    match expr {
        AstExpression::BinaryOperation { node_id, op, left, right } => {
            print!("{indent}BinaryOp({})", op);
            print_node_type(node_id, metadata);
            println!();

            print_expression(left, metadata, level + 1);
            print_expression(right, metadata, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::UnaryOperation { node_id, op, expr } => {
            print!("{indent}UnaryOp({})", op);
            print_node_type(node_id, metadata);
            println!();

            print_expression(expr, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Cast { target_type, expr, .. } => {
            let target_type_str = target_type.to_string();

            if target_type_str.is_empty() {
                print!("{indent}Cast");
            } else {
                print!("{indent}Cast(to=\"{}\")", target_type_str);
            }

            print_resolved_type(target_type);
            println!();

            print_expression(expr, metadata, level + 1);
        }

        AstExpression::Deref { node_id, expr } => {
            print!("{indent}Deref");
            print_node_type(node_id, metadata);
            println!();

            print_expression(expr, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::AddressOf { node_id, expr } => {
            print!("{indent}AddressOf");
            print_node_type(node_id, metadata);
            println!();

            print_expression(expr, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Subscript { node_id, expr1, expr2 } => {
            print!("{indent}Subscript");
            print_node_type(node_id, metadata);
            println!();

            print_expression(expr1, metadata, level + 1);
            print_expression(expr2, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Assignment { node_id, computation_node_id, op, lhs, rhs } => {
            if *op == AstAssignmentOp::Assignment {
                print!("{indent}Assignment");
            } else {
                let token_type = lexer::TokenType::from(*op);

                print!("{indent}CompoundAssignment ('{token_type}'");

                if let Some(metadata) = metadata
                    && let Some(comp_node_type) = metadata.get_node_type(computation_node_id)
                {
                    print!(" [Computation Type = '{comp_node_type}']")
                }

                print!(")");
            }

            print_node_type(node_id, metadata);
            println!();

            print_expression(lhs, metadata, level + 1);
            print_expression(rhs, metadata, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Conditional { node_id, expr, consequent, alternative } => {
            print!("{indent}Ternary");
            print_node_type(node_id, metadata);
            println!();

            // Condition
            {
                let indent = make_indent(level + 1);
                println!("{indent}condition:");
                print_expression(expr, metadata, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            // Consequent
            {
                let indent = make_indent(level + 1);
                println!("{indent}consequent:");
                print_expression(consequent, metadata, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            // Consequent
            {
                let indent = make_indent(level + 1);
                println!("{indent}alternative:");
                print_expression(alternative, metadata, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::FunctionCall { node_id, designator, args, .. } => {
            print!("{indent}Call");
            print_node_type(node_id, metadata);
            println!();

            // Designator
            {
                let indent = make_indent(level + 1);
                println!("{indent}designator:");
                print_expression(designator, metadata, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            // Args
            {
                let indent = make_indent(level + 1);
                print!("{indent}args:");

                if args.is_empty() {
                    println!(" void");
                } else {
                    println!();
                    for arg in args {
                        print_expression(arg, metadata, level + 2);
                    }
                }

                println!("{}|", make_close_indent(level + 2));
            }

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Identifier { node_id, name, unique_name, .. } => {
            print!("{indent}Identifier(name=\"{}\", unique=\"{}\")", name, unique_name);
            print_node_type(node_id, metadata);
            println!();
        }

        AstExpression::IntegerLiteral { node_id, literal, literal_base, value, kind } => {
            print!("{indent}");

            // If we've performed typechecking then we don't need to print its kind, since we can print its
            // resolved AstType instead.
            if metadata.is_some() {
                print!("IntegerLiteral({literal}");
                if *literal_base != 10 {
                    print!(" [{value}]");
                }
                print!(")");
                print_node_type(node_id, metadata);
                println!();
            } else {
                print!("IntegerLiteral({kind} = {literal}");
                if *literal_base != 10 {
                    print!(" [{value}]");
                }
                println!(")");
            }
        }

        AstExpression::FloatLiteral { node_id, literal, literal_base, kind, value } => {
            print!("{indent}");
            if *literal_base == 10 {
                print!("FloatLiteral({kind} = {literal})");
            } else {
                print!("FloatLiteral({kind} = {literal} [{value}])");
            }
            print_node_type(node_id, metadata);
            println!();
        }
    }
}

fn print_resolved_type(declared_type: &AstDeclaredType) {
    if let Some(ty) = &declared_type.resolved_type {
        print!(" [Type = '{ty}']");
    }
}

fn print_node_type(node_id: &AstNodeId, metadata: Option<&AstMetadata>) {
    if let Some(metadata) = metadata
        && let Some(node_type) = metadata.get_node_type(node_id)
    {
        print!(" [Type = '{node_type}']")
    }
}

fn make_indent(level: usize) -> String {
    "|  ".repeat(level)
}

fn make_close_indent(level: usize) -> String {
    if level == 0 {
        return String::new();
    }
    let first = "|  ".repeat(level - 1);
    let second = "|__";
    first + second
}
