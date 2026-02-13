// Copyright 2025-2026 Neil Henderson
//
//! The `printer` module is used by the parent parser module to print a tree of the C AST to stdout.

use crate::parser::meta::AstMetadata;
use crate::parser::*;

/// Prints the AST with indentation
pub fn print(ast_root: &AstRoot, metadata: &AstMetadata) {
    println!("{}", format_stmt_node("TranslationUnit"));

    let len = ast_root.0.len();
    for (idx, decl) in ast_root.0.iter().enumerate() {
        print_declaration(decl, metadata, String::new(), idx == len - 1);
    }
}

fn print_declaration(decl: &AstDeclaration, metadata: &AstMetadata, prefix: String, is_last: bool) {
    match decl {
        AstDeclaration::Variable(var_decl) => print_variable_declaration(var_decl, metadata, prefix, is_last),
        AstDeclaration::Function(func_decl) => print_function(func_decl, metadata, prefix, is_last),
        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(alias_decl, metadata, prefix, is_last),
    }
}

fn print_variable_declaration(
    var_decl: &AstVariableDeclaration,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    print!("{prefix}{}", make_connector(is_last));

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
        print!("{}: ", format_decl_node("VariableDecl"));
    } else {
        print!("{}: ", format_decl_node("VariableDefn"));
    }

    print!(
        "{} [{}], declared=\"{}\"",
        format_ident(&var_decl.ident.name),
        var_decl.unique_name,
        var_decl.declared_type
    );
    print_resolved_type(&var_decl.declared_type);
    println!();

    // TODO: If parent is a TypeAliasDecl then ignore these attributes

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);

        println!("{}{}Linkage: {linkage}", child_prefix, make_child_connector());

        if !var_decl.is_declaration_only {
            println!("{}{}Storage: {storage}", child_prefix, make_child_connector());
        }

        // Initializer expression
        print!("{}{}Initializer", child_prefix, make_last_child_connector());

        if let Some(init) = &var_decl.initializer {
            println!();

            let has_constant_eval = !var_decl.init_constant_eval.is_empty();

            let new_prefix = new_tree_prefix(child_prefix.clone(), true);
            print_variable_initializer(init, metadata, new_prefix.clone(), !has_constant_eval);

            // Evaluated constant initializer
            if has_constant_eval {
                let connector = make_last_child_connector();
                print!("{new_prefix}{connector}Constant Eval: ");

                let is_aggregate = var_decl.init_constant_eval.len() > 1;
                if is_aggregate {
                    print!("{{ ");
                }

                const VALUES_PER_LINE: usize = 32;
                if var_decl.init_constant_eval.len() > VALUES_PER_LINE {
                    print!("\n    ");
                }

                let mut first = true;
                for (idx, val) in var_decl.init_constant_eval.iter().enumerate() {
                    if !first {
                        print!(", ");
                    }

                    if idx > 0 && idx.is_multiple_of(VALUES_PER_LINE) {
                        print!("\n    ");
                    }

                    let val = format_expr_node(&val.to_string());
                    print!("{val}");
                    first = false;
                }

                if is_aggregate {
                    print!(" }}");
                }

                println!();
            }
        } else {
            println!(": none");
        }
    }
}

fn print_variable_initializer(init: &AstVariableInitializer, metadata: &AstMetadata, prefix: String, is_last: bool) {
    match init {
        AstVariableInitializer::Scalar(expr) => {
            print_expression(expr, metadata, prefix, is_last);
        }

        AstVariableInitializer::Aggregate { node_id, init } => {
            print!("{}{}Aggregate", prefix, make_connector(is_last));
            print_node_metadata(*node_id, metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix.clone(), is_last);

            let len = init.len();
            for (idx, ini) in init.iter().enumerate() {
                print_variable_initializer(ini, metadata, new_prefix.clone(), idx == len - 1);
            }
        }
    }
}

fn print_function(function: &AstFunction, metadata: &AstMetadata, prefix: String, is_last: bool) {
    print!("{}{}", prefix, make_connector(is_last));

    let is_decl = function.body.is_none();

    let linkage = match function.linkage {
        AstLinkage::None => "none",
        AstLinkage::Internal => "internal",
        AstLinkage::External => "external",
    };

    if is_decl {
        print!("{}", format_decl_node("FunctionDecl"));
    } else {
        print!("{}", format_decl_node("FunctionDefn"));
    }

    print!(
        ": {}, signature: \"{}\", linkage: {linkage}",
        format_ident(function.unique_name.as_str()),
        function.declared_type
    );
    print_resolved_type(&function.declared_type);
    println!();

    {
        if let Some(func_body) = &function.body {
            let child_prefix = new_tree_prefix(prefix.clone(), is_last);

            let params_node = format_decl_node("Params");
            print!("{}{}{params_node}", child_prefix.clone(), make_child_connector());

            if function.param_names.is_empty() {
                println!(": none");
            } else {
                println!();

                let params_prefix = new_tree_prefix(child_prefix.clone(), false);

                let len = function.param_names.len();
                for (idx, (param_ident, unique)) in function.param_names.iter().enumerate() {
                    let param_node = format_decl_node("ParamDecl");
                    print!("{}{}{param_node}: ", params_prefix.clone(), make_connector(idx == len - 1));

                    let name = format_ident(&param_ident.name);
                    println!("{name} [{unique}]");
                }
            }

            let body_node = format_stmt_node("Body");
            println!("{}{}{body_node}", child_prefix.clone(), make_last_child_connector());

            let new_prefix = new_tree_prefix(child_prefix.clone(), true);
            print_block(func_body, metadata, new_prefix);
        }
    }
}

fn print_type_alias_declaration(
    alias_decl: &AstTypeAliasDeclaration,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let type_alias_node = format_decl_node("TypeAliasDecl");
    let connector = make_connector(is_last);
    println!("{prefix}{connector}{type_alias_node}");

    {
        let new_prefix = new_tree_prefix(prefix, is_last);
        print_declaration(&alias_decl.decl, metadata, new_prefix, true);
    }
}

fn print_block(block: &AstBlock, metadata: &AstMetadata, prefix: String) {
    let block_items = &block.0;
    let len = block_items.len();
    for (idx, item) in block_items.iter().enumerate() {
        match item {
            AstBlockItem::Declaration(decl) => print_declaration(decl, metadata, prefix.clone(), idx == len - 1),
            AstBlockItem::Statement(stmt) => print_statement(stmt, metadata, prefix.clone(), idx == len - 1),
        }
    }
}

fn print_statement(stmt: &AstStatement, metadata: &AstMetadata, prefix: String, is_last: bool) {
    match stmt {
        AstStatement::Expression(expr) => print_expression_statement(expr, metadata, prefix, is_last),
        AstStatement::Labeled { node_id: _, label_name, stmt } => {
            print_labeled_statement(label_name, stmt, metadata, prefix, is_last)
        }
        AstStatement::Compound(block) => print_compound_statement(block, metadata, prefix, is_last),
        AstStatement::Null => print_null_statement(prefix, is_last),

        // Control statements: Conditional
        AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            print_if_statement(controlling_expr, then_stmt, else_stmt, metadata, prefix, is_last)
        }
        AstStatement::Switch { node_id, controlling_expr, body } => {
            print_switch_statement(*node_id, controlling_expr, body, metadata, prefix, is_last)
        }

        // Control statements: Loops
        AstStatement::While { node_id, controlling_expr, body } => {
            print_while_statement(*node_id, controlling_expr, body, metadata, prefix, is_last)
        }
        AstStatement::DoWhile { node_id, body, controlling_expr } => {
            print_do_while_statement(*node_id, body, controlling_expr, metadata, prefix, is_last)
        }
        AstStatement::For { .. } => print_for_statement(stmt, metadata, prefix, is_last),

        // Control statements: Jumps
        AstStatement::Break { enclosing_stmt_node_id } => {
            print_break_statement(*enclosing_stmt_node_id, prefix, is_last)
        }
        AstStatement::Continue { loop_node_id } => print_continue_statement(*loop_node_id, prefix, is_last),
        AstStatement::Goto { node_id: _, label_name } => print_goto_statement(label_name, prefix, is_last),
        AstStatement::Case { switch_node_id, constant_expr, stmt } => {
            print_switch_case_statement(*switch_node_id, constant_expr, stmt, metadata, prefix, is_last)
        }
        AstStatement::Default { switch_node_id, stmt } => {
            print_switch_default_statement(*switch_node_id, stmt, metadata, prefix, is_last)
        }
        AstStatement::Return(expr) => print_return_statement(expr, metadata, prefix, is_last),
    }
}

fn print_expression_statement(expr: &AstExpression, metadata: &AstMetadata, prefix: String, is_last: bool) {
    println!("{prefix}{}{}", make_connector(is_last), format_stmt_node("ExprStatement"));
    let new_prefix = new_tree_prefix(prefix, is_last);
    print_expression(expr, metadata, new_prefix, true);
}

fn print_labeled_statement(
    label_name: &String,
    stmt: &AstStatement,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    println!("{prefix}{}{}(name=\"{label_name}\")", make_connector(is_last), format_stmt_node("Labeled"));

    let new_prefix = new_tree_prefix(prefix, is_last);
    print_statement(stmt, metadata, new_prefix, true);
}

fn print_compound_statement(block: &AstBlock, metadata: &AstMetadata, prefix: String, is_last: bool) {
    println!("{prefix}{}{}", make_connector(is_last), format_stmt_node("Compound"));

    let new_prefix = new_tree_prefix(prefix, true);
    print_block(block, metadata, new_prefix);
}

fn print_null_statement(prefix: String, is_last: bool) {
    println!("{prefix}{}{}", make_connector(is_last), format_stmt_node("NullStatement"));
}

fn print_if_statement(
    controlling_expr: &AstExpression,
    then_stmt: &AstStatement,
    else_stmt: &Option<Box<AstStatement>>,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    println!("{prefix}{}{}", make_connector(is_last), format_stmt_node("If"));

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Condition", child_prefix, make_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, is_last);
        print_expression(controlling_expr, metadata, new_prefix, true);
    }

    let has_else = else_stmt.is_some();

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Then", child_prefix, make_connector(!has_else));
        let new_prefix = new_tree_prefix(child_prefix, !has_else);
        print_statement(then_stmt, metadata, new_prefix, true);
    }

    if has_else {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Else", child_prefix, make_last_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, true);

        let else_stmt = else_stmt.as_ref().unwrap();
        print_statement(else_stmt, metadata, new_prefix, true);
    }
}

fn print_switch_statement(
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let switch_node = format_stmt_node("Switch");
    print!("{prefix}{}{switch_node}", make_connector(is_last));
    print_node_metadata(node_id, metadata);
    println!();

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Condition", child_prefix, make_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, false);
        print_expression(controlling_expr, metadata, new_prefix, true);
    }

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Body", child_prefix, make_last_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, true);
        print_statement(body, metadata, new_prefix, true);
    }
}

fn print_while_statement(
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let while_node = format_stmt_node("While");
    print!("{prefix}{}{while_node}", make_connector(is_last));
    print_node_metadata(node_id, metadata);
    println!();

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Condition", child_prefix, make_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, false);
        print_expression(controlling_expr, metadata, new_prefix, true);
    }

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Body", child_prefix, make_last_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, true);
        print_statement(body, metadata, new_prefix, true);
    }
}

fn print_do_while_statement(
    node_id: AstNodeId,
    body: &AstStatement,
    controlling_expr: &AstExpression,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let do_node = format_stmt_node("Do");
    print!("{prefix}{}{do_node}", make_connector(is_last));
    print_node_metadata(node_id, metadata);
    println!();

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Body", child_prefix, make_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, false);
        print_statement(body, metadata, new_prefix, true);
    }

    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}While", child_prefix, make_last_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, true);
        print_expression(controlling_expr, metadata, new_prefix, true);
    }
}

fn print_for_statement(stmt: &AstStatement, metadata: &AstMetadata, prefix: String, is_last: bool) {
    let AstStatement::For { node_id, init, controlling_expr, post_expr, body } = stmt else {
        ICE!("Expected AstStatement::For");
    };

    let for_node = format_stmt_node("For");
    print!("{prefix}{}{for_node}", make_connector(is_last));
    print_node_metadata(*node_id, metadata);
    println!();

    // Init
    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Init", child_prefix, make_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, false);

        match init.as_ref() {
            AstForInitializer::Declaration(var_decls) => {
                let len = var_decls.len();
                for (idx, decl) in var_decls.iter().enumerate() {
                    print_declaration(decl, metadata, new_prefix.clone(), idx == len - 1);
                }
            }
            AstForInitializer::Expression(expr) => match expr {
                Some(expr) => {
                    print_expression(expr, metadata, new_prefix, true);
                }
                None => {
                    println!("{new_prefix}None");
                }
            },
        }
    }

    // Condition
    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        print!("{}{}Condition", child_prefix, make_child_connector());

        match controlling_expr {
            Some(controlling_expr) => {
                println!();
                let new_prefix = new_tree_prefix(child_prefix, false);
                print_expression(controlling_expr, metadata, new_prefix, true);
            }
            None => {
                println!(": None");
            }
        }
    }

    // Post
    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        print!("{}{}Post", child_prefix, make_child_connector());

        match post_expr {
            Some(post_expr) => {
                println!();
                let new_prefix = new_tree_prefix(child_prefix, false);
                print_expression(post_expr, metadata, new_prefix, true);
            }
            None => {
                println!(": None");
            }
        }
    }

    // Body
    {
        let child_prefix = new_tree_prefix(prefix.clone(), is_last);
        println!("{}{}Body", child_prefix, make_last_child_connector());
        let new_prefix = new_tree_prefix(child_prefix, true);
        print_statement(body, metadata, new_prefix, true);
    }
}

fn print_break_statement(enclosing_stmt_node_id: AstNodeId, prefix: String, is_last: bool) {
    let break_node = format_stmt_node("Break");
    println!("{prefix}{}{break_node} [Enclosing Stmt Id = {enclosing_stmt_node_id}]", make_connector(is_last));
}

fn print_continue_statement(loop_node_id: AstNodeId, prefix: String, is_last: bool) {
    let continue_node = format_stmt_node("Continue");
    println!("{prefix}{}{continue_node}(loop_id={loop_node_id})", make_connector(is_last));
}

fn print_goto_statement(label: &String, prefix: String, is_last: bool) {
    let goto_node = format_stmt_node("Goto");
    println!("{prefix}{}{goto_node}(label={label})", make_connector(is_last));
}

fn print_switch_case_statement(
    switch_node_id: AstNodeId,
    constant_expr: &AstExpression,
    stmt: &AstStatement,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let case_node = format_stmt_node("Case");
    println!("{prefix}{}{case_node} [Switch Id = {switch_node_id}]", make_connector(is_last));

    let child_prefix = new_tree_prefix(prefix.clone(), is_last);
    println!("{}{}Constant Expr", child_prefix, make_child_connector());
    let new_prefix = new_tree_prefix(child_prefix, false);
    print_expression(constant_expr, metadata, new_prefix, true);

    let child_prefix = new_tree_prefix(prefix.clone(), is_last);
    println!("{}{}Statement", child_prefix, make_last_child_connector());
    let new_prefix = new_tree_prefix(child_prefix, true);
    print_statement(stmt, metadata, new_prefix, true);
}

fn print_switch_default_statement(
    switch_node_id: AstNodeId,
    stmt: &AstStatement,
    metadata: &AstMetadata,
    prefix: String,
    is_last: bool,
) {
    let default_node = format_stmt_node("Default");
    println!("{prefix}{}{default_node} [Switch Id = {switch_node_id}]", make_connector(is_last));

    let new_prefix = new_tree_prefix(prefix, true);
    print_statement(stmt, metadata, new_prefix, true);
}

fn print_return_statement(expr: &AstExpression, metadata: &AstMetadata, prefix: String, is_last: bool) {
    let return_node = format_stmt_node("Return");
    println!("{prefix}{}{return_node}", make_connector(is_last));

    let new_prefix = new_tree_prefix(prefix, is_last);
    print_expression(expr, metadata, new_prefix, true);
}

fn print_expression(expr: &AstExpression, metadata: &AstMetadata, prefix: String, is_last: bool) {
    let node_name = get_expression_node_name(expr.kind());
    let connector = make_connector(is_last);

    match expr.kind() {
        AstExpressionKind::Unary { op, operand } => {
            let op = format_expr_node(&op.to_string());
            print!("{prefix}{connector}{node_name}({op})");
            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(operand, metadata, new_prefix, true);
        }

        AstExpressionKind::Binary { op, lhs, rhs } => {
            let op = format_expr_node(&op.to_string());
            print!("{prefix}{connector}{node_name}({op})");
            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(lhs, metadata, new_prefix.clone(), false);
            print_expression(rhs, metadata, new_prefix, true);
        }

        AstExpressionKind::Assignment { computation_node_id, op, lhs, rhs } => {
            if *op == AstAssignmentOp::Assignment {
                print!("{prefix}{connector}{node_name}");
            } else {
                let token_type = lexer::TokenType::from(*op);
                let token_type = format_expr_node(&token_type.to_string());

                print!("{prefix}{connector}{node_name} ({token_type})");

                if let Some(comp_node_type) = metadata.try_get_node_type(*computation_node_id) {
                    let comp_type = format!("[Computation Type = '{comp_node_type}']");
                    let comp_type = format_metadata(&comp_type);
                    print!(" {comp_type}")
                }
            }

            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(lhs, metadata, new_prefix.clone(), false);
            print_expression(rhs, metadata, new_prefix, true);
        }

        AstExpressionKind::Cast { target_type, inner, .. } => {
            let target_type_str = target_type.to_string();

            if target_type_str.is_empty() {
                print!("{prefix}{connector}{node_name}");
            } else {
                let target_type_str = format_expr_node(&target_type_str);
                print!("{prefix}{connector}{node_name}: {target_type_str}");
            }

            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(inner, metadata, new_prefix, true);
        }

        AstExpressionKind::Deref { pointer } => {
            print!("{prefix}{connector}{node_name}");
            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(pointer, metadata, new_prefix, true);
        }

        AstExpressionKind::AddressOf { target } => {
            print!("{prefix}{connector}{node_name}");
            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(target, metadata, new_prefix, true);
        }

        AstExpressionKind::Subscript { expr1, expr2 } => {
            print!("{prefix}{connector}{node_name}");
            print_node_metadata(expr.id(), metadata);
            println!();

            let new_prefix = new_tree_prefix(prefix, is_last);
            print_expression(expr1, metadata, new_prefix.clone(), false);
            print_expression(expr2, metadata, new_prefix, true);
        }

        AstExpressionKind::Conditional { condition, consequent, alternative } => {
            print!("{prefix}{connector}{node_name}");
            print_node_metadata(expr.id(), metadata);
            println!();

            // Condition
            {
                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                println!("{}{}Condition", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);
                print_expression(condition, metadata, new_prefix, true);
            }

            // Consequent
            {
                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                println!("{}{}Consequent", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);
                print_expression(consequent, metadata, new_prefix, true);
            }

            // Alternative
            {
                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                println!("{}{}Alternative", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);
                print_expression(alternative, metadata, new_prefix, true);
            }
        }

        AstExpressionKind::FunctionCall { designator, args, .. } => {
            print!("{prefix}{connector}{node_name}");
            print_node_metadata(expr.id(), metadata);
            println!();

            // Designator
            {
                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                println!("{}{}Designator", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);
                print_expression(designator, metadata, new_prefix, true);
            }

            // Args
            {
                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                print!("{}{}Args:", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);

                if args.is_empty() {
                    println!(" void");
                } else {
                    println!();
                    for arg in args {
                        print_expression(arg, metadata, new_prefix.clone(), true);
                    }
                }
            }
        }

        AstExpressionKind::Ident { name, unique_name, .. } => {
            let name = format_ident(name);
            print!("{prefix}{connector}{node_name}: {name} [{unique_name}]");
            print_node_metadata(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::CharLiteral { literal, value } => {
            print!("{prefix}{connector}{node_name}({literal} [{value}])");
            print_node_metadata(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::StringLiteral { literals, ascii } => {
            let ascii_joined = ascii.join("");

            if literals.len() == 1 {
                let literal = &literals[0];
                print!("{prefix}{connector}{node_name}({literal} [\"{ascii_joined}\"])");
                print_node_metadata(expr.id(), metadata);
                println!();
            } else {
                println!("{prefix}{connector}{node_name}");

                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                print!("{}{}Literals", child_prefix, make_child_connector());
                let new_prefix = new_tree_prefix(child_prefix, is_last);

                for lit in literals {
                    println!("{new_prefix}{lit}");
                }

                let child_prefix = new_tree_prefix(prefix.clone(), is_last);
                print!("{}{}Evaluated:  \"{ascii_joined}\"", child_prefix, make_child_connector());

                print_node_metadata(expr.id(), metadata);
                println!();
            }
        }

        AstExpressionKind::IntegerLiteral { literal, literal_base, value, kind } => {
            print!("{prefix}{connector}{node_name}");

            let literal = format_expr_node(literal);

            // If we've performed typechecking then we don't need to print its kind, since we can print its
            // resolved AstType instead.
            if metadata.try_get_node_type(expr.id()).is_some() {
                print!("({literal}");
            } else {
                print!("({kind} = {literal}");
            }

            if *literal_base != 10 {
                print!(" [{value}]");
            }
            print!(")");
            print_node_metadata(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::FloatLiteral { literal, literal_base, kind, value } => {
            print!("{prefix}{connector}{node_name}");

            let literal = format_expr_node(literal);

            if *literal_base == 10 {
                print!("({kind} = {literal})");
            } else {
                print!("({kind} = {literal} [{value}])");
            }
            print_node_metadata(expr.id(), metadata);
            println!();
        }
    }
}

fn get_expression_node_name(kind: &AstExpressionKind) -> String {
    match kind {
        AstExpressionKind::Unary { .. } => format_expr_node("Unary"),
        AstExpressionKind::Binary { .. } => format_expr_node("Binary"),
        AstExpressionKind::Assignment { op, .. } if op.is_compound_assignment() => {
            format_expr_node("CompoundAssignment")
        }
        AstExpressionKind::Assignment { .. } => format_expr_node("Assignment"),
        AstExpressionKind::Conditional { .. } => format_expr_node("Ternary"),
        AstExpressionKind::FunctionCall { .. } => format_expr_node("Call"),
        AstExpressionKind::Deref { .. } => format_expr_node("Deref"),
        AstExpressionKind::AddressOf { .. } => format_expr_node("AddressOf"),
        AstExpressionKind::Subscript { .. } => format_expr_node("Subscript"),
        AstExpressionKind::Cast { is_implicit, .. } if *is_implicit => format_expr_node("ImplicitCast"),
        AstExpressionKind::Cast { .. } => format_expr_node("Cast"),
        AstExpressionKind::Ident { .. } => format_expr_node("Ident"),
        AstExpressionKind::CharLiteral { .. } => format_expr_node("CharLiteral"),
        AstExpressionKind::StringLiteral { .. } => format_expr_node("StringLiteral"),
        AstExpressionKind::IntegerLiteral { .. } => format_expr_node("IntegerLiteral"),
        AstExpressionKind::FloatLiteral { .. } => format_expr_node("FloatLiteral"),
    }
}

fn format_expr_node(name: &str) -> String {
    let cyan = "\x1b[96m";
    let reset = "\x1b[0m";
    format!("{cyan}{name}{reset}")
}

fn format_decl_node(name: &str) -> String {
    let green = "\x1b[92m";
    let reset = "\x1b[0m";
    format!("{green}{name}{reset}")
}

fn format_stmt_node(name: &str) -> String {
    let purple = "\x1b[95m";
    let reset = "\x1b[0m";
    format!("{purple}{name}{reset}")
}

fn format_ident(name: &str) -> String {
    let white = "\x1b[97m";
    let reset = "\x1b[0m";
    format!("{white}{name}{reset}")
}

fn format_metadata(md: &str) -> String {
    let yellow = "\x1b[33m";
    let reset = "\x1b[0m";
    format!("{yellow}{md}{reset}")
}

fn print_resolved_type(declared_type: &AstDeclaredType) {
    if let Some(ty) = &declared_type.resolved_type {
        let yellow = "\x1b[33m";
        let strong_yellow = "\x1b[93m";
        let reset = "\x1b[0m";
        print!(" {yellow}[Type = '{strong_yellow}{ty}{yellow}']{reset}");
    }
}

fn print_node_metadata(node_id: AstNodeId, metadata: &AstMetadata) {
    let yellow = "\x1b[33m";
    let strong_yellow = "\x1b[93m";
    let reset = "\x1b[0m";

    print!("{yellow}");
    print!(" [Id = {node_id}]");

    if let Some(ty) = metadata.try_get_node_type(node_id) {
        print!(" [Type = '{strong_yellow}{ty}{yellow}']");
    }

    if metadata.is_expr_flag_set(node_id, AstExpressionFlag::IsConstant) {
        print!(" [ConstantExpr]");
    }

    if metadata.is_expr_flag_set(node_id, AstExpressionFlag::PromotedToInt) {
        print!(" [PromotedToInt]");
    }
    print!("{reset}");
}

fn new_tree_prefix(prefix: String, is_last: bool) -> String {
    let mut new_prefix = prefix;
    if is_last {
        new_prefix.push_str("   ");
    } else {
        new_prefix.push_str("│  ");
    }
    new_prefix
}

fn make_connector(is_last: bool) -> &'static str {
    if is_last { make_last_child_connector() } else { make_child_connector() }
}

fn make_child_connector() -> &'static str {
    "├─ "
}

fn make_last_child_connector() -> &'static str {
    "└─ "
}
