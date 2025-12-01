// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `printer` module is used by the parent parser module to print a tree of the C AST to stdout.

use crate::parser::*;

/// Prints the AST with indentation
pub fn print(ast_root: &AstRoot) {
    println!("TranslationUnit");
    for decl in &ast_root.0 {
        print_declaration(decl, 1);
    }
    println!("{}|", make_close_indent(1));
}

fn print_declaration(decl: &AstDeclaration, level: usize) {
    match decl {
        AstDeclaration::Variable(var_decl) => print_variable_declaration(var_decl, level),
        AstDeclaration::Function(func_decl) => print_function(func_decl, level),
        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(alias_decl, level),
    }
}

fn print_variable_declaration(var_decl: &AstVariableDeclaration, level: usize) {
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

    println!(
        "name=\"{}\", unique=\"{}\", declared=\"{}\")",
        var_decl.ident.name, var_decl.unique_name, var_decl.declared_type
    );

    {
        let indent = make_indent(level + 1);

        if let Some(ty) = &var_decl.declared_type.resolved_type {
            println!("{indent}type={ty}");
        }
        
        println!("{indent}linkage={linkage}");

        if !var_decl.is_declaration_only {
            println!("{indent}storage={storage}");
        }

        if let Some(expr) = &var_decl.init_expr {
            println!("{indent}init:");
            print_full_expression(expr, level + 2);
            println!("{}|", make_close_indent(level + 2));
        }

        if let Some(constant_value) = &var_decl.init_constant_value {
            println!("{indent}init-evaluated-constant: {constant_value}");
        }

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_function(function: &AstFunction, level: usize) {
    let indent = make_indent(level);

    let is_decl = function.body.is_none();

    let linkage = match function.linkage {
        AstLinkage::None => internal_error::ICE("Function must have linkage"),
        AstLinkage::Internal => "internal",
        AstLinkage::External => "external",
    };

    print!("{}", indent);
    if is_decl {
        print!("FunctionDecl");
    } else {
        print!("Function");
    }
    println!("(name=\"{}\", signature={}, linkage={linkage})", function.unique_name, function.declared_type);

    {
        let indent = make_indent(level + 1);

        if let Some(ty) = &function.declared_type.resolved_type {
            println!("{indent}type={ty}");
        }

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
            print_block(func_body, level + 2);
            println!("{}|", make_close_indent(level + 2));
        }

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_type_alias_declaration(alias_decl: &AstTypeAliasDeclaration, level: usize) {
    let indent = make_indent(level);
    println!("{indent}TypeAliasDecl");
    
    {
        let indent = make_indent(level + 1);
        println!("{indent}decl=");

        print_declaration(&alias_decl.decl, level + 2);

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_block(block: &AstBlock, level: usize) {
    let block_items = &block.0;
    for item in block_items {
        match item {
            AstBlockItem::Declaration(decl) => print_declaration(decl, level),
            AstBlockItem::Statement(stmt) => print_statement(stmt, level),
        }
    }
}

fn print_statement(stmt: &AstStatement, level: usize) {
    match stmt {
        AstStatement::Expression(expr) => print_expression_statement(expr, level),
        AstStatement::Labeled { node_id: _, label_name, stmt } => print_labeled_statement(label_name, stmt, level),
        AstStatement::Compound(block) => print_compound_statement(block, level),
        AstStatement::Null => print_null_statement(level),

        // Control statements: Conditional
        AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            print_if_statement(controlling_expr, then_stmt, else_stmt, level)
        }
        AstStatement::Switch { node_id, controlling_expr, body } => {
            print_switch_statement(node_id, controlling_expr, body, level)
        }

        // Control statements: Loops
        AstStatement::While { node_id, controlling_expr, body } => {
            print_while_statement(node_id, controlling_expr, body, level)
        }
        AstStatement::DoWhile { node_id, body, controlling_expr } => {
            print_do_while_statement(node_id, body, controlling_expr, level)
        }
        AstStatement::For { node_id, init, controlling_expr, post, body } => {
            print_for_statement(node_id, init, controlling_expr, post, body, level)
        }

        // Control statements: Jumps
        AstStatement::Break { enclosing_stmt_node_id } => print_break_statement(enclosing_stmt_node_id, level),
        AstStatement::Continue { loop_node_id } => print_continue_statement(loop_node_id, level),
        AstStatement::Goto { node_id: _, label_name } => print_goto_statement(label_name, level),
        AstStatement::Case { switch_node_id, constant_expr, stmt } => {
            print_switch_case_statement(switch_node_id, constant_expr, stmt, level)
        }
        AstStatement::Default { switch_node_id, stmt } => print_switch_default_statement(switch_node_id, stmt, level),
        AstStatement::Return(expr) => print_return_statement(expr, level),
    }
}

fn print_expression_statement(expr: &AstFullExpression, level: usize) {
    let indent = make_indent(level);
    println!("{indent}ExprStatement");
    print_full_expression(expr, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_labeled_statement(label_name: &String, stmt: &AstStatement, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Labeled(name=\"{}\")", label_name);

    print_statement(stmt, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_compound_statement(block: &AstBlock, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Compound");
    print_block(block, level + 1);
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
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}If()");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}then:");
        print_statement(then_stmt, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    if else_stmt.is_some() {
        let indent = make_indent(level + 1);
        println!("{indent}else:");
        let else_stmt = else_stmt.as_ref().unwrap();
        print_statement(else_stmt, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_statement(
    node_id: &AstNodeId,
    controlling_expr: &AstFullExpression,
    body: &AstStatement,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}Switch(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_while_statement(node_id: &AstNodeId, controlling_expr: &AstFullExpression, body: &AstStatement, level: usize) {
    let indent = make_indent(level);

    println!("{indent}While(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_full_expression(controlling_expr, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_do_while_statement(
    node_id: &AstNodeId,
    body: &AstStatement,
    controlling_expr: &AstFullExpression,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}Do(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}body:");
        print_statement(body, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    {
        let indent = make_indent(level + 1);
        println!("{indent}while:");
        print_full_expression(controlling_expr, level + 2);
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
                    match decl {
                        AstDeclaration::Variable(var_decl) => print_variable_declaration(var_decl, level + 2),
                        AstDeclaration::Function(fn_decl) => print_function(fn_decl, level + 2),
                        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(alias_decl, level + 2),
                    }
                }
            }
            AstForInitializer::Expression(expr) => match expr {
                Some(expr) => {
                    print_full_expression(expr, level + 2);
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
                print_full_expression(controlling_expr, level + 2);
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
                print_full_expression(post_expr, level + 2);
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
        print_statement(body, level + 2);
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
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}Case(switch_id={})", switch_node_id);

    print!("{}", indent);
    println!("  condition=");
    print_full_expression(constant_expr, level + 1);

    print!("{}", indent);
    println!("  body=");
    print_statement(stmt, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_default_statement(switch_node_id: &AstNodeId, stmt: &AstStatement, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Default(switch_id={})", switch_node_id);
    print_statement(stmt, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_return_statement(expr: &AstFullExpression, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Return");
    print_full_expression(expr, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_full_expression(full_expr: &AstFullExpression, level: usize) {
    print_expression(&full_expr.expr, level);
}

fn print_expression(expr: &AstExpression, level: usize) {
    let indent = make_indent(level);
    match expr {
        AstExpression::BinaryOperation { op, left, right, .. } => {
            println!("{indent}BinaryOp({})", op);

            print_expression(left, level + 1);
            print_expression(right, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::UnaryOperation { op, expr, .. } => {
            println!("{indent}UnaryOp({})", op);
            print_expression(expr, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Cast { target_type, expr, .. } => {
            if let Some(ty) = &target_type.resolved_type {
                println!("{indent}Cast(as=\"{}\")", ty);
            } else {
                println!("{indent}Cast(as=\"{}\")", target_type);
            }
            print_expression(expr, level + 1);
        }

        AstExpression::Deref { expr, .. } => {
            println!("{indent}Deref");
            print_expression(expr, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::AddressOf { expr, .. } => {
            println!("{indent}AddressOf");
            print_expression(expr, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Assignment { lhs, rhs, .. } => {
            println!("{indent}Assignment");

            print_expression(lhs, level + 1);
            print_expression(rhs, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Conditional { expr, consequent, alternative, .. } => {
            println!("{indent}Ternary");

            // Condition
            {
                let indent = make_indent(level + 1);
                println!("{indent}condition:");
                print_expression(expr, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            // Consequent
            {
                let indent = make_indent(level + 1);
                println!("{indent}consequent:");
                print_expression(consequent, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            // Consequent
            {
                let indent = make_indent(level + 1);
                println!("{indent}alternative:");
                print_expression(alternative, level + 2);
                println!("{}|", make_close_indent(level + 2));
            }

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::FunctionCall { fn_name, args, .. } => {
            if args.is_empty() {
                println!("{indent}Call(function=\"{fn_name}\", args: void)");
                return;
            }

            println!("{indent}Call(function=\"{fn_name}\")");

            // Args
            {
                let indent = make_indent(level + 1);
                println!("{indent}args:");

                for arg in args {
                    print_expression(arg, level + 2);
                }

                println!("{}|", make_close_indent(level + 2));
            }

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpression::Variable { name, unique_name, .. } => {
            println!("{indent}Variable(name=\"{}\", unique=\"{}\")", name, unique_name);
        }

        AstExpression::IntegerLiteral { literal, literal_base, value, kind, .. } => {
            print!("{indent}");
            if *literal_base == 10 {
                println!("IntegerLiteral({kind} = {literal})");
            } else {
                println!("IntegerLiteral({kind} = {literal} [{value}])");
            }
        }

        AstExpression::FloatLiteral { literal, literal_base, kind, value, .. } => {
            print!("{indent}");
            if *literal_base == 10 {
                println!("FloatLiteral({kind} = {literal})");
            } else {
                println!("FloatLiteral({kind} = {literal} [{value}])");
            }
        }
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
