// Copyright 2025-2026 Neil Henderson
//
//! The `printer` module is used by the parent parser module to print a tree of the C AST to stdout.

use crate::parser::meta::AstMetadata;
use crate::parser::*;

/// Prints the AST with indentation
pub fn print(ast_root: &AstRoot, metadata: &AstMetadata) {
    println!("TranslationUnit");
    for decl in &ast_root.0 {
        print_declaration(decl, metadata, 1);
    }
    println!("{}|", make_close_indent(1));
}

fn print_declaration(decl: &AstDeclaration, metadata: &AstMetadata, level: usize) {
    match decl {
        AstDeclaration::Variable(var_decl) => print_variable_declaration(var_decl, metadata, level),
        AstDeclaration::Function(func_decl) => print_function(func_decl, metadata, level),
        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(alias_decl, metadata, level),
    }
}

fn print_variable_declaration(var_decl: &AstVariableDeclaration, metadata: &AstMetadata, level: usize) {
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

fn print_variable_initializer(init: &AstVariableInitializer, metadata: &AstMetadata, level: usize) {
    match init {
        AstVariableInitializer::Scalar(expr) => {
            let indent = make_indent(level);
            print!("{indent}Scalar");
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(expr, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstVariableInitializer::Aggregate { node_id, init } => {
            let indent = make_indent(level);
            print!("{indent}Aggregate");
            print_node_type(*node_id, metadata);
            println!();

            for i in init {
                print_variable_initializer(i, metadata, level + 1);
            }

            println!("{}|", make_close_indent(level + 1));
        }
    }
}

fn print_function(function: &AstFunction, metadata: &AstMetadata, level: usize) {
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

fn print_type_alias_declaration(alias_decl: &AstTypeAliasDeclaration, metadata: &AstMetadata, level: usize) {
    let indent = make_indent(level);
    println!("{indent}TypeAliasDecl");

    {
        let indent = make_indent(level + 1);
        println!("{indent}decl=");

        print_declaration(&alias_decl.decl, metadata, level + 2);

        println!("{}|", make_close_indent(level + 1));
    }
}

fn print_block(block: &AstBlock, metadata: &AstMetadata, level: usize) {
    let block_items = &block.0;
    for item in block_items {
        match item {
            AstBlockItem::Declaration(decl) => print_declaration(decl, metadata, level),
            AstBlockItem::Statement(stmt) => print_statement(stmt, metadata, level),
        }
    }
}

fn print_statement(stmt: &AstStatement, metadata: &AstMetadata, level: usize) {
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
            print_switch_statement(*node_id, controlling_expr, body, metadata, level)
        }

        // Control statements: Loops
        AstStatement::While { node_id, controlling_expr, body } => {
            print_while_statement(*node_id, controlling_expr, body, metadata, level)
        }
        AstStatement::DoWhile { node_id, body, controlling_expr } => {
            print_do_while_statement(*node_id, body, controlling_expr, metadata, level)
        }
        AstStatement::For { node_id, init, controlling_expr, post_expr, body } => {
            print_for_statement(*node_id, init, controlling_expr, post_expr, body, metadata, level)
        }

        // Control statements: Jumps
        AstStatement::Break { enclosing_stmt_node_id } => print_break_statement(*enclosing_stmt_node_id, level),
        AstStatement::Continue { loop_node_id } => print_continue_statement(*loop_node_id, level),
        AstStatement::Goto { node_id: _, label_name } => print_goto_statement(label_name, level),
        AstStatement::Case { switch_node_id, constant_expr, stmt } => {
            print_switch_case_statement(*switch_node_id, constant_expr, stmt, metadata, level)
        }
        AstStatement::Default { switch_node_id, stmt } => {
            print_switch_default_statement(*switch_node_id, stmt, metadata, level)
        }
        AstStatement::Return(expr) => print_return_statement(expr, metadata, level),
    }
}

fn print_expression_statement(expr: &AstExpression, metadata: &AstMetadata, level: usize) {
    let indent = make_indent(level);
    println!("{indent}ExprStatement");
    print_expression(expr, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_labeled_statement(label_name: &String, stmt: &AstStatement, metadata: &AstMetadata, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Labeled(name=\"{}\")", label_name);

    print_statement(stmt, metadata, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_compound_statement(block: &AstBlock, metadata: &AstMetadata, level: usize) {
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
    controlling_expr: &AstExpression,
    then_stmt: &AstStatement,
    else_stmt: &Option<Box<AstStatement>>,
    metadata: &AstMetadata,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}If()");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_expression(controlling_expr, metadata, level + 2);
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
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    metadata: &AstMetadata,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}Switch(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_expression(controlling_expr, metadata, level + 2);
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
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    metadata: &AstMetadata,
    level: usize,
) {
    let indent = make_indent(level);

    println!("{indent}While(id={node_id})");

    {
        let indent = make_indent(level + 1);
        println!("{indent}condition:");
        print_expression(controlling_expr, metadata, level + 2);
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
    node_id: AstNodeId,
    body: &AstStatement,
    controlling_expr: &AstExpression,
    metadata: &AstMetadata,
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
        print_expression(controlling_expr, metadata, level + 2);
        println!("{}|", make_close_indent(level + 2));
    }

    println!("{}|", make_close_indent(level + 1));
}

fn print_for_statement(
    node_id: AstNodeId,
    init: &AstForInitializer,
    controlling_expr: &Option<AstExpression>,
    post_expr: &Option<AstExpression>,
    body: &AstStatement,
    metadata: &AstMetadata,
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
                    print_expression(expr, metadata, level + 2);
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
                print_expression(controlling_expr, metadata, level + 2);
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
                print_expression(post_expr, metadata, level + 2);
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

fn print_break_statement(enclosing_stmt_node_id: AstNodeId, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Break(enclosing_stmt_id={})", enclosing_stmt_node_id);
    println!("{}|", make_close_indent(level + 1));
}

fn print_continue_statement(loop_node_id: AstNodeId, level: usize) {
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
    switch_node_id: AstNodeId,
    constant_expr: &AstExpression,
    stmt: &AstStatement,
    metadata: &AstMetadata,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}Case(switch_id={})", switch_node_id);

    print!("{}", indent);
    println!("  condition=");
    print_expression(constant_expr, metadata, level + 1);

    print!("{}", indent);
    println!("  body=");
    print_statement(stmt, metadata, level + 1);

    println!("{}|", make_close_indent(level + 1));
}

fn print_switch_default_statement(
    switch_node_id: AstNodeId,
    stmt: &AstStatement,
    metadata: &AstMetadata,
    level: usize,
) {
    let indent = make_indent(level);
    println!("{indent}Default(switch_id={})", switch_node_id);
    print_statement(stmt, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_return_statement(expr: &AstExpression, metadata: &AstMetadata, level: usize) {
    let indent = make_indent(level);
    println!("{indent}Return");
    print_expression(expr, metadata, level + 1);
    println!("{}|", make_close_indent(level + 1));
}

fn print_expression(expr: &AstExpression, metadata: &AstMetadata, level: usize) {
    let indent = make_indent(level);
    match expr.kind() {
        AstExpressionKind::Binary { op, lhs, rhs } => {
            print!("{indent}BinaryOp({})", op);
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(lhs, metadata, level + 1);
            print_expression(rhs, metadata, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::Unary { op, operand } => {
            print!("{indent}UnaryOp({})", op);
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(operand, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::Cast { target_type, inner, is_implicit } => {
            let target_type_str = target_type.to_string();

            if target_type_str.is_empty() {
                print!("{indent}Cast");
            } else {
                print!("{indent}Cast(to=\"{}\")", target_type_str);
            }

            if *is_implicit {
                print!(" [Implicit]");
            }

            print_node_type(expr.id(), metadata);
            println!();

            print_expression(inner, metadata, level + 1);
        }

        AstExpressionKind::Deref { pointer } => {
            print!("{indent}Deref");
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(pointer, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::AddressOf { target } => {
            print!("{indent}AddressOf");
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(target, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::Subscript { expr1, expr2 } => {
            print!("{indent}Subscript");
            print_node_type(expr.id(), metadata);
            println!();

            print_expression(expr1, metadata, level + 1);
            print_expression(expr2, metadata, level + 1);
            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::Assignment { computation_node_id, op, lhs, rhs } => {
            if *op == AstAssignmentOp::Assignment {
                print!("{indent}Assignment");
            } else {
                let token_type = lexer::TokenType::from(*op);

                print!("{indent}CompoundAssignment ('{token_type}'");

                if let Some(comp_node_type) = metadata.try_get_node_type(*computation_node_id) {
                    print!(" [Computation Type = '{comp_node_type}']")
                }

                print!(")");
            }

            print_node_type(expr.id(), metadata);
            println!();

            print_expression(lhs, metadata, level + 1);
            print_expression(rhs, metadata, level + 1);

            println!("{}|", make_close_indent(level + 1));
        }

        AstExpressionKind::Conditional { condition, consequent, alternative } => {
            print!("{indent}Ternary");
            print_node_type(expr.id(), metadata);
            println!();

            // Condition
            {
                let indent = make_indent(level + 1);
                println!("{indent}condition:");
                print_expression(condition, metadata, level + 2);
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

        AstExpressionKind::FunctionCall { designator, args, .. } => {
            print!("{indent}Call");
            print_node_type(expr.id(), metadata);
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

        AstExpressionKind::Ident { name, unique_name, .. } => {
            print!("{indent}Ident(name=\"{}\", unique=\"{}\")", name, unique_name);
            print_node_type(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::CharLiteral { literal, value } => {
            print!("{indent}CharLiteral({literal} [{value}])");
            print_node_type(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::StringLiteral { literals, ascii } => {
            let ascii_joined = ascii.join("");

            if literals.len() == 1 {
                let literal = &literals[0];
                print!("{indent}StringLiteral({literal} [\"{ascii_joined}\"])");
                print_node_type(expr.id(), metadata);
                println!();
            } else {
                println!("{indent}StringLiteral");

                let indent1 = make_indent(level + 1);
                println!("{indent1}literals:");

                let indent2 = make_indent(level + 2);
                for lit in literals {
                    println!("{indent2}{lit}");
                }

                print!("{indent1}evaluated: \"{ascii_joined}\"");
                print_node_type(expr.id(), metadata);
                println!();
            }
        }

        AstExpressionKind::IntegerLiteral { literal, literal_base, value, kind } => {
            print!("{indent}");

            // If we've performed typechecking then we don't need to print its kind, since we can print its
            // resolved AstType instead.
            if metadata.try_get_node_type(expr.id()).is_some() {
                print!("IntegerLiteral({literal}");
            } else {
                print!("IntegerLiteral({kind} = {literal}");
            }

            if *literal_base != 10 {
                print!(" [{value}]");
            }
            print!(")");
            print_node_type(expr.id(), metadata);
            println!();
        }

        AstExpressionKind::FloatLiteral { literal, literal_base, kind, value } => {
            print!("{indent}");
            if *literal_base == 10 {
                print!("FloatLiteral({kind} = {literal})");
            } else {
                print!("FloatLiteral({kind} = {literal} [{value}])");
            }
            print_node_type(expr.id(), metadata);
            println!();
        }
    }
}

fn print_resolved_type(declared_type: &AstDeclaredType) {
    if let Some(ty) = &declared_type.resolved_type {
        print!(" [Type = '{ty}']");
    }
}

fn print_node_type(node_id: AstNodeId, metadata: &AstMetadata) {
    print!(" [Id = {node_id}]");

    if let Some(node_type) = metadata.try_get_node_type(node_id) {
        print!(" [Type = '{node_type}']");
    }

    if metadata.is_expr_flag_set(node_id, AstExpressionFlag::IsConstant) {
        print!(" [ConstantExpr]");
    }

    if metadata.is_expr_flag_set(node_id, AstExpressionFlag::PromotedToInt) {
        print!(" [PromotedToInt]");
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
