// Copyright 2025-2026 Neil Henderson
//
//! The `printer` module is used by the parent parser module to print a tree of the C AST to stdout.

use crate::parser::meta::AstMetadata;
use crate::parser::*;

use std::io::{self, BufWriter, Write};

/// The mutable state for the AST printer.
struct AstPrinter<'m, W: Write> {
    writer: BufWriter<W>,
    metadata: &'m AstMetadata,
    prefix: String,
    print_node_ids: bool,
    inside_type_alias_decl: bool,
    decl_color: &'static str,
    stmt_color: &'static str,
    expr_color: &'static str,
    ident_color: &'static str,
    meta_color: &'static str,
    type_color: &'static str,
    reset_color: &'static str,
}

impl<'m, W: Write> AstPrinter<'m, W> {
    /// Creates a new AST printer
    pub fn new(writer: W, metadata: &'m AstMetadata, color_enabled: bool, print_node_ids: bool) -> Self {
        const YELLOW: &str = "\x1b[33m";
        const BOLD_GREEN: &str = "\x1b[92m";
        const BOLD_YELLOW: &str = "\x1b[93m";
        const BOLD_PURPLE: &str = "\x1b[95m";
        const BOLD_CYAN: &str = "\x1b[96m";
        const BOLD_WHITE: &str = "\x1b[97m";
        const RESET: &str = "\x1b[0m";

        let decl_color = if color_enabled { BOLD_GREEN } else { "" };
        let stmt_color = if color_enabled { BOLD_PURPLE } else { "" };
        let expr_color = if color_enabled { BOLD_CYAN } else { "" };
        let ident_color = if color_enabled { BOLD_WHITE } else { "" };
        let meta_color = if color_enabled { YELLOW } else { "" };
        let type_color = if color_enabled { BOLD_YELLOW } else { "" };
        let reset_color = if color_enabled { RESET } else { "" };

        Self {
            writer: BufWriter::new(writer),
            metadata,
            prefix: String::new(),
            print_node_ids,
            inside_type_alias_decl: false,
            decl_color,
            stmt_color,
            expr_color,
            ident_color,
            meta_color,
            type_color,
            reset_color,
        }
    }

    /// Prints the AST.
    pub fn print_ast(&mut self, ast_root: &AstRoot) {
        _ = self.write_stmt_str("TranslationUnit");
        _ = self.new_line();

        let len = ast_root.0.len();
        for (idx, decl) in ast_root.0.iter().enumerate() {
            _ = print_declaration(self, decl, idx == len - 1);
        }

        _ = self.writer.flush();
    }

    /// Indents the tree and calls the given closure `f`, passing the current `AstPrinter` to it.
    pub fn with_indent<F>(&mut self, is_last: bool, f: F) -> io::Result<()>
    where
        F: FnOnce(&mut Self) -> io::Result<()>,
    {
        let old_len = self.prefix.len();
        self.prefix.push_str(if is_last { "  " } else { "│ " });

        let result = f(self);

        self.prefix.truncate(old_len);
        result
    }

    /// Calls the given closure `f`, passing the current `AstPrinter` to it, after raising a flag to indicate that
    /// the printer has just processed a type alias declaration.
    pub fn inside_type_alias_decl<F>(&mut self, f: F) -> io::Result<()>
    where
        F: FnOnce(&mut Self) -> io::Result<()>,
    {
        let was_inside_alias_decl = std::mem::replace(&mut self.inside_type_alias_decl, true);

        let result = f(self);

        self.inside_type_alias_decl = was_inside_alias_decl;
        result
    }

    /// Writes the given string to the buffered output, without a newline.
    pub fn write(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{str}")
    }

    /// Writes the given string to the buffered output with a newline.
    pub fn writeln(&mut self, str: &str) -> io::Result<()> {
        writeln!(self.writer, "{str}")
    }

    /// Writes a newline character to the buffered output to begin a new line.
    pub fn new_line(&mut self) -> io::Result<()> {
        writeln!(self.writer)
    }

    /// Writes the current prefix and the given connector to the buffered output.
    pub fn write_prefix_and_connector(&mut self, connector: &str) -> io::Result<()> {
        write!(self.writer, "{}{connector}", self.prefix)
    }

    /// Writes the given statement string to the buffered output, formatted with the appropriate color if enabled.
    pub fn write_stmt_str(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{}{str}{}", self.stmt_color, self.reset_color)
    }

    /// Writes the given declaration string to the buffered output, formatted with the appropriate color if enabled.
    pub fn write_decl_str(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{}{str}{}", self.decl_color, self.reset_color)
    }

    /// Writes the given expression string to the buffered output, formatted with the appropriate color if enabled.
    pub fn write_expr_str(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{}{str}{}", self.expr_color, self.reset_color)
    }

    /// Writes the given identifier string to the buffered output, formatted with the appropriate color if enabled.
    pub fn write_ident_str(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{}{str}{}", self.ident_color, self.reset_color)
    }

    /// Writes the given metadata string to the buffered output, formatted with the appropriate color if enabled.
    pub fn write_meta_str(&mut self, str: &str) -> io::Result<()> {
        write!(self.writer, "{}{str}{}", self.meta_color, self.reset_color)
    }
}

/// Prints the AST.
///
/// By default, the printer prints the AST to `stdout`.
///
/// There are some undocumented compiler flags which allows the printer to print to a text file instead. We use these
/// flags in the integration tests when testing the AST printer.
///
/// Pass `-fredirect_stdout -fredirect_file=output_file.txt` to print to `output_file.txt`.
pub fn print(ast_root: &AstRoot, metadata: &AstMetadata, driver: &mut Driver) {
    if driver.options().flags.contains("redirect_stdout") {
        print_to_file(ast_root, metadata, driver);
    } else {
        let print_node_ids = !driver.options().flags.contains("ast_printer_no_ids");
        let color_enabled = !driver.options().no_color;
        let stdout = io::stdout();
        let mut printer = AstPrinter::new(stdout.lock(), metadata, color_enabled, print_node_ids);
        printer.print_ast(ast_root);
    }
}

fn print_to_file(ast_root: &AstRoot, metadata: &AstMetadata, driver: &mut Driver) {
    const REDIRECT_FILE_FLAG_PREFIX: &str = "redirect_file=";
    let redirect_to_file = driver.options().flags.iter().find(|flag| flag.starts_with(REDIRECT_FILE_FLAG_PREFIX));

    if let Some(redirect_to_file) = redirect_to_file {
        // Safe to 'unwrap' because we checked above that the string has this prefix.
        let redirect_to_file = redirect_to_file.strip_prefix(REDIRECT_FILE_FLAG_PREFIX).unwrap();

        if let Ok(file) = std::fs::File::create(redirect_to_file) {
            let print_node_ids = !driver.options().flags.contains("ast_printer_no_ids");
            let color_enabled = !driver.options().no_color;
            let mut printer = AstPrinter::new(file, metadata, color_enabled, print_node_ids);
            printer.print_ast(ast_root);
        } else {
            eprintln!("Error: Cannot open '{redirect_to_file}'");
        }
    } else {
        eprintln!("Error: 'redirect_file' flag not specified");
    }
}

fn print_declaration<W: Write>(p: &mut AstPrinter<W>, decl: &AstDeclaration, is_last: bool) -> io::Result<()> {
    match decl {
        AstDeclaration::Variable(var_decl) => print_variable_declaration(p, var_decl, is_last),
        AstDeclaration::Function(func_decl) => print_function(p, func_decl, is_last),
        AstDeclaration::TypeAlias(alias_decl) => print_type_alias_declaration(p, alias_decl, is_last),
    }
}

fn print_variable_declaration<W: Write>(
    p: &mut AstPrinter<W>,
    var_decl: &AstVariableDeclaration,
    is_last: bool,
) -> io::Result<()> {
    let connector = make_connector(is_last);

    let node_name = if var_decl.is_declaration_only { "VariableDecl" } else { "VariableDefn" };
    write!(p.writer, "{}{connector}", p.prefix)?;
    p.write_decl_str(node_name)?;

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

    p.write(": ")?;
    p.write_ident_str(&var_decl.ident.name)?;
    write!(p.writer, " [{}], declared=\"{}\"", var_decl.unique_name, var_decl.declared_type)?;
    print_resolved_type(p, &var_decl.declared_type)?;
    p.new_line()?;

    if p.inside_type_alias_decl {
        return Ok(());
    }

    p.with_indent(is_last, |p| {
        writeln!(p.writer, "{}{}Linkage: {linkage}", p.prefix, make_child_connector())?;

        if !var_decl.is_declaration_only {
            writeln!(p.writer, "{}{}Storage: {storage}", p.prefix, make_child_connector())?;
        }

        // Initializer expression
        write!(p.writer, "{}{}Initializer", p.prefix, make_last_child_connector())?;

        if let Some(init) = &var_decl.initializer {
            p.new_line()?;

            let has_constant_eval = !var_decl.init_constant_eval.is_empty();

            p.with_indent(true, |p| {
                print_variable_initializer(p, init, !has_constant_eval)?;

                // Evaluated constant initializer
                if has_constant_eval {
                    let connector = make_last_child_connector();
                    write!(p.writer, "{}{connector}Constant Eval: ", p.prefix)?;

                    let is_aggregate = var_decl.init_constant_eval.len() > 1;
                    if is_aggregate {
                        p.write("{{ ")?;
                    }

                    const VALUES_PER_LINE: usize = 32;
                    if var_decl.init_constant_eval.len() > VALUES_PER_LINE {
                        p.write("\n    ")?;
                    }

                    let mut first = true;
                    for (idx, val) in var_decl.init_constant_eval.iter().enumerate() {
                        if !first {
                            p.write(", ")?;
                        }

                        if idx > 0 && idx.is_multiple_of(VALUES_PER_LINE) {
                            p.write("\n    ")?;
                        }

                        p.write_expr_str(&val.to_string())?;
                        first = false;
                    }

                    if is_aggregate {
                        p.write(" }}")?;
                    }

                    p.new_line()?;
                }

                Ok(())
            })
        } else {
            writeln!(p.writer, ": none")
        }
    })
}

fn print_variable_initializer<W: Write>(
    p: &mut AstPrinter<W>,
    init: &AstVariableInitializer,
    is_last: bool,
) -> io::Result<()> {
    match init {
        AstVariableInitializer::Scalar(expr) => print_expression(p, expr, is_last),

        AstVariableInitializer::Aggregate { node_id, init } => {
            write!(p.writer, "{}{}Aggregate", p.prefix, make_connector(is_last))?;
            print_node_metadata(p, *node_id)?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                let len = init.len();
                for (idx, ini) in init.iter().enumerate() {
                    print_variable_initializer(p, ini, idx == len - 1)?;
                }

                Ok(())
            })
        }
    }
}

fn print_function<W: Write>(p: &mut AstPrinter<W>, function: &AstFunction, is_last: bool) -> io::Result<()> {
    let connector = make_connector(is_last);

    let is_decl = function.body.is_none();

    let node_name = if is_decl { "FunctionDecl" } else { "FunctionDefn" };
    write!(p.writer, "{}{connector}{}{node_name}{}", p.prefix, p.decl_color, p.reset_color)?;

    let linkage = match function.linkage {
        AstLinkage::None => "none",
        AstLinkage::Internal => "internal",
        AstLinkage::External => "external",
    };

    p.write(": ")?;
    p.write_ident_str(&function.unique_name.to_string())?;
    write!(p.writer, ", signature: \"{}\"", function.declared_type)?;
    print_resolved_type(p, &function.declared_type)?;
    p.new_line()?;

    if let Some(func_body) = &function.body {
        p.with_indent(is_last, |p| {
            writeln!(p.writer, "{}{}Linkage: {linkage}", p.prefix, make_child_connector())?;

            write!(p.writer, "{}{}", p.prefix, make_child_connector())?;
            p.write_decl_str("Params")?;

            if function.param_names.is_empty() {
                p.writeln(": none")?;
            } else {
                p.new_line()?;

                let param_types = if let Some(fn_type) = function.declared_type.resolved_type.as_ref() {
                    let AstType::Function { params, .. } = fn_type else {
                        ICE!("Expected AstType::Function");
                    };
                    params
                } else {
                    &Vec::new()
                };

                p.with_indent(false, |p| {
                    let len = function.param_names.len();
                    for (idx, (param_ident, unique)) in function.param_names.iter().enumerate() {
                        let param_name = &param_ident.name;

                        write!(p.writer, "{}{}", p.prefix, make_connector(idx == len - 1),)?;
                        p.write_decl_str("ParamDecl")?;
                        p.write(": ")?;
                        p.write_ident_str(param_name)?;
                        write!(p.writer, " [{unique}]")?;

                        if !param_types.is_empty() {
                            let param_type = &param_types[idx];
                            print_type(p, param_type)?;
                        }
                        p.new_line()?;
                    }

                    Ok(())
                })?
            }

            write!(p.writer, "{}{}", p.prefix, make_last_child_connector())?;
            p.write_stmt_str("Body")?;
            p.new_line()?;

            p.with_indent(true, |p| print_block(p, func_body))
        })
    } else {
        Ok(())
    }
}

fn print_block<W: Write>(p: &mut AstPrinter<W>, block: &AstBlock) -> io::Result<()> {
    let block_items = &block.0;
    let len = block_items.len();
    for (idx, item) in block_items.iter().enumerate() {
        match item {
            AstBlockItem::Declaration(decl) => print_declaration(p, decl, idx == len - 1)?,
            AstBlockItem::Statement(stmt) => print_statement(p, stmt, idx == len - 1)?,
        }
    }

    Ok(())
}

fn print_type_alias_declaration<W: Write>(
    p: &mut AstPrinter<W>,
    alias_decl: &AstTypeAliasDeclaration,
    is_last: bool,
) -> io::Result<()> {
    let connector = make_connector(is_last);
    write!(p.writer, "{}{}", p.prefix, connector)?;
    p.write_decl_str("TypeAliasDecl")?;
    p.new_line()?;

    p.with_indent(is_last, |p| p.inside_type_alias_decl(|p| print_declaration(p, &alias_decl.decl, true)))
}

fn print_statement<W: Write>(p: &mut AstPrinter<W>, stmt: &AstStatement, is_last: bool) -> io::Result<()> {
    match stmt {
        AstStatement::Expression(expr) => print_expression_statement(p, expr, is_last),
        AstStatement::Labeled { node_id: _, label_name, stmt } => print_labeled_statement(p, label_name, stmt, is_last),
        AstStatement::Compound(block) => print_compound_statement(p, block, is_last),
        AstStatement::Null => print_null_statement(p, is_last),

        // Control statements: Conditional
        AstStatement::If { controlling_expr, then_stmt, else_stmt } => {
            print_if_statement(p, controlling_expr, then_stmt, else_stmt, is_last)
        }
        AstStatement::Switch { node_id, controlling_expr, body } => {
            print_switch_statement(p, *node_id, controlling_expr, body, is_last)
        }

        // Control statements: Loops
        AstStatement::While { node_id, controlling_expr, body } => {
            print_while_statement(p, *node_id, controlling_expr, body, is_last)
        }
        AstStatement::DoWhile { node_id, body, controlling_expr } => {
            print_do_while_statement(p, *node_id, body, controlling_expr, is_last)
        }
        AstStatement::For { .. } => print_for_statement(p, stmt, is_last),

        // Control statements: Jumps
        AstStatement::Break { enclosing_stmt_node_id } => print_break_statement(p, *enclosing_stmt_node_id, is_last),
        AstStatement::Continue { loop_node_id } => print_continue_statement(p, *loop_node_id, is_last),
        AstStatement::Goto { node_id: _, label_name } => print_goto_statement(p, label_name, is_last),
        AstStatement::Case { switch_node_id, constant_expr, stmt } => {
            print_switch_case_statement(p, *switch_node_id, constant_expr, stmt, is_last)
        }
        AstStatement::Default { switch_node_id, stmt } => {
            print_switch_default_statement(p, *switch_node_id, stmt, is_last)
        }
        AstStatement::Return(expr) => print_return_statement(p, expr, is_last),
    }
}

fn print_expression_statement<W: Write>(p: &mut AstPrinter<W>, expr: &AstExpression, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("ExprStmt")?;
    p.new_line()?;

    p.with_indent(is_last, |p| print_expression(p, expr, true))
}

fn print_labeled_statement<W: Write>(
    p: &mut AstPrinter<W>,
    label_name: &str,
    stmt: &AstStatement,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("LabeledStmt")?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        p.write_prefix_and_connector(make_child_connector())?;
        p.write("Label: ")?;
        p.write_ident_str(label_name)?;
        p.new_line()?;

        print_statement(p, stmt, true)
    })
}

fn print_compound_statement<W: Write>(p: &mut AstPrinter<W>, block: &AstBlock, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("CompoundStmt")?;
    p.new_line()?;

    p.with_indent(is_last, |p| print_block(p, block))
}

fn print_null_statement<W: Write>(p: &mut AstPrinter<W>, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("NullStmt")?;
    p.new_line()
}

fn print_if_statement<W: Write>(
    p: &mut AstPrinter<W>,
    controlling_expr: &AstExpression,
    then_stmt: &AstStatement,
    else_stmt: &Option<Box<AstStatement>>,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("IfStmt")?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        // Condition (never the last child)
        writeln!(p.writer, "{}{}Condition", p.prefix, make_child_connector())?;
        p.with_indent(false, |p| print_expression(p, controlling_expr, true))?;

        // Then (might be the last child, if there's no else)
        let has_else = else_stmt.is_some();
        writeln!(p.writer, "{}{}Then", p.prefix, make_connector(!has_else))?;
        p.with_indent(!has_else, |p| print_statement(p, then_stmt, true))?;

        // Else (always the last child, if it exists)
        if let Some(else_stmt) = else_stmt {
            writeln!(p.writer, "{}{}Else", p.prefix, make_last_child_connector())?;
            p.with_indent(true, |p| print_statement(p, else_stmt, true))?;
        }

        Ok(())
    })
}

fn print_switch_statement<W: Write>(
    p: &mut AstPrinter<W>,
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("SwitchStmt")?;
    print_node_metadata(p, node_id)?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        writeln!(p.writer, "{}{}Condition", p.prefix, make_child_connector())?;
        p.with_indent(false, |p| print_expression(p, controlling_expr, true))?;

        writeln!(p.writer, "{}{}Body", p.prefix, make_last_child_connector())?;
        p.with_indent(true, |p| print_statement(p, body, true))
    })
}

fn print_while_statement<W: Write>(
    p: &mut AstPrinter<W>,
    node_id: AstNodeId,
    controlling_expr: &AstExpression,
    body: &AstStatement,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("WhileStmt")?;
    print_node_metadata(p, node_id)?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        writeln!(p.writer, "{}{}Condition", p.prefix, make_child_connector())?;
        p.with_indent(false, |p| print_expression(p, controlling_expr, true))?;

        writeln!(p.writer, "{}{}Body", p.prefix, make_last_child_connector())?;
        p.with_indent(true, |p| print_statement(p, body, true))
    })
}

fn print_do_while_statement<W: Write>(
    p: &mut AstPrinter<W>,
    node_id: AstNodeId,
    body: &AstStatement,
    controlling_expr: &AstExpression,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("DoStmt")?;
    print_node_metadata(p, node_id)?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        writeln!(p.writer, "{}{}Body", p.prefix, make_child_connector())?;
        p.with_indent(false, |p| print_statement(p, body, true))?;

        writeln!(p.writer, "{}{}WhileCondition", p.prefix, make_last_child_connector())?;
        p.with_indent(true, |p| print_expression(p, controlling_expr, true))
    })
}

fn print_for_statement<W: Write>(p: &mut AstPrinter<W>, stmt: &AstStatement, is_last: bool) -> io::Result<()> {
    let AstStatement::For { node_id, init, controlling_expr, post_expr, body } = stmt else {
        ICE!("Expected AstStatement::For");
    };

    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("ForStmt")?;
    print_node_metadata(p, *node_id)?;
    p.new_line()?;

    p.with_indent(is_last, |p| {
        // Init
        {
            write!(p.writer, "{}{}Init", p.prefix, make_child_connector())?;

            match init.as_ref() {
                AstForInitializer::Declaration(var_decls) => {
                    p.new_line()?;

                    p.with_indent(false, |p| {
                        let len = var_decls.len();
                        for (idx, decl) in var_decls.iter().enumerate() {
                            print_declaration(p, decl, idx == len - 1)?;
                        }

                        Ok(())
                    })?
                }
                AstForInitializer::Expression(expr) => match expr {
                    Some(expr) => {
                        p.new_line()?;
                        p.with_indent(false, |p| print_expression(p, expr, true))?
                    }
                    None => {
                        p.writeln(": None")?;
                    }
                },
            }
        }

        // Condition
        {
            write!(p.writer, "{}{}Condition", p.prefix, make_child_connector())?;

            match controlling_expr {
                Some(controlling_expr) => {
                    p.new_line()?;
                    p.with_indent(false, |p| print_expression(p, controlling_expr, true))?
                }
                None => {
                    p.writeln(": None")?;
                }
            }
        }

        // Post
        {
            write!(p.writer, "{}{}Post", p.prefix, make_child_connector())?;

            match post_expr {
                Some(post_expr) => {
                    p.new_line()?;
                    p.with_indent(false, |p| print_expression(p, post_expr, true))?
                }
                None => {
                    p.writeln(": None")?;
                }
            }
        }

        // Body
        writeln!(p.writer, "{}{}Body", p.prefix, make_last_child_connector())?;
        p.with_indent(true, |p| print_statement(p, body, true))
    })
}

fn print_break_statement<W: Write>(
    p: &mut AstPrinter<W>,
    enclosing_stmt_node_id: AstNodeId,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("BreakStmt")?;
    if p.print_node_ids {
        write!(p.writer, " [Enclosing Stmt Id = {enclosing_stmt_node_id}]")?;
    }
    p.new_line()
}

fn print_switch_case_statement<W: Write>(
    p: &mut AstPrinter<W>,
    switch_node_id: AstNodeId,
    constant_expr: &AstExpression,
    stmt: &AstStatement,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("CaseStmt")?;
    if p.print_node_ids {
        write!(p.writer, " [Switch Id = {switch_node_id}]")?;
    }
    p.new_line()?;

    p.with_indent(is_last, |p| {
        writeln!(p.writer, "{}{}Constant Expr", p.prefix, make_child_connector())?;
        p.with_indent(false, |p| print_expression(p, constant_expr, true))?;

        writeln!(p.writer, "{}{}Statement", p.prefix, make_last_child_connector())?;
        p.with_indent(true, |p| print_statement(p, stmt, true))
    })
}

fn print_switch_default_statement<W: Write>(
    p: &mut AstPrinter<W>,
    switch_node_id: AstNodeId,
    stmt: &AstStatement,
    is_last: bool,
) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("DefaultStmt")?;
    if p.print_node_ids {
        write!(p.writer, " [Switch Id = {switch_node_id}]")?;
    }
    p.new_line()?;

    p.with_indent(is_last, |p| print_statement(p, stmt, true))
}

fn print_return_statement<W: Write>(p: &mut AstPrinter<W>, expr: &AstExpression, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("ReturnStmt")?;
    p.new_line()?;

    p.with_indent(is_last, |p| print_expression(p, expr, true))
}

fn print_continue_statement<W: Write>(p: &mut AstPrinter<W>, loop_node_id: AstNodeId, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("ContinueStmt")?;
    if p.print_node_ids {
        write!(p.writer, " [Loop Id = {loop_node_id}]")?;
    }
    p.new_line()
}

fn print_goto_statement<W: Write>(p: &mut AstPrinter<W>, label: &str, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_stmt_str("GotoStmt")?;
    p.write(": ")?;
    p.write_ident_str(label)?;
    p.new_line()
}

fn print_expression<W: Write>(p: &mut AstPrinter<W>, expr: &AstExpression, is_last: bool) -> io::Result<()> {
    p.write_prefix_and_connector(make_connector(is_last))?;
    p.write_expr_str(get_expression_node_name(expr.kind()))?;

    match expr.kind() {
        AstExpressionKind::Unary { op, operand } => {
            let token_type = lexer::TokenType::from(*op);
            write!(p.writer, " ({}{op} {token_type}{})", p.expr_color, p.reset_color)?;
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| print_expression(p, operand, true))
        }

        AstExpressionKind::Binary { op, lhs, rhs } => {
            let token_type = lexer::TokenType::from(*op);
            write!(p.writer, " ({}{op} {token_type}{})", p.expr_color, p.reset_color)?;
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                print_expression(p, lhs, false)?;
                print_expression(p, rhs, true)
            })
        }

        AstExpressionKind::Assignment { computation_node_id, op, lhs, rhs } => {
            if op.is_compound_assignment() {
                let token_type = lexer::TokenType::from(*op);
                write!(p.writer, " ({}{op} {token_type}{})", p.expr_color, p.reset_color)?;

                if let Some(comp_node_type) = p.metadata.try_get_node_type(*computation_node_id) {
                    let comp_type = format!("[Computation Type = '{comp_node_type}']");
                    p.write(" ")?;
                    p.write_meta_str(&comp_type)?;
                }
            }

            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                print_expression(p, lhs, false)?;
                print_expression(p, rhs, true)
            })
        }

        AstExpressionKind::Cast { target_type, inner, .. } => {
            write!(p.writer, " ({}{target_type}{})", p.expr_color, p.reset_color)?;
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| print_expression(p, inner, true))
        }

        AstExpressionKind::Deref { pointer } => {
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| print_expression(p, pointer, true))
        }

        AstExpressionKind::AddressOf { target } => {
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| print_expression(p, target, true))
        }

        AstExpressionKind::Subscript { expr1, expr2 } => {
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                print_expression(p, expr1, false)?;
                print_expression(p, expr2, true)
            })
        }

        AstExpressionKind::Conditional { condition, consequent, alternative } => {
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                writeln!(p.writer, "{}{}Condition", p.prefix, make_child_connector())?;
                p.with_indent(false, |p| print_expression(p, condition, true))?;

                writeln!(p.writer, "{}{}Consequent", p.prefix, make_child_connector())?;
                p.with_indent(false, |p| print_expression(p, consequent, true))?;

                writeln!(p.writer, "{}{}Alternative", p.prefix, make_last_child_connector())?;
                p.with_indent(true, |p| print_expression(p, alternative, true))
            })
        }

        AstExpressionKind::FunctionCall { designator, args, .. } => {
            print_node_metadata(p, expr.id())?;
            p.new_line()?;

            p.with_indent(is_last, |p| {
                writeln!(p.writer, "{}{}Designator", p.prefix, make_child_connector())?;
                p.with_indent(false, |p| print_expression(p, designator, true))?;

                // Args
                write!(p.writer, "{}{}Args", p.prefix, make_last_child_connector())?;
                if args.is_empty() {
                    p.writeln(": void")
                } else {
                    p.new_line()?;

                    p.with_indent(true, |p| {
                        let len = args.len();
                        for (idx, arg) in args.iter().enumerate() {
                            print_expression(p, arg, idx == len - 1)?;
                        }
                        Ok(())
                    })
                }
            })
        }

        AstExpressionKind::Ident { name, unique_name, .. } => {
            p.write(": ")?;
            p.write_ident_str(name)?;
            write!(p.writer, " [{unique_name}]")?;
            print_node_metadata(p, expr.id())?;
            p.new_line()
        }

        AstExpressionKind::CharLiteral { literal, is_multichar, value } => {
            let multichar_str = if *is_multichar { " [Multi-char]" } else { "" };

            write!(
                p.writer,
                "({}{literal}{} [{}{value}{}]{multichar_str})",
                p.expr_color, p.reset_color, p.expr_color, p.reset_color
            )?;
            print_node_metadata(p, expr.id())?;
            p.new_line()
        }

        AstExpressionKind::StringLiteral { literals, ascii } => {
            let ascii_joined = ascii.join("");

            if literals.len() == 1 {
                let literal = &literals[0];
                write!(p.writer, "({literal} [\"{ascii_joined}\"])")?;
                print_node_metadata(p, expr.id())?;
                p.new_line()
            } else {
                p.new_line()?;

                p.with_indent(is_last, |p| {
                    writeln!(p.writer, "{}{}Literals", p.prefix, make_child_connector())?;

                    p.with_indent(false, |p| {
                        let len = literals.len();
                        for (idx, lit) in literals.iter().enumerate() {
                            writeln!(p.writer, "{}{}{lit}", p.prefix, make_connector(idx == len - 1))?;
                        }
                        Ok(())
                    })?;

                    write!(p.writer, "{}{}Evaluated:  \"{ascii_joined}\"", p.prefix, make_last_child_connector())?;
                    print_node_metadata(p, expr.id())?;
                    p.new_line()
                })
            }
        }

        AstExpressionKind::IntegerLiteral { literal, literal_base, value, kind } => {
            // If we've performed typechecking then we don't need to print its kind, since we can print its
            // resolved AstType instead.
            if p.metadata.try_get_node_type(expr.id()).is_some() {
                write!(p.writer, "({}{literal}{}", p.expr_color, p.reset_color)?;
            } else {
                write!(p.writer, "({kind} = {}{literal}{}", p.expr_color, p.reset_color)?;
            }

            if *literal_base != 10 {
                write!(p.writer, " [{}{value}{}]", p.expr_color, p.reset_color)?;
            }
            p.write(")")?;

            print_node_metadata(p, expr.id())?;
            p.new_line()
        }

        AstExpressionKind::FloatLiteral { literal, literal_base, kind, value } => {
            if *literal_base == 10 {
                write!(p.writer, "({kind} = {}{literal}{})", p.expr_color, p.reset_color)?;
            } else {
                write!(
                    p.writer,
                    "({kind} = {}{literal}{} [{}{value}{}])",
                    p.expr_color, p.reset_color, p.expr_color, p.reset_color
                )?;
            }
            print_node_metadata(p, expr.id())?;
            p.new_line()
        }
    }
}

fn get_expression_node_name(kind: &AstExpressionKind) -> &str {
    match kind {
        AstExpressionKind::Unary { .. } => "Unary",
        AstExpressionKind::Binary { .. } => "Binary",
        AstExpressionKind::Assignment { op, .. } if op.is_compound_assignment() => "CompoundAssignment",
        AstExpressionKind::Assignment { .. } => "Assignment",
        AstExpressionKind::Conditional { .. } => "Ternary",
        AstExpressionKind::FunctionCall { .. } => "Call",
        AstExpressionKind::Deref { .. } => "Deref",
        AstExpressionKind::AddressOf { .. } => "AddressOf",
        AstExpressionKind::Subscript { .. } => "Subscript",
        AstExpressionKind::Cast { is_implicit, .. } if *is_implicit => "ImplicitCast",
        AstExpressionKind::Cast { .. } => "Cast",
        AstExpressionKind::Ident { .. } => "Ident",
        AstExpressionKind::CharLiteral { .. } => "CharLiteral",
        AstExpressionKind::StringLiteral { .. } => "StringLiteral",
        AstExpressionKind::IntegerLiteral { .. } => "IntegerLiteral",
        AstExpressionKind::FloatLiteral { .. } => "FloatLiteral",
    }
}

fn print_resolved_type<W: Write>(p: &mut AstPrinter<W>, declared_type: &AstDeclaredType) -> io::Result<()> {
    if let Some(ty) = &declared_type.resolved_type {
        print_type(p, ty)?;
    }

    Ok(())
}

fn print_type<W: Write>(p: &mut AstPrinter<W>, ty: &AstType) -> io::Result<()> {
    write!(p.writer, " {}[Type = '{}{ty}{}']{}", p.meta_color, p.type_color, p.meta_color, p.reset_color)
}

fn print_node_metadata<W: Write>(p: &mut AstPrinter<W>, node_id: AstNodeId) -> io::Result<()> {
    if p.print_node_ids {
        write!(p.writer, "{} [Id = {node_id}]", p.meta_color)?;
    }

    if let Some(ty) = p.metadata.try_get_node_type(node_id) {
        print_type(p, ty)?;
    }

    write!(p.writer, "{}", p.meta_color)?;

    if p.metadata.is_expr_flag_set(node_id, AstExpressionFlag::IsConstant) {
        p.write(" [ConstantExpr]")?;
    }

    if p.metadata.is_expr_flag_set(node_id, AstExpressionFlag::PromotedToInt) {
        p.write(" [PromotedToInt]")?;
    }
    write!(p.writer, "{}", p.reset_color)?;

    Ok(())
}

fn make_connector(is_last: bool) -> &'static str {
    if is_last { make_last_child_connector() } else { make_child_connector() }
}

fn make_child_connector() -> &'static str {
    "├ "
}

fn make_last_child_connector() -> &'static str {
    "└ "
}
