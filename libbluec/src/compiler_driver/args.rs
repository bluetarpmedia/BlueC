// Copyright 2025-2026 Neil Henderson
//
//! The `args` module defines the command-line argument parser.
//! Although crates like `clap` are excellent, we have a design goal to minimize dependencies on third-party crates
//! and so BlueC uses a custom argument parser.

use std::collections::{HashMap, HashSet};
use std::iter::Peekable;

use crate::ICE;

use super::{DriverOptions, WarningKind};

/// The command-line argument parser.
pub struct Parser {
    tokens: Peekable<std::vec::IntoIter<String>>,

    // Flags which were specified but have no value. E.g. '-W' or '--print-ast'.
    flags: HashSet<String>,

    // Flags which were specified with '-f<flag>'. We hold these separately from above since this whole HashSet
    // will be moved into the `DriverOptions`, rather than translating each flag individually like above.
    f_flags: HashSet<String>,

    // Options with a key and one or more values. A value may need to be further parsed.
    options: HashMap<String, Vec<String>>,

    // Preprocessor definitions.
    defines: HashMap<String, Option<String>>,

    // Object files specified on the command-line to link with.
    link_object_files: Vec<String>,
}

impl Parser {
    /// Parses the command-line arguments into the compiler driver options.
    pub fn parse_command_line_args() -> Option<(Vec<String>, DriverOptions)> {
        let args = std::env::args().collect::<Vec<String>>();
        if args.is_empty() {
            ICE!("std::env::args should at least return 1 arg for program name");
        }

        let mut parser = Parser {
            tokens: args.into_iter().peekable(),
            flags: HashSet::new(),
            f_flags: HashSet::new(),
            options: HashMap::new(),
            defines: HashMap::new(),
            link_object_files: Vec::new(),
        };

        _ = parser.take_next().unwrap(); // Skip program name
        let mut source_files = Vec::new();
        let mut parse_positional_only = false;

        while let Some(token) = parser.take_next() {
            if token == "-h" || token == "--help" {
                print_help();
                return None;
            }

            if token == "-V" || token == "--version" {
                print_version();
                return None;
            }

            // Double-dash signals end of options and start of positional args, though we also support parsing
            // positional args without this.
            //
            if token == "--" {
                parse_positional_only = true;
                continue;
            }

            if parse_positional_only {
                take_positional_argument(token, &mut source_files, &mut parser.link_object_files);
                continue;
            }

            // Long arguments start with double-dash.
            //
            if let Some(argument) = token.strip_prefix("--") {
                parse_long_arg(argument, &mut parser);
                continue;
            }

            // Short args start with a single dash and must have at least 2 chars, e.g. '-W' or '-Werror'.
            // They may optionally be followed by a value, e.g. '-o output.txt'.
            //
            if let Some(argument) = token.strip_prefix("-") {
                if !argument.is_empty() {
                    parse_short_arg(argument, &mut parser);
                }
                continue;
            }

            // Must be a positional argument
            take_positional_argument(token, &mut source_files, &mut parser.link_object_files);
        }

        if source_files.is_empty() {
            print_help();
            return None;
        }

        let driver_options = parser.into_options();
        Some((source_files, driver_options))
    }

    /// Takes the next token from the stream and returns it.
    fn take_next(&mut self) -> Option<String> {
        self.tokens.next()
    }

    /// Peeks at the next token without taking it from the stream.
    fn peek(&mut self) -> Option<&String> {
        self.tokens.peek()
    }

    /// Consumes the `Parser` and returns the `DriverOptions` from the parsed arguments.
    fn into_options(mut self) -> DriverOptions {
        let preprocessor_defns = self
            .defines
            .into_iter()
            .map(|(key, value)| if let Some(value) = value { format!("{key}={value}") } else { key })
            .collect();

        // Is the given flag set?
        let is_flag_set = |flag: &str| -> bool { self.flags.contains(flag) };

        // Does an option exist with the given key and value? E.g. for '-Werror', key is 'W' and value is 'error'.
        let is_option_set = |options: &HashMap<String, Vec<String>>, key: &str, value: &str| -> bool {
            if let Some(values) = options.get(key) { values.iter().any(|v| v == value) } else { false }
        };

        // Create the driver options with the appropriate warnings.
        let disable_all_warnings = is_flag_set("w");
        let mut options = if disable_all_warnings {
            DriverOptions::without_warnings()
        } else if let Some((enabled, disabled)) = parse_warning_args(&self.options) {
            DriverOptions::with_warnings(enabled, disabled)
        } else {
            DriverOptions::with_default_warnings()
        };

        options.preprocessor_defns = preprocessor_defns;
        options.generate_object_file = is_flag_set("c");
        options.output_file = self.options.remove("o").and_then(|vec| vec.into_iter().next());
        options.link_obj_files = self.link_object_files;
        options.link_libs = self.options.get("l").cloned().unwrap_or_default();
        options.warnings_as_errors = is_option_set(&self.options, "W", "error");
        options.only_create_asm_file = is_flag_set("S");
        options.lex = is_flag_set("lex");
        options.parse = is_flag_set("parse");
        options.validate = is_flag_set("validate");
        options.codegen = is_flag_set("codegen");
        options.print_ast = is_flag_set("print-ast");
        options.print_typechecked_ast = is_flag_set("print-tast");
        options.print_ir = is_flag_set("print-ir");
        options.flags = self.f_flags;

        options
    }
}

fn parse_warning_args(options: &HashMap<String, Vec<String>>) -> Option<(HashSet<WarningKind>, HashSet<WarningKind>)> {
    let warning_options = options.get("W")?;

    let print_unknown_warning_option = |option: &str| {
        println!("warning: Unknown warning option '{option}'");
    };

    // Gather all the warnings that the user wants to enable.
    let enable_all = warning_options.iter().any(|v| v == "all");
    let enabled_warnings = if enable_all {
        WarningKind::all()
    } else {
        warning_options
            .iter()
            .filter_map(|option| {
                if option.starts_with("no-") || option == "error" {
                    None
                } else {
                    let warning_kind = option.parse::<WarningKind>();

                    if warning_kind.is_err() {
                        print_unknown_warning_option(option);
                    }

                    warning_kind.ok()
                }
            })
            .collect()
    };

    // Gather all the warnings that the user wants to disable.
    let disabled_warnings = warning_options
        .iter()
        .filter_map(|option| {
            if option.starts_with("no-") {
                let warning_kind = option.strip_prefix("no-").unwrap().parse::<WarningKind>();

                if warning_kind.is_err() {
                    print_unknown_warning_option(option);
                }

                warning_kind.ok()
            } else {
                None
            }
        })
        .collect();

    Some((enabled_warnings, disabled_warnings))
}

fn take_positional_argument(arg: String, sources: &mut Vec<String>, objects: &mut Vec<String>) {
    if arg.ends_with(".o") {
        objects.push(arg);
    } else {
        sources.push(arg);
    }
}

fn parse_long_arg(argument: &str, parser: &mut Parser) {
    // Is the argument an option with a value after an equals character, like 'level=2'?
    //
    if let Some(pos) = argument.find('=') {
        let key = argument[..pos].to_string();
        let val = argument[pos + 1..].to_string();
        parser.options.entry(key).or_default().push(val);
    }
    // Otherwise the argument is a flag, like 'print'.
    //
    else {
        parser.flags.insert(argument.to_string());
    }
}

fn parse_short_arg(argument: &str, parser: &mut Parser) {
    // Special case for '-D' preprocessor definitions, since we want to extract their keys
    // and possible values, e.g. '-Dfoo=1', into a dedicated map.
    //
    if let Some(definition) = argument.strip_prefix('D') {
        parse_prepro_definition(definition, parser);
        return;
    }

    // Special case for '-f' flags. We store these separately in a HashSet so that they can be
    // moved into the `DriverOptions`, rather than translated individually, since there will be
    // too many.
    //
    if let Some(flag) = argument.strip_prefix('f') {
        parse_f_flag(flag, parser);
        return;
    }

    // Arguments like '-Werror' are treated as options with key 'W' and value 'error'.
    //      The value is not parsed further, e.g. 'Wlevel=1' becomes key 'W' and value 'level=1'.
    //
    let mut chars = argument.chars();
    let key = chars.next().unwrap().to_string();
    let attached = chars.collect::<String>();

    if !attached.is_empty() {
        parser.options.entry(key).or_default().push(attached);
        return;
    }

    // The argument can be a flag like '-c', or an option like '-o' with a value as the next token.
    // But we can't just always take the next token because it might be a positional argument.
    // E.g. "-c test.c" means '-c' flag followed by positional source file.
    // So we have to lookup the short argument to determine if it takes a value.
    //
    if SHORT_ARG_TAKES_VALUE.contains(&argument) {
        if parser.peek().map(|next_token| !next_token.starts_with('-')).unwrap_or(false) {
            let value = parser.take_next().unwrap();
            parser.options.entry(key).or_default().push(value);
        }
    } else {
        parser.flags.insert(key);
    }
}

fn parse_prepro_definition(definition: &str, parser: &mut Parser) {
    let parse_defn = |definition: &str, parser: &mut Parser| {
        if let Some(pos) = definition.find('=') {
            let key = definition[..pos].to_string();
            let val = Some(definition[pos + 1..].to_string());
            _ = parser.defines.insert(key, val); // Updates key with new value if definition is redefined
        } else {
            parser.defines.insert(definition.to_string(), None);
        }
    };

    // If we're parsing `-D foo=1` or `-D foo` then peek at the next token to get the definition.
    //
    if definition.is_empty() && parser.peek().is_some() {
        let next = parser.take_next().unwrap();
        parse_defn(&next, parser);
    }
    // We're parsing `-Dfoo=1` or `-Dfoo` then `definition` contains the `key[=value]`.
    //
    else {
        parse_defn(definition, parser);
    }
}

fn parse_f_flag(flag: &str, parser: &mut Parser) {
    _ = parser.f_flags.insert(flag.to_string());
}

fn print_version() {
    const VERSION: &str = env!("CARGO_PKG_VERSION");
    println!("BlueC compiler driver {VERSION}");
}

fn print_help() {
    const VERSION: &str = env!("CARGO_PKG_VERSION");

    println!("BlueC compiler driver {VERSION}");
    println!();
    println!("Usage: BlueC [options] file...");
    println!();
    println!("Options:");

    for opt in OPTIONS.chunks(2) {
        if opt[0].is_empty() {
            println!();
        } else {
            println!("  {: <25}  {: <50}", opt[0], opt[1]);
        }
    }

    println!("\n  Warnings");
    let warnings = WarningKind::all_strings();
    for warning in warnings {
        println!("  -W{warning}");
    }
}

#[rustfmt::skip]
static OPTIONS: &[&str] = &[
    "-D <macro>=<value>",    "Define <macro> to <value> (or 1 if <value> omitted)",
    "-S",                    "Only run preprocess and compilation steps to produce a '.s' assembly file",
    "-c",                    "Only run preprocess, compiler, and assembler steps to produce an object file (.o)",
    "-l <lib>",              "Specifies a library to link with",
    "-o <file>",             "Write output to <file>",
    "","",
    "-Wall",                 "Enable all warnings",
    "-w",                    "Disable all warnings",
    "-Werror",               "Treat warnings as errors",
    "-W<warning>",           "Enable a specific warning",
    "-Wno-<warning>",        "Disable a specific warning",
    "","",
    "-fprint-terse",         "Print terse diagnostics without text wrapping, nor filename/line/column and source code",
    "-fprint-no-source-loc", "Print diagnostics without filename/line/column",
    "","",
    "Developer options","",
    "--lex",                 "Only run the lexer",
    "--parse",               "Only run the lexer and parser",
    "--validate",            "Only run the lexer, parser, and sema stages",
    "--codegen",             "Only run the lexer, parser, sema, and codegen stages",
    "--print-ast",           "Print the parsed AST to stdout (--parse is implied)",
    "--print-tast",          "Print the typechecked AST to stdout (--validate is implied)",
    "--print-ir",            "Print the IR to stdout (and stops after lowering to IR)",
    "","",
    "-h, --help",            "Print help",
    "-V, --version",         "Print version",
];

// These arguments expect the next token to be the value, e.g. "-o outputfile".
static SHORT_ARG_TAKES_VALUE: &[&str] = &["o", "l"];
