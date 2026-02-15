// Copyright 2025-2026 Neil Henderson

//! Integration tests for AST printing.

use std::path::{Path, PathBuf};

use libbluec::compiler_driver::Driver;
use libbluec::compiler_driver::options::DriverOptions;
use libbluec::core::TempFile;

#[test]
fn var_decls_ast() {
    print_ast_no_color(&get_test_source_file_path("ast_printer/var_decls.c"))
}

#[test]
fn var_decls_typechecked_ast() {
    print_typechecked_ast_no_color(&get_test_source_file_path("ast_printer/var_decls.c"))
}

#[test]
fn var_decls_typechecked_color_ast() {
    print_typechecked_ast_with_color(&get_test_source_file_path("ast_printer/var_decls.c"))
}

#[test]
fn fn_pointers_ast() {
    print_ast_no_color(&get_test_source_file_path("ast_printer/fn_pointers.c"))
}

#[test]
fn fn_pointers_typechecked_ast() {
    print_typechecked_ast_no_color(&get_test_source_file_path("ast_printer/fn_pointers.c"))
}

#[test]
fn stmt_and_expr_ast() {
    print_ast_no_color(&get_test_source_file_path("ast_printer/stmt_and_expr.c"))
}

#[test]
fn stmt_and_expr_typechecked_ast() {
    print_typechecked_ast_no_color(&get_test_source_file_path("ast_printer/stmt_and_expr.c"))
}

fn print_ast_no_color(source_filename: &str) {
    print_ast(source_filename, false, true);
}

fn print_typechecked_ast_no_color(source_filename: &str) {
    print_ast(source_filename, true, true);
}

fn print_typechecked_ast_with_color(source_filename: &str) {
    print_ast(source_filename, true, false);
}

fn print_ast(source_filename: &str, type_checked: bool, no_color: bool) {
    let actual_file = TempFile::try_create_with_extension("bluec", "txt");
    let actual_file = actual_file.expect("Did not create temp txt file");
    let actual_file_path = actual_file.path_as_str();

    let mut options = DriverOptions::without_warnings();

    if type_checked {
        options.print_typechecked_ast = true;
    } else {
        options.print_ast = true;
    }

    options.no_color = no_color;

    // Undocumented flag to direct the Driver to print its AST without Node IDs.
    options.flags.insert(format!("ast_printer_no_ids"));

    // Undocumented flag to direct the Driver to print to a file instead of 'stdout'.
    options.flags.insert(format!("redirect_stdout"));
    options.flags.insert(format!("redirect_file={actual_file_path}"));

    let mut driver = Driver::new(source_filename, options);
    _ = driver.run();

    assert!(!driver.has_error_diagnostics());

    let actual = std::fs::read(actual_file_path).unwrap_or_else(|_| {
        panic!("Cannot open file: {}", &actual_file_path);
    });

    let expected_file_path = if type_checked {
        if no_color {
            get_expected_result_file_path(&append_filename_suffix(source_filename, "tc"))
        } else {
            get_expected_result_file_path(&append_filename_suffix(source_filename, "tc_color"))
        }
    } else {
        get_expected_result_file_path(source_filename)
    };

    let expected = std::fs::read(&expected_file_path).unwrap_or_else(|_| {
        eprintln!("Test case returned `{}`", String::from_utf8(actual.clone()).unwrap());
        panic!("Cannot open file: {}", &expected_file_path);
    });

    if actual != expected {
        eprintln!(
            "Actual:\n`{}`\n\nExpected:\n`{}`",
            String::from_utf8(actual).unwrap(),
            String::from_utf8(expected).unwrap()
        );
        assert!(false);
    }
}

fn get_expected_result_file_path(path: &str) -> String {
    let path = Path::new(path).with_extension("txt");
    let filename = path.file_name().unwrap();
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // Only set when using `cargo ...`
    resource_path.push("tests/ast_printer/expected");
    resource_path.push(filename);
    resource_path.into_os_string().into_string().expect("Invalid path")
}

fn get_test_source_file_path(path: &str) -> String {
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // Only set when using `cargo ...`
    resource_path.push("tests/");
    resource_path.push(path);
    resource_path.into_os_string().into_string().expect("Invalid path")
}

fn append_filename_suffix(path: &str, suffix: &str) -> String {
    let path = Path::new(path);
    let stem = path.file_stem().map(|s| s.to_string_lossy()).unwrap_or_default();

    let extension = path.extension().map(|e| e.to_string_lossy()).unwrap_or_default();
    let new_filename =
        if extension.is_empty() { format!("{stem}_{suffix}") } else { format!("{stem}_{suffix}.{extension}") };
    let path = path.with_file_name(new_filename);

    path.into_os_string().into_string().expect("Invalid path")
}
