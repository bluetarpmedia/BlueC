// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `compiler_driver` module defines the functions and types which orchestrate the different compilation stages.

pub mod args;
pub mod diagnostics;
pub mod errors;
pub mod multi_file_driver;
pub mod options;
pub mod tempfile;

mod warning;
mod warning_kind;
mod driver;

pub use warning::Warning;
pub use warning_kind::WarningKind;
pub use driver::Driver;

use crate::compiler_driver::options::DriverOptions;
use crate::lexer;

use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Stdio};

/// An error returned by the compiler driver.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DriverError {
    PreprocessorFailed,
    LexerFailed,
    CompilerFailed,
    AssemblerFailed,
}

/// A file path returned by the compiler driver.
#[derive(Debug, Clone)]
pub enum CompilerGeneratedFile {
    /// No file was generated.
    None,

    /// A file containing assembly code.
    AssemblyFile(String),

    /// Either an object file ('.o') or an executable binary file.
    ObjectFile(String),
}

/// Preprocesses the source file (using an external tool) and writes the resulting translation unit to the given
/// destination filename.
fn preprocces(
    in_source_file: &str,
    out_translation_unit_file: &str,
    definitions: &Vec<String>,
) -> Result<(), DriverError> {
    #[allow(unreachable_code)]
    if cfg!(windows) {
        // Future:
        //
        // MSVC's cl.exe prints the source filename to stderr when compiling/preprocessing,
        // which we don't want. So we pipe the stderr output and then only display it if
        // the process returned an error code. E.g. if there's a preprocessing error we do
        // want to show that to the user.
        #[allow(unused_variables)]
        let output = Command::new("cl.exe")
            .arg("/nologo")
            .arg("/P") // Run preprocessor
            .arg("/EP") // Do not emit #line directives
            .arg("/C") // Preserve comments
            .arg(in_source_file)
            .stderr(Stdio::piped())
            .output()
            .expect("failed to run cl.exe for preprocessor step; make sure cl.exe is available on the PATH.");

        todo!();

        if output.status.success() {
            Ok(())
        } else {
            let _ = io::stderr().write_all(&output.stderr);
            Err(DriverError::PreprocessorFailed)
        }
    } else {
        // Generate args for the preprocessor.
        let mut prpr_cmd = Command::new("gcc");

        for defn in definitions {
            prpr_cmd.arg("-D");
            prpr_cmd.arg(defn);
        }

        prpr_cmd.arg("-E"); // Preprocess the source file
        prpr_cmd.arg("-P"); // Do not generate line markers
        prpr_cmd.arg("-w"); // Disable warnings

        prpr_cmd.arg(in_source_file);
        prpr_cmd.arg("-o");
        prpr_cmd.arg(out_translation_unit_file);

        let status = prpr_cmd.status().expect("failed to run gcc for preprocessor step");

        if status.success() { Ok(()) } else { Err(DriverError::PreprocessorFailed) }
    }
}

/// Compiles the preprocessed translation unit into assembly code.
fn compile(driver: &mut Driver) -> Result<(), DriverError> {
    // Run the lexer.
    //      This kicks off the compiler pipeline and each stage subsequently calls the next, or returns.
    //      Note: diagnostics are recorded on the Driver itself, and not returned as errors in the `Result<>` type.
    lexer::lex(driver).map_err(|_| DriverError::LexerFailed)?;

    if driver.has_error_diagnostics() {
        return Err(DriverError::CompilerFailed);
    }

    Ok(())
}

/// Assembles the x86_64 assembly code into an object/executable file and return its file path.
fn assemble(driver: &mut Driver) -> Result<String, DriverError> {
    // Generate args for the assembler.
    let mut assembler_cmd = Command::new("gcc");

    if cfg!(target_os = "macos") {
        assembler_cmd.arg("-arch");
        assembler_cmd.arg("x86_64");
    }

    if driver.options().generate_object_file {
        assembler_cmd.arg("-c");
    }

    // Source assembly file (.s)
    assembler_cmd.arg(&driver.asm_filename);

    // Object files to link with: specify these after the source assembly file.
    for obj_file in &driver.options().link_obj_files {
        assembler_cmd.arg(obj_file);
    }

    // Libraries to link with: specify these after the source assembly file and any object files.
    for lib in &driver.options().link_libs {
        assembler_cmd.arg(format!("-l{lib}"));
    }

    // Output file
    assembler_cmd.arg("-o");

    let output_filename_ext = match driver.options().generate_object_file {
        true => "o",
        false => "",
    };

    let output_filename =
        create_output_filename(&driver.options().output_file, &driver.asm_filename, output_filename_ext);

    assembler_cmd.arg(&output_filename);

    // Run the assembler
    let status = assembler_cmd.status().expect("failed to run gcc for assembly step");

    // Delete the assembly file (.s) after assembling
    std::fs::remove_file(&driver.asm_filename).expect("failed to delete assembly file");

    if !status.success() {
        return Err(DriverError::AssemblerFailed);
    }

    Ok(output_filename)
}

fn create_output_filename(user_filename: &Option<String>, asm_filename: &str, extension: &str) -> String {
    match user_filename {
        Some(filename) => filename.clone(),
        None => Path::new(asm_filename).with_extension(extension).to_string_lossy().to_string(),
    }
}
