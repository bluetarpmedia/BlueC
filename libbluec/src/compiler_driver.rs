// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The compiler_driver module defines the wrapper tool that orchestrates the different compilation stages.

pub mod args;
pub mod diagnostics;
pub mod errors;
pub mod multi_file_driver;
pub mod options;
pub mod tempfile;
pub mod warnings;

use crate::ICE;
use crate::compiler_driver::diagnostics::{DiagnosticKind, Printer};
use crate::compiler_driver::options::{DriverFlag, DriverOptions};
use crate::compiler_driver::tempfile::TempFile;
use crate::lexer;

use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Stdio};

/// The BlueC compiler driver.
pub struct Driver {
    // The path to the source `.c` file.
    pub source_filename: String,

    // The path to the temp preprocessed translation unit file.
    pub translation_unit_filename: String,

    // The path to the `.s` assembly file created by our compiler.
    pub asm_filename: String,

    // Options that control the driver.
    options: Option<DriverOptions>,

    // Diagnostics emitted by the compiler driver
    diagnostics_enabled: bool,
    errors: Vec<diagnostics::Diagnostic>,
    warnings: Vec<diagnostics::Diagnostic>,

    // Owns the temporary file we create for the preprocessed translation unit file.
    _translation_unit_temp_file: TempFile,
}

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

impl Driver {
    /// Creates a new compiler driver configured to compile the given source file.
    pub fn new(source_filename: &str, options: DriverOptions) -> Self {
        let (translation_unit_temp_file, translation_unit_filename) = create_temp_file_path(source_filename);
        let asm_filename = Path::new(&source_filename).with_extension("s").to_string_lossy().to_string();

        Self {
            source_filename: source_filename.to_string(),
            translation_unit_filename,
            asm_filename,
            options: Some(options),
            diagnostics_enabled: true,
            errors: Vec::new(),
            warnings: Vec::new(),
            _translation_unit_temp_file: translation_unit_temp_file,
        }
    }

    /// Creates a new compiler driver for unit tests, with default options and no source filename.
    #[cfg(test)]
    pub fn for_testing() -> Self {
        Self {
            source_filename: String::new(),
            translation_unit_filename: String::new(),
            asm_filename: String::new(),
            options: Some(DriverOptions::default()),
            diagnostics_enabled: true,
            errors: Vec::new(),
            warnings: Vec::new(),
            _translation_unit_temp_file: TempFile::none(),
        }
    }

    /// Runs the compiler pipeline and returns the appropriate file depending on the given options.
    pub fn run(&mut self) -> Result<CompilerGeneratedFile, DriverError> {
        // Run the preprocessor (external tool)
        preprocces(&self.source_filename, &self.translation_unit_filename, &self.options().preprocessor_defns)?;

        // Begin the compilation pipeline
        //      Note: diagnostics are recorded on the Driver itself, and not returned as errors in the `Result<>` type.
        compile(self)?;

        if self.options().lex
            || self.options().parse
            || self.options().validate
            || self.options().print_ast
            || self.options().print_typechecked_ast
            || self.options().print_ir
            || self.options().codegen
        {
            return Ok(CompilerGeneratedFile::None);
        }

        if self.options().only_create_asm_file {
            return Ok(CompilerGeneratedFile::AssemblyFile(self.asm_filename.clone()));
        }

        // Run the assembler
        let assembler_output_filename = assemble(self)?;

        Ok(CompilerGeneratedFile::ObjectFile(assembler_output_filename))
    }

    /// The compiler driver's options.
    pub fn options(&self) -> &DriverOptions {
        self.options.as_ref().expect("ICE: Options should exist")
    }

    /// Takes ownership of the compiler driver's options.
    pub fn take_options(&mut self) -> Option<DriverOptions> {
        self.options.take()
    }

    /// Is the given flag set in the options?
    pub fn is_flag_set(&self, flag: &str) -> bool {
        self.options().flags.contains(flag)
    }

    /// Turns a flag on.
    pub fn set_flag(&mut self, flag: &str) {
        self.options.as_mut().unwrap().flags.insert(flag.to_string());
    }

    /// Are diagnostics enabled?
    pub fn diagnostics_enabled(&self) -> bool {
        self.diagnostics_enabled
    }

    /// Sets whether diagnostics are enabled.
    pub fn set_diagnostics_enabled(&mut self, enabled: bool) {
        self.diagnostics_enabled = enabled;
    }

    /// Adds a diagnostic (error or warning).
    pub fn add_diagnostic(&mut self, diagnostic: diagnostics::Diagnostic) {
        if !self.diagnostics_enabled {
            return;
        }

        match diagnostic.kind() {
            DiagnosticKind::Error => self.errors.push(diagnostic),
            DiagnosticKind::Warning => {
                if self.options().warnings_enabled {
                    if self.options().warnings_as_errors {
                        self.errors.push(diagnostic.convert_to_error());
                    } else {
                        self.warnings.push(diagnostic);
                    }
                }
            }
        }
    }

    /// Are there any error diagnostics?
    pub fn has_error_diagnostics(&self) -> bool {
        !self.errors.is_empty()
    }

    /// The number of error diagnostics.
    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    /// The number of warning diagnostics.
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }

    /// Prints all diagnostics to `stderr`, with any errors printed first before any warnings.
    pub fn print_diagnostics(&self) {
        self.print_diagnostics_to_buffer(std::io::stderr());
    }

    /// Prints all diagnostics to the given `buffer`, with any errors printed first before any warnings.
    ///
    /// You probably want `print_diagnostics` instead of this function, unless you deliberately want to print
    /// diagnostics into a buffer.
    pub fn print_diagnostics_to_buffer(&self, buffer: impl Write) {
        if self.errors.is_empty() && self.warnings.is_empty() {
            return;
        }

        let terse = self.is_flag_set(DriverFlag::PRINT_TERSE);
        let show_source_loc = !self.is_flag_set(DriverFlag::PRINT_NO_SOURCE_LOC);

        let mut printer = Printer::with_source_and_tu(buffer, &self.source_filename, &self.translation_unit_filename);
        printer.set_terse(terse);
        printer.show_source_file_and_loc(show_source_loc);
        printer.print_diagnostics(&self.errors, &self.warnings);
    }

    /// For tests and debugging purposes, prints the diagnostics using the Debug trait.
    #[cfg(any(test, debug_assertions))]
    pub fn debug_print_diagnostics(&self) {
        for error in &self.errors {
            println!("{:?}", error);
        }
        for warning in &self.warnings {
            println!("{:?}", warning);
        }
    }
}

/// Preprocesses the source file (using an external tool) and writes the resulting translation unit to the given destination filename.
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

fn create_temp_file_path(source_filename: &str) -> (TempFile, String) {
    let temp_file_prefix = Path::new(source_filename).file_stem().unwrap_or_default().to_str().unwrap_or_default();

    let temp_file = TempFile::try_create(temp_file_prefix);
    if temp_file.is_none() {
        ICE!("Cannot create temporary file path");
    }

    let temp_file = temp_file.unwrap();
    let temp_file_path = temp_file.path_to_string();

    (temp_file, temp_file_path)
}
