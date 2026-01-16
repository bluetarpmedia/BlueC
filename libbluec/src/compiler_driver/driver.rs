// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `driver` module defines `Driver`, which is the BlueC compiler driver type.

use super::diagnostics::{Diagnostic, DiagnosticKind, Printer};
use super::options::{DriverFlag, DriverOptions};
use super::tempfile::TempFile;
use super::{CompilerGeneratedFile, DriverError};

use crate::ICE;

use std::io::Write;
use std::path::Path;

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
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,

    // Owns the temporary file we create for the preprocessed translation unit file.
    _translation_unit_temp_file: TempFile,
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
        super::preprocces(&self.source_filename, &self.translation_unit_filename, &self.options().preprocessor_defns)?;

        // Begin the compilation pipeline
        //      Note: diagnostics are recorded on the Driver itself, and not returned as errors in the `Result<>` type.
        super::compile(self)?;

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
        let assembler_output_filename = super::assemble(self)?;

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
    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        if !self.diagnostics_enabled {
            return;
        }

        match diagnostic.kind() {
            DiagnosticKind::Error => self.errors.push(diagnostic),
            
            DiagnosticKind::Warning(warning_kind) => {
                if self.options().is_warning_enabled(warning_kind) {
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
