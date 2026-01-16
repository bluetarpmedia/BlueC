// Copyright 2025 Neil Henderson, Blue Tarp Media.

//! Integration tests that expect the compiler to generate error diagnostics when compiling ill-formed source files.

use libbluec::compiler_driver::options::DriverOptions;
use libbluec::compiler_driver::{Driver, DriverError};

// Include the generated test case functions.
include!(concat!(env!("OUT_DIR"), "/generated_invalid_program_tests.rs"));

/// Compiles the given source file and verifies that there is one or more error diagnostics.
fn compile_source_file_and_expect_diagnostics(source_filename: &str) {
    let mut options = DriverOptions::default();

    // Only run up to the codegen stage; if the program does compile (unexpectedly) we don't want an executable
    // binary left over.
    options.codegen = true;

    let mut driver = Driver::new(source_filename, options);
    let driver_result = driver.run();

    assert!(driver_result.is_err_and(|e| e == DriverError::CompilerFailed));
    assert!(driver.has_error_diagnostics());

    // Future: Once error printing is stabilised, print to a file and compare against an expected result.
}
