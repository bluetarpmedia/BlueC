// Copyright 2025-2026 Neil Henderson

//! main entry point for the compiler driver executable.

use std::process::ExitCode;

use libbluec::compiler_driver::args;
use libbluec::compiler_driver::{Driver, DriverError, multi_file_driver};

fn main() -> ExitCode {
    // Parse the command-line arguments into the compiler driver's options.
    let Some((source_files, driver_options)) = args::Parser::parse_command_line_args() else {
        return ExitCode::SUCCESS;
    };

    // If we have multiple source files to compile and link then we use a helper library function.
    if source_files.len() > 1 {
        // Cannot specify '-c' and '-o' with multiple source files.
        if driver_options.generate_object_file && driver_options.output_file.is_some() {
            eprintln!(
                "error: Cannot specify '-o' when generating multiple object files (because of '-c' and more than one source file)"
            );
            return ExitCode::FAILURE;
        }

        let driver_result = multi_file_driver::compile_and_link(driver_options, &source_files);

        match driver_result {
            Ok(_) => return ExitCode::SUCCESS,
            Err(_) => return ExitCode::FAILURE,
        }
    }

    // Run the compiler driver's pipeline for a single source file.
    let mut driver = Driver::new(&source_files[0], driver_options);
    let driver_result = driver.run();

    let mut exit_code = ExitCode::SUCCESS;

    // If any error diagnostics were emitted, print them and update our exit code.
    // Warnings are also printed but do not effect the exit code.
    if let Err(driver_error) = driver_result {
        match driver_error {
            DriverError::CompilerFailed => {
                driver.print_diagnostics();
                exit_code = ExitCode::FAILURE;
            }
            _ => exit_code = ExitCode::FAILURE,
        }
    } else {
        debug_assert!(!driver.has_error_diagnostics());
        driver.print_diagnostics();
    }

    exit_code
}
