// Copyright 2025-2026 Neil Henderson
//
//! The `multi_file_driver` module provides functionality for multi-file compilation and linking.

use std::process::Command;

use crate::ICE;
use crate::compiler_driver::{CompilerGeneratedFile, Driver, DriverError, DriverOptions};

const DEFAULT_EXECUTABLE_FILENAME: &str = "a.out";

/// An error returned by the multi-file compiler and linker.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MultiFileError {
    CompilerFailed,
    LinkerFailed,
}

/// Compiles the given set of source files into object files and then links them together to produce an executable
/// binary file.
///
/// Returns the linked executable filename on success, or an error.
pub fn compile_and_link(
    driver_options: DriverOptions,
    source_filenames: &Vec<String>,
) -> Result<String, MultiFileError> {
    let mut object_files = Vec::new();
    let mut driver_options = driver_options;

    // The client can specify the output filename with '-o', or we use a default to match gcc.
    let executable_filename = driver_options.output_file.take().unwrap_or(DEFAULT_EXECUTABLE_FILENAME.into());

    // The compiler driver needs to generate an object file for each source file, regardless of what options
    // were specified.
    driver_options.generate_object_file = true;

    let mut compile_failed = false;

    // Compile each of the source files
    for source_filename in source_filenames {
        // Create a compiler driver and move the options into it
        let mut driver = Driver::new(source_filename, driver_options);

        // Run the compiler driver's pipeline
        let driver_result = driver.run();

        match driver_result {
            Ok(generated_file) => {
                // Print any warnings; should have no errors.
                debug_assert!(!driver.has_error_diagnostics());
                driver.print_diagnostics();

                if let CompilerGeneratedFile::ObjectFile(object_file) = generated_file {
                    object_files.push(object_file);
                } else {
                    ICE!("Compiler driver succeeded but did not return an object file");
                }
            }

            // If an error occurred then print diagnostics, set a flag, and then move on to the next source file.
            Err(driver_error) => match driver_error {
                DriverError::CompilerFailed => {
                    driver.print_diagnostics();
                    compile_failed = true;
                }
                _ => compile_failed = true,
            },
        }

        // Move the options out of the driver (which is about to drop) so we can move them into the next driver.
        driver_options = driver.take_options().expect("ICE: Driver options should exist");
    }

    if compile_failed || object_files.is_empty() {
        return Err(MultiFileError::CompilerFailed);
    }

    // Run the linker to link the object files into the executable file. Afterwards, delete the object files.
    let link_obj_files = driver_options.link_obj_files;
    let link_libs = driver_options.link_libs;
    link(&object_files, &executable_filename, link_obj_files, link_libs).map_err(|_| MultiFileError::LinkerFailed)?;

    Ok(executable_filename)
}

/// Run the linker to link object files into a binary executable file.
fn link(
    object_files: &Vec<String>,
    output_filename: &str,
    link_obj_files: Vec<String>,
    link_libs: Vec<String>,
) -> Result<(), ()> {
    // Generate args for the linker.
    let mut linker_cmd = Command::new("gcc");

    if cfg!(target_os = "macos") {
        linker_cmd.arg("-arch");
        linker_cmd.arg("x86_64");
    }

    // Object file(s) compiled by BlueC
    for filename in object_files {
        linker_cmd.arg(filename);
    }

    // Additional object files to link with: specify these after the BlueC-compiled object file(s).
    for obj_file in link_obj_files {
        linker_cmd.arg(obj_file);
    }

    // Libraries to link with: these should be specified after the object file(s).
    for lib in link_libs {
        linker_cmd.arg(format!("-l{lib}"));
    }

    // Output file
    linker_cmd.arg("-o");
    linker_cmd.arg(output_filename);

    // Run the linker
    let status = linker_cmd.status().expect("failed to run gcc for linker step");

    // Delete the object files after linking
    for filename in object_files {
        std::fs::remove_file(filename).expect("failed to delete object file");
    }

    if !status.success() {
        return Err(());
    }

    Ok(())
}
