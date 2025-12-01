// Copyright 2025 Neil Henderson, Blue Tarp Media.

//! Integration tests for compiler warnings.

use std::path::{Path, PathBuf};

use libbluec::compiler_driver::options::{DriverFlag, DriverOptions};
use libbluec::compiler_driver::{Driver, DriverError};

#[test]
fn warning_as_error() {
    let options = DriverOptions { warnings_enabled: true, warnings_as_errors: true, ..Default::default() };
    let mut driver = Driver::new(&get_test_source_file_path("warnings/uninitialized.c"), options);
    let driver_result = driver.run();

    assert!(driver_result.is_err_and(|e| e == DriverError::CompilerFailed));
    assert_eq!(driver.error_count(), 1);
    assert_eq!(driver.warning_count(), 0);
}

// Include the generated test case functions.
include!(concat!(env!("OUT_DIR"), "/generated_valid_program_with_warnings_tests.rs"));

fn compile_source_file_and_expect_warnings(source_filename: &str) {
    let options = DriverOptions { warnings_enabled: true, validate: true, ..Default::default() };
    let mut driver = Driver::new(source_filename, options);
    driver.set_flag(DriverFlag::PRINT_NO_SOURCE_LOC);
    let driver_result = driver.run();

    assert!(driver_result.is_ok());
    assert!(!driver.has_error_diagnostics());
    assert!(driver.warning_count() > 0);

    let mut buf: Vec<u8> = Vec::new();
    driver.print_diagnostics_to_buffer(&mut buf);

    let expected_file_path = get_expected_result_file_path(source_filename);
    let expected = std::fs::read(&expected_file_path).unwrap_or_else(|_| {
        eprintln!("Test case returned `{}`", String::from_utf8(buf.clone()).unwrap());
        panic!("Cannot open file: {}", &expected_file_path);
    });

    if buf != expected {
        eprintln!(
            "Actual:\n`{}`\n\nExpected:\n`{}`",
            String::from_utf8(buf).unwrap(),
            String::from_utf8(expected).unwrap()
        );
        assert!(false);
    }
}

fn get_expected_result_file_path(path: &str) -> String {
    let path = Path::new(path).with_extension("txt");
    let filename = path.file_name().unwrap();
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // Only set when using `cargo ...`
    resource_path.push("tests/warnings/expected");
    resource_path.push(filename);
    resource_path.into_os_string().into_string().expect("Invalid path")
}

fn get_test_source_file_path(path: &str) -> String {
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // Only set when using `cargo ...`
    resource_path.push("tests/");
    resource_path.push(path);
    resource_path.into_os_string().into_string().expect("Invalid path")
}
