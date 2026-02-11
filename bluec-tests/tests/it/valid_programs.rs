// Copyright 2025-2026 Neil Henderson

//! Integration tests that compile source files, run the resulting binaries, and verify their exit codes.

use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use libbluec::compiler_driver::options::DriverOptions;
use libbluec::compiler_driver::{Driver, multi_file_driver};
use libbluec::core::TempFile;

const DEPENDENCIES_JSON_FILENAME: &str = "dependencies.json";

// Include the generated test case functions.
include!(concat!(env!("OUT_DIR"), "/generated_valid_program_tests.rs"));

fn expected_exit_codes_map() -> &'static HashMap<String, i32> {
    static HASHMAP: OnceLock<HashMap<String, i32>> = OnceLock::new();

    HASHMAP.get_or_init(|| load_expected_result_maps())
}

/// Returns the expected return/exit code for a given test case executable.
///
/// If an `expected_results.json` file exists in the test case directory, and if it has an entry for the test case,
/// then that exit code is returned. Otherwise we assume the test program is meant to return zero for success.
fn get_expected_exit_code(test_case: &str) -> i32 {
    if let Some(exit_code) = expected_exit_codes_map().get(test_case) { *exit_code } else { 0 }
}

/// An integration test case for a single source file which we expect to compile and run successfully.
/// The process exit code should match what we expect.
struct ExpectedSuccessTestCase {
    source_filename: String,
    exe_filename: String,
    expected_exit_code: i32,
    dep_obj_files: Vec<TempFile>,
}

impl Drop for ExpectedSuccessTestCase {
    fn drop(&mut self) {
        if let Ok(true) = std::fs::exists(&self.exe_filename) {
            std::fs::remove_file(&self.exe_filename).expect("failed to delete executable file");
        }
    }
}

impl ExpectedSuccessTestCase {
    /// Creates a new test case which we expect to compile and run successfully.
    pub fn new(source_filename: &str, exe_filename: &str, expected_exit_code: i32) -> Self {
        Self {
            source_filename: source_filename.to_string(),
            exe_filename: exe_filename.to_string(),
            expected_exit_code,
            dep_obj_files: Vec::new(),
        }
    }

    /// Compiles the source file, runs the executable, and then compares the process exit code with the expected code.
    pub fn run(&mut self) {
        let mut driver_options = make_driver_options();

        let run_parse_and_sema_only = std::env::var("BLUEC_VALIDATE_ONLY").is_ok();
        if run_parse_and_sema_only {
            driver_options.validate = true;
        }

        // Check if we need to compile any external dependencies this test case. If so, we'll compile the source file
        // with gcc into an '.o' object file and then pass the object file to BlueC to link with the test case source.
        if !driver_options.validate
            && let Ok(dep_obj_files) = self.get_dependency_obj_files()
        {
            self.dep_obj_files = dep_obj_files;

            driver_options.link_obj_files =
                self.dep_obj_files.iter().map(|obj_file| obj_file.path_to_string()).collect();
        }

        let mut driver = Driver::new(&self.source_filename, driver_options);
        let driver_result = driver.run();

        if driver.has_error_diagnostics() {
            driver.print_diagnostics();
            assert!(false);
        }

        assert!(driver_result.is_ok());

        if run_parse_and_sema_only {
            return;
        }

        // Run the executable
        //      The Drop trait will delete the exe file when the test case drops.
        //      Future: Add a switch to keep the exe file for debugging purposes.
        assert!(std::fs::exists(&self.exe_filename).is_ok(), "Compiled executable binary file does not exist");

        let exe_output = Command::new(&self.exe_filename).output().expect("failed to run the executable");

        let status = exe_output.status;

        match status.code() {
            Some(code) => assert_eq!(code, self.expected_exit_code),
            None => assert!(false, "Process exited without a status code (signal"),
        }
    }

    fn get_dependency_obj_files(&self) -> Result<Vec<TempFile>, ()> {
        let mut out = Vec::new();

        let test_case_filename = Path::new(&self.source_filename);
        let test_case_dir =
            test_case_filename.parent().expect("Did not get parent directory for test case source file");
        let deps_json_filename = test_case_dir.join(DEPENDENCIES_JSON_FILENAME);
        let test_case_base_name =
            test_case_filename.file_name().expect("Did not base file name for test case source file").to_str().unwrap();

        if let Ok(true) = std::fs::exists(&deps_json_filename) {
            if let Some(deps_map) = read_dependencies_as_map(&deps_json_filename) {
                if let Some(deps) = deps_map.get(test_case_base_name) {
                    for src_filename in deps {
                        let src_filename = test_case_dir.join(src_filename);
                        let src_filename = src_filename.to_str().unwrap();

                        let obj_file = TempFile::try_create_with_extension("bluec", "o");
                        let obj_file = obj_file.expect("Did not create temp .o file");

                        gcc_compile_to_obj_file(src_filename, obj_file.path_as_str())?;
                        out.push(obj_file);
                    }
                }
            }
        }

        Ok(out)
    }
}

/// An integration test case for multiple source files which we expect to compile, link, and run successfully.
/// The process exit code should match what we expect.
struct ExpectedMultiFileSuccessTestCase {
    source_filenames: Vec<&'static str>,
    exe_filename: String,
    expected_exit_code: i32,
}

impl Drop for ExpectedMultiFileSuccessTestCase {
    fn drop(&mut self) {
        if let Ok(true) = std::fs::exists(&self.exe_filename) {
            std::fs::remove_file(&self.exe_filename).expect("failed to delete executable file");
        }
    }
}

impl ExpectedMultiFileSuccessTestCase {
    /// Creates a new test case for multiple source files which we expect to compile, link, and run successfully.
    pub fn new(source_filenames: Vec<&'static str>, exe_filename: &str, expected_exit_code: i32) -> Self {
        Self { source_filenames, exe_filename: exe_filename.to_string(), expected_exit_code }
    }

    pub fn run(&mut self) {
        // We'll skip multi-file test cases if the client only wants to run the parser and sema.
        // The `compile_and_link` function assumes object files will be created, or an error occurs.
        let run_parse_and_sema_only = std::env::var("BLUEC_VALIDATE_ONLY").is_ok();
        if run_parse_and_sema_only {
            return;
        }

        let source_filenames = self.source_filenames.iter().map(|s| String::from(*s)).collect::<Vec<String>>();

        // If we have 2 source files (which is the expected scenario) then we'll run the test twice.
        //      1. Compile 'first' with BlueC and 'second' with gcc and link them together and run.
        //      2. Compile 'first' with gcc and 'second' with BlueC and link them together and run.
        // This verifies that BlueC is adhering to the the System V ABI.
        // Otherwise, we'll use BlueC to compile all the source files and link them together and then run.
        //
        if source_filenames.len() == 2 {
            self.bluec_link_with_gcc(&source_filenames[0], &source_filenames[1]);
            self.gcc_link_with_bluec(&source_filenames[0], &source_filenames[1]);
        } else {
            // Remember: Tests run in parallel so we don't want to use the same output filename, or the default 'a.out'.
            let mut driver_options = make_driver_options();
            driver_options.output_file = Some(self.exe_filename.clone());

            let driver_result = multi_file_driver::compile_and_link(driver_options, &source_filenames);
            assert!(driver_result.is_ok());

            self.run_executable();
        }
    }

    fn bluec_link_with_gcc(&mut self, first: &str, second: &str) {
        // Use gcc to compile the second source file to an object file
        let obj_file = TempFile::try_create_with_extension(second, "o");
        let obj_file = obj_file.expect("Did not create temp .o file");
        assert!(gcc_compile_to_obj_file(second, obj_file.path_as_str()).is_ok());

        // Compile the first file with BlueC and link with the object file produced by gcc
        let mut options = make_driver_options();
        options.link_obj_files.push(obj_file.path_to_string());
        options.output_file = Some(self.exe_filename.clone());

        let mut driver = Driver::new(first, options);
        let driver_result = driver.run();
        assert!(driver_result.is_ok());

        self.run_executable();
    }

    fn gcc_link_with_bluec(&mut self, first: &str, second: &str) {
        // Use BlueC to compile the second source file to an object file
        let obj_file = TempFile::try_create_with_extension(second, "o");
        let obj_file = obj_file.expect("Did not create temp .o file");

        let mut options = make_driver_options();
        options.generate_object_file = true;
        options.output_file = Some(obj_file.path_to_string());

        let mut driver = Driver::new(second, options);
        let driver_result = driver.run();
        assert!(driver_result.is_ok());

        // Compile the first file with gcc and link with the object file produced by BlueC
        assert!(gcc_compile_and_link_with_obj_file(first, obj_file.path_as_str(), &self.exe_filename).is_ok());

        self.run_executable();
    }

    fn run_executable(&self) {
        // Run the executable
        //      The Drop trait will delete the exe file when the test case drops.
        //      Future: Add a switch to keep the exe file for debugging purposes.
        assert!(std::fs::exists(&self.exe_filename).is_ok(), "Compiled executable binary file does not exist");

        // This executable will be in our current directory so we need to prefix the command with "./" to run it.
        let exe_output = Command::new(&format!("./{}", &self.exe_filename))
            .output()
            .expect(&format!("failed to run the executable: {}", &self.exe_filename));

        let status = exe_output.status;

        match status.code() {
            Some(code) => assert_eq!(code, self.expected_exit_code),
            None => assert!(false, "Process exited without a status code (signal"),
        }
    }
}

fn read_expected_exit_codes_as_map(json_filename: &str) -> Option<HashMap<String, i32>> {
    let expected_results_reader =
        BufReader::new(File::open(json_filename).expect("Cannot open expected results json file"));

    let v: serde_json::Value = serde_json::from_reader(expected_results_reader).ok()?;

    let map: HashMap<String, i32> = v
        .as_array()
        .ok_or("Expected a JSON array")
        .ok()?
        .iter()
        .filter_map(|item| {
            let key = item.get("filename")?.as_str()?.to_string();
            let value = item.get("exit_code")?.as_i64()? as i32;
            Some((key, value))
        })
        .collect();

    Some(map)
}

fn read_dependencies_as_map(json_file_path: &PathBuf) -> Option<HashMap<String, Vec<String>>> {
    let dependencies_reader = BufReader::new(File::open(json_file_path).expect("Cannot open dependencies json file"));

    let v: serde_json::Value = serde_json::from_reader(dependencies_reader).ok()?;

    let map: HashMap<String, Vec<String>> = v
        .as_array()
        .ok_or("Expected a JSON array")
        .ok()?
        .iter()
        .filter_map(|item| {
            let key = item.get("filename")?.as_str()?.to_string();

            let dependencies_value = item.get("dependencies")?.as_array()?;
            let dependencies =
                dependencies_value.iter().map(|value| value.as_str().unwrap_or_default().to_string()).collect();
            Some((key, dependencies))
        })
        .collect();

    Some(map)
}

fn make_driver_options() -> DriverOptions {
    if cfg!(target_os = "macos") {
        DriverOptions::default()
    } else {
        let mut driver_options = DriverOptions::default();

        // Always link with the 'libm' math library for the integration tests, even though most don't need it.
        // On macOS this happens implicitly but we need it for Linux (and GitHub CI).
        driver_options.link_libs.push("m".to_string());

        driver_options
    }
}

fn gcc_compile_to_obj_file(src_filename: &str, obj_filename: &str) -> Result<String, ()> {
    let mut gcc_cmd = Command::new("gcc");

    if cfg!(target_os = "macos") {
        gcc_cmd.arg("-arch");
        gcc_cmd.arg("x86_64");
    }

    gcc_cmd.arg("-c"); // Compile to object file
    gcc_cmd.arg("-w"); // Disable warnings

    gcc_cmd.arg(src_filename);

    gcc_cmd.arg("-o");
    gcc_cmd.arg(obj_filename);

    let status = gcc_cmd.status().expect("failed to run gcc");

    if status.success() {
        Ok(obj_filename.to_string())
    } else {
        eprintln!("gcc failed when compiling '{src_filename}': {status}");
        Err(())
    }
}

fn gcc_compile_and_link_with_obj_file(
    src_filename: &str,
    obj_filename: &str,
    output_filename: &str,
) -> Result<String, ()> {
    let mut gcc_cmd = Command::new("gcc");

    if cfg!(target_os = "macos") {
        gcc_cmd.arg("-arch");
        gcc_cmd.arg("x86_64");
    }

    gcc_cmd.arg("-w"); // Disable warnings

    gcc_cmd.arg(src_filename);

    gcc_cmd.arg(obj_filename);

    // Link with libm for math.h
    gcc_cmd.arg("-lm");

    gcc_cmd.arg("-o");
    gcc_cmd.arg(output_filename);

    let status = gcc_cmd.status().expect("failed to run gcc");

    if status.success() {
        Ok(obj_filename.to_string())
    } else {
        eprintln!("gcc failed when compiling '{src_filename}': {status}");
        Err(())
    }
}
