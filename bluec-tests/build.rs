// Copyright 2025 Neil Henderson, Blue Tarp Media.

//! Build script to generate integration test cases based on files in the `tests/valid` and `tests/invalid` directories.

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::io::{BufReader, BufWriter};
use std::path::Path;
use std::path::PathBuf;

use glob::glob;

const INVALID_TEST_CASES_FILENAME: &str = "generated_invalid_program_tests.rs";
const VALID_TEST_CASES_FILENAME: &str = "generated_valid_program_tests.rs";
const WARNINGS_TEST_CASES_FILENAME: &str = "generated_valid_program_with_warnings_tests.rs";

const EXPECTED_RESULTS_JSON_FILENAME: &str = "expected_results.json";
const MULTI_FILE_TEST_CASE_JSON_FILENAME: &str = "multi_file_tests.json";

fn main() {
    generate_invalid_program_tests();
    generate_valid_program_tests();
    generate_valid_programs_with_warnings_tests();

    // We want cargo to rerun our build.rs script if any of the integration test files change.
    println!("cargo:rerun-if-changed=tests");
}

/// Generate test functions for cases we expect to fail due to the compiler emitting a diagnostic
/// because the single-source file has lexical, syntax, or semantic errors.
fn generate_invalid_program_tests() {
    let out_dir = std::env::var("OUT_DIR").unwrap(); // Set by cargo when running `cargo test`
    let generated_rs_filename = Path::new(&out_dir).join(INVALID_TEST_CASES_FILENAME);
    let mut writer = BufWriter::new(File::create(generated_rs_filename).expect("Failed to create file"));

    let invalid_tests_dir_path = get_tests_directory_path("invalid");

    for entry in glob(&format!("{}/**/*.c", invalid_tests_dir_path)).expect("Failed to read glob pattern") {
        match entry {
            Ok(source_file_path) => {
                let test_case_name = make_test_case_name(&invalid_tests_dir_path, &source_file_path);
                let source_filename = source_file_path.into_os_string().into_string().expect("Invalid path");

                let test_case_code = &format!(
                    r#"
                        #[test]
                        #[allow(non_snake_case)]
                        fn {test_case_name}() {{
                            compile_source_file_and_expect_diagnostics("{source_filename}");
                        }}
                    "#
                );

                let res = writer.write_all(test_case_code.as_bytes());
                assert!(res.is_ok());
            }
            Err(e) => println!("{:?}", e),
        }
    }
}

/// Generate test functions for cases we expect to be valid (successfully compile and run) where the case
/// only involves one source file.
fn generate_valid_program_tests() {
    let out_dir = std::env::var("OUT_DIR").unwrap(); // Set by cargo when running `cargo test`
    let generated_rs_filename = Path::new(&out_dir).join(VALID_TEST_CASES_FILENAME);
    let mut writer = BufWriter::new(File::create(generated_rs_filename).expect("Failed to create file"));

    generate_load_expected_result_maps_function(&mut writer);

    let valid_tests_dir_path = get_tests_directory_path("valid");

    for entry in glob(&format!("{}/**/*.c", valid_tests_dir_path)).expect("Failed to read glob pattern") {
        match entry {
            Ok(source_file_path) => {
                // Skip any files in directories named "dependencies". These are helpers used by some test cases.
                if source_file_path.components().any(|c| c.as_os_str() == "dependencies") {
                    continue;
                }

                // Skip the source file if it's in the "multi" directory. These are handled by generate_valid_multi_file_test_cases.
                if source_file_path.components().any(|c| c.as_os_str() == "multi") {
                    continue;
                }

                let test_case_name = make_test_case_name(&valid_tests_dir_path, &source_file_path);

                // Exe filename is full path to the source file, so it will be created alongside it.
                let mut exe_filename = source_file_path.clone();
                exe_filename.set_extension("");
                let exe_filename = exe_filename.into_os_string().into_string().expect("Invalid path");

                let source_filename = source_file_path.into_os_string().into_string().expect("Invalid path");

                let test_case_code = &format!(
                    r#"
                        #[test]
                        #[allow(non_snake_case)]
                        fn {test_case_name}() {{
                            let source_filename: &str = "{source_filename}";
                            let exe_filename: &str = "{exe_filename}";
                            let expected_exit_code = get_expected_exit_code("{test_case_name}.c");

                            _ = ExpectedSuccessTestCase::new(source_filename, exe_filename, expected_exit_code).run();
                        }}
                    "#
                );

                let res = writer.write_all(test_case_code.as_bytes());
                assert!(res.is_ok());
            }
            Err(e) => println!("{:?}", e),
        }
    }

    generate_valid_multi_file_test_cases(&mut writer);
}

/// Generate test functions for cases we expect to be valid (successfully compile and run) where they have
/// multiple source files to compile and link.
fn generate_valid_multi_file_test_cases(writer: &mut BufWriter<File>) {
    let valid_tests_dir_path = get_tests_directory_path("valid");

    for json_file_path in glob(&format!("{}/**/{}", valid_tests_dir_path, MULTI_FILE_TEST_CASE_JSON_FILENAME))
        .expect("Failed to read glob pattern")
        .flatten()
    {
        let base_dir = json_file_path.parent().unwrap();
        let json_filename = json_file_path.to_str().unwrap();
        let multi_file_cases = read_multi_file_test_cases(json_filename).expect("Did not read multi file cases");

        for (test_case_name, source_filenames) in multi_file_cases {
            let source_filename_list = source_filenames
                .into_iter()
                .map(|name| format!("\"{}/{}\"", base_dir.to_str().unwrap(), name))
                .collect::<Vec<_>>()
                .join(", ");

            let test_case_code = &format!(
                r#"
                    #[test]
                    #[allow(non_snake_case)]
                    fn {test_case_name}() {{
                        let source_filenames = vec![{source_filename_list}];
                        let expected_exit_code = get_expected_exit_code("{test_case_name}");

                        ExpectedMultiFileSuccessTestCase::new(source_filenames, "{test_case_name}", expected_exit_code).run();
                    }}
                "#
            );

            let res = writer.write_all(test_case_code.as_bytes());
            assert!(res.is_ok());
        }
    }
}

/// Generate test functions for cases we expect to succeed but with warnings.
fn generate_valid_programs_with_warnings_tests() {
    let out_dir = std::env::var("OUT_DIR").unwrap(); // Set by cargo when running `cargo test`
    let generated_rs_filename = Path::new(&out_dir).join(WARNINGS_TEST_CASES_FILENAME);
    let mut writer = BufWriter::new(File::create(generated_rs_filename).expect("Failed to create file"));

    let warnings_tests_dir_path = get_tests_directory_path("warnings");

    for entry in glob(&format!("{}/**/*.c", warnings_tests_dir_path)).expect("Failed to read glob pattern") {
        match entry {
            Ok(source_file_path) => {
                let test_case_name = make_test_case_name(&warnings_tests_dir_path, &source_file_path);
                let source_filename = source_file_path.into_os_string().into_string().expect("Invalid path");

                let test_case_code = &format!(
                    r#"
                        #[test]
                        #[allow(non_snake_case)]
                        fn {}() {{
                            compile_source_file_and_expect_warnings("{}");
                        }}
                    "#,
                    test_case_name, source_filename
                );

                let res = writer.write_all(test_case_code.as_bytes());
                assert!(res.is_ok());
            }
            Err(e) => println!("{:?}", e),
        }
    }
}

fn read_multi_file_test_cases(json_filename: &str) -> Option<HashMap<String, Vec<String>>> {
    let test_case_reader = BufReader::new(File::open(json_filename).expect("Cannot open expected json file"));

    let v: serde_json::Value = serde_json::from_reader(test_case_reader).ok()?;

    let map: HashMap<String, Vec<String>> = v
        .as_array()
        .ok_or("Expected a JSON array")
        .ok()?
        .iter()
        .filter_map(|item| {
            let key = item.get("test_case")?.as_str()?.to_string();
            let filenames_value = item.get("filenames")?.as_array()?;
            let filenames =
                filenames_value.iter().map(|value| value.as_str().unwrap_or_default().to_string()).collect();
            Some((key, filenames))
        })
        .collect();

    Some(map)
}

fn generate_load_expected_result_maps_function(writer: &mut BufWriter<File>) {
    let valid_tests_dir_path = get_tests_directory_path("valid");

    _ = writer.write_all(b"fn load_expected_result_maps() -> HashMap<String, i32> {");
    _ = writer.write_all(b"    let mut map = HashMap::new();");

    // Find all of the "expected_results.json" files underneath the "valid" directory.
    //      For each one, we'll make a call to `read_expected_exit_codes_as_map` to load the json as a HashMap,
    //      and then merge it with one main map, and then return the single map from the function we're generating.
    //
    for json_file_path in glob(&format!("{}/**/{}", valid_tests_dir_path, EXPECTED_RESULTS_JSON_FILENAME))
        .expect("Failed to read glob pattern")
        .flatten()
    {
        let json_filename = json_file_path.to_str().unwrap();

        let line = format!("    let expected = read_expected_exit_codes_as_map(\"{}\");", json_filename);
        _ = writer.write_all(line.as_bytes());

        _ = writer.write_all(b"    if expected.is_some() { map.extend(expected.unwrap()); }");
    }

    _ = writer.write_all(b"    map");
    _ = writer.write_all(b"}");
}

fn make_test_case_name(root_dir_path: &str, source_file_path: &Path) -> String {
    let root = Path::new(root_dir_path);
    let rel = match source_file_path.strip_prefix(root) {
        Ok(r) => r,
        Err(_) => source_file_path,
    };

    // Replace '/' with '_'
    let name = rel.with_extension("").iter().map(|s| s.to_string_lossy()).collect::<Vec<_>>().join("_");

    name.replace('-', "_")
}

fn get_tests_directory_path(directory: &str) -> String {
    let mut resource_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // Only set when using `cargo ...`
    resource_path.push("tests/");
    resource_path.push(directory);
    resource_path.into_os_string().into_string().expect("Invalid path")
}
