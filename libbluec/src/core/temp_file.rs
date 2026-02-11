// Copyright 2025-2026 Neil Henderson
//
//! The `temp_file` module defines the [TempFile] type. This implementation does not provide the same guarantees as the
//! `tempfile` crate but we have a design goal of no external dependencies for the BlueC library and executable.

use std::fs::{self, OpenOptions};
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// The path to a temporary file.
///
/// If the file exists when `TempFile` is dropped then it is automatically deleted.
///
/// # Examples
///
/// ```no_run
/// # use libbluec::core::TempFile;
/// if let Some(temp_file) = TempFile::try_create("bluec") {
///     let temp_file_path = temp_file.path_as_str();
///     // Write to file at `temp_file_path`
///     // When `temp_file` drops, if a file exists at `temp_file_path`, the file is deleted.
/// }
/// ```
#[derive(Debug)]
pub struct TempFile {
    pub path: PathBuf,
}

impl Drop for TempFile {
    fn drop(&mut self) {
        _ = std::fs::remove_file(&self.path);
    }
}

impl TempFile {
    /// Tries to create a unique temporary file path in the system's temp directory.
    ///
    /// If successful, the file will be automatically deleted when `TempFile` drops.
    pub fn try_create(prefix: &str) -> Option<Self> {
        let path = try_make_unique_temp_path(prefix, "")?;
        Some(TempFile { path })
    }

    /// Tries to create a unique temporary file path in the system's temp directory with the given file extension.
    ///
    /// If successful, the file will be automatically deleted when `TempFile` drops.
    pub fn try_create_with_extension(prefix: &str, ext: &str) -> Option<Self> {
        debug_assert!(!ext.contains('.'));

        let path = try_make_unique_temp_path(prefix, ext)?;
        Some(TempFile { path })
    }

    /// Creates an empty `TempFile`.
    #[cfg(test)]
    pub fn none() -> Self {
        TempFile { path: PathBuf::new() }
    }

    /// The `TempFile` path.
    pub fn path(&self) -> &std::path::Path {
        self.path.as_path()
    }

    /// The `TempFile` path as a `&str` string slice.
    pub fn path_as_str(&self) -> &str {
        self.path.to_str().expect("Expected the path to be UTF-8")
    }

    /// The `TempFile` path as a `String`.
    pub fn path_to_string(&self) -> String {
        self.path.to_string_lossy().into_owned()
    }
}

fn try_make_unique_temp_path(prefix: &str, ext: &str) -> Option<PathBuf> {
    let mut counter: u32 = 0;
    let pid = std::process::id();
    let temp_dir_path = std::env::temp_dir();

    const RETRY_COUNT: u32 = 10;

    loop {
        let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        let name = if ext.is_empty() {
            format!("{prefix}_{pid}_{now}_{counter}")
        } else {
            format!("{prefix}_{pid}_{now}_{counter}.{ext}")
        };
        let candidate = temp_dir_path.join(name);

        // If we can create a new file for the candidate path then we'll immediately delete the file
        // and then return that path. This is not a secure tempfile implementation, e.g. it has TOCTOU
        // issues, but it's fine for a compiler, and in the future we'll implement our own preprocessor
        // which will remove some of the race conditions.
        //
        match OpenOptions::new().write(true).create_new(true).open(&candidate) {
            Ok(_) => {
                let _ = fs::remove_file(&candidate);
                return Some(candidate);
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                counter = counter.wrapping_add(1);
                continue;
            }
            _ => {
                if counter >= RETRY_COUNT {
                    return None;
                }

                continue;
            }
        }
    }
}
