// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `hex_float` module provides parser functionality to parse and convert hex floating-point literals into `f32`
//! and `f64` values. It calls two C stdlib functions `strtod` and `strtof` which require `unsafe` blocks.
//! This module is only built if the `hex-float-literal` feature is enabled.

use std::ffi::CString;
use std::os::raw::{c_char, c_double, c_float, c_int};
use std::ptr;
use std::str;

unsafe extern "C" {
    /// `strtof` from C stdlib
    /// Interprets a floating-point value in a byte string pointed to by `str`.
    ///
    /// `float strtof(const char* str, char** str_end);`
    fn strtof(str: *const c_char, str_end: *mut *mut c_char) -> c_float;

    /// `strtod` from C stdlib
    /// Interprets a floating-point value in a byte string pointed to by `str`.
    ///
    /// `double strtod(const char* str, char** str_end);`
    fn strtod(str: *const c_char, str_end: *mut *mut c_char) -> c_double;

    // We also need a platform-specific API to get the location of the `errno` global variable.
    //
    #[cfg(target_os = "macos")]
    fn __error() -> *mut c_int;

    #[cfg(all(unix, not(target_os = "macos")))]
    fn __errno_location() -> *mut c_int;

    #[cfg(windows)]
    fn _errno() -> *mut c_int;
}

/// Returns the location of the `errno` global variable.
#[allow(unreachable_code)]
pub(in super::super) fn errno_location() -> *mut c_int {
    unsafe {
        #[cfg(all(unix, not(target_os = "macos")))]
        {
            return __errno_location();
        }
        #[cfg(target_os = "macos")]
        {
            return __error();
        }
        #[cfg(windows)]
        {
            return _errno();
        }
        ptr::null_mut()
    }
}

/// Clears `errno`.
pub(in super::super) fn clear_errno() {
    let err_loc = errno_location();
    if !err_loc.is_null() {
        unsafe {
            *err_loc = 0;
        }
    }
}

/// Gets the value of `errno`.
pub(in super::super) fn get_errno() -> i32 {
    let err_loc = errno_location();
    unsafe { if err_loc.is_null() { 0 } else { *err_loc as i32 } }
}

const ERANGE: c_int = 34;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ParseHexFloatErr {
    StringContainsNull,
    OutOfRange,
}

/// Parse a hex floating-point literal string using C's `strtod` and checks `errno` for range errors.
pub fn parse_as_f64(s: &str) -> Result<f64, ParseHexFloatErr> {
    let c_str = CString::new(s).map_err(|_| ParseHexFloatErr::StringContainsNull)?;
    let c_ptr = c_str.as_ptr();

    clear_errno();

    let val = unsafe { strtod(c_ptr, ptr::null_mut()) };

    if get_errno() == ERANGE {
        return Err(ParseHexFloatErr::OutOfRange);
    }

    Ok(val)
}

/// Parse a hex floating-point literal string using C's `strtof` and checks `errno` for range errors.
pub fn parse_as_f32(s: &str) -> Result<f32, ParseHexFloatErr> {
    let c_str = CString::new(s).map_err(|_| ParseHexFloatErr::StringContainsNull)?;
    let c_ptr = c_str.as_ptr();

    clear_errno();

    let val = unsafe { strtof(c_ptr, ptr::null_mut()) };

    if get_errno() == ERANGE {
        return Err(ParseHexFloatErr::OutOfRange);
    }

    Ok(val)
}
