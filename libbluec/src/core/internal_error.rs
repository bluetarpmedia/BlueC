// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `internal_error` module defines functions to report Internal Compiler Errors (ICE).

/// Generates an internal compiler error.
#[allow(non_snake_case)]
pub fn ICE<S: AsRef<str>>(error_message: S) -> ! {
    panic!("ICE: {}", error_message.as_ref());
}

/// Generates an internal compiler error.
#[macro_export]
macro_rules! ICE {
    ($($arg:tt)*) => {
        std::panic!("BlueC ICE: {}", format_args!($($arg)*))
    }
}
