// Copyright 2025-2026 Neil Henderson
//
//! The `internal_error` module defines a macro to emit an Internal Compiler Error (ICE).

/// Generates an internal compiler error.
#[macro_export]
macro_rules! ICE {
    ($($arg:tt)*) => {
        std::panic!("BlueC ICE: {}\n\n\
        This is a bug in the BlueC compiler.\n\
        Please report the bug at github.com/bluetarpmedia/BlueC\n\
        with minimal C code that reproduces the problem.\n", format_args!($($arg)*))
    }
}
