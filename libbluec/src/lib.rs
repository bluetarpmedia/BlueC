// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The BlueC library.

#![doc(html_no_source)]

pub mod codegen;
pub mod compiler_driver;
pub mod core;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod sema;

pub use core::ICE;
