// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `source_location` module defines the `SourceLocation` type.

use std::fmt;

/// The location and span of a token in the source file.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line: {}, col: {}", self.line, self.column)
    }
}

impl SourceLocation {
    /// Creates a new source location.
    pub fn new(line: usize, column: usize, length: usize) -> Self {
        Self { line, column, length }
    }

    /// Merges this and the `other` location into a new location, but only if they have the same line number.
    /// 
    /// If not, returns this location unchanged.
    pub fn merge_with(self, other: SourceLocation) -> Self {
        if self.line == other.line {
            let line = self.line;
            let column = std::cmp::min(self.column, other.column);
            let last_column = std::cmp::max(self.column + self.length, other.column + other.length);
            SourceLocation { line, column, length: last_column - column }
        } else {
            self
        }
    }

    /// Sets this location's length so that this location's span finishes immediately before the given `other` one,
    /// but only if both locations have the same line number.
    /// 
    /// This function will shrink or extend this location's length depending on the given `other` one.
    pub fn set_span_up_to_location(&mut self, other: &SourceLocation) {
        if self.line == other.line && other.column > self.column {
            self.length = other.column - self.column - 1;
        }
    }

    /// Gets the next source location, of length 1, after this one.
    pub fn get_next_location(&self) -> SourceLocation {
        let mut loc = *self;
        loc.column += loc.length;
        loc.length = 1;
        loc
    }
}
