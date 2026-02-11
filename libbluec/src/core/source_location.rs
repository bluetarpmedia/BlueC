// Copyright 2025-2026 Neil Henderson
//
//! The `source_location` module defines the [SourceLocation] type, which represents the location and span of a token
//! in the source code.

use std::fmt;

use super::FilePosition;

/// The location and span of a token in the preprocessed translation unit.
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file_pos: FilePosition,
    pub length: u32,
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pos: {}, len: {}", self.file_pos, self.length)
    }
}

impl SourceLocation {
    /// Creates a new source location for the given file position and length in bytes.
    pub fn new(file_pos: FilePosition, length: usize) -> Self {
        debug_assert!(length > 0 && length <= u32::MAX as usize);
        Self { file_pos, length: length as u32 }
    }

    /// Creates a new source location which indicates a 'null' / no value.
    pub fn none() -> Self {
        Self { file_pos: FilePosition::from(0), length: 0 }
    }

    /// Merges this and the `other` location into a new location, taking the union of the two.
    pub fn merge_with(self, other: SourceLocation) -> Self {
        let this_start = self.file_pos;
        let this_end = self.file_pos + self.length;

        let other_start = other.file_pos;
        let other_end = other.file_pos + other.length;

        let start = std::cmp::min(this_start, other_start);
        let end = std::cmp::max(this_end, other_end);

        Self { file_pos: start, length: (end - start).into() }
    }

    /// Modifies this location's length so that this location's span finishes immediately before the given `other` one
    /// begins.
    ///
    /// This function will shrink or extend this location's length depending on the `other` one's start position.
    /// The `other` location must start after this one, or otherwise the operation is a no-op.
    pub fn set_span_up_to_location(&mut self, other: &SourceLocation) {
        if other.file_pos > self.file_pos {
            self.length = (other.file_pos - self.file_pos).into();
        }
    }

    /// Gets the next source location, of length 1, after this one.
    pub fn get_next_location(&self) -> SourceLocation {
        let next_file_pos = self.file_pos + self.length;
        SourceLocation::new(next_file_pos, 1)
    }
}
