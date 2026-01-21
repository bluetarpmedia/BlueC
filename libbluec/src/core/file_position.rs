// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `file_position` module defines `FilePosition`, which represents a byte offset in a file.

use std::fmt;
use std::ops::{Add, AddAssign, Sub, SubAssign};

/// A 32-bit numerical value, starting from zero, that indicates the location of a byte in a file.
/// 
/// `FilePosition` is primarily used by the `lexer` to record the source location in the preprocessed translation
/// unit of every token. This information is later used when emitting diagnostics to include the relevant source code.
///
/// A 4 GB C source file should be enough for anybody, right?
#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePosition(u32);

macro_rules! impl_from {
    ($($t:ty),*) => {
        $(
            impl From<$t> for FilePosition {
                fn from(value: $t) -> Self {
                    let val = u32::try_from(value).expect("Value out of range for FilePosition");
                    Self(val)
                }
            }
        )*
    };
}

impl From<FilePosition> for u32 {
    fn from(val: FilePosition) -> Self {
        val.0
    }
}

// Allow conversion from these types
impl_from!(u32, i32, usize);

impl<T> Add<T> for FilePosition
where
    T: TryInto<u32>,
    <T as TryInto<u32>>::Error: std::fmt::Debug,
{
    type Output = FilePosition;

    fn add(self, rhs: T) -> Self::Output {
        let rhs_u32: u32 = rhs.try_into().expect("Value out of range for FilePosition");

        FilePosition(self.0 + rhs_u32)
    }
}

impl Sub<u32> for FilePosition {
    type Output = FilePosition;
    fn sub(self, rhs: u32) -> Self::Output {
        FilePosition(self.0 - rhs)
    }
}

impl Sub<FilePosition> for FilePosition {
    type Output = FilePosition;
    fn sub(self, rhs: FilePosition) -> Self::Output {
        FilePosition(self.0 - rhs.0)
    }
}

impl AddAssign<u32> for FilePosition {
    fn add_assign(&mut self, rhs: u32) {
        *self = *self + rhs;
    }
}

impl SubAssign<u32> for FilePosition {
    fn sub_assign(&mut self, rhs: u32) {
        *self = *self - rhs;
    }
}

impl fmt::Display for FilePosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
