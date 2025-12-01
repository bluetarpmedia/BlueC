// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::lexer::SourceLocation;

#[test]
fn merge_with() {
    let a = SourceLocation::new(1, 10, 1);
    let b = SourceLocation::new(1, 10, 1);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 10, 1));

    let a = SourceLocation::new(1, 10, 10);
    let b = SourceLocation::new(1, 20, 5);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 10, 15));

    let a = SourceLocation::new(1, 20, 5);
    let b = SourceLocation::new(1, 10, 10);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 10, 15));

    let a = SourceLocation::new(1, 10, 30);
    let b = SourceLocation::new(1, 20, 5);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 10, 30));

    let a = SourceLocation::new(1, 20, 5);
    let b = SourceLocation::new(1, 10, 30);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 10, 30));

    let a = SourceLocation::new(1, 1, 1);
    let b = SourceLocation::new(1, 1000, 1);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 1, 1000));

    let a = SourceLocation::new(1, 1000, 1);
    let b = SourceLocation::new(1, 1, 1);
    assert_eq!(a.merge_with(b), SourceLocation::new(1, 1, 1000));
}

#[test]
fn set_span_up_to_location() {
    // Extend
    let mut loc = SourceLocation::new(1, 10, 1);
    loc.set_span_up_to_location(&SourceLocation::new(1, 15, 1));
    assert_eq!(loc, SourceLocation::new(1, 10, 4));

    // Shrink
    let mut loc = SourceLocation::new(1, 10, 10);
    loc.set_span_up_to_location(&SourceLocation::new(1, 15, 1));
    assert_eq!(loc, SourceLocation::new(1, 10, 4));

    // No change: different line number
    let mut loc = SourceLocation::new(1, 10, 10);
    loc.set_span_up_to_location(&SourceLocation::new(2, 15, 1));
    assert_eq!(loc, SourceLocation::new(1, 10, 10));

    // No change: other loc is before the original
    let mut loc = SourceLocation::new(1, 10, 10);
    loc.set_span_up_to_location(&SourceLocation::new(1, 1, 1));
    assert_eq!(loc, SourceLocation::new(1, 10, 10));
}

#[test]
fn get_next_location() {
    let loc = SourceLocation::new(1, 10, 1);
    assert_eq!(loc.get_next_location(), SourceLocation::new(1, 11, 1));

    let loc = SourceLocation::new(1, 10, 5);
    assert_eq!(loc.get_next_location(), SourceLocation::new(1, 15, 1));
}
