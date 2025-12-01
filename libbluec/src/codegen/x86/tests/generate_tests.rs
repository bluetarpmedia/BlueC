// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::generate::round_up_to_multiple_of_16;

#[test]
fn test_round_up_to_multiple_of_16() {
    assert_eq!(round_up_to_multiple_of_16(0), 0);
    assert_eq!(round_up_to_multiple_of_16(1), 16);
    assert_eq!(round_up_to_multiple_of_16(4), 16);
    assert_eq!(round_up_to_multiple_of_16(8), 16);
    assert_eq!(round_up_to_multiple_of_16(15), 16);
    assert_eq!(round_up_to_multiple_of_16(16), 16);
    assert_eq!(round_up_to_multiple_of_16(17), 32);
    assert_eq!(round_up_to_multiple_of_16(32), 32);
    assert_eq!(round_up_to_multiple_of_16(33), 48);
}
