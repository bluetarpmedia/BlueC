// Copyright 2025-2026 Neil Henderson

use super::super::generate::{align_stack_addr, round_up_to_multiple_of_16};

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

#[test]
fn test_align_stack_addr() {
    assert_eq!(align_stack_addr(0, 1), 0);
    assert_eq!(align_stack_addr(0, 4), 0);
    assert_eq!(align_stack_addr(0, 8), 0);
    assert_eq!(align_stack_addr(0, 16), 0);

    assert_eq!(align_stack_addr(-1, 4), -4);
    assert_eq!(align_stack_addr(-4, 4), -4);
    assert_eq!(align_stack_addr(-8, 4), -8);
    assert_eq!(align_stack_addr(-16, 4), -16);
    assert_eq!(align_stack_addr(-17, 4), -20);

    assert_eq!(align_stack_addr(-1, 8), -8);
    assert_eq!(align_stack_addr(-4, 8), -8);
    assert_eq!(align_stack_addr(-8, 8), -8);
    assert_eq!(align_stack_addr(-16, 8), -16);
    assert_eq!(align_stack_addr(-17, 8), -24);

    assert_eq!(align_stack_addr(-1, 16), -16);
    assert_eq!(align_stack_addr(-4, 16), -16);
    assert_eq!(align_stack_addr(-8, 16), -16);
    assert_eq!(align_stack_addr(-16, 16), -16);
    assert_eq!(align_stack_addr(-17, 16), -32);
}
