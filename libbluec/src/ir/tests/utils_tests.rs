// Copyright 2025 Neil Henderson, Blue Tarp Media.

use super::super::translator::utils::*;

#[test]
    fn test_merge_nulls() {
        let mut input = vec!["abc\\000".to_string(), "\\000".to_string(), "\\000".to_string(), "def".to_string()];
        merge_adjacent_nulls(&mut input);

        assert_eq!(input, vec!["abc\\000", "\\000\\000", "def"]);
    }