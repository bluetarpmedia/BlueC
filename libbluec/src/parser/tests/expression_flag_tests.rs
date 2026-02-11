// Copyright 2025-2026 Neil Henderson

use super::super::{AstExpressionFlag, AstExpressionFlags};

#[test]
fn none() {
    let flags = AstExpressionFlags::none();
    assert!(!flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));
}

#[test]
fn insert() {
    let mut flags = AstExpressionFlags::none();
    flags.insert(AstExpressionFlag::HasParens);
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));

    flags.insert(AstExpressionFlag::IsConstant);
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(flags.contains(AstExpressionFlag::IsConstant));

    flags.insert(AstExpressionFlag::HasParens);
    flags.insert(AstExpressionFlag::IsConstant);
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(flags.contains(AstExpressionFlag::IsConstant));
}

#[test]
fn remove() {
    let mut flags = AstExpressionFlags::none();
    flags.insert(AstExpressionFlag::HasParens);
    flags.insert(AstExpressionFlag::IsConstant);
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(flags.contains(AstExpressionFlag::IsConstant));

    flags.remove(AstExpressionFlag::IsConstant);
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));

    flags.remove(AstExpressionFlag::HasParens);
    assert!(!flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));

    flags.remove(AstExpressionFlag::IsConstant);
    flags.remove(AstExpressionFlag::HasParens);
    assert!(!flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));
}

#[test]
fn bitor() {
    let flags = AstExpressionFlags::none() | AstExpressionFlag::IsConstant;
    assert!(!flags.contains(AstExpressionFlag::HasParens));
    assert!(flags.contains(AstExpressionFlag::IsConstant));

    let mut flags = AstExpressionFlags::none();
    assert!(!flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));

    flags |= AstExpressionFlag::HasParens;
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(!flags.contains(AstExpressionFlag::IsConstant));

    flags |= AstExpressionFlag::IsConstant;
    assert!(flags.contains(AstExpressionFlag::HasParens));
    assert!(flags.contains(AstExpressionFlag::IsConstant));
}

#[test]
fn bitand() {
    let flags = AstExpressionFlag::IsConstant | AstExpressionFlag::HasParens;
    let mask = AstExpressionFlag::IsConstant | AstExpressionFlag::HasParens;
    assert_eq!(flags & mask, flags);

    let flags = AstExpressionFlag::IsConstant | AstExpressionFlag::HasParens;
    let mask = AstExpressionFlags::from(AstExpressionFlag::IsConstant);
    assert_eq!(flags & mask, mask);

    let flags = AstExpressionFlags::from(AstExpressionFlag::IsConstant);
    let mask = AstExpressionFlags::from(AstExpressionFlag::HasParens);
    assert_eq!(flags & mask, AstExpressionFlags::none());
}
