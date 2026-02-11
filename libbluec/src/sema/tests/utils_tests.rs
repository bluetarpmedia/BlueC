// Copyright 2025-2026 Neil Henderson

use crate::parser::{AstConstantFp, AstConstantInteger, AstStaticStorageInitializer};

use super::super::type_check::utils;

#[test]
fn combine_consecutive_zero_bytes_test() {
    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(4),
            AstStaticStorageInitializer::ZeroBytes(2),
            AstStaticStorageInitializer::ZeroBytes(4)
        ]),
        vec![
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(10)
        ]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstStaticStorageInitializer::ZeroBytes(1024),
            AstStaticStorageInitializer::ZeroBytes(1),
            AstStaticStorageInitializer::ZeroBytes(1),
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
        ]),
        vec![
            AstStaticStorageInitializer::ZeroBytes(1026),
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13))
        ]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstStaticStorageInitializer::ZeroBytes(1),
            AstStaticStorageInitializer::ZeroBytes(1),
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(1),
            AstStaticStorageInitializer::ZeroBytes(1),
        ]),
        vec![
            AstStaticStorageInitializer::ZeroBytes(2),
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(2)
        ]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(4),
            AstStaticStorageInitializer::Fp(AstConstantFp::Double(1.5)),
            AstStaticStorageInitializer::ZeroBytes(4)
        ]),
        vec![
            AstStaticStorageInitializer::Integer(AstConstantInteger::Int(13)),
            AstStaticStorageInitializer::ZeroBytes(4),
            AstStaticStorageInitializer::Fp(AstConstantFp::Double(1.5)),
            AstStaticStorageInitializer::ZeroBytes(4)
        ]
    );
}
