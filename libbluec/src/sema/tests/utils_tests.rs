use super::super::type_check::utils;
use crate::parser::{AstConstantFp, AstConstantInteger, AstConstantValue};

#[test]
fn combine_consecutive_zero_bytes_test() {
    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
            AstConstantValue::ZeroBytes(4),
            AstConstantValue::ZeroBytes(2),
            AstConstantValue::ZeroBytes(4)
        ]),
        vec![AstConstantValue::Integer(AstConstantInteger::Int(13)), AstConstantValue::ZeroBytes(10)]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstConstantValue::ZeroBytes(1024),
            AstConstantValue::ZeroBytes(1),
            AstConstantValue::ZeroBytes(1),
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
        ]),
        vec![AstConstantValue::ZeroBytes(1026), AstConstantValue::Integer(AstConstantInteger::Int(13))]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstConstantValue::ZeroBytes(1),
            AstConstantValue::ZeroBytes(1),
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
            AstConstantValue::ZeroBytes(1),
            AstConstantValue::ZeroBytes(1),
        ]),
        vec![
            AstConstantValue::ZeroBytes(2),
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
            AstConstantValue::ZeroBytes(2)
        ]
    );

    assert_eq!(
        utils::combine_consecutive_zero_bytes(vec![
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
            AstConstantValue::ZeroBytes(4),
            AstConstantValue::Fp(AstConstantFp::Double(1.5)),
            AstConstantValue::ZeroBytes(4)
        ]),
        vec![
            AstConstantValue::Integer(AstConstantInteger::Int(13)),
            AstConstantValue::ZeroBytes(4),
            AstConstantValue::Fp(AstConstantFp::Double(1.5)),
            AstConstantValue::ZeroBytes(4)
        ]
    );
}
