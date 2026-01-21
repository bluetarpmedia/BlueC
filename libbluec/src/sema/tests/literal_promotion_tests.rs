// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::parser::{AstDeclaredType, AstExpression, AstIntegerLiteralKind, AstMetadata, AstNodeId, AstType};

use super::super::literal_promotion;

#[test]
fn promote_integer_literals() {
    test_promotion(1, AstIntegerLiteralKind::Int, &AstType::Long);
    test_promotion(1, AstIntegerLiteralKind::Int, &AstType::Float);
    test_promotion(1, AstIntegerLiteralKind::Int, &AstType::Double);
    test_promotion(1, AstIntegerLiteralKind::Int, &AstType::LongDouble);

    test_promotion(1, AstIntegerLiteralKind::Int, &AstType::UnsignedInt);
    test_promotion(2, AstIntegerLiteralKind::Int, &AstType::UnsignedLong);
    test_promotion(3, AstIntegerLiteralKind::Int, &AstType::UnsignedLongLong);

    test_promotion(123, AstIntegerLiteralKind::Long, &AstType::LongLong);
    test_promotion(123, AstIntegerLiteralKind::Long, &AstType::Float);
    test_promotion(123, AstIntegerLiteralKind::Long, &AstType::Double);
    test_promotion(123, AstIntegerLiteralKind::Long, &AstType::LongDouble);

    test_promotion(1, AstIntegerLiteralKind::Long, &AstType::UnsignedLong);
    test_promotion(2, AstIntegerLiteralKind::Long, &AstType::UnsignedLongLong);
}

fn test_promotion(lit_value: u64, kind: AstIntegerLiteralKind, cast_to: &AstType) {
    let (mut expr, mut metadata) = make_int_literal_cast_expression(cast_to, lit_value.to_string(), lit_value, kind);

    assert!(matches!(expr, AstExpression::Cast { .. }));

    literal_promotion::promote_integer_literals_in_expr(&mut expr, &mut metadata);

    if cast_to.is_integer() {
        if let AstExpression::IntegerLiteral { value, kind, .. } = expr {
            assert_eq!(value, lit_value);
            assert_eq!(kind.data_type(), cast_to.clone());
        } else {
            println!("{:?}", expr);
            assert!(false, "Did not promote integer literal {lit_value} {kind} to '{cast_to}'");
        }
    } else if cast_to.is_floating_point() {
        if let AstExpression::FloatLiteral { value, kind, .. } = expr {
            assert_eq!(value, lit_value as f64);
            assert_eq!(kind.data_type(), cast_to.clone());
        } else {
            println!("{:?}", expr);
            assert!(false, "Did not promote integer literal {lit_value} {kind} to '{cast_to}'");
        }
    }
}

fn make_int_literal_cast_expression(
    cast_to: &AstType,
    literal: String,
    value: u64,
    kind: AstIntegerLiteralKind,
) -> (AstExpression, AstMetadata) {
    let mut metadata = AstMetadata::new();

    let node_id = AstNodeId::new();
    metadata.set_node_type(node_id.clone(), AstType::Int);
    let int_lit_expr = AstExpression::IntegerLiteral { node_id, literal, literal_base: 10, value, kind };

    let node_id = AstNodeId::new();
    metadata.set_node_type(node_id.clone(), cast_to.clone());
    let target_type = AstDeclaredType::resolved(cast_to);
    let cast = AstExpression::Cast { node_id, target_type, expr: Box::new(int_lit_expr) };

    (cast, metadata)
}
