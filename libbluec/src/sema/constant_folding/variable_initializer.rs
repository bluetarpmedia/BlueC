// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `variable_initializer` module provides functionality to fold a character array variable initializer into an
//! ASCII string.

use crate::ICE;
use crate::core::string;
use crate::parser::{AstExpression, AstFullExpression, AstType, AstUnaryOp, AstVariableInitializer};

use super::TypeChecker;

/// Folds a variable initializer for a character array into a String Literal.
///
/// ```c
/// char text[4]   = {'a', 'b', 'b' + 1, 0};                          -->  char text[4]   = "abc\0";
/// char two[2][4] = { {'t', 'e', 's', 't'}, {'f', 'o', 'u', 'r'} };  -->  char two[2][4] = { "test", "four" };
/// ```
pub fn fold_character_array_variable_initializer(
    var_init: &[AstVariableInitializer],
    data_type: AstType,
    chk: &mut TypeChecker,
) -> AstVariableInitializer {
    let ascii = var_init
        .iter()
        .map(|init| {
            let AstVariableInitializer::Scalar(full_expr) = init else {
                ICE!("Unexpected aggregate initializer for character array initializer")
            };

            string::to_ascii(get_char_value(&full_expr.expr) as u32)
        })
        .collect();

    let string_literal = make_string_literal(ascii, data_type.clone(), chk);

    let full_expr = AstFullExpression::new(string_literal);
    chk.set_data_type(full_expr.node_id, &data_type);
    chk.metadata.propagate_const_flag_from_child(full_expr.expr.node_id(), full_expr.node_id);

    AstVariableInitializer::Scalar(full_expr)
}

fn get_char_value(expr: &AstExpression) -> u8 {
    match expr {
        AstExpression::CharLiteral { value, .. } => *value as u8,

        AstExpression::IntegerLiteral { value, .. } => *value as u8,

        AstExpression::Cast { target_type, expr, .. } => {
            debug_assert!(target_type.resolved_type.as_ref().unwrap().is_character());
            get_char_value(expr)
        }

        AstExpression::UnaryOperation { op, expr, .. } => match op {
            AstUnaryOp::Negate => {
                let value = get_char_value(expr);
                let negated = -(value as i32);
                negated as u8
            }
            AstUnaryOp::Plus => get_char_value(expr),
            AstUnaryOp::BitwiseNot => {
                let value = get_char_value(expr);
                !value
            }
            AstUnaryOp::LogicalNot => {
                let value = get_char_value(expr);
                let boolean = value != 0;
                !boolean as u8
            }
            _ => ICE!("Invalid unary operator '{op}'"),
        },

        _ => ICE!("Unexpected expression in character array initializer"),
    }
}

fn make_string_literal(ascii: Vec<String>, data_type: AstType, chk: &mut TypeChecker) -> AstExpression {
    let node_id = super::make_constant_expr_node_id(data_type, chk);
    let literals = vec![format!("\"{}\"", ascii.join(""))];
    AstExpression::StringLiteral { node_id, literals, ascii }
}
