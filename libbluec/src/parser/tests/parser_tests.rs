// Copyright 2025-2026 Neil Henderson

use crate::compiler_driver;
use crate::parser;
use crate::parser::recursive_descent::{block, decl};
use crate::parser::{AstDeclaration, AstNodeId};

use super::utils::make_parser;

#[test]
fn make_node_id() {
    let node_id1 = AstNodeId::new();
    let node_id2 = AstNodeId::new();
    let node_id3 = AstNodeId::new();

    assert!(node_id1 != node_id2 && node_id2 != node_id3);
    assert!(node_id1 != AstNodeId::null() && node_id2 != AstNodeId::null() && node_id3 != AstNodeId::null());
}

#[test]
fn disable_diagnostics_during_parse() {
    let source = "int main(void) { \
        int x = ; \
        return 0; \
    }";

    let mut driver = compiler_driver::Driver::for_testing();

    // Diagnostics disabled
    {
        let mut parser = make_parser(&mut driver, source);

        parser.disable_diagnostics_during(&mut driver, |parser, driver| {
            _ = parser::recursive_descent::parse_translation_unit(parser, driver);
        });

        assert!(!driver.has_error_diagnostics());
        assert_eq!(driver.error_count(), 0);
    }

    // Diagnostics enabled
    {
        let mut parser = make_parser(&mut driver, source);
        _ = parser::recursive_descent::parse_translation_unit(&mut parser, &mut driver);

        assert!(driver.has_error_diagnostics());
        assert_eq!(driver.error_count(), 1);
    }
}

#[test]
fn with_new_scope() {
    // We'll parse this with pretend scopes, so the first x is at file-scope, but the other two are inside scopes.
    let source = "int x; int x; int x;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let x1 = decl::parse_declaration(&mut parser, &mut driver).expect("Should have parsed");
    let x2 = parser.with_new_scope(|p| decl::parse_declaration(p, &mut driver)).expect("Should have parsed");
    let x3 = parser.with_new_scope(|p| decl::parse_declaration(p, &mut driver)).expect("Should have parsed");

    let x1 = &x1[0];
    let x2 = &x2[0];
    let x3 = &x3[0];

    let AstDeclaration::Variable(x1) = x1 else {
        assert!(false);
        return;
    };

    let AstDeclaration::Variable(x2) = x2 else {
        assert!(false);
        return;
    };

    let AstDeclaration::Variable(x3) = x3 else {
        assert!(false);
        return;
    };

    assert_eq!(x1.ident.name, "x");
    assert_eq!(x2.ident.name, "x");
    assert_eq!(x3.ident.name, "x");

    // Variables with external linkage are not renamed
    assert!(x1.unique_name == "x");

    // But the other declarations were inside pretend scopes.
    assert!(x2.unique_name != "x" && x2.unique_name.len() > 1);
    assert!(x3.unique_name != "x" && x3.unique_name.len() > 1);
}

#[test]
fn break_with_enclosing_loop_statement() {
    let source = "break;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let node_id = AstNodeId::new();
    let stmt = parser
        .with_enclosing_statement(parser::EnclosingStatement::Loop(node_id), |p| {
            verify_current_enclosing_statement(p, Some(node_id), None);

            parser::recursive_descent::stmt::parse_statement(p, &mut driver)
        })
        .expect("Should have parsed");

    let parser::AstStatement::Break { enclosing_stmt_node_id } = stmt else {
        assert!(false, "Expected a break statement");
        return;
    };

    assert_eq!(node_id, enclosing_stmt_node_id);
}

#[test]
fn break_with_enclosing_switch_statement() {
    let source = "break;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let node_id = AstNodeId::new();
    let stmt = parser
        .with_enclosing_statement(parser::EnclosingStatement::Switch(node_id), |p| {
            verify_current_enclosing_statement(p, None, Some(node_id));

            parser::recursive_descent::stmt::parse_statement(p, &mut driver)
        })
        .expect("Should have parsed");

    let parser::AstStatement::Break { enclosing_stmt_node_id } = stmt else {
        assert!(false, "Expected a break statement");
        return;
    };

    assert_eq!(node_id, enclosing_stmt_node_id);
}

#[test]
fn break_inside_loop_inside_switch() {
    let source = "break;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    // Outer switch statement
    let switch_node_id = AstNodeId::with_id(111);
    let stmt = parser
        .with_enclosing_statement(parser::EnclosingStatement::Switch(switch_node_id), |p| {
            verify_current_enclosing_statement(p, None, Some(switch_node_id));

            // Inner loop statement
            let loop_node_id = AstNodeId::with_id(222);
            let stmt = p.with_enclosing_statement(parser::EnclosingStatement::Loop(loop_node_id), |p| {
                verify_current_enclosing_statement(p, Some(loop_node_id), Some(switch_node_id));

                parser::recursive_descent::stmt::parse_statement(p, &mut driver)
            });

            verify_current_enclosing_statement(p, None, Some(switch_node_id));

            stmt
        })
        .expect("Should have parsed");

    let parser::AstStatement::Break { enclosing_stmt_node_id } = stmt else {
        assert!(false, "Expected a break statement");
        return;
    };

    assert_eq!(AstNodeId::with_id(222), enclosing_stmt_node_id);
}

#[test]
fn break_inside_switch_inside_loop() {
    let source = "break;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    // Outer loop statement
    let loop_node_id = AstNodeId::with_id(1000);
    let stmt = parser
        .with_enclosing_statement(parser::EnclosingStatement::Loop(loop_node_id), |p| {
            verify_current_enclosing_statement(p, Some(loop_node_id), None);

            // Inner switch statement
            let switch_node_id = AstNodeId::with_id(1001);
            let stmt = p.with_enclosing_statement(parser::EnclosingStatement::Switch(switch_node_id), |p| {
                verify_current_enclosing_statement(p, Some(loop_node_id), Some(switch_node_id));

                parser::recursive_descent::stmt::parse_statement(p, &mut driver)
            });

            verify_current_enclosing_statement(p, Some(loop_node_id), None);

            stmt
        })
        .expect("Should have parsed");

    let parser::AstStatement::Break { enclosing_stmt_node_id } = stmt else {
        assert!(false, "Expected a break statement");
        return;
    };

    assert_eq!(AstNodeId::with_id(1001), enclosing_stmt_node_id);
}

#[test]
fn restore_token_stream_after_parsing() {
    let source = "return 10;";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let validate_return_stmt = |stmt| -> bool {
        let Ok(parser::AstStatement::Return(expr)) = stmt else {
            return false;
        };
        let parser::AstExpressionKind::IntegerLiteral { value, .. } = expr.kind() else {
            return false;
        };
        *value == 10
    };

    // Parse and then rollback
    parser.restore_token_stream_after(|p| {
        let stmt = parser::recursive_descent::stmt::parse_statement(p, &mut driver);
        assert!(validate_return_stmt(stmt));
    });

    // Should parse the exact same statement
    let stmt = parser::recursive_descent::stmt::parse_statement(&mut parser, &mut driver);
    assert!(validate_return_stmt(stmt));
}

#[test]
fn has_errors_emitted() {
    let source = "{ \
        typedeff int foo; \
        if (a > b) \
            return 0; \
    }";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    _ = block::parse_block(&mut parser, &mut driver);

    assert_eq!(driver.error_count(), 2);
}

#[test]
fn no_errors_emitted() {
    let source = "{ \
        typedef int foo; \
        typedef foo bar; \
        typedef bar baz; \
        baz a = 0; \
        baz b = 1; \
        if (a > b) \
            return 0; \
    }";

    let mut driver = compiler_driver::Driver::for_testing();
    let mut parser = make_parser(&mut driver, source);

    let block = block::parse_block(&mut parser, &mut driver);

    assert_eq!(driver.error_count(), 0);
    assert!(block.is_ok());

    let block = block.unwrap();
    let block_items = block.0;
    assert_eq!(block_items.len(), 6);
}

fn verify_current_enclosing_statement(
    parser: &mut parser::Parser,
    expected_loop_id: Option<AstNodeId>,
    expected_switch_id: Option<AstNodeId>,
) {
    match parser.current_enclosing_statement() {
        Some(parser::EnclosingStatementChain::Loop { loop_node_id, parent_switch_id }) => {
            assert_eq!(loop_node_id, expected_loop_id.expect("Expected loop id"));
            assert_eq!(parent_switch_id, expected_switch_id);
        }
        Some(parser::EnclosingStatementChain::Switch { switch_node_id, parent_loop_id }) => {
            assert_eq!(switch_node_id, expected_switch_id.expect("Expected switch id"));
            assert_eq!(parent_loop_id, expected_loop_id);
        }
        _ => {
            assert!(false, "Invalid current_enclosing_statement")
        }
    }
}
