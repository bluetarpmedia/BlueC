// Copyright 2025-2026 Neil Henderson

use crate::parser::AstNodeId;

use super::super::label_maker::*;

#[test]
fn user_labels() {
    let mut maker = LabelMaker::new();
    maker.reset_for_new_function("main");

    // User labels are only unique by their given name within a function
    let lb1 = maker.make_user_label("exit");
    let lb2 = maker.make_user_label("exit");
    let lb3 = maker.make_user_label("exit");
    let lb4 = maker.make_user_label("exit2");

    assert_eq!(lb1, lb2);
    assert_eq!(lb1, lb3);
    assert_ne!(lb1, lb4);

    maker.reset_for_new_function("calculate");

    let lb5 = maker.make_user_label("exit");
    assert_ne!(lb1, lb5);
}

#[test]
fn switch_labels() {
    let mut maker = LabelMaker::new();
    maker.reset_for_new_function("main");

    let switch_node_id = AstNodeId::with_id(123);
    let switch = maker.make_switch_label("my_switch", switch_node_id);
    let case1 = maker.make_switch_case_label(switch_node_id, AstNodeId::with_id(1));
    let case2 = maker.make_switch_case_label(switch_node_id, AstNodeId::with_id(2));
    let case3 = maker.make_switch_case_label(switch_node_id, AstNodeId::with_id(3));

    assert_ne!(switch, case1);
    assert_ne!(switch, case2);
    assert_ne!(switch, case3);
    assert_ne!(case1, case2);
    assert_ne!(case1, case3);
}

#[test]
fn control_stmt_labels() {
    let mut maker = LabelMaker::new();
    maker.reset_for_new_function("main");

    let if_lbl = maker.make_control_label("if", AstNodeId::with_id(111));
    let for_lbl = maker.make_control_label("for", AstNodeId::with_id(222));
    let while_lbl = maker.make_control_label("while", AstNodeId::with_id(333));
    let do_lbl = maker.make_control_label("do", AstNodeId::with_id(444));

    assert_ne!(if_lbl, for_lbl);
    assert_ne!(for_lbl, while_lbl);
    assert_ne!(while_lbl, do_lbl);
}

#[test]
fn unique_labels() {
    let mut maker = LabelMaker::new();

    let lb0 = maker.make_unique_label("condition");

    maker.reset_for_new_function("test1");

    let lb1 = maker.make_unique_label("condition");
    let lb2 = maker.make_unique_label("condition");
    let lb3 = maker.make_unique_label("condition");

    assert_ne!(lb0, lb1);
    assert_ne!(lb1, lb2);
    assert_ne!(lb1, lb3);

    maker.reset_for_new_function("test2");

    let lb4 = maker.make_unique_label("condition");
    let lb5 = maker.make_unique_label("condition");

    assert_ne!(lb1, lb4);
    assert_ne!(lb4, lb5);
}
