// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `warning` module defines functions to emit warning diagnostics.

use crate::ICE;
use crate::core::{SourceIdentifier, SourceLocation, SymbolKind};
use crate::lexer::TokenType;
use crate::parser::expr::ops;
use crate::parser::{AstBinaryOp, AstBinaryOpFamily, AstType};

use super::super::{Driver, WarningKind};
use super::Diagnostic;

/// A warning diagnostic.
pub struct Warning;

impl Warning {
    /// Emits an implicit conversion warning for a numeric literal, optionally with a change in signedness.
    ///
    /// -Wconstant-conversion
    pub fn constant_conversion(
        old_type: &AstType,
        new_type: &AstType,
        old_value: &str,
        new_value: &str,
        sign_change: bool,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ConstantConversion;

        let message = if sign_change {
            format!(
                "Implicit conversion from '{old_type}' to '{new_type}' changes signedness \
                    and value from {old_value} to {new_value}"
            )
        } else {
            format!(
                "Implicit conversion from '{old_type}' to '{new_type}' changes value from \
                    {old_value} to {new_value}"
            )
        };

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits an implicit conversion warning for an arithmetic type, where the conversion either changes signedness
    /// or results in precision loss.
    ///
    /// -Wimplicit-int-conversion
    /// -Wimplicit-float-conversion
    /// -Wsign-conversion
    pub fn implicit_arithmetic_conversion(
        old_type: &AstType,
        new_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let precision_loss = new_type.bits() < old_type.bits();
        let sign_change = (old_type.is_signed_integer() && new_type.is_unsigned_integer())
            || (new_type.is_signed_integer() && old_type.is_unsigned_integer());

        let (kind, message) = if sign_change && precision_loss {
            let kind = WarningKind::SignConversion;
            (
                kind,
                format!("Implicit conversion from '{old_type}' to '{new_type}' loses precision and changes signedness"),
            )
        } else if sign_change {
            let kind = WarningKind::SignConversion;
            (kind, format!("Implicit conversion from '{old_type}' to '{new_type}' changes signedness"))
        } else if precision_loss {
            let kind = if old_type.is_integer() {
                WarningKind::ImplicitIntConversion
            } else if old_type.is_floating_point() {
                WarningKind::ImplicitFloatConversion
            } else {
                WarningKind::ImplicitConversion
            };

            (kind, format!("Implicit conversion from '{old_type}' to '{new_type}' loses precision"))
        } else {
            ICE!("Did not handle narrowing warning for '{old_type}' to '{new_type}'");
        };

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits an implicit conversion warning for a switch statement case value.
    ///
    /// -Wimplicit-conversion
    pub fn implicit_switch_case_conversion(
        case_data_type: &AstType,
        switch_data_type: &AstType,
        old_value: &str,
        new_value: &str,
        sign_change: bool,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = if sign_change { WarningKind::SignConversion } else { WarningKind::ConstantConversion };

        let warning = format!(
            "Implicit conversion from case value type '{case_data_type}' to switch \
            condition type '{switch_data_type}' changes value from {old_value} to {new_value}"
        );

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning about an 'extern' variable with an initializer.
    pub fn extern_initializer(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ExternInitializer;

        let warning = String::from(
            "'extern' variable has an initializer, which makes it a variable definition. \
            Remove 'extern' since it has no effect.",
        );

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning for a duplicate declaration specifier (e.g. duplicate storage class or 'signed'/'unsigned').
    ///
    /// -Wduplicate-decl-specifier
    pub fn duplicate_decl_specifier(specifier: SourceIdentifier, driver: &mut Driver) {
        let kind = WarningKind::DuplicateDeclSpecifier;
        let warning = format!("Duplicate '{}' specifier has no effect", specifier.0);
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, specifier.1));
    }

    /// Emits a warning that a declaration is missing its identifier.
    ///
    /// -Wmissing-declarations
    pub fn missing_declaration_identifier(is_typedef: bool, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::MissingDeclarations;

        let warning = if is_typedef {
            "Type alias declaration is missing the alias name"
        } else {
            "Declaration does not declare anything"
        }
        .to_string();

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning that a variable is uninitialized.
    ///
    /// -Wuninitialized
    pub fn uninitialized_variable(variable: SourceIdentifier, driver: &mut Driver) {
        let kind = WarningKind::Uninitialized;
        let warning = format!("Variable '{}' is uninitialized", variable.0);
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, variable.1));
    }

    /// Emits a warning that an integer literal is too large to be interpreted as signed and therefore is implicitly
    /// converted to unsigned.
    ///
    /// -Wimplicitly-unsigned-literal
    pub fn integer_literal_implicitly_unsigned(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ImplicitlyUnsignedLiteral;
        let warning = "Integer literal is interpreted as unsigned because it is too large \
                       for a signed integer type";
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning.to_string(), loc));
    }

    /// Emits a warning that a symbol is unused.
    ///
    /// -Wunused-variable
    /// -Wunused-function
    /// -Wunused-local-typedef
    pub fn unused_symbol(symbol: SourceIdentifier, kind: SymbolKind, driver: &mut Driver) {
        let (kind, warning) = if kind == SymbolKind::TypeAlias {
            (WarningKind::UnusedLocalTypedef, format!("Locally defined type alias '{}' is unused", symbol.0))
        } else if kind == SymbolKind::Function {
            (WarningKind::UnusedFunction, format!("Function '{}' is unused", symbol.0))
        } else {
            (WarningKind::UnusedVariable, format!("Variable '{}' is unused", symbol.0))
        };

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, symbol.1));
    }

    /// Emits a warning that an expression has mixed operators and is missing parentheses.
    ///
    /// -Wlogical-op-parentheses
    /// -Wbitwise-op-parentheses
    /// -Wparentheses
    pub fn mixed_operators_missing_parens(
        child_expr_op: AstBinaryOp,
        parent_expr_op: AstBinaryOp,
        child_expr_loc: SourceLocation,
        parent_expr_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let same_family = child_expr_op.family() == parent_expr_op.family();

        // We want to display "&&" instead of "LogicalAnd".
        let child_op_str = TokenType::from(child_expr_op).to_string();
        let parent_op_str = TokenType::from(parent_expr_op).to_string();

        let add_note_about_parent_op;

        let warning = if same_family {
            add_note_about_parent_op = false;
            format!("'{child_op_str}' used within '{parent_op_str}' without parentheses")
        } else {
            let child_prec = ops::operator_precedence(&child_expr_op.into());
            let parent_prec = ops::operator_precedence(&parent_expr_op.into());

            add_note_about_parent_op = true;

            if child_prec > parent_prec {
                format!(
                    "\
                    '{parent_op_str}' has a lower precedence than '{child_op_str}' which means the \
                    '{child_op_str}' expression is evaluated first."
                )
            } else {
                ICE!("mixed_operators_missing_parens did not handle {child_op_str} <= {parent_op_str}");
            }
        };

        let kind = if same_family && parent_expr_op.family() == AstBinaryOpFamily::Logical {
            WarningKind::LogicalOpParentheses
        } else if same_family && parent_expr_op.family() == AstBinaryOpFamily::Bitwise {
            WarningKind::BitwiseOpParentheses
        } else {
            WarningKind::Parentheses
        };

        let mut diag = Diagnostic::warning_at_location(kind, warning, parent_expr_loc);
        diag.add_location(child_expr_loc);

        let note = format!(
            "\
            Add parentheses around the '{child_op_str}' expression to clarify the precedence \
            of operations to readers of this source code."
        );
        diag.add_note(note, Some(child_expr_loc));

        if add_note_about_parent_op {
            let note = format!("Or add parentheses around the '{parent_op_str}' expression to evaluate it first.");
            diag.add_note(note, Some(parent_expr_loc));
        }

        driver.add_diagnostic(diag);
    }

    /// Emits a warning that the result of an assignment is used as a condition without parentheses.
    ///
    /// -Wparentheses
    pub fn assignment_in_condition_missing_parens(
        assignment_expr_loc: SourceLocation,
        assignment_operator_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::Parentheses;
        let warning = "The result of an assignment is used as a condition; add parentheses to clarify intent";
        let mut diag = Diagnostic::warning_at_location(kind, warning.to_string(), assignment_operator_loc);

        {
            let note = "Add parentheses around the assignment expression to silence this warning.";

            let column_no = driver.tu_file.get_column_no(assignment_expr_loc);

            let indent = " ".repeat(column_no as usize - 1);

            debug_assert!(assignment_expr_loc.length >= 3); // An assignment expression must be at least 3 chars
            let space = " ".repeat(assignment_expr_loc.length as usize - 2);
            let suggested = format!("{indent}({space})");

            diag.add_note_with_suggested_code(note.to_string(), suggested, Some(assignment_expr_loc));
        }

        {
            let note = "Use '==' to change the assignment into an equality comparison.";

            let column_no = driver.tu_file.get_column_no(assignment_operator_loc);
            let indent = " ".repeat(column_no as usize - 1);
            let suggested = format!("{indent}==");

            diag.add_note_with_suggested_code(note.to_string(), suggested, Some(assignment_operator_loc));
        }

        driver.add_diagnostic(diag);
    }

    /// Emits a warning that two different pointer types are being compared.
    ///
    /// -Wcompare-distinct-pointer-types
    pub fn compare_different_pointer_types(
        a: &AstType,
        b: &AstType,
        cmp_loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::CompareDistinctPointerTypes;
        let warning = format!("Comparison of different pointer types ('{a}' and '{b}')");
        let mut diag = Diagnostic::warning_at_location(kind, warning, cmp_loc);
        diag.add_location(a_loc);
        diag.add_location(b_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that a pointer and an integer are being compared.
    ///
    /// -Wpointer-integer-compare
    pub fn compare_pointer_and_integer(
        ptr_type: &AstType,
        int_type: &AstType,
        cmp_loc: SourceLocation,
        ptr_loc: SourceLocation,
        int_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::PointerIntegerCompare;
        let warning = format!("Comparison between pointer type '{ptr_type}' and integer type '{int_type}'");
        let mut diag = Diagnostic::warning_at_location(kind, warning, cmp_loc);
        diag.add_location(ptr_loc);
        diag.add_location(int_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that a pointer is casted to an integer type smaller than pointer type.
    ///
    /// -Wpointer-to-int-cast
    pub fn pointer_to_smaller_int_cast(
        ptr_type: &AstType,
        int_type: &AstType,
        cmp_loc: SourceLocation,
        int_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::PointerToIntCast;
        let warning = format!("Cast to smaller integer type '{int_type}' from pointer '{ptr_type}'");
        let mut diag = Diagnostic::warning_at_location(kind, warning, cmp_loc);
        diag.add_location(int_loc);

        let ptr_bits = ptr_type.bits();
        diag.add_note(format!("Change the cast to a {ptr_bits}-bit integer type"), None);

        driver.add_diagnostic(diag);
    }

    /// Emits a warning that an expression evaluated to zero and is interpreted as a null pointer constant.
    ///
    /// -Wnon-literal-null-conversion
    pub fn expression_interpreted_as_null_ptr_constant(loc: SourceLocation, ptr_type: &AstType, driver: &mut Driver) {
        let kind = WarningKind::NonLiteralNullConversion;
        let warning = format!(
            "Expression evaluates to zero and is interpreted as a null pointer constant (of type '{ptr_type}')"
        );
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning that a ternary condition's consequent and alternative types do not match.
    ///
    /// -Wconditional-type-mismatch
    pub fn conditional_type_mismatch(
        a: &AstType,
        b: &AstType,
        ternary_loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ConditionalTypeMismatch;
        let warning = format!("Type mismatch in conditional expression ('{a}' and '{b}')");
        let mut diag = Diagnostic::warning_at_location(kind, warning, ternary_loc);
        diag.add_location(a_loc);
        diag.add_location(b_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that two pointers in an expression have different types.
    ///
    /// -Wpointer-type-mismatch
    pub fn pointer_type_mismatch(
        a: &AstType,
        b: &AstType,
        expr_loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::PointerTypeMismatch;
        let warning = format!("Pointer type mismatch ('{a}' and '{b}')");
        let mut diag = Diagnostic::warning_at_location(kind, warning, expr_loc);
        diag.add_location(a_loc);
        diag.add_location(b_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that there are too many elements in the initializer list for the given `variable_type`.
    ///
    /// -Wexcess-initializers
    pub fn too_many_elements_for_initializer(variable_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let type_description = if variable_type.is_scalar() {
            "scalar type"
        } else if variable_type.is_array() {
            "array type"
        } else {
            "aggregate type"
        };

        let kind = WarningKind::ExcessInitializers;
        let warning = format!("Too many elements in initializer list for {type_description} '{variable_type}'");
        let mut diag = Diagnostic::warning_at_location(kind, warning, loc);

        let note = if variable_type.is_scalar() {
            "The variable is initialized with the first element and the remainder is ignored."
        } else if variable_type.is_array() {
            "The array is initialized with the required number of elements and the remainder is ignored."
        } else {
            "The variable is initialized with the required number of elements and the remainder is ignored."
        };

        diag.add_note(note.into(), None);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that an initializer string is too long for a char array.
    ///
    /// -Wexcess-initializers
    pub fn initializer_string_too_long_for_array(array_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ExcessInitializers;
        let warning = format!("Initializer string is too long for array of type '{array_type}'");
        let mut diag = Diagnostic::warning_at_location(kind, warning, loc);

        if let AstType::Array { count, .. } = array_type {
            let note = format!("The array is initialized with the first {count} characters from the string.");
            diag.add_note(note, None);
        }

        driver.add_diagnostic(diag);
    }

    /// Emits a warning that a subobject initializer is missing braces.
    ///
    /// -Wmissing-braces
    pub fn missing_braces_around_sub_object(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::MissingBraces;
        let warning = "Initialization of sub-object is missing braces".to_string();
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning that there are too many braces around an initializer list for a scalar type.
    ///
    /// -Wmany-braces-around-scalar-init
    pub fn too_many_braces_for_scalar_initializer(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ManyBracesAroundScalarInit;
        let warning = "Too many braces around scalar initializer".to_string();

        let braces_loc = SourceLocation { file_pos: loc.file_pos + 1, length: loc.length - 1 };
        let open_brace_loc = SourceLocation { length: 1, ..loc };

        let mut diag = Diagnostic::warning_at_location(kind, warning, open_brace_loc);
        diag.add_location(braces_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that an array subscript index is out of bounds.
    ///
    /// -Warray-bounds
    pub fn array_index_out_of_bounds(index: i32, array_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ArrayBounds;
        let warning = format!("Array index {index} is out of bounds for the array of type '{array_type}'");
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }
}
