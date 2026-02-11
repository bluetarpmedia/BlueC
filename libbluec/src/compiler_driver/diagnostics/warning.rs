// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `warning` module defines functions to emit warning diagnostics.

use crate::ICE;
use crate::core::{SourceIdentifier, SourceLocation, SymbolKind};
use crate::lexer::TokenType;
use crate::parser::expr::ops;
use crate::parser::{AstBinaryOp, AstBinaryOpFamily, AstType};

use super::super::{Driver, WarningKind};
use super::{Diagnostic, SuggestedCode};

/// A warning diagnostic.
pub struct Warning;

impl Warning {
    /// Emits an implicit conversion warning for a constant that changes value.
    ///
    /// -Wconstant-conversion
    pub fn constant_conversion(
        old_type: &AstType,
        new_type: &AstType,
        old_value: &str,
        new_value: &str,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ConstantConversion;
        let is_float_to_int = old_type.is_floating_point() && new_type.is_integer();

        let message = if is_float_to_int {
            // Since the value has changed, we know that truncation has occured, and this is UB (C11 6.3.1.4).
            format!("Implicit conversion of an out-of-range value from '{old_type}' to '{new_type}' is undefined")
        } else {
            format!(
                "Implicit conversion from '{old_type}' to '{new_type}' changes value from {old_value} to {new_value}"
            )
        };

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits a conversion warning for a floating-point constant that is explicitly cast to an integer type, which
    /// despite the explicit cast is still UB. This is a variation of the warning above, but points out the explicit
    /// cast to the user.
    ///
    /// -Wconstant-conversion
    pub fn constant_conversion_float_int_explicit_cast(
        old_type: &AstType,
        new_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ConstantConversion;
        debug_assert!(old_type.is_floating_point() && new_type.is_integer());

        let message = format!(
            "Conversion of an out-of-range value from '{old_type}' to '{new_type}' \
            is undefined, even with an explicit cast"
        );

        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits a warning about an undefined division/remainder by zero.
    ///
    /// -Wdivision-by-zero
    pub fn division_by_zero(
        op: &AstBinaryOp,
        op_loc: SourceLocation,
        zero_expr_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::DivisionByZero;
        let message = if *op == AstBinaryOp::Divide {
            "Division by zero is undefined".to_string()
        } else if *op == AstBinaryOp::Remainder {
            "Remainder by zero is undefined".to_string()
        } else {
            ICE!("AstBinaryOp should be Divide or Remainder");
        };

        let mut diag = Diagnostic::warning_at_location(kind, message, op_loc);
        diag.add_location(zero_expr_loc);
        driver.add_diagnostic(diag);
    }

    /// Emits a warning that a shift count is negative.
    ///
    /// -Wshift-count-negative
    pub fn shift_count_negative(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ShiftCountNegative;
        let message = "Shift count is negative".to_string();
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits a warning that a shift count is zero.
    ///
    /// -Wshift-count-zero
    pub fn shift_count_zero(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::ShiftCountZero;
        let message = "Shift count is zero and has no effect".to_string();
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits a warning that a shift count is >= the valid number of bits for the type.
    ///
    /// -Wshift-count-overflow
    pub fn shift_count_overflow(
        bits_allowed: usize,
        expr_promoted_to_int: bool,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ShiftCountOverflow;
        let message = format!("Shift count >= width of the type in bits ({bits_allowed})");
        let mut diag = Diagnostic::warning_at_location(kind, message, loc);
        if expr_promoted_to_int {
            let note = "Expression was promoted to 'int' as part of C language integer promotion rules.";
            diag.add_note(note.into(), None);
        }
        driver.add_diagnostic(diag);
    }

    /// Emits a warning about an integer overflow in an expression.
    ///
    /// -Winteger-overflow
    pub fn integer_overflow(int_type: &AstType, new_value: &str, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::IntegerOverflow;
        let message = format!(
            "Integer overflow in expression of type '{int_type}'; \
            the value after overflow is {new_value}"
        );
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits a warning about a floating-point overflow in an expression.
    ///
    /// -Wfloating-point-overflow
    pub fn floating_point_overflow(fp_type: &AstType, new_value: &str, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::FloatingPointOverflow;
        let message = format!(
            "Floating-point overflow in expression of type '{fp_type}'; \
            the value after overflow is {new_value}"
        );
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, message, loc));
    }

    /// Emits an implicit conversion warning where an expression that was promoted to 'int' is now implicitly
    /// converted and loses precision.
    ///
    /// -Wimplicit-promotion-conversion
    pub fn implicit_int_promotion_conversion(
        old_type: &AstType,
        new_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ImplicitPromotionConversion;
        let message =
            format!("Implicit conversion from promoted type '{old_type}' to '{new_type}' may change its value");

        let mut diag = Diagnostic::warning_at_location(kind, message, loc);

        let note = format!(
            "Expression was promoted to 'int' as part of C language integer promotion rules. \
                Wrap the expression in a cast to '{new_type}' to silence this warning."
        );

        let column_no = driver.tu_file.get_column_no(loc);
        let indent = " ".repeat(column_no as usize - 1);
        let suggested_code = SuggestedCode::FormatString(format!("{indent}({new_type})($$1)"), vec![loc]);
        diag.add_note_with_suggested_code(note, suggested_code, None);

        driver.add_diagnostic(diag);
    }

    /// Emits an implicit conversion warning for an arithmetic type, where the conversion either changes signedness
    /// or results in precision loss.
    ///
    /// -Wimplicit-conversion
    /// -Wimplicit-int-conversion
    /// -Wimplicit-float-conversion
    /// -Wsign-conversion
    pub fn implicit_arithmetic_conversion(
        old_type: &AstType,
        new_type: &AstType,
        loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let float_to_int = old_type.is_floating_point() && new_type.is_integer();
        let int_to_float = old_type.is_integer() && new_type.is_floating_point();

        let precision_loss = new_type.bits() < old_type.bits();
        let sign_change = (old_type.is_signed_integer() && new_type.is_unsigned_integer())
            || (new_type.is_signed_integer() && old_type.is_unsigned_integer());

        let (kind, message) = if float_to_int {
            let kind = WarningKind::FloatConversion; // Float-to-int
            (kind, format!("Implicit conversion from '{old_type}' to '{new_type}' may change its value"))
        } else if int_to_float {
            let kind = WarningKind::ImplicitIntFloatConversion; // Int-to-float
            (kind, format!("Implicit conversion from '{old_type}' to '{new_type}' may change its value"))
        } else if sign_change && precision_loss {
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
                WarningKind::ImplicitFloatConversion // Float-to-float
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

    /// Emits a warning that a literal constant value is outside the type's valid range.
    ///
    /// -Wliteral-range
    pub fn literal_range(literal_type: &AstType, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::LiteralRange;
        let warning = if let Some(valid_range_str) = literal_type.valid_range_as_str() {
            format!("Constant value is too large for literal type '{literal_type}'; valid range is {valid_range_str}")
        } else {
            format!("Constant value is too large for literal type '{literal_type}'")
        };
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
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

    /// Emits a warning that the value result of an expression (with no side-effects) is unused.
    ///
    /// -Wunused-value
    pub fn unused_value(loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::UnusedValue;
        let warning = "Expression result is unused; the statement has no effect".to_string();
        driver.add_diagnostic(Diagnostic::warning_at_location(kind, warning, loc));
    }

    /// Emits a warning that the comparison result of an expression (with no side-effects) is unused.
    ///
    /// -Wunused-comparison
    pub fn unused_comparison(op: AstBinaryOp, loc: SourceLocation, driver: &mut Driver) {
        let kind = WarningKind::UnusedComparison;

        let (warning, note) = if op == AstBinaryOp::EqualTo {
            (
                "Equality comparison result of expression is unused; the statement has no effect",
                Some("Did you mean to use '=' for an assignment?"),
            )
        } else if op == AstBinaryOp::NotEqualTo {
            (
                "Inequality comparison result of expression is unused; the statement has no effect",
                Some("Did you mean to use '|=' for an Or compound assignment?"),
            )
        } else {
            ("Relational comparison result of expression is unused; the statement has no effect", None)
        };

        let mut diag = Diagnostic::warning_at_location(kind, warning.to_string(), loc);
        if let Some(note) = note {
            diag.add_note(note.to_string(), None);
        }

        driver.add_diagnostic(diag);
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
        child_op_loc: SourceLocation,
        parent_op_loc: SourceLocation,
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

        let mut diag = Diagnostic::warning_at_location(kind, warning, parent_op_loc);
        diag.add_location(child_op_loc);

        let note = format!(
            "\
            Add parentheses around the '{child_op_str}' expression to clarify the precedence \
            of operations to readers of this source code."
        );
        diag.add_note(note, Some(child_op_loc));

        if add_note_about_parent_op {
            let note = format!("Or add parentheses around the '{parent_op_str}' expression to evaluate it first.");
            diag.add_note(note, Some(parent_op_loc));
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
            let suggested_code = SuggestedCode::Code(format!("{indent}({space})"));

            diag.add_note_with_suggested_code(note.to_string(), suggested_code, Some(assignment_expr_loc));
        }

        {
            let note = "Use '==' to change the assignment into an equality comparison.";

            let column_no = driver.tu_file.get_column_no(assignment_operator_loc);
            let indent = " ".repeat(column_no as usize - 1);
            let suggested_code = SuggestedCode::Code(format!("{indent}=="));

            diag.add_note_with_suggested_code(note.to_string(), suggested_code, Some(assignment_operator_loc));
        }

        driver.add_diagnostic(diag);
    }

    /// Emits a warning that two different pointer types are being compared.
    ///
    /// -Wcompare-distinct-pointer-types
    pub fn compare_distinct_pointer_types(
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
        op_loc: SourceLocation,
        ptr_loc: SourceLocation,
        int_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::PointerIntegerCompare;
        let warning = format!("Comparison between pointer type '{ptr_type}' and integer type '{int_type}'");
        let mut diag = Diagnostic::warning_at_location(kind, warning, op_loc);
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
        ternary_op_loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::ConditionalTypeMismatch;
        let warning = format!("Type mismatch in conditional expression ('{a}' and '{b}')");
        let mut diag = Diagnostic::warning_at_location(kind, warning, ternary_op_loc);
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
        operator_loc: SourceLocation,
        a_loc: SourceLocation,
        b_loc: SourceLocation,
        driver: &mut Driver,
    ) {
        let kind = WarningKind::PointerTypeMismatch;
        let warning = format!("Pointer type mismatch ('{a}' and '{b}')");
        let mut diag = Diagnostic::warning_at_location(kind, warning, operator_loc);
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
