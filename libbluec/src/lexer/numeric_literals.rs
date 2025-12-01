// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `numeric_literals` module provides lexing functionality for integer and floating-point literals.

use crate::compiler_driver;
use crate::compiler_driver::diagnostics::Diagnostic;
use crate::compiler_driver::errors::Error;
use crate::internal_error;
use crate::lexer::line_lexer::LineLexer;
use crate::lexer::tokens::FloatLiteralSuffix;
use crate::lexer::{IntegerLiteralSuffix, NumericLiteralBase, SourceLocation, Token, TokenType};

/// Makes a token for an integer or floating-point literal.
pub fn make_numeric_literal(line_lexer: &mut LineLexer) -> Result<Token, ()> {
    // Peek at the first 1 or 2 chars to see if there's a base prefix, e.g. "0x".
    //
    let base = match line_lexer.peek_2_chars() {
        (Some('0'), Some('x')) | (Some('0'), Some('X')) => NumericLiteralBase::Hex,

        (Some('0'), Some('b')) | (Some('0'), Some('B')) => NumericLiteralBase::Binary,

        // Octal integer or decimal float (`0001.0` is a valid decimal float).
        //      We'll pass the base as Octal but `make_numeric_literal_for_base` can override it if necessary.
        (Some('0'), Some(second)) if second.is_ascii_digit() => NumericLiteralBase::Octal,

        _ => NumericLiteralBase::Decimal,
    };

    make_numeric_literal_for_base(line_lexer, base)
}

/// Makes an integer or floating-point literal token for the given base.
fn make_numeric_literal_for_base(line_lexer: &mut LineLexer, base: NumericLiteralBase) -> Result<Token, ()> {
    let line_no = line_lexer.line_no();

    // Read the prefix chars for the base (e.g. "0x");
    //
    let (prefix_count, prefix_str, prefix_start_col) = lex_numeric_literal_prefix(line_lexer, base);

    let mut start_col = None;
    let mut literal = String::new();

    if let Some(prefix_str) = prefix_str {
        literal.push_str(prefix_str);
        start_col = prefix_start_col;
    }

    // Consume the possibly valid characters for int and float literals; we'll validate after.
    //      But we stop after detecting a floating-point exponent, since we want to validate that separately.
    //
    let mut found_period = false;
    let mut is_float_scientific_not = false;
    let mut exponent_col_no = None;

    while let Some((idx, ch)) = line_lexer.cursor().next_if(|(_, ch)| ch.is_alphanumeric() || *ch == '.') {
        literal.push(ch);
        line_lexer.set_column_no(idx + 1);

        if start_col.is_none() {
            start_col = Some(line_lexer.column_no());
        }

        let is_decimal_exponent =
            (ch == 'e' || ch == 'E') && (base == NumericLiteralBase::Decimal || base == NumericLiteralBase::Octal);
        let is_hex_exponent = (ch == 'p' || ch == 'P') && base == NumericLiteralBase::Hex;

        if ch == '.' {
            found_period = true;
        }
        // Stop after we detect a floating-point exponent; we'll lex it separately. We need to include Octal here
        // in case the float literal starts with a zero.
        //
        else if is_decimal_exponent || is_hex_exponent {
            is_float_scientific_not = true;
            exponent_col_no = Some(line_lexer.column_no());
            break;
        }
    }

    // If we found a floating-point literal and our given base is Octal then override it to Decimal.
    //      E.g. `00000.0` and `01e1` are valid decimal floating-point literals.
    //
    let mut base = base;
    if base == NumericLiteralBase::Octal && (found_period || is_float_scientific_not) {
        base = NumericLiteralBase::Decimal;
    }

    let is_float_literal = is_float_scientific_not || (found_period && base == NumericLiteralBase::Decimal);

    // Read the floating-point exponent.
    //
    let mut significand = None; // Aka mantissa
    let mut exponent = None;

    if is_float_scientific_not {
        significand = Some(literal[..literal.len() - 1].to_string()); // Everything except the exponent char (1 byte).

        let mut exponent_literal = String::new();

        // Take the optional +/- sign char.
        if let Some((idx, ch)) = line_lexer.cursor().next_if(|(_, ch)| *ch == '+' || *ch == '-') {
            exponent_literal.push(ch);
            line_lexer.set_column_no(idx + 1);
        }

        // Take the remaining chars
        while let Some((idx, ch)) = line_lexer.cursor().next_if(|(_, ch)| ch.is_alphanumeric() || *ch == '.') {
            exponent_literal.push(ch);
            line_lexer.set_column_no(idx + 1);
        }

        literal.push_str(&exponent_literal);
        exponent = Some(exponent_literal);
    }

    // Consume any remaining alphanumeric or identifier characters which are attached to the literal,
    // so that we can correctly emit a diagnostic.
    while let Some((idx, ch)) = line_lexer.cursor().next_if(|(_, ch)| ch.is_alphanumeric() || *ch == '_' || *ch == '.')
    {
        literal.push(ch);
        line_lexer.set_column_no(idx + 1);
    }

    // Extract all the suffix chars, if present (even if there's more than the max 3 allowed.)
    //
    let token_len = literal.len();
    let (literal, literal_suffix) =
        if is_float_literal { extract_valid_float_literal_suffix_chars(literal) } else { extract_valid_integer_literal_suffix_chars(literal) };

    // Validate the literal
    //
    let is_valid_literal = if is_float_literal {
        if is_float_scientific_not {
            let mut exponent = exponent.unwrap();
            exponent = exponent.strip_suffix(&literal_suffix).unwrap().to_string();

            let components = ScientificNotation {
                significand: significand.unwrap(),
                significand_start_column: start_col.unwrap(),
                exponent,
                exponent_start_column: exponent_col_no.unwrap(),
            };

            is_float_scientific_notation_valid(line_lexer.driver(), line_no, base, prefix_count, components, &literal)?
        } else {
            debug_assert!(base == NumericLiteralBase::Decimal);
            let is_valid_char = |ch: char| is_valid_char_for_base(ch, base) || ch == '.';
            literal.len() > prefix_count && literal.chars().skip(prefix_count).all(is_valid_char)
        }
    } else {
        let is_valid_char = |ch: char| is_valid_char_for_base(ch, base);
        literal.len() > prefix_count && literal.chars().skip(prefix_count).all(is_valid_char)
    };

    if !is_valid_literal {
        let loc = SourceLocation::new(line_no, start_col.unwrap(), token_len);
        Error::invalid_numeric_literal(&literal, is_float_literal, base, loc, line_lexer.driver());
        return Err(());
    }

    // Validate the suffix.
    //      Max length of an integer literal suffix is 3 for 'ULL' or variations of that,
    //      and 1 for a floating point literal ('F' or 'L').
    //
    let suffix_max_len = if is_float_literal { 1 } else { 3 };
    if literal_suffix.len() > suffix_max_len {
        let literal_suffix_start_loc = start_col.unwrap() + literal.len();
        let loc = SourceLocation::new(line_no, literal_suffix_start_loc, literal_suffix.len());
        Error::invalid_numeric_literal_suffix(&literal_suffix, is_float_literal, loc, line_lexer.driver());
        return Err(());
    }

    let literal_suffix_start_column = start_col.unwrap() + literal.len();
    let driver = line_lexer.driver();

    let token_type = if is_float_literal {
        debug_assert!(base == NumericLiteralBase::Decimal || base == NumericLiteralBase::Hex);
        let suffix = make_float_literal_suffix(driver, line_no, literal_suffix_start_column, &literal_suffix)?;
        TokenType::FloatLiteral { literal, base, suffix }
    } else {
        let suffix = make_integer_literal_suffix(driver, line_no, literal_suffix_start_column, &literal_suffix)?;
        TokenType::IntegerLiteral { literal, base, suffix }
    };

    Ok(line_lexer.make_token_at_column(token_type, start_col.unwrap(), token_len))
}

fn lex_numeric_literal_prefix(
    line_lexer: &mut LineLexer,
    base: NumericLiteralBase,
) -> (usize, Option<&'static str>, Option<usize>) {
    match base {
        NumericLiteralBase::Hex => {
            let (idx, _) = line_lexer.cursor().next().unwrap();
            line_lexer.set_column_no(idx + 1);
            let start_col = Some(line_lexer.column_no());

            let (idx, _) = line_lexer.cursor().next().unwrap();
            line_lexer.set_column_no(idx + 1);

            (2, Some("0x"), start_col)
        }

        NumericLiteralBase::Binary => {
            let (idx, _) = line_lexer.cursor().next().unwrap();
            line_lexer.set_column_no(idx + 1);
            let start_col = Some(line_lexer.column_no());

            let (idx, _) = line_lexer.cursor().next().unwrap();
            line_lexer.set_column_no(idx + 1);

            (2, Some("0b"), start_col)
        }

        NumericLiteralBase::Octal => {
            let (idx, _) = line_lexer.cursor().next().unwrap();
            line_lexer.set_column_no(idx + 1);

            (1, Some("0"), Some(line_lexer.column_no()))
        }

        NumericLiteralBase::Decimal => (0, None, None),
    }
}

struct ScientificNotation {
    significand: String,
    significand_start_column: usize,
    exponent: String,
    exponent_start_column: usize,
}

fn is_float_scientific_notation_valid(
    driver: &mut compiler_driver::Driver,
    line_no: usize,
    base: NumericLiteralBase,
    prefix_count: usize,
    components: ScientificNotation,
    literal: &str,
) -> Result<bool, ()> {
    let significand = components.significand;
    let significand_start_col = components.significand_start_column;
    let significand_loc = SourceLocation::new(line_no, significand_start_col, significand.len());
    validate_float_significand(driver, base, prefix_count, &significand, significand_loc)?;

    let exponent = components.exponent;
    let exponent_loc = SourceLocation::new(line_no, components.exponent_start_column, 1);
    validate_float_exponent(driver, &exponent, exponent_loc)?;

    let lower_exp = if base == NumericLiteralBase::Decimal { 'e' } else { 'p' };
    let upper_exp = if base == NumericLiteralBase::Decimal { 'E' } else { 'P' };

    let is_valid_char = |ch: char| {
        is_valid_char_for_base(ch, base) || ch == '.' || ch == lower_exp || ch == upper_exp || ch == '-' || ch == '+'
    };

    Ok(literal.len() > prefix_count && literal.chars().skip(prefix_count).all(is_valid_char))
}

fn validate_float_significand(
    driver: &mut compiler_driver::Driver,
    base: NumericLiteralBase,
    prefix_count: usize,
    significand: &str,
    loc: SourceLocation,
) -> Result<(), ()> {
    debug_assert!(base == NumericLiteralBase::Decimal || base == NumericLiteralBase::Hex);

    let is_valid_char = |ch: char| is_valid_char_for_base(ch, base) || ch == '.';

    let valid = significand.len() > prefix_count && significand.chars().skip(prefix_count).all(is_valid_char);
    if !valid {
        let err = format!("Invalid significand (mantissa) '{}' in floating-point literal", significand);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    Ok(())
}

fn validate_float_exponent(
    driver: &mut compiler_driver::Driver,
    exponent: &str,
    exponent_loc: SourceLocation,
) -> Result<(), ()> {
    let mut emit_no_digits_error = || {
        let err = "Exponent has no digits".to_string();
        driver.add_diagnostic(Diagnostic::error_at_location(err, exponent_loc));
    };

    if exponent.is_empty() {
        emit_no_digits_error();
        return Err(());
    }

    // Skip over the +/- sign character if it exists.
    let first = exponent.chars().next().unwrap();
    let skip_count = if first == '+' || first == '-' { 1 } else { 0 };

    // Ensure we have at least 1 digit after the +/- sign.
    if skip_count == 1 && exponent.len() == 1 {
        emit_no_digits_error();
        return Err(());
    }

    // Ensure all the remaining chars are digits
    if let Some((idx, _)) = exponent.char_indices().skip(skip_count).find(|(_, ch)| !ch.is_ascii_digit()) {
        let invalid_suffix = exponent[idx..].to_string();
        let err = format!("Invalid suffix '{invalid_suffix}' on floating-point literal");
        let loc =
            SourceLocation { column: exponent_loc.column + idx + 1, length: invalid_suffix.len(), ..exponent_loc };
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    Ok(())
}

pub(super) fn make_integer_literal_suffix(
    driver: &mut compiler_driver::Driver,
    line_no: usize,
    literal_suffix_start_column: usize,
    literal_suffix: &str,
) -> Result<Option<IntegerLiteralSuffix>, ()> {
    // Count the 'U' and 'L' chars. We need to count upper and lowercase 'L' to detect errors.
    let (u_count, l_upper, l_lower) = literal_suffix.chars().fold((0, 0, 0), |(u, lu, ll), ch| match ch {
        'U' => (u + 1, lu, ll),
        'u' => (u + 1, lu, ll),
        'L' => (u, lu + 1, ll),
        'l' => (u, lu, ll + 1),
        _ => (u, ll, lu),
    });

    // There can only be one 'U' / 'u' char, and it cannot split two 'L's. E.g. 'lul' is invalid.
    // For 'LL' suffixes, both chars must have the same case. 'LL' or 'll'.
    // If 'L' or 'LL' is combined with 'U' then the 'U' can be upper or lower. E.g. 'uL', 'uLL', 'Ull' are fine.
    //
    if literal_suffix.len() > 1 {
        let invalid_u_position = literal_suffix.len() == 3
            && (literal_suffix.chars().nth(1) == Some('U') || literal_suffix.chars().nth(1) == Some('u'));

        if invalid_u_position || u_count > 1 || (l_lower > 0 && l_upper > 0) || (l_lower + l_upper > 2) {
            let loc = SourceLocation::new(line_no, literal_suffix_start_column, literal_suffix.len());
            driver.add_diagnostic(Diagnostic::error_at_location(
                format!("Invalid suffix '{}' on integer constant.", &literal_suffix),
                loc,
            ));
            return Err(());
        }
    }

    // Determine the correct suffix
    debug_assert!(l_lower == 0 || l_upper == 0);
    let l_count = l_lower + l_upper; // At least one of these must be zero, so we end up with either 1 or 2 'L's.

    let suffix = match (u_count, l_count) {
        (1, 2) => Some(IntegerLiteralSuffix::ULL),
        (1, 1) => Some(IntegerLiteralSuffix::UL),
        (1, 0) => Some(IntegerLiteralSuffix::U),
        (0, 2) => Some(IntegerLiteralSuffix::LL),
        (0, 1) => Some(IntegerLiteralSuffix::L),
        (0, 0) => None,
        _ => internal_error::ICE(format!("Unhandled literal suffix '{}'", literal_suffix)),
    };

    Ok(suffix)
}

pub(super) fn make_float_literal_suffix(
    driver: &mut compiler_driver::Driver,
    line_no: usize,
    literal_suffix_start_column: usize,
    literal_suffix: &str,
) -> Result<Option<FloatLiteralSuffix>, ()> {
    if literal_suffix.is_empty() {
        Ok(None)
    } else if literal_suffix == "f" || literal_suffix == "F" {
        Ok(Some(FloatLiteralSuffix::F))
    } else if literal_suffix == "l" || literal_suffix == "L" {
        Ok(Some(FloatLiteralSuffix::L))
    } else {
        let loc = SourceLocation::new(line_no, literal_suffix_start_column, literal_suffix.len());
        let err = format!("Invalid suffix '{}' on floating-point constant.", &literal_suffix);
        driver.add_diagnostic(Diagnostic::error_at_location(err, loc));
        Err(())
    }
}

fn is_valid_char_for_base(ch: char, base: NumericLiteralBase) -> bool {
    match base {
        NumericLiteralBase::Decimal => ch.is_ascii_digit(),
        NumericLiteralBase::Hex => ch.is_ascii_hexdigit(),
        NumericLiteralBase::Octal => matches!(ch, '0'..='7'),
        NumericLiteralBase::Binary => matches!(ch, '0'..='1'),
    }
}

pub(super) fn extract_valid_integer_literal_suffix_chars(literal: String) -> (String, String) {
    extract_literal_suffix_chars(literal, |&ch| matches!(ch, 'u' | 'U' | 'l' | 'L'))
}

pub(super) fn extract_valid_float_literal_suffix_chars(literal: String) -> (String, String) {
    extract_literal_suffix_chars(literal, |&ch| matches!(ch, 'f' | 'F' | 'l' | 'L'))
}

fn extract_literal_suffix_chars<F>(literal: String, f: F) -> (String, String)
where
    F: FnMut(&char) -> bool,
{
    if literal.is_empty() {
        return (literal, String::new());
    }

    let mut suffix: Vec<char> = literal.chars().rev().take_while(f).collect();

    if suffix.is_empty() {
        return (literal, String::new());
    }

    suffix.reverse();
    let suffix = suffix.into_iter().collect::<String>();

    let mut literal = literal;
    literal.truncate(literal.len() - suffix.len());

    (literal, suffix)
}
