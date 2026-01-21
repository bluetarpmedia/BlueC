// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `char_literal` module defines lexing functions for character literals.

use std::iter::Peekable;

use crate::ICE;
use crate::compiler_driver::{Diagnostic, WarningKind};
use crate::core::{FilePosition, SourceLocation};

use super::line_lexer::LineLexer;
use super::{Token, TokenType};

/// Makes a token for a character literal.
///
/// Supports multi-char character literals.
///
/// ```c
/// char a = 'x';
/// char b = 'ABCD';
/// char c = '\n';
/// char d = 'this is strange \t but supported\n';
/// char e = '\xFF';
/// char f = '\77';
/// ```
pub fn make_char_literal(line_lexer: &mut LineLexer) -> Result<Token, ()> {
    // Expect the first char to be the opening single quote.
    let Some((idx, first)) = line_lexer.cursor().next() else {
        ICE!("Expected character");
    };
    line_lexer.set_cursor_index(idx);

    let start_pos = line_lexer.cursor_pos();

    if first != '\'' {
        ICE!("Expected single quote");
    }

    // Consume all chars up to and including the closing single quote.
    //      Remember that an escape \ character before a single quote does not close the literal, e.g. '\''.
    //
    let (literal, is_closed) = take_remaining_literal_chars(line_lexer);

    let token_len = literal.len();

    // Do we have an empty '' character literal?
    if literal == "''" {
        let loc = SourceLocation::new(start_pos, token_len);
        let err = "Empty character literal".to_string();
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    // Is the literal unclosed?
    if !is_closed {
        let loc = SourceLocation::new(start_pos, 1);
        let err = "Missing closing single quote (') for character literal".to_string();
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    let char_count = literal.len() - 2; // Without enclosing single quotes

    // Evaluate the chars in the literal and warn about invalid escape sequences
    let (mut values, has_hex_or_octal) =
        evaluate_chars(literal.chars().skip(1).take(char_count), start_pos, line_lexer)?;

    // Check length of literal; a multichar literal can only have up to 4 char values in order to convert to 'int'.
    //      If the length is > 4 then the last 4 are taken as the value.
    //
    if values.len() > 4 {
        let warning = "Character literal is too long; the last 4 chars are taken as the value.".to_string();
        let loc = SourceLocation::new(start_pos, token_len);
        line_lexer.driver().add_diagnostic(Diagnostic::warning_at_location(WarningKind::Multichar, warning, loc));
    }

    // If the char literal contains a hex and/or octal escape sequence then (like gcc/clang) we just take the
    // last value, if the literal is multichar.
    if has_hex_or_octal && values.len() > 1 {
        values.drain(0..values.len() - 1); // Remove all except the last element
    }

    // Evaluate the literal's 'int' value from up to the last 4 char values.
    //      E.g. 'abcABCD' is evaluated as 'ABCD'.
    let value = values.iter().rev().take(4).rev().fold(0, |acc, value| (acc << 8) + value) as i32;

    let token_type = TokenType::CharLiteral { literal, value };
    let location = SourceLocation::new(start_pos, token_len);

    Ok(Token { token_type, location })
}

/// Consumes all of the char literal up to and including the closing single quote.
fn take_remaining_literal_chars(line_lexer: &mut LineLexer) -> (String, bool) {
    let mut literal = String::from("'"); // Includes the opening single quote.

    let mut is_closed = false;
    let mut prev_is_escape = false;

    while let Some((idx, ch)) = line_lexer.cursor().next() {
        literal.push(ch);
        line_lexer.set_cursor_index(idx);

        match ch {
            '\\' => prev_is_escape = !prev_is_escape,

            '\'' => {
                if prev_is_escape {
                    prev_is_escape = false;
                } else {
                    is_closed = true;
                    break;
                }
            }

            _ => prev_is_escape = false,
        }
    }

    (literal, is_closed)
}

/// Evaluates the literal chars into their corresponding values.
pub fn evaluate_chars<I>(
    input: I,
    literal_start_pos: FilePosition,
    line_lexer: &mut LineLexer,
) -> Result<(Vec<u32>, bool), ()>
where
    I: Iterator<Item = char>,
{
    let mut result = Vec::new();
    let mut chars = input.enumerate().peekable();
    let mut has_hex_or_octal = false;

    while let Some((_, ch)) = chars.next() {
        let is_escape = ch == '\\';

        let value = if is_escape {
            let next_ch = chars.next();

            match next_ch {
                // Hex escape sequence
                Some((ch_idx, 'x')) => {
                    let (value, new_chars) = evaluate_hex_chars(chars, literal_start_pos + ch_idx, line_lexer)?;
                    chars = new_chars;
                    has_hex_or_octal = true;

                    value
                }

                // Octal escape sequence
                Some((ch_idx, ch)) if ch.is_digit(8) => {
                    let (value, new_chars) = evaluate_octal_chars(chars, ch, literal_start_pos + ch_idx, line_lexer)?;
                    chars = new_chars;
                    has_hex_or_octal = true;

                    value
                }

                // Escape sequence
                Some((ch_idx, ch)) => {
                    if let Ok(code) = get_escape_code(ch) {
                        code
                    } else {
                        let warning = format!("Unknown escape sequence '\\{ch}'");
                        let loc = SourceLocation::new(literal_start_pos + ch_idx, 2);
                        let diag = Diagnostic::warning_at_location(WarningKind::UnknownEscapeSequence, warning, loc);
                        line_lexer.driver().add_diagnostic(diag);

                        ch as u32
                    }
                }

                // No char after leading '\'
                None => ICE!(
                    "Should have validated that the literal is closed, \
                     and an escape \\ char always has a successor"
                ),
            }
        } else {
            ch as u32
        };

        result.push(value)
    }

    Ok((result, has_hex_or_octal))
}

#[rustfmt::skip]
enum EscapeSequenceCode {
    AudibleAlert   = 7,     // \a
    Backspace      = 8,     // \b
    FormFeed       = 12,    // \f
    NewLine        = 10,    // \n
    CarriageReturn = 13,    // \r
    HorizontalTab  = 9,     // \t
    VerticalTab    = 11,    // \v
    // We don't need values for other escape sequences like \', \", \?, \\ because their value is just the int
    // value of the char following the '\'.
}

/// Gets the value for an escape sequence.
fn get_escape_code(ch: char) -> Result<u32, ()> {
    match ch {
        'a' => Ok(EscapeSequenceCode::AudibleAlert as u32),
        'b' => Ok(EscapeSequenceCode::Backspace as u32),
        'f' => Ok(EscapeSequenceCode::FormFeed as u32),
        'n' => Ok(EscapeSequenceCode::NewLine as u32),
        'r' => Ok(EscapeSequenceCode::CarriageReturn as u32),
        't' => Ok(EscapeSequenceCode::HorizontalTab as u32),
        'v' => Ok(EscapeSequenceCode::VerticalTab as u32),

        ch if matches!(ch, '\'' | '"' | '?' | '\\') => Ok(ch as u32),

        _ => Err(()),
    }
}

/// Evaluates the value from a hex escape sequence.
fn evaluate_hex_chars<I>(
    mut chars: Peekable<I>,
    hex_start_pos: FilePosition,
    line_lexer: &mut LineLexer,
) -> Result<(u32, Peekable<I>), ()>
where
    I: Iterator<Item = (usize, char)>,
{
    let mut value = 0u32;

    let has_hex_char = chars.peek().is_some_and(|(_, ch)| ch.is_ascii_hexdigit());
    if !has_hex_char {
        let err = "Hex escape sequence \\x used without any hex digits".to_string();
        let loc = SourceLocation::new(hex_start_pos, 2);
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    let mut hex_char_count = 0;

    // Consume all valid hex digits
    while let Some(&(_, next_ch)) = chars.peek() {
        if let Some(digit) = next_ch.to_digit(16) {
            hex_char_count += 1;

            value = (value << 4) | digit;
            chars.next();
        } else {
            break;
        }
    }

    // Value must fit in 'unsigned char'
    if value > u8::MAX as u32 {
        let err = "Hex escape sequence out of range".to_string();
        let loc = SourceLocation::new(hex_start_pos, 2 + hex_char_count);
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    Ok((value, chars))
}

/// Evaluates the value from an octal escape sequence.
fn evaluate_octal_chars<I>(
    mut chars: Peekable<I>,
    first_ch: char,
    oct_start_pos: FilePosition,
    line_lexer: &mut LineLexer,
) -> Result<(u32, Peekable<I>), ()>
where
    I: Iterator<Item = (usize, char)>,
{
    let mut value = first_ch.to_digit(8).unwrap();
    let mut oct_char_count = 1;

    // Octal escape sequence can have up to 3 digits and does not require a leading zero.
    //      \012
    //      \1
    //      \11
    //      \111
    //
    while oct_char_count < 3 {
        if let Some(&(_, next_ch)) = chars.peek()
            && let Some(digit) = next_ch.to_digit(8)
        {
            value = (value << 3) | digit;

            chars.next();
            oct_char_count += 1;

            continue;
        }

        break;
    }

    // Value must fit in 'unsigned char'
    if value > u8::MAX as u32 {
        let err = "Octal escape sequence out of range".to_string();
        let loc = SourceLocation::new(oct_start_pos, oct_char_count + 1);
        line_lexer.driver().add_diagnostic(Diagnostic::error_at_location(err, loc));
        return Err(());
    }

    Ok((value, chars))
}
