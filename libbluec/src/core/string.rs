// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `string` module provides common string utilities.

/// Converts an integer value to an ASCII string.
///
/// Values that fit in the printable ASCII range are converted to ASCII chars. Otherwise the value is transformed
/// to an escape sequence.
pub fn to_ascii(value: u32) -> String {
    match value {
        // \a and \v are handled by `to_octal_escape` because they cannot appear in x86_64 assembly .ascii directives
        //
        8 => "\\b".to_string(),
        12 => "\\f".to_string(),
        10 => "\\n".to_string(),
        13 => "\\r".to_string(),
        9 => "\\t".to_string(),

        34 => "\\\"".to_string(),
        92 => "\\\\".to_string(),

        // Printable ASCII range (except for " which we handle above).
        32..=126 => char::from_u32(value).unwrap().to_string(),

        // Anything else is converted to an octal escape code.
        _ => to_octal_escape(value),
    }
}

fn to_octal_escape(value: u32) -> String {
    // 0: pad with zeros
    // 3: total width
    format!("\\{:03o}", value)
}
