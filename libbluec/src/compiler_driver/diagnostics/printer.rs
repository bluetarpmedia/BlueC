// Copyright 2025-2026 Neil Henderson
//
//! The `printer` module defines functionality for printing diagnostics.

use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek, SeekFrom, Write};

use crate::core::{FilePosition, SourceLocation};

use super::super::tu_file::TuFile;
use super::{Diagnostic, DiagnosticKind, SuggestedCode};

/// Prints diagnostics.
#[derive(Debug)]
pub struct Printer<'a, W: Write> {
    buffer: W,
    tu_file: &'a TuFile,
    tu_reader: BufReader<File>,
    terse: bool,
    show_source_loc: bool,
    error_color: &'static str,
    warn_color: &'static str,
    bold_color: &'static str,
    margin_color: &'static str,
    suggest_color: &'static str,
    reset_color: &'static str,
}

#[derive(Debug, Clone)]
struct DisplayLocation {
    source_code_file_pos: FilePosition,
    filename: String,
    line_no: u32,
    column_no: u32,
    length: u32,
}

impl<'a, W: Write> Printer<'a, W> {
    const MAX_LINE_WIDTH: usize = 80;

    /// Creates a new diagnostic printer that writes to the given `buffer`.
    ///
    /// `buffer` must implement `std::io::Write`.
    pub fn new(buffer: W, tu_file: &'a TuFile, color_enabled: bool) -> Self {
        let tu_reader = BufReader::new(File::open(tu_file.file_path()).expect("Cannot open file"));

        const CYAN: &str = "\x1b[36m";
        const BRIGHT_RED: &str = "\x1b[91m";
        const BRIGHT_GREEN: &str = "\x1b[92m";
        const BRIGHT_YELLOW: &str = "\x1b[93m";
        const BRIGHT_WHITE: &str = "\x1b[97m";
        const RESET: &str = "\x1b[0m";

        let error_color = if color_enabled { BRIGHT_RED } else { "" };
        let warn_color = if color_enabled { BRIGHT_YELLOW } else { "" };
        let bold_color = if color_enabled { BRIGHT_WHITE } else { "" };
        let margin_color = if color_enabled { CYAN } else { "" };
        let suggest_color = if color_enabled { BRIGHT_GREEN } else { "" };
        let reset_color = if color_enabled { RESET } else { "" };

        Self {
            buffer,
            tu_file,
            tu_reader,
            terse: false,
            show_source_loc: true,
            error_color,
            warn_color,
            bold_color,
            margin_color,
            suggest_color,
            reset_color,
        }
    }

    /// Sets whether to print in terse mode.
    pub fn set_terse(&mut self, terse: bool) {
        self.terse = terse;
    }

    /// Sets whether to show the source filename and 'line:column' location.
    pub fn show_source_file_and_loc(&mut self, show: bool) {
        self.show_source_loc = show;
    }

    /// Prints all the diagnostics, with any errors printed first before any warnings.
    pub fn print_diagnostics(&mut self, errors: &mut Vec<Diagnostic>, warnings: &mut Vec<Diagnostic>) {
        let sort_diags = |a: &Diagnostic, b: &Diagnostic| -> std::cmp::Ordering {
            if let Some(a_loc) = a.locations.first()
                && let Some(b_loc) = b.locations.first()
            {
                a_loc.file_pos.cmp(&b_loc.file_pos).then(a_loc.length.cmp(&b_loc.length))
            } else {
                a.message.cmp(&b.message)
            }
        };

        // Sort by line & column and then remove duplicates
        //
        errors.sort_by(sort_diags);
        warnings.sort_by(sort_diags);
        errors.dedup_by(|a, b| a.message == b.message && a.locations == b.locations);
        warnings.dedup_by(|a, b| a.kind == b.kind && a.locations == b.locations);

        for error in errors.iter() {
            self.print(error);
        }

        for warning in warnings.iter() {
            self.print(warning);
        }

        if self.terse {
            return;
        }

        if !errors.is_empty() {
            let error_count = errors.len();
            let errors_label = if error_count == 1 { "error" } else { "errors" };
            _ = writeln!(self.buffer, "{} {} generated", error_count, errors_label);
        }

        if !warnings.is_empty() {
            let warning_count = warnings.len();
            let warnings_label = if warning_count == 1 { "warning" } else { "warnings" };
            _ = writeln!(self.buffer, "{} {} generated", warning_count, warnings_label);
        }

        _ = self.buffer.flush();
    }

    /// Prints a diagnostic.
    fn print(&mut self, diagnostic: &Diagnostic) {
        self.print_diagnostic_message(diagnostic);

        // Calculate the width of the left margin.
        //      The margin is the space before the vertical pipe | where we print the source line numbers.
        //      It has to be wide enough for the character length of the line number, plus 1 extra space.
        let margin_width = self.calc_margin_width(diagnostic);

        let margin_str = " ".repeat(margin_width);

        // Print the location(s). Usually just one.
        if !diagnostic.locations.is_empty() {
            let display_locations = self.transform_source_locations_for_display(diagnostic);

            if diagnostic.locations.len() == 1 {
                self.print_source_location_header(&margin_str, &display_locations[0]);
                self.print_source_line_with_highlight(diagnostic.kind, display_locations, margin_width);
            } else {
                // If all the locations are on the same line then we only want to print the "<filename>:<line>:<col>"
                // header once.
                let primary_line_no = display_locations[0].line_no;
                let all_locs_on_same_line = display_locations.iter().all(|loc| loc.line_no == primary_line_no);

                if all_locs_on_same_line {
                    self.print_source_location_header(&margin_str, &display_locations[0]);
                    self.print_source_line_with_highlight(diagnostic.kind, display_locations, margin_width);
                } else {
                    for location in display_locations {
                        self.print_source_location_header(&margin_str, &location);
                        self.print_source_line_with_highlight(diagnostic.kind, vec![location], margin_width);
                    }
                }
            }

            _ = writeln!(self.buffer, "{} {}|{}", margin_str, self.margin_color, self.reset_color);
        }

        // Print the note(s)
        if let Some(notes) = &diagnostic.notes {
            for note in notes {
                let line_indent = "note: ".len();
                let note_text = make_wrapped_text(&note.note, line_indent, Self::MAX_LINE_WIDTH);
                _ = writeln!(self.buffer, "{}note:{} {}", self.bold_color, self.reset_color, note_text);

                if let Some(note_location) = &note.loc {
                    let display_loc = self.transform_location_for_display(*note_location);
                    self.print_source_line_with_highlight(diagnostic.kind, vec![display_loc], margin_width);
                }

                if let Some(suggested_code) = &note.suggested_code {
                    match suggested_code {
                        SuggestedCode::Code(code) => self.print_suggested_code(code, margin_width),
                        SuggestedCode::FormatString(format_string, locations) => {
                            self.print_suggested_code_format_string(format_string, locations, margin_width)
                        }
                    }
                }

                _ = writeln!(self.buffer, "{} {}|{}", margin_str, self.margin_color, self.reset_color);
            }
        }

        _ = writeln!(self.buffer);
    }

    /// Prints the source location header information
    ///
    /// ```markdown
    /// <filename>:<line>:<col>
    /// ```
    fn print_source_location_header(&mut self, margin_str: &str, display_location: &DisplayLocation) {
        if self.show_source_loc {
            let filename = &display_location.filename;
            let line_no = display_location.line_no;
            let column_no = display_location.column_no;

            let margin_color = self.margin_color;
            let reset_color = self.reset_color;

            _ = writeln!(self.buffer, "{margin_str}{margin_color}-->{reset_color} {filename}:{line_no}:{column_no}");
        }
    }

    /// Prints a line of source code with '^' highlight characters underneath pointing at the relevant location.
    fn print_source_line_with_highlight(
        &mut self,
        kind: DiagnosticKind,
        mut locations: Vec<DisplayLocation>,
        margin_width: usize,
    ) {
        debug_assert!(!locations.is_empty());

        let margin_color = self.margin_color;
        let reset_color = self.reset_color;

        let margin_str = " ".repeat(margin_width);
        _ = writeln!(self.buffer, "{margin_str} {margin_color}|{reset_color}");

        let primary_loc = &locations[0];
        let primary_column_no = primary_loc.column_no;
        let source_line = self.get_line_of_source_code(primary_loc.source_code_file_pos);

        if self.show_source_loc {
            _ = writeln!(
                self.buffer,
                "{margin_color}{:>width$} |{reset_color} {source_line}",
                primary_loc.line_no,
                width = margin_width
            );
        } else {
            _ = writeln!(self.buffer, "{margin_str} {margin_color}|{reset_color} {source_line}");
        }

        _ = write!(self.buffer, "{margin_str} {margin_color}|{reset_color}");

        let mut running_offset = 0;

        // Sort by column number, remove exact duplicates, then merge any overlapping source location spans.
        locations.sort_by_key(|loc| loc.column_no);
        locations.dedup_by(|a, b| a.column_no == b.column_no && a.length == b.length);
        merge_overlapping_display_locations(&mut locations);

        for location in &locations {
            let highlight_indent = " ".repeat(location.column_no as usize - running_offset);
            let highlight_len = if location.length == 0 { 1 } else { location.length as usize };

            let highlight_char = if location.column_no == primary_column_no { '^' } else { '~' };
            let highlight = highlight_char.to_string().repeat(highlight_len);

            let highlight_color = if kind == DiagnosticKind::Error { self.error_color } else { self.warn_color };
            let reset_color = self.reset_color;

            _ = write!(self.buffer, "{highlight_indent}{highlight_color}{highlight}{reset_color}");

            running_offset = location.column_no as usize + highlight_len;
        }
        _ = writeln!(self.buffer);
    }

    /// Gets a line from the source file.
    fn get_line_of_source_code(&mut self, file_pos: FilePosition) -> String {
        let source_line_no = self.tu_file.get_source_file_line_no(file_pos);
        self.tu_reader.seek(SeekFrom::Start(0)).expect("Expect to seek to start");
        let mut lines = self.tu_reader.by_ref().lines();
        lines.nth(source_line_no).expect("Cannot read line").expect("Cannot read line")
    }

    /// Print the suggested code.
    fn print_suggested_code(&mut self, suggested_code: &str, margin_width: usize) {
        let margin_str = " ".repeat(margin_width);
        let margin_color = self.margin_color;
        let reset_color = self.reset_color;

        _ = writeln!(self.buffer, "{margin_str} {margin_color}|{reset_color}");
        _ = writeln!(self.buffer, "{margin_str} {margin_color}|{reset_color} {suggested_code}");
    }

    /// Print the suggested code based on a format string.
    fn print_suggested_code_format_string(
        &mut self,
        format_string: &str,
        locations: &[SourceLocation],
        margin_width: usize,
    ) {
        if locations.is_empty() {
            debug_assert!(false, "SuggestedCode format string '{format_string}' has no source locations");
            self.print_suggested_code(format_string, margin_width);
            return;
        }

        let suggest_color = self.suggest_color;
        let reset_color = self.reset_color;

        let mut formatted = format!("{suggest_color}{format_string}{reset_color}");
        let mut next_loc_idx = 0;
        let mut next_token = "$$1".to_string();

        while next_loc_idx < locations.len() {
            if !formatted.contains(&next_token) {
                debug_assert!(false, "SuggestedCode format string is missing token '{next_token}'");
                break;
            }

            let loc = locations[next_loc_idx];
            let line = self.get_line_of_source_code(loc.file_pos);
            let start_idx = (self.tu_file.get_column_no(loc) - 1) as usize; // column_no is 1-based
            let end_idx = start_idx + loc.length as usize;
            let loc_source_code = &line[start_idx..end_idx];
            let formatted_source_code = format!("{reset_color}{loc_source_code}{suggest_color}");

            formatted = formatted.replace(&next_token, &formatted_source_code);

            next_loc_idx += 1;
            next_token = format!("$${}", next_loc_idx + 1); // 1-based
        }

        // Validate that the formatted string doesn't have the next token
        if formatted.contains(&next_token) {
            debug_assert!(
                false,
                "SourceLocation not provided for token '{next_token}' in SuggestedCode format string '{format_string}'"
            );
        }

        self.print_suggested_code(&formatted, margin_width);
    }

    /// Prints a diagnostic message.
    fn print_diagnostic_message(&mut self, diagnostic: &Diagnostic) {
        let prefix = match diagnostic.kind {
            DiagnosticKind::Error => {
                _ = write!(self.buffer, "{}error{}", self.error_color, self.reset_color);
                "error: "
            }
            DiagnosticKind::Warning(_) => {
                _ = write!(self.buffer, "{}warning{}", self.warn_color, self.reset_color);
                "warning: "
            }
        };

        if self.terse || diagnostic.message.len() <= Self::MAX_LINE_WIDTH {
            _ = writeln!(self.buffer, "{}: {}{}", self.bold_color, diagnostic.message, self.reset_color);
            return;
        }

        // The line indentation allows us to align the wrapped lines like this:
        //
        // error: some text here\n
        //        more text here\n
        //        even more
        //
        let line_indent = prefix.len();
        let wrapped_text = make_wrapped_text(&diagnostic.message, line_indent, Self::MAX_LINE_WIDTH);
        _ = writeln!(self.buffer, "{}: {}{}", self.bold_color, wrapped_text, self.reset_color);
    }

    /// Calculate the required margin width based on line numbers in the diagnostic's location(s) and note(s).
    fn calc_margin_width(&self, diagnostic: &Diagnostic) -> usize {
        if !self.show_source_loc {
            return 2;
        }

        let max_location_line_no = diagnostic
            .locations
            .iter()
            .map(|loc| {
                let (_, line_no) = self.tu_file.get_filename_and_line_no(*loc);
                line_no
            })
            .max()
            .unwrap_or(0);

        let max_notes_line_no = if let Some(notes) = &diagnostic.notes {
            notes
                .iter()
                .filter_map(|note| {
                    if let Some(loc) = note.loc {
                        let (_, line_no) = self.tu_file.get_filename_and_line_no(loc);
                        Some(line_no)
                    } else {
                        None
                    }
                })
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        let max_line_no = std::cmp::max(max_location_line_no, max_notes_line_no);
        let num_digits = count_digits(max_line_no) as usize;

        // We'll create space for 5 digits of line numbers as a minimum, but this will be larger if `max_line_no`
        // is > 99999.
        std::cmp::max(num_digits, 5)
    }

    /// Transforms a diagnostic's source locations into a display-friendly type.
    fn transform_source_locations_for_display(&self, diagnostic: &Diagnostic) -> Vec<DisplayLocation> {
        diagnostic.locations.iter().map(|loc| self.transform_location_for_display(*loc)).collect()
    }

    /// Transforms a source location into a display-friendly type.
    fn transform_location_for_display(&self, loc: SourceLocation) -> DisplayLocation {
        let (filename, line_no) = self.tu_file.get_filename_and_line_no(loc);
        let column_no = self.tu_file.get_column_no(loc);
        DisplayLocation { source_code_file_pos: loc.file_pos, filename, line_no, column_no, length: loc.length }
    }
}

/// Creates a wrapped version of the given input text.
fn make_wrapped_text(text: &str, indent: usize, max_width: usize) -> String {
    let mut result = String::new();
    let mut start = 0;

    while start < text.len() {
        let end = if start + max_width >= text.len() {
            text.len()
        } else {
            // Look backwards from max_width to find the last whitespace
            let slice = &text[start..start + max_width];
            match slice.rfind(char::is_whitespace) {
                Some(pos) => start + pos + 1, // +1 to skip the whitespace
                None => start + max_width,    // No whitespace found, hard break
            }
        };

        // Push the line and trim trailing whitespace
        result.push_str(text[start..end].trim_end());

        start = end;

        // If there's more text to process then push a newline and indentation,
        // but we don't want to do so if we're finished.
        if start < text.len() {
            result.push('\n');
            result.push_str(&" ".repeat(indent));
        }
    }

    result
}

fn count_digits(n: u32) -> u32 {
    if n == 0 {
        1
    } else {
        n.checked_ilog10().unwrap_or(0) + 1 // Plus 1 because `checked_ilog10` returns the floor of log10(n)
    }
}

/// Merges overlapping (but not adjacent) display locations.
///
/// A 'span' is a range that represents the column number start position and the length.
///
/// (5, 10), (6, 3)  => (5, 10)
/// (5, 10), (6, 10) => (5, 11)
fn merge_overlapping_display_locations(locations: &mut Vec<DisplayLocation>) {
    if locations.is_empty() {
        return;
    }

    let old_locations = std::mem::take(locations);
    let mut merged: Vec<DisplayLocation> = Vec::with_capacity(old_locations.len());

    let mut iter = old_locations.into_iter();

    // The input `locations` Vec is sorted by `column_no` so we can merge overlapping spans in a single pass.
    //
    //      We keep track of a `current_span` and only update it when we find a new location that does not overlap with
    //      it. But before that, for every span that overlaps with our current one, we merge that into the current one.
    //
    if let Some(mut current_span) = iter.next() {
        for next_span in iter {
            let current_span_end = current_span.column_no + current_span.length;

            // If `next_span` overlaps with `current_span`, update our current span's length in order to merge them.
            // But don't merge adjacent spans.
            //
            if next_span.column_no < current_span_end {
                let next_span_end = next_span.column_no + next_span.length;

                // If `next_span` is full contained inside `current_span` we want to keep our current span's length.
                let new_end = std::cmp::max(current_span_end, next_span_end);

                current_span.length = new_end - current_span.column_no;
            } else {
                // We've found a new span that does not overlap with our current one. So push the current span
                // and update `current_span` to the new one.
                merged.push(current_span);
                current_span = next_span;
            }
        }

        merged.push(current_span);
    }

    *locations = merged;
}
