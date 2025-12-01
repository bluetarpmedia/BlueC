// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The diagnostics module defines errors and warnings emitted by the compiler driver.

use crate::lexer::SourceLocation;

use std::fs::File;
use std::io::{BufRead, BufReader, Read, Seek, SeekFrom, Write};

/// A diagnostic emitted by the compiler.
#[derive(Debug)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    message: String,
    locations: Vec<SourceLocation>,
    notes: Option<Vec<Note>>,
}

/// The kind of diagnostic.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

/// A note that can be attached to a diagnostic.
#[derive(Clone, Debug)]
pub struct Note {
    pub note: String,
    pub loc: Option<SourceLocation>,
    pub suggested_code: Option<String>,
}

/// A diagnostic printer.
#[derive(Debug)]
pub struct Printer<W: Write> {
    buffer: W,
    source_filename: String,
    tu_reader: BufReader<File>,
    terse: bool,
    show_source_loc: bool,
}

/// An identifier string and its source code location.
#[derive(Debug, Copy, Clone)]
pub struct SourceIdentifier<'a>(pub &'a str, pub SourceLocation);

impl Diagnostic {
    /// Creates an error diagnostic with the given error message.
    pub fn error(message: String) -> Self {
        Self { kind: DiagnosticKind::Error, message, locations: Vec::new(), notes: None }
    }

    /// Creates an error diagnostic with the given error message and source code location.
    pub fn error_at_location(message: String, loc: SourceLocation) -> Self {
        Self { kind: DiagnosticKind::Error, message, locations: vec![loc], notes: None }
    }

    /// Creates a new warning diagnostic with the given message and source code location.
    pub fn warning_at_location(message: String, loc: SourceLocation) -> Self {
        Self { kind: DiagnosticKind::Warning, message, locations: vec![loc], notes: None }
    }

    /// Consumes the diagnostic and returns an equivalent with its kind set to `DiagnosticKind:Error`.
    pub fn convert_to_error(self) -> Self {
        Self { kind: DiagnosticKind::Error, ..self }
    }

    /// The kind of diagnostic.
    pub fn kind(&self) -> DiagnosticKind {
        self.kind
    }

    /// Adds an additional source location to the diagnostic.
    pub fn add_location(&mut self, loc: SourceLocation) {
        self.locations.push(loc);
    }

    /// Adds a note to the diagnostic.
    pub fn add_note(&mut self, note: String, loc: Option<SourceLocation>) {
        if self.notes.is_none() {
            self.notes = Some(Vec::new());
        }

        let notes = self.notes.as_mut().expect("Should exist");
        notes.push(Note { note, loc, suggested_code: None });
    }

    /// Adds a note to the diagnostic.
    pub fn add_note_with_suggested_code(&mut self, note: String, suggested_code: String, loc: Option<SourceLocation>) {
        if self.notes.is_none() {
            self.notes = Some(Vec::new());
        }

        let notes = self.notes.as_mut().expect("Should exist");
        notes.push(Note { note, loc, suggested_code: Some(suggested_code) });
    }
}

impl<W: Write> Printer<W> {
    const MAX_LINE_WIDTH: usize = 80;

    /// Creates a new diagnostic printer.
    pub fn with_source_and_tu(buffer: W, source_filename: &str, tu_filename: &str) -> Self {
        let tu_reader = BufReader::new(File::open(tu_filename).expect("Cannot open file"));
        Self { buffer, source_filename: source_filename.into(), tu_reader, terse: false, show_source_loc: true }
    }

    /// Sets whether to print in terse mode.
    pub fn set_terse(&mut self, terse: bool) {
        self.terse = terse;
    }

    /// Sets whether to show the source filename and line:column location.
    pub fn show_source_file_and_loc(&mut self, show: bool) {
        self.show_source_loc = show;
    }

    /// Prints all the diagnostics, with any errors printed first before any warnings.
    pub fn print_diagnostics(&mut self, errors: &Vec<Diagnostic>, warnings: &Vec<Diagnostic>) {
        for error in errors {
            self.print(error);
        }

        for warning in warnings {
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
        let prefix = match diagnostic.kind {
            DiagnosticKind::Error => "error: ",
            DiagnosticKind::Warning => "warning: ",
        };

        if self.terse {
            _ = writeln!(self.buffer, "{}{}", prefix, diagnostic.message);
            return;
        }

        self.print_text_with_wrapping(prefix, &diagnostic.message);

        // Calculate the width of the left margin.
        //      The margin is the space before the vertical pipe | where we print the source line numbers.
        //      It has to be wide enough for the character length of the line number, plus 1 extra space.
        let mut margin_width = calc_margin_width(&diagnostic.locations);

        // A note might have a line number with a bigger character length than the diagnostic line number.
        if let Some(notes) = &diagnostic.notes {
            let widest_note_line =
                notes.iter().map(|note| note.loc.unwrap_or_default().line.to_string().len() + 1).max();

            if let Some(widest_note_line) = widest_note_line {
                margin_width = std::cmp::max(margin_width, widest_note_line);
            }
        }

        let margin_str = " ".repeat(margin_width);

        // Print the location(s). Usually just one.
        if diagnostic.locations.len() == 1 {
            let location = &diagnostic.locations[0];

            // <filename>:<line>:<col>
            if self.show_source_loc {
                _ = writeln!(
                    self.buffer,
                    "{}--> {}:{}:{}",
                    margin_str, self.source_filename, location.line, location.column
                );
            }

            self.print_source_line_with_highlight(location.line, diagnostic.locations.clone(), margin_width);
            _ = writeln!(self.buffer, "{} |", margin_str);
        } else if diagnostic.locations.len() > 1 {
            let all_locs_same_line = diagnostic.locations.iter().all(|loc| loc.line == diagnostic.locations[0].line);

            if all_locs_same_line {
                let primary_loc = diagnostic.locations[0];

                // <filename>:<line>:<col>
                if self.show_source_loc {
                    _ = writeln!(
                        self.buffer,
                        "{}--> {}:{}:{}",
                        margin_str, self.source_filename, primary_loc.line, primary_loc.column
                    );
                }

                self.print_source_line_with_highlight(primary_loc.line, diagnostic.locations.clone(), margin_width);
                _ = writeln!(self.buffer, "{} |", margin_str);
            } else {
                for location in &diagnostic.locations {
                    if self.show_source_loc {
                        // <filename>:<line>:<col>
                        _ = writeln!(
                            self.buffer,
                            "{}--> {}:{}:{}",
                            margin_str, self.source_filename, location.line, location.column
                        );
                    }

                    self.print_source_line_with_highlight(location.line, vec![*location], margin_width);
                    _ = writeln!(self.buffer, "{} |", margin_str);
                }
            }
        }

        // Print the note(s)
        if let Some(notes) = &diagnostic.notes {
            for note in notes {
                self.print_text_with_wrapping("note: ", &note.note);

                // Note location
                if let Some(note_location) = &note.loc {
                    self.print_source_line_with_highlight(note_location.line, vec![*note_location], margin_width);
                }

                // Suggested code
                if let Some(suggested_code) = &note.suggested_code {
                    self.print_suggested_code(suggested_code, margin_width);
                }

                _ = writeln!(self.buffer, "{} |", margin_str);
            }
        }

        _ = writeln!(self.buffer);
    }

    /// Gets a line from the source file. (TU file, for now.)
    fn get_source_line(&mut self, line_no: usize) -> String {
        if line_no == 0 {
            return String::new();
        }

        self.tu_reader.seek(SeekFrom::Start(0)).expect("Expect to seek to start");
        let mut lines = self.tu_reader.by_ref().lines();
        lines.nth(line_no - 1).expect("Cannot read line").expect("Cannot read line")
    }

    /// Prints a line of source code with '^' highlight characters underneath pointing at the relevant location.
    fn print_source_line_with_highlight(
        &mut self,
        line_no: usize,
        mut locations: Vec<SourceLocation>,
        margin_width: usize,
    ) {
        debug_assert!(!locations.is_empty());

        let margin_str = " ".repeat(margin_width);
        _ = writeln!(self.buffer, "{margin_str} |");

        let source_line = self.get_source_line(line_no);

        if self.show_source_loc {
            _ = writeln!(self.buffer, "{:>width$} | {source_line}", line_no, width = margin_width);
        } else {
            _ = writeln!(self.buffer, "{margin_str} | {source_line}");
        }

        _ = write!(self.buffer, "{margin_str} |");

        let primary_highlight_col = locations[0].column;
        let mut running_offset = 0;

        locations.sort_by_key(|loc| loc.column);

        for location in locations {
            let highlight_indent = " ".repeat(location.column - running_offset);
            let highlight_len = if location.length == 0 { 1 } else { location.length };

            let highlight_char = if location.column == primary_highlight_col { '^' } else { '~' };
            let highlight = highlight_char.to_string().repeat(highlight_len);

            _ = write!(self.buffer, "{highlight_indent}{highlight}");

            running_offset = location.column + highlight_len;
        }
        _ = writeln!(self.buffer);
    }

    /// Print some suggested code with '~' highlight characters underneath the relevant location.
    fn print_suggested_code(&mut self, suggested_code: &String, margin_width: usize) {
        let margin_str = " ".repeat(margin_width);
        _ = writeln!(self.buffer, "{} |", margin_str);
        _ = writeln!(self.buffer, "{} | {}", margin_str, suggested_code);
    }

    /// Prints text with line wrapping if needed.
    fn print_text_with_wrapping(&mut self, prefix: &str, text: &str) {
        if text.len() <= Self::MAX_LINE_WIDTH {
            _ = writeln!(self.buffer, "{}{}", prefix, text);
            return;
        }

        // The line indentation allows us to align the wrapped lines like this:
        //
        // note: some text here\n
        //       more text here\n
        //       even more
        //
        let line_indent = prefix.len();
        _ = writeln!(self.buffer, "{}{}", prefix, make_wrapped_text(text, line_indent, Self::MAX_LINE_WIDTH));
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

fn calc_margin_width(locs: &[SourceLocation]) -> usize {
    if locs.is_empty() {
        return 0;
    }

    let max_line_no = locs.iter().map(|loc| loc.line).max().unwrap(); // Safe to unwrap since we checked `is_empty`
    max_line_no.to_string().len() + 1
}
