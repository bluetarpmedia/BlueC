// Copyright 2025-2026 Neil Henderson
//
//! The `diagnostics` module defines errors and warnings emitted by the compiler driver.

pub mod error;
pub mod printer;
pub mod warning;
pub mod warning_kind;

use crate::core::SourceLocation;

use super::WarningKind;

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
    Warning(WarningKind),
}

/// Suggested code.
#[derive(Clone, Debug)]
pub enum SuggestedCode {
    /// Suggested code as a string.
    Code(String),

    /// A rudimentary and custom format string into which source code is inserted during diagnostic printing.
    /// The original source code for each `SourceLocation` in the vector is inserted in the format string by
    /// replacing `$$<index>` tokens, where index is 1-based.
    ///
    /// E.g.
    /// "(char)($$1) = $$2", vec![loc, another]
    FormatString(String, Vec<SourceLocation>),
}

/// A note that can be attached to a diagnostic.
#[derive(Clone, Debug)]
pub struct Note {
    pub note: String,
    pub loc: Option<SourceLocation>,
    pub suggested_code: Option<SuggestedCode>,
}

impl Diagnostic {
    /// Creates an error diagnostic with the given error message.
    pub fn error(message: String) -> Self {
        Self { kind: DiagnosticKind::Error, message, locations: Vec::new(), notes: None }
    }

    /// Creates an error diagnostic with the given error message and source code location.
    pub fn error_at_location(message: String, loc: SourceLocation) -> Self {
        let locations = if loc == SourceLocation::none() { Vec::new() } else { vec![loc] };
        Self { kind: DiagnosticKind::Error, message, locations, notes: None }
    }

    /// Creates a new warning diagnostic with the given message and source code location.
    pub fn warning_at_location(kind: WarningKind, message: String, loc: SourceLocation) -> Self {
        let message = format!("{message} [-W{kind}]");
        let locations = if loc == SourceLocation::none() { Vec::new() } else { vec![loc] };
        Self { kind: DiagnosticKind::Warning(kind), message, locations, notes: None }
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
    pub fn add_note_with_suggested_code(&mut self, note: String, code: SuggestedCode, loc: Option<SourceLocation>) {
        if self.notes.is_none() {
            self.notes = Some(Vec::new());
        }

        let notes = self.notes.as_mut().expect("Should exist");
        notes.push(Note { note, loc, suggested_code: Some(code) });
    }
}
