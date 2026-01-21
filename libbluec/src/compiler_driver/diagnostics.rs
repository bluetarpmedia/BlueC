// Copyright 2025 Neil Henderson, Blue Tarp Media.
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

/// A note that can be attached to a diagnostic.
#[derive(Clone, Debug)]
pub struct Note {
    pub note: String,
    pub loc: Option<SourceLocation>,
    pub suggested_code: Option<String>,
}

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
    pub fn warning_at_location(kind: WarningKind, message: String, loc: SourceLocation) -> Self {
        let message = format!("{message} [-W{kind}]");
        Self { kind: DiagnosticKind::Warning(kind), message, locations: vec![loc], notes: None }
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
