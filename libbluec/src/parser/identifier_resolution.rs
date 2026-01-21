// Copyright 2025 Neil Henderson, Blue Tarp Media.
//
//! The `identifier_resolution` module provides identifier resolution functionality across parsing scopes.

use std::collections::HashMap;

use crate::ICE;
use crate::core::{SourceLocation, SymbolKind};

use super::{AstIdentifier, AstLinkage, AstUniqueName};

/// The identifier resolver is responsible for tracking identifiers across scopes and resolving their declared
/// names to their unique names.
///
/// In some cases, like functions or variables with external linkage, their declared name will be their unique name.
#[derive(Debug)]
pub struct IdentifierResolver {
    scopes: Vec<Scope>,
    unique_identifier_index: usize,
}

/// An identifier's unique name, its kind, whether it has any linkage, and the location of its declaration.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DeclaredIdentifier {
    pub unique: AstUniqueName,
    pub kind: SymbolKind,
    pub linkage: AstLinkage,
    pub loc: SourceLocation,
}

/// Errors returned by resolution functions.
#[derive(Debug, Clone)]
pub enum ResolveError {
    NotAtFileScope,
    RedefinitionOfExistingIdentifier(SymbolKind, SourceLocation),
}

/// The scope of the search to use for identifier resolution when calling `resolve_identifier`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SearchScope {
    All,
    File,
    Current,
}

/// A scope containing identifier declarations.
#[derive(Debug, Default)]
struct Scope {
    identifiers: HashMap<String, DeclaredIdentifier>,
}

impl Default for IdentifierResolver {
    fn default() -> Self {
        Self::new()
    }
}

impl IdentifierResolver {
    /// Creates a new identifier resolver.
    ///
    /// The resolver is created with a scope representing the file scope.
    pub fn new() -> Self {
        Self { scopes: vec![Scope::default()], unique_identifier_index: 0 }
    }

    /// Is the identifier resolver currently at file scope?
    pub fn is_at_file_scope(&self) -> bool {
        self.scopes.len() == 1
    }

    /// Begins/enters a new scope.
    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    /// Ends/exits the current scope.
    ///
    /// Precondition: self.scopes.len() > 1 (cannot end the file scope); otherwise: ICE.
    pub fn end_scope(&mut self) {
        if self.scopes.len() > 1 {
            _ = self.scopes.pop();
        } else {
            ICE!("Cannot pop the file scope");
        }
    }

    /// Resolves an identifier by searching for it in the given scope(s).
    ///
    /// Returns the identifier's declaration if it was previously declared and is visible from the given
    /// search scope(s). Otherwise returns `None`.
    ///
    /// Pass `SearchScope::FileScope` as the search scope to only look for the declared identifier in file scope.
    /// Pass `SearchScope::CurrentScope` to only look for the declared identifier in the current scope (which
    /// can be file scope if `self.is_at_file_scope` is `true`). Pass `SearchScope::All` to look for the declared
    /// identifer starting in the current scope and moving upwards to file scope.
    pub fn resolve_identifier(&self, name: &str, search: SearchScope) -> Option<&DeclaredIdentifier> {
        match search {
            SearchScope::File => self.scopes.first().unwrap().identifiers.get(name),
            SearchScope::Current => self.scopes.last().unwrap().identifiers.get(name),
            SearchScope::All => self.scopes.iter().rev().find_map(|scope| scope.identifiers.get(name)),
        }
    }

    /// Resolves a type alias declaration.
    ///
    /// Returns `Ok` with the alias's unique name or `Err`.
    pub fn add_type_alias_declaration(&mut self, alias_ident: &AstIdentifier) -> Result<AstUniqueName, ResolveError> {
        let name = &alias_ident.name;
        let loc = alias_ident.loc;

        // If this identifier is already declared in the current scope then it's valid to redeclare the same
        // alias again. Here we only check that the identifier remains a type alias; the sema type checking
        // verifies whether the underlying type remains the same.
        let unique_name = if let Some(decl) = self.resolve_identifier(name, SearchScope::Current) {
            if decl.kind == SymbolKind::TypeAlias {
                decl.unique.clone()
            } else {
                return Err(ResolveError::RedefinitionOfExistingIdentifier(decl.kind, decl.loc));
            }
        } else {
            self.make_unique_name(name)
        };

        let _ = self.scopes.last_mut().unwrap().identifiers.insert(
            name.into(),
            DeclaredIdentifier {
                unique: unique_name.clone(),
                kind: SymbolKind::TypeAlias,
                linkage: AstLinkage::None, // Type alias does not have linkage
                loc,
            },
        );

        Ok(unique_name)
    }

    /// Resolves a function declaration.
    ///
    /// Returns `Ok` with the function's unique name (which is the same as its declared name) or `Err`.
    pub fn add_function_declaration(
        &mut self,
        ident: &AstIdentifier,
        linkage: AstLinkage,
    ) -> Result<AstUniqueName, ResolveError> {
        let name = &ident.name;
        let loc = ident.loc;

        debug_assert!(linkage.has_linkage());

        // Cannot redefine the identifier as a different kind of symbol.
        if let Some(decl) = self.resolve_identifier(name, SearchScope::Current)
            && decl.kind != SymbolKind::Function
        {
            return Err(ResolveError::RedefinitionOfExistingIdentifier(decl.kind, decl.loc));
        }

        let unique_name = AstUniqueName::new(name);

        let _ = self.scopes.last_mut().unwrap().identifiers.insert(
            name.into(),
            DeclaredIdentifier { unique: unique_name.clone(), kind: SymbolKind::Function, linkage, loc },
        );

        Ok(unique_name)
    }

    /// Resolves a file scope variable declaration.
    pub fn add_file_scope_variable_declaration(
        &mut self,
        ident: &AstIdentifier,
        linkage: AstLinkage,
    ) -> Result<AstUniqueName, ResolveError> {
        if !self.is_at_file_scope() {
            return Err(ResolveError::NotAtFileScope);
        }

        let name = &ident.name;
        let loc = ident.loc;

        debug_assert!(linkage.has_linkage());

        // Cannot redefine the identifier as a different kind of symbol.
        if let Some(decl) = self.resolve_identifier(name, SearchScope::Current)
            && decl.kind != SymbolKind::Variable
        {
            return Err(ResolveError::RedefinitionOfExistingIdentifier(decl.kind, decl.loc));
        }

        let unique_name = AstUniqueName::new(name);

        let _ = self.scopes.last_mut().unwrap().identifiers.insert(
            name.into(),
            DeclaredIdentifier { unique: unique_name.clone(), kind: SymbolKind::Variable, linkage, loc },
        );

        Ok(unique_name)
    }

    /// Resolves a block scope variable declaration.
    ///
    /// Returns `Ok` with the variable's unique name (which may be renamed) or `Err`.
    pub fn add_block_scope_variable_declaration(
        &mut self,
        ident: &AstIdentifier,
        is_declared_extern: bool,
        linkage: AstLinkage,
    ) -> Result<AstUniqueName, ResolveError> {
        let name = &ident.name;
        let loc = ident.loc;

        // If this identifier is already declared in the current scope then it's only valid to redeclare it if the
        // new declaration is 'extern' and if the existing identifier has linkage (i.e. it's a file-scope variable).
        if let Some(decl) = self.resolve_identifier(name, SearchScope::Current)
            && !(decl.linkage.has_linkage() && is_declared_extern)
        {
            return Err(ResolveError::RedefinitionOfExistingIdentifier(decl.kind, decl.loc));
        }

        let unique_name = if is_declared_extern { AstUniqueName::new(name) } else { self.make_unique_name(name) };

        let _ = self.scopes.last_mut().unwrap().identifiers.insert(
            name.into(),
            DeclaredIdentifier { unique: unique_name.clone(), kind: SymbolKind::Variable, linkage, loc },
        );

        Ok(unique_name)
    }

    /// Makes a unique identifier name.
    fn make_unique_name(&mut self, name: &str) -> AstUniqueName {
        let idx = self.unique_identifier_index;
        if idx == usize::MAX {
            ICE!("Exhausted unique identifiers"); // Technically we have 1 more available
        }
        self.unique_identifier_index += 1;

        AstUniqueName::new(format!("id.{}.{}", name, idx))
    }
}
