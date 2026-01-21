// Copyright 2025 Neil Henderson, Blue Tarp Media.

use crate::core::{FilePosition, SourceLocation, SymbolKind};

use super::super::identifier_resolution::*;
use super::super::{AstIdentifier, AstLinkage, AstUniqueName};

#[test]
fn declare_and_resolve_identifiers_with_linkage() {
    let mut resolver = IdentifierResolver::new();

    // Not yet declared
    assert!(resolver.resolve_identifier("my_global", SearchScope::All).is_none());
    assert!(resolver.resolve_identifier("some_internal_var", SearchScope::All).is_none());
    assert!(resolver.resolve_identifier("func1", SearchScope::All).is_none());
    assert!(resolver.resolve_identifier("func2", SearchScope::All).is_none());

    assert!(resolver.resolve_identifier("my_global", SearchScope::Current).is_none());
    assert!(resolver.resolve_identifier("some_internal_var", SearchScope::Current).is_none());
    assert!(resolver.resolve_identifier("func1", SearchScope::Current).is_none());
    assert!(resolver.resolve_identifier("func2", SearchScope::Current).is_none());

    // Variables at file scope are not renamed
    assert!(same_name(
        "my_global",
        resolver.add_file_scope_variable_declaration(&make_ident("my_global"), AstLinkage::External).unwrap()
    ));
    assert!(same_name(
        "some_var",
        resolver.add_file_scope_variable_declaration(&make_ident("some_var"), AstLinkage::External).unwrap()
    ));

    // Functions are not renamed
    assert!(same_name("func1", resolver.add_function_declaration(&make_ident("func1"), AstLinkage::External).unwrap()));
    assert!(same_name("func2", resolver.add_function_declaration(&make_ident("func2"), AstLinkage::External).unwrap()));

    assert!(
        resolver
            .resolve_identifier("my_global", SearchScope::All)
            .is_some_and(|decl| decl.unique == AstUniqueName::new("my_global"))
    );
    assert!(
        resolver
            .resolve_identifier("some_var", SearchScope::All)
            .is_some_and(|decl| decl.unique == AstUniqueName::new("some_var"))
    );
    assert!(
        resolver
            .resolve_identifier("func1", SearchScope::All)
            .is_some_and(|decl| decl.unique == AstUniqueName::new("func1"))
    );
    assert!(
        resolver
            .resolve_identifier("func2", SearchScope::All)
            .is_some_and(|decl| decl.unique == AstUniqueName::new("func2"))
    );

    // Repeat declarations
    assert!(same_name(
        "my_global",
        resolver.add_file_scope_variable_declaration(&make_ident("my_global"), AstLinkage::External).unwrap()
    ));
    assert!(same_name(
        "some_var",
        resolver.add_file_scope_variable_declaration(&make_ident("some_var"), AstLinkage::External).unwrap()
    ));
    assert!(same_name("func1", resolver.add_function_declaration(&make_ident("func1"), AstLinkage::External).unwrap()));
    assert!(same_name("func2", resolver.add_function_declaration(&make_ident("func2"), AstLinkage::External).unwrap()));

    // Resolve declarations
    //      All these declarations are at file scope, and we haven't changed scope, so we should be
    //      able to resolve the identifers using all search scopes. Additionally, these declarations
    //      have linkage so they are not renamed.
    //
    let can_resolve = |identifier: &str, search: SearchScope| -> bool {
        resolver
            .resolve_identifier(identifier, search)
            .is_some_and(|decl| decl.unique == AstUniqueName::new(identifier))
    };

    assert!(can_resolve("my_global", SearchScope::All));
    assert!(can_resolve("my_global", SearchScope::File));
    assert!(can_resolve("my_global", SearchScope::Current));

    assert!(can_resolve("some_var", SearchScope::All));
    assert!(can_resolve("some_var", SearchScope::File));
    assert!(can_resolve("some_var", SearchScope::Current));

    assert!(can_resolve("func1", SearchScope::All));
    assert!(can_resolve("func1", SearchScope::File));
    assert!(can_resolve("func1", SearchScope::Current));

    assert!(can_resolve("func2", SearchScope::All));
    assert!(can_resolve("func2", SearchScope::File));
    assert!(can_resolve("func2", SearchScope::Current));
}

#[test]
fn declare_and_resolve_identifiers_without_linkage() {
    let mut resolver = IdentifierResolver::new();

    resolver.begin_scope(); // Not really needed for the test but makes semantic sense

    assert!(unique_name_differs(
        "a",
        resolver.add_block_scope_variable_declaration(&make_ident("a"), false, AstLinkage::None).unwrap()
    ));
    assert!(unique_name_differs(
        "b",
        resolver.add_block_scope_variable_declaration(&make_ident("b"), false, AstLinkage::None).unwrap()
    ));
    assert!(unique_name_differs(
        "c",
        resolver.add_block_scope_variable_declaration(&make_ident("c"), false, AstLinkage::None).unwrap()
    ));

    assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));

    assert!(resolver.resolve_identifier("a", SearchScope::File).is_none());
    assert!(resolver.resolve_identifier("b", SearchScope::File).is_none());
    assert!(resolver.resolve_identifier("c", SearchScope::File).is_none());
    assert!(resolver.resolve_identifier("MyInt>", SearchScope::File).is_none());

    assert!(
        resolver
            .resolve_identifier("a", SearchScope::Current)
            .is_some_and(|decl| decl.kind == SymbolKind::Variable && unique_name_differs("a", decl.unique.clone()))
    );

    assert!(
        resolver
            .resolve_identifier("b", SearchScope::Current)
            .is_some_and(|decl| decl.kind == SymbolKind::Variable && unique_name_differs("b", decl.unique.clone()))
    );

    assert!(
        resolver
            .resolve_identifier("c", SearchScope::Current)
            .is_some_and(|decl| decl.kind == SymbolKind::Variable && unique_name_differs("c", decl.unique.clone()))
    );

    assert!(resolver.resolve_identifier("MyInt", SearchScope::Current).is_some_and(|decl| decl.kind
        == SymbolKind::TypeAlias
        && unique_name_differs("MyInt", decl.unique.clone())));

    resolver.end_scope();
}

#[test]
fn scopes() {
    let mut resolver = IdentifierResolver::new();
    assert!(resolver.is_at_file_scope());

    assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));
    assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));
    assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));

    assert!(same_name(
        "a",
        resolver.add_file_scope_variable_declaration(&make_ident("a"), AstLinkage::External).unwrap()
    ));
    assert!(same_name(
        "b",
        resolver.add_file_scope_variable_declaration(&make_ident("b"), AstLinkage::External).unwrap()
    ));

    // Can repeat
    assert!(same_name(
        "x",
        resolver.add_file_scope_variable_declaration(&make_ident("x"), AstLinkage::External).unwrap()
    ));
    assert!(same_name(
        "x",
        resolver.add_file_scope_variable_declaration(&make_ident("x"), AstLinkage::External).unwrap()
    ));
    assert!(same_name(
        "x",
        resolver.add_file_scope_variable_declaration(&make_ident("x"), AstLinkage::External).unwrap()
    ));

    let myint_name_file_scope = resolver.resolve_identifier("MyInt", SearchScope::All).cloned().unwrap();
    let a_name_file_scope = resolver.resolve_identifier("a", SearchScope::All).cloned().unwrap();
    let b_name_file_scope = resolver.resolve_identifier("b", SearchScope::All).cloned().unwrap();
    let x_name_file_scope = resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap();

    assert!(resolver.resolve_identifier("MyInt", SearchScope::Current).is_some_and(|id| *id == myint_name_file_scope));
    assert!(resolver.resolve_identifier("a", SearchScope::Current).is_some_and(|id| *id == a_name_file_scope));
    assert!(resolver.resolve_identifier("b", SearchScope::Current).is_some_and(|id| *id == b_name_file_scope));
    assert!(resolver.resolve_identifier("x", SearchScope::Current).is_some_and(|id| *id == x_name_file_scope));

    resolver.begin_scope(); // We'll call this "scope 1", since file scope == scope zero.
    {
        // Can repeat identical type alias declarations
        assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));
        assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));
        assert!(unique_name_differs("MyInt", resolver.add_type_alias_declaration(&make_ident("MyInt")).unwrap()));

        // Local variables
        assert!(unique_name_differs(
            "x",
            resolver.add_block_scope_variable_declaration(&make_ident("x"), false, AstLinkage::None).unwrap()
        ));
        assert!(unique_name_differs(
            "y",
            resolver.add_block_scope_variable_declaration(&make_ident("y"), false, AstLinkage::None).unwrap()
        ));

        let myint_name_scope1 = resolver.resolve_identifier("MyInt", SearchScope::All).cloned().unwrap();
        let a_name_scope1 = resolver.resolve_identifier("a", SearchScope::All).cloned().unwrap();
        let b_name_scope1 = resolver.resolve_identifier("b", SearchScope::All).cloned().unwrap();
        let x_name_scope1 = resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap();
        let y_name_scope1 = resolver.resolve_identifier("y", SearchScope::All).cloned().unwrap();

        // The `MyInt` type alias and variables in the inner scope are different to the ones in the outer scope.
        assert_ne!(myint_name_file_scope, myint_name_scope1);
        assert_ne!(x_name_file_scope, x_name_scope1);

        // We haven't declared `a` or `b` in this inner scope, so the identifiers still refer to the ones in the outer scope.
        assert_eq!(a_name_file_scope, a_name_scope1);
        assert_eq!(b_name_file_scope, b_name_scope1);

        assert!(resolver.resolve_identifier("MyInt", SearchScope::Current).is_some_and(|id| *id == myint_name_scope1));
        assert!(resolver.resolve_identifier("a", SearchScope::Current).is_none());
        assert!(resolver.resolve_identifier("b", SearchScope::Current).is_none());
        assert!(resolver.resolve_identifier("x", SearchScope::Current).is_some_and(|id| *id == x_name_scope1));
        assert!(resolver.resolve_identifier("y", SearchScope::Current).is_some_and(|id| *id == y_name_scope1));

        resolver.begin_scope(); // We'll call this "scope 2"
        {
            assert_eq!(myint_name_scope1, resolver.resolve_identifier("MyInt", SearchScope::All).cloned().unwrap());
            assert_eq!(a_name_scope1, resolver.resolve_identifier("a", SearchScope::All).cloned().unwrap());
            assert_eq!(b_name_scope1, resolver.resolve_identifier("b", SearchScope::All).cloned().unwrap());
            assert_eq!(x_name_scope1, resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap());
            assert_eq!(y_name_scope1, resolver.resolve_identifier("y", SearchScope::All).cloned().unwrap());

            assert!(resolver.resolve_identifier("MyInt", SearchScope::Current).is_none());
            assert!(resolver.resolve_identifier("a", SearchScope::Current).is_none());
            assert!(resolver.resolve_identifier("b", SearchScope::Current).is_none());
            assert!(resolver.resolve_identifier("x", SearchScope::Current).is_none());
            assert!(resolver.resolve_identifier("y", SearchScope::Current).is_none());

            // Declare `x` and `y` in this scope.
            assert!(unique_name_differs(
                "x",
                resolver.add_block_scope_variable_declaration(&make_ident("x"), false, AstLinkage::None).unwrap()
            ));
            assert!(unique_name_differs(
                "y",
                resolver.add_block_scope_variable_declaration(&make_ident("y"), false, AstLinkage::None).unwrap()
            ));

            assert!(resolver.resolve_identifier("x", SearchScope::Current).is_some());
            assert!(resolver.resolve_identifier("y", SearchScope::Current).is_some());

            // They no longer match the identifiers in "scope 1" nor file scope.
            assert_ne!(x_name_scope1, resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap());
            assert_ne!(y_name_scope1, resolver.resolve_identifier("y", SearchScope::All).cloned().unwrap());
            assert_ne!(x_name_file_scope, resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap());

            // Can resolve the file scope identifiers if needed from the current scope
            assert!(
                resolver
                    .resolve_identifier("a", SearchScope::File)
                    .is_some_and(|decl| decl.unique == a_name_file_scope.unique)
            );
            assert!(
                resolver
                    .resolve_identifier("b", SearchScope::File)
                    .is_some_and(|decl| decl.unique == b_name_file_scope.unique)
            );
            assert!(
                resolver
                    .resolve_identifier("x", SearchScope::File)
                    .is_some_and(|decl| decl.unique == x_name_file_scope.unique)
            );
            assert!(
                resolver
                    .resolve_identifier("MyInt", SearchScope::File)
                    .is_some_and(|decl| decl.unique == myint_name_file_scope.unique)
            );
        }
        resolver.end_scope();

        // After the end of "scope 2" the identifiers resolve to their "scope 1" unique names.
        assert_eq!(myint_name_scope1, resolver.resolve_identifier("MyInt", SearchScope::All).cloned().unwrap());
        assert_eq!(a_name_scope1, resolver.resolve_identifier("a", SearchScope::All).cloned().unwrap());
        assert_eq!(b_name_scope1, resolver.resolve_identifier("b", SearchScope::All).cloned().unwrap());
        assert_eq!(x_name_scope1, resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap());
        assert_eq!(y_name_scope1, resolver.resolve_identifier("y", SearchScope::All).cloned().unwrap());
    }
    resolver.end_scope();

    // After the end of "scope 1" the identifiers resolve to their file scope unique names.
    assert_eq!(myint_name_file_scope, resolver.resolve_identifier("MyInt", SearchScope::All).cloned().unwrap());
    assert_eq!(a_name_file_scope, resolver.resolve_identifier("a", SearchScope::All).cloned().unwrap());
    assert_eq!(b_name_file_scope, resolver.resolve_identifier("b", SearchScope::All).cloned().unwrap());
    assert_eq!(x_name_file_scope, resolver.resolve_identifier("x", SearchScope::All).cloned().unwrap());
}

fn same_name(name: &str, unique_name: AstUniqueName) -> bool {
    unique_name == AstUniqueName::new(name)
}

fn unique_name_differs(name: &str, unique_name: AstUniqueName) -> bool {
    unique_name != AstUniqueName::new(name) && unique_name.len() > name.len()
}

fn make_ident(name: &str) -> AstIdentifier {
    AstIdentifier::new(name, SourceLocation::new(FilePosition::default(), 1))
}
