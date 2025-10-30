/// Shared Types
///
/// Common type definitions used across lexicon_graphql modules.
/// This module exists to break import cycles between schema_builder and mutation_builder.

/// Lexicon definition structure (simplified)
pub type Lexicon {
  Lexicon(id: String, defs: Defs)
}

/// Lexicon definitions container
pub type Defs {
  Defs(main: RecordDef)
}

/// Record definition
pub type RecordDef {
  RecordDef(type_: String, properties: List(#(String, Property)))
}

/// Property definition
pub type Property {
  Property(type_: String, required: Bool)
}
