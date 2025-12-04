/// Shared Types
///
/// Common type definitions used across lexicon_graphql modules.
/// This module exists to break import cycles between schema_builder and mutation_builder.
import gleam/dict.{type Dict}
import gleam/option.{type Option}

/// Lexicon definition structure (simplified)
pub type Lexicon {
  Lexicon(id: String, defs: Defs)
}

/// Lexicon definitions container
/// Contains an optional main record definition and any other named definitions (e.g., object types)
/// Some lexicons (like social.grain.defs) only contain helper object types, not a main record
pub type Defs {
  Defs(main: Option(RecordDef), others: Dict(String, Def))
}

/// A definition can be either a record or an object
pub type Def {
  Record(RecordDef)
  Object(ObjectDef)
}

/// Record definition (a collection/record type)
pub type RecordDef {
  RecordDef(
    type_: String,
    key: Option(String),
    properties: List(#(String, Property)),
  )
}

/// Object definition (a nested object type like aspectRatio)
pub type ObjectDef {
  ObjectDef(
    type_: String,
    required_fields: List(String),
    properties: List(#(String, Property)),
  )
}

/// Property definition
pub type Property {
  Property(
    type_: String,
    required: Bool,
    format: Option(String),
    ref: Option(String),
    refs: Option(List(String)),
    items: Option(ArrayItems),
  )
}

/// Array items definition (for array properties)
pub type ArrayItems {
  ArrayItems(type_: String, ref: Option(String), refs: Option(List(String)))
}
