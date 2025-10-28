/// GraphQL Value types
///
/// Per GraphQL spec Section 2 - Language, values can be scalars, enums,
/// lists, or objects. This module defines the core Value type used throughout
/// the GraphQL implementation.
/// A GraphQL value that can be used in queries, responses, and variables
pub type Value {
  /// Represents null/absence of a value
  Null

  /// Integer value (32-bit signed integer per spec)
  Int(Int)

  /// Floating point value (IEEE 754 double precision per spec)
  Float(Float)

  /// UTF-8 string value
  String(String)

  /// Boolean true or false
  Boolean(Bool)

  /// Enum value represented as a string (e.g., "ACTIVE", "PENDING")
  Enum(String)

  /// Ordered list of values
  List(List(Value))

  /// Unordered set of key-value pairs
  /// Using list of tuples for simplicity and ordering preservation
  Object(List(#(String, Value)))
}
