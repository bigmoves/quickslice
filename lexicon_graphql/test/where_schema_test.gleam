/// Snapshot tests for WhereInput schema generation
///
/// Uses birdie to capture and verify the GraphQL schema types
/// generated for where input filtering

import birdie
import lexicon_graphql/connection
import gleeunit
import graphql/schema
import graphql/sdl

pub fn main() {
  gleeunit.main()
}

// ===== Simple Record Type =====

pub fn simple_post_where_input_snapshot_test() {
  // Simulate a simple post lexicon with basic fields
  let field_names = ["uri", "cid", "text", "createdAt"]

  let where_input_type =
    connection.build_where_input_type("AppBskyFeedPost", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Simple Post WhereInput with basic fields",
    content: serialized,
  )
}

// ===== Complex Record Type =====

pub fn complex_post_where_input_snapshot_test() {
  // Simulate a complex post with many fields
  let field_names = [
    "uri", "cid", "text", "createdAt", "indexedAt", "likeCount", "replyCount",
    "repostCount", "author",
  ]

  let where_input_type =
    connection.build_where_input_type("AppBskyFeedPost", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Complex Post WhereInput with many fields",
    content: serialized,
  )
}

// ===== Profile Record Type =====

pub fn profile_where_input_snapshot_test() {
  // Simulate actor profile lexicon
  let field_names = ["did", "handle", "displayName", "description", "avatar"]

  let where_input_type =
    connection.build_where_input_type("AppBskyActorProfile", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Profile WhereInput with actor fields",
    content: serialized,
  )
}

// ===== Empty Record (Edge Case) =====

pub fn empty_where_input_snapshot_test() {
  // Edge case: record with no filterable fields
  let field_names = []

  let where_input_type =
    connection.build_where_input_type("EmptyRecord", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "Empty WhereInput with only AND/OR fields",
    content: serialized,
  )
}

// ===== Field Condition Type =====

pub fn field_condition_snapshot_test() {
  // Test the FieldCondition type that defines operators
  let condition_type =
    connection.build_where_condition_input_type(
      "AppBskyFeedPost",
      schema.string_type(),
    )

  let serialized = sdl.print_type(condition_type)

  birdie.snap(
    title: "Field condition type with all operators",
    content: serialized,
  )
}

// ===== Related Types Together =====

pub fn related_types_snapshot_test() {
  // Test serializing both the WhereInput and its FieldCondition together
  let field_names = ["text", "createdAt", "likes"]

  let where_input_type =
    connection.build_where_input_type("TestRecord", field_names)

  let condition_type =
    connection.build_where_condition_input_type(
      "TestRecord",
      schema.string_type(),
    )

  let serialized =
    sdl.print_types([where_input_type, condition_type])

  birdie.snap(
    title: "WhereInput and FieldCondition types together",
    content: serialized,
  )
}

// ===== Different Record Types Have Different Names =====

pub fn multiple_record_types_snapshot_test() {
  // Verify different record types generate different WhereInput names
  let post_where =
    connection.build_where_input_type("AppBskyFeedPost", [
      "text",
      "createdAt",
    ])

  let profile_where =
    connection.build_where_input_type("AppBskyActorProfile", [
      "displayName",
      "description",
    ])

  let serialized =
    sdl.print_types([post_where, profile_where])

  birdie.snap(
    title: "Multiple record types with different WhereInput names",
    content: serialized,
  )
}

// ===== Verify Recursive AND/OR Fields =====

pub fn recursive_and_or_fields_snapshot_test() {
  // This test specifically highlights the recursive AND/OR structure
  let field_names = ["name", "status"]

  let where_input_type =
    connection.build_where_input_type("RecursiveTest", field_names)

  let serialized = sdl.print_type(where_input_type)

  birdie.snap(
    title: "WhereInput showing recursive AND/OR fields",
    content: serialized,
  )
}
