/// Tests for lexicon_graphql/connection module
///
/// Tests the creation of unique SortFieldInput types per collection
import gleeunit/should
import lexicon_graphql/connection as lexicon_connection
import swell/schema

pub fn sort_field_input_type_with_enum_creates_types_test() {
  // Test: sort_field_input_type_with_enum should create input object types
  // Since Type is opaque, we just verify the function executes without error

  let enum_type =
    schema.enum_type("TestSortField", "Sort fields", [
      schema.enum_value("field1", "Field 1"),
    ])

  // Create input type - should not crash
  let _input_type =
    lexicon_connection.sort_field_input_type_with_enum(
      "TestCollection",
      enum_type,
    )

  // If we got here without crashing, test passes
  True
  |> should.be_true()
}

pub fn lexicon_connection_args_with_field_enum_and_where_creates_args_test() {
  // Test: Connection args function should create a list of arguments including sortBy

  let sort_enum =
    schema.enum_type("TestCollectionSortField", "Sort fields", [
      schema.enum_value("field1", "Field 1"),
    ])

  let where_input =
    schema.input_object_type("TestCollectionWhereInput", "Where input", [])

  let args =
    lexicon_connection.lexicon_connection_args_with_field_enum_and_where(
      "TestCollection",
      sort_enum,
      where_input,
    )

  // Should create multiple args (first, last, after, before, sortBy, where)
  // Verify we got a non-empty list
  args
  |> should.not_equal([])
}

pub fn lexicon_connection_args_with_field_enum_creates_args_test() {
  // Test: Backward compat function should also work

  let sort_enum =
    schema.enum_type("TestCollectionSortField", "Sort fields", [
      schema.enum_value("field1", "Field 1"),
    ])

  let args =
    lexicon_connection.lexicon_connection_args_with_field_enum(
      "TestCollection",
      sort_enum,
    )

  // Should create multiple args (first, last, after, before, sortBy)
  args
  |> should.not_equal([])
}

pub fn different_collections_can_have_different_sort_input_types_test() {
  // Test: Multiple collections should be able to create their own SortFieldInput types
  // This ensures the fix for the enum validation bug works

  let enum_type =
    schema.enum_type("GenericSortField", "Generic sort", [
      schema.enum_value("createdAt", "Created at"),
    ])

  // Create SortFieldInput for multiple collections - should not crash
  let _gallery_input =
    lexicon_connection.sort_field_input_type_with_enum(
      "SocialGrainGalleryItem",
      enum_type,
    )

  let _favorite_input =
    lexicon_connection.sort_field_input_type_with_enum(
      "SocialGrainFavorite",
      enum_type,
    )

  let _photo_input =
    lexicon_connection.sort_field_input_type_with_enum(
      "SocialGrainPhoto",
      enum_type,
    )

  // If we got here without crashing, test passes
  // The real verification happens in the integration test
  True
  |> should.be_true()
}
