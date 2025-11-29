/// Tests for array type mapping in schema generation
import gleam/dict
import gleam/option
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder as schema_builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

pub fn array_field_generates_list_type_test() {
  let lexicon =
    types.Lexicon(
      id: "fm.teal.alpha.feed.track",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "artistNames",
              types.Property(
                type_: "array",
                required: False,
                format: option.None,
                ref: option.None,
                items: option.Some(types.ArrayItems(
                  type_: "string",
                  ref: option.None,
                  refs: option.None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  case schema_builder.build_schema([lexicon]) {
    Ok(test_schema) -> {
      let all_types = introspection.get_all_schema_types(test_schema)
      let serialized = sdl.print_types(all_types)

      // Should contain artistNames field with list type
      string.contains(serialized, "artistNames: [String!]")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

pub fn array_of_refs_generates_object_list_type_test() {
  // Create a lexicon with both a record (main) and an object def (artist)
  // The record has an array field that refs the object def
  let lexicon =
    types.Lexicon(
      id: "fm.teal.alpha.feed.track",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "artists",
              types.Property(
                type_: "array",
                required: False,
                format: option.None,
                ref: option.None,
                items: option.Some(types.ArrayItems(
                  type_: "ref",
                  ref: option.Some("fm.teal.alpha.feed.track#artist"),
                  refs: option.None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "artist",
            types.Object(
              types.ObjectDef(
                type_: "object",
                required_fields: ["name"],
                properties: [
                  #(
                    "name",
                    types.Property(
                      type_: "string",
                      required: True,
                      format: option.None,
                      ref: option.None,
                      items: option.None,
                    ),
                  ),
                ],
              ),
            ),
          ),
        ]),
      ),
    )

  case schema_builder.build_schema([lexicon]) {
    Ok(test_schema) -> {
      let all_types = introspection.get_all_schema_types(test_schema)
      let serialized = sdl.print_types(all_types)

      // Should contain artists field with object type list, NOT [String!]
      string.contains(serialized, "artists: [FmTealAlphaFeedTrackArtist!]")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
