/// Tests for array ref expansion in object types
///
/// Tests that array fields with refs properly resolve to object types.
import gleam/dict
import gleam/option
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

/// Test that array fields in records with refs to object defs resolve correctly.
/// This mirrors the pattern in array_type_test but verifies the full ref path.
pub fn array_in_record_with_ref_to_object_test() {
  // Similar to the existing array_of_refs_generates_object_list_type_test
  // but uses a different lexicon to verify our changes work
  let lexicon =
    types.Lexicon(
      id: "app.bsky.embed.images",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "images",
              types.Property(
                type_: "array",
                required: True,
                format: option.None,
                ref: option.None,
                refs: option.None,
                items: option.Some(types.ArrayItems(
                  type_: "ref",
                  ref: option.Some("app.bsky.embed.images#image"),
                  refs: option.None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "image",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "alt",
                  types.Property(
                    type_: "string",
                    required: False,
                    format: option.None,
                    ref: option.None,
                    refs: option.None,
                    items: option.None,
                  ),
                ),
              ]),
            ),
          ),
        ]),
      ),
    )

  let result = builder.build_schema([lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The image type should exist
      string.contains(serialized, "type AppBskyEmbedImagesImage")
      |> should.be_true

      // The image type should have the alt field
      string.contains(serialized, "alt: String")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test that object types in "others" get properly built with their fields.
/// This verifies the object_builder creates types from ObjectDef definitions.
pub fn object_def_in_others_builds_correctly_test() {
  // Create a record that references an object def in others
  let lexicon =
    types.Lexicon(
      id: "social.grain.example",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.None, properties: [
            #(
              "ratio",
              types.Property(
                type_: "ref",
                required: False,
                format: option.None,
                ref: option.Some("social.grain.example#aspectRatio"),
                refs: option.None,
                items: option.None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "aspectRatio",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "width",
                  types.Property(
                    type_: "integer",
                    required: True,
                    format: option.None,
                    ref: option.None,
                    refs: option.None,
                    items: option.None,
                  ),
                ),
                #(
                  "height",
                  types.Property(
                    type_: "integer",
                    required: True,
                    format: option.None,
                    ref: option.None,
                    refs: option.None,
                    items: option.None,
                  ),
                ),
              ]),
            ),
          ),
        ]),
      ),
    )

  let result = builder.build_schema([lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The aspectRatio type should exist
      string.contains(serialized, "SocialGrainExampleAspectRatio")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
