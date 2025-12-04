/// Tests for local ref resolution in schema builder
///
/// Verifies that refs like "#replyRef" resolve to their object types
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

/// Test that a local ref (starting with #) resolves to its object type
pub fn local_ref_resolves_to_object_type_test() {
  // Create a lexicon with a record that has a local ref field
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "reply",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("#replyRef"),
                // Local ref!
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "replyRef",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "root",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "parent",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
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

      // The replyRef object type should exist
      string.contains(serialized, "type AppBskyFeedPostReplyRef")
      |> should.be_true

      // The reply field should be AppBskyFeedPostReplyRef, NOT String
      string.contains(serialized, "reply: AppBskyFeedPostReplyRef")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test that local refs in arrays also resolve correctly
pub fn local_ref_in_array_resolves_to_object_type_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "entities",
              types.Property(
                type_: "array",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "ref",
                  ref: Some("#entity"),
                  // Local ref in array!
                  refs: None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "entity",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "type",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "value",
                  types.Property(
                    type_: "string",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
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

      // The entity object type should exist
      string.contains(serialized, "type AppBskyFeedPostEntity")
      |> should.be_true

      // The entities field should be [AppBskyFeedPostEntity!], NOT [String!]
      string.contains(serialized, "entities: [AppBskyFeedPostEntity!]")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
