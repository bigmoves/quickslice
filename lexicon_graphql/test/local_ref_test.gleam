/// Tests for local ref resolution in schema builder
///
/// Verifies that refs like "#replyRef" resolve to their object types
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should
import lexicon_graphql/internal/lexicon/parser
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

/// Test that refs within "others" object types resolve correctly.
/// This tests the pattern where an others object type has a field that refs another others type.
/// We need a record type to make the types reachable from Query.
pub fn nested_ref_in_others_resolves_test() {
  // A record type that references an "others" object type
  let record_lexicon =
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
              "entity",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("#entityRef"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "entityRef",
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
                  "mention",
                  types.Property(
                    type_: "ref",
                    required: False,
                    format: None,
                    ref: Some("#mentionRef"),
                    refs: None,
                    items: None,
                  ),
                ),
              ]),
            ),
          ),
          #(
            "mentionRef",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "did",
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

  let result = builder.build_schema([record_lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The entityRef type should exist
      string.contains(serialized, "type AppBskyFeedPostEntityRef")
      |> should.be_true

      // The mentionRef type should exist
      string.contains(serialized, "type AppBskyFeedPostMentionRef")
      |> should.be_true

      // The mention field in entityRef should be AppBskyFeedPostMentionRef, NOT String
      string.contains(serialized, "mention: AppBskyFeedPostMentionRef")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test cross-lexicon ref from record to another lexicon's others type
pub fn cross_lexicon_ref_to_others_test() {
  // Post lexicon references facet lexicon's main type
  let post_lexicon =
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
              "facets",
              types.Property(
                type_: "array",
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "ref",
                  ref: Some("app.bsky.richtext.facet"),
                  refs: None,
                )),
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let facet_lexicon =
    types.Lexicon(
      id: "app.bsky.richtext.facet",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "index",
              types.Property(
                type_: "ref",
                required: True,
                format: None,
                ref: Some("#byteSlice"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "byteSlice",
            types.Object(
              types.ObjectDef(type_: "object", required_fields: [], properties: [
                #(
                  "byteStart",
                  types.Property(
                    type_: "integer",
                    required: True,
                    format: None,
                    ref: None,
                    refs: None,
                    items: None,
                  ),
                ),
                #(
                  "byteEnd",
                  types.Property(
                    type_: "integer",
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

  let result = builder.build_schema([post_lexicon, facet_lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // The facet type should exist
      string.contains(serialized, "type AppBskyRichtextFacet")
      |> should.be_true

      // The byteSlice type should exist
      string.contains(serialized, "type AppBskyRichtextFacetByteSlice")
      |> should.be_true

      // facets field should be array of AppBskyRichtextFacet
      string.contains(serialized, "facets: [AppBskyRichtextFacet!]")
      |> should.be_true

      // index field in facet should be byteSlice type, NOT String
      // Note: we check without ! since required is based on ObjectDef.required_fields, not Property.required
      string.contains(serialized, "index: AppBskyRichtextFacetByteSlice")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Test with the REAL facet lexicon JSON to debug the actual issue
pub fn real_facet_lexicon_json_test() {
  let facet_json =
    "{
  \"lexicon\": 1,
  \"id\": \"app.bsky.richtext.facet\",
  \"defs\": {
    \"tag\": {
      \"type\": \"object\",
      \"required\": [\"tag\"],
      \"properties\": {
        \"tag\": {\"type\": \"string\"}
      }
    },
    \"link\": {
      \"type\": \"object\",
      \"required\": [\"uri\"],
      \"properties\": {
        \"uri\": {\"type\": \"string\", \"format\": \"uri\"}
      }
    },
    \"main\": {
      \"type\": \"object\",
      \"required\": [\"index\", \"features\"],
      \"properties\": {
        \"index\": {
          \"ref\": \"#byteSlice\",
          \"type\": \"ref\"
        },
        \"features\": {
          \"type\": \"array\",
          \"items\": {
            \"refs\": [\"#mention\", \"#link\", \"#tag\"],
            \"type\": \"union\"
          }
        }
      }
    },
    \"mention\": {
      \"type\": \"object\",
      \"required\": [\"did\"],
      \"properties\": {
        \"did\": {\"type\": \"string\", \"format\": \"did\"}
      }
    },
    \"byteSlice\": {
      \"type\": \"object\",
      \"required\": [\"byteStart\", \"byteEnd\"],
      \"properties\": {
        \"byteEnd\": {\"type\": \"integer\"},
        \"byteStart\": {\"type\": \"integer\"}
      }
    }
  }
}"

  // Parse the JSON
  let parsed = parser.parse_lexicon(facet_json)
  should.be_ok(parsed)

  case parsed {
    Ok(lexicon) -> {
      // Build the schema with a record that references the facet
      let post_lexicon =
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
                  "facets",
                  types.Property(
                    type_: "array",
                    required: False,
                    format: None,
                    ref: None,
                    refs: None,
                    items: Some(types.ArrayItems(
                      type_: "ref",
                      ref: Some("app.bsky.richtext.facet"),
                      refs: None,
                    )),
                  ),
                ),
              ]),
            ),
            others: dict.new(),
          ),
        )

      let result = builder.build_schema([post_lexicon, lexicon])
      should.be_ok(result)

      case result {
        Ok(schema_val) -> {
          let all_types = introspection.get_all_schema_types(schema_val)
          let serialized = sdl.print_types(all_types)

          // The facet type should exist
          string.contains(serialized, "type AppBskyRichtextFacet")
          |> should.be_true

          // The byteSlice type should exist
          string.contains(serialized, "type AppBskyRichtextFacetByteSlice")
          |> should.be_true

          // index field in facet should be byteSlice type, NOT String
          string.contains(serialized, "index: AppBskyRichtextFacetByteSlice")
          |> should.be_true
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
