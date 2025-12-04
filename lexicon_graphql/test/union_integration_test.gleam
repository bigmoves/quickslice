/// Integration tests for union type support with real Bluesky lexicons
import gleam/dict
import gleam/option
import gleam/string
import gleeunit/should
import lexicon_graphql/internal/lexicon/parser as lexicon_parser
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

/// Test parsing real Bluesky post lexicon with embed union field
pub fn bluesky_post_embed_union_test() {
  // This is a simplified version of the real app.bsky.feed.post lexicon
  let post_json =
    "{
      \"lexicon\": 1,
      \"id\": \"app.bsky.feed.post\",
      \"defs\": {
        \"main\": {
          \"type\": \"record\",
          \"record\": {
            \"type\": \"object\",
            \"required\": [\"text\", \"createdAt\"],
            \"properties\": {
              \"text\": {\"type\": \"string\"},
              \"createdAt\": {\"type\": \"string\", \"format\": \"datetime\"},
              \"embed\": {
                \"type\": \"union\",
                \"refs\": [
                  \"app.bsky.embed.images\",
                  \"app.bsky.embed.video\",
                  \"app.bsky.embed.external\"
                ]
              }
            }
          }
        }
      }
    }"

  let result = lexicon_parser.parse_lexicon(post_json)
  should.be_ok(result)

  case result {
    Ok(lexicon) -> {
      // Verify embed field has refs parsed
      case lexicon.defs.main {
        option.Some(record_def) -> {
          case
            record_def.properties
            |> find_property("embed")
          {
            option.Some(embed_prop) -> {
              should.equal(embed_prop.type_, "union")
              should.equal(
                embed_prop.refs,
                option.Some([
                  "app.bsky.embed.images",
                  "app.bsky.embed.video",
                  "app.bsky.embed.external",
                ]),
              )
            }
            option.None -> should.fail()
          }
        }
        option.None -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Test full pipeline: build schema with multiple embed types and union field
/// This tests the complete flow from lexicon types to GraphQL schema
pub fn full_pipeline_union_type_test() {
  // Create embed type lexicons as object types
  let embed_images_lexicon =
    types.Lexicon(
      id: "app.bsky.embed.images",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "object", key: option.None, properties: [
            #(
              "images",
              types.Property(
                type_: "array",
                required: True,
                format: option.None,
                ref: option.None,
                refs: option.None,
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

  let embed_video_lexicon =
    types.Lexicon(
      id: "app.bsky.embed.video",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "object", key: option.None, properties: [
            #(
              "video",
              types.Property(
                type_: "blob",
                required: True,
                format: option.None,
                ref: option.None,
                refs: option.None,
                items: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Create post lexicon with union property referencing both embed types
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: option.Some(
          types.RecordDef(type_: "record", key: option.Some("tid"), properties: [
            #(
              "text",
              types.Property(
                type_: "string",
                required: True,
                format: option.None,
                ref: option.None,
                refs: option.None,
                items: option.None,
              ),
            ),
            #(
              "embed",
              types.Property(
                type_: "union",
                required: False,
                format: option.None,
                ref: option.None,
                refs: option.Some([
                  "app.bsky.embed.images",
                  "app.bsky.embed.video",
                ]),
                items: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  // Build schema with all lexicons
  let result =
    builder.build_schema([
      embed_images_lexicon,
      embed_video_lexicon,
      post_lexicon,
    ])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // Verify union type was generated with correct name
      string.contains(serialized, "union AppBskyFeedPostEmbed")
      |> should.be_true

      // Verify both embed object types exist
      string.contains(serialized, "AppBskyEmbedImages")
      |> should.be_true

      string.contains(serialized, "AppBskyEmbedVideo")
      |> should.be_true

      // Verify post type exists and has embed field
      string.contains(serialized, "type AppBskyFeedPost")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

/// Helper to find a property by name
fn find_property(
  properties: List(#(String, a)),
  name: String,
) -> option.Option(a) {
  case properties {
    [] -> option.None
    [#(prop_name, prop), ..rest] ->
      case prop_name == name {
        True -> option.Some(prop)
        False -> find_property(rest, name)
      }
  }
}
