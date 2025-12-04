/// Tests for object-type lexicon GraphQL generation
import gleam/dict
import gleam/option
import gleam/string
import gleeunit/should
import lexicon_graphql/schema/builder
import lexicon_graphql/types
import swell/introspection
import swell/sdl

pub fn object_type_lexicon_referenced_from_record_test() {
  // Create an object-type lexicon (like app.bsky.embed.images)
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

  // Create a record that references the object type
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
                type_: "ref",
                required: False,
                format: option.None,
                ref: option.Some("app.bsky.embed.images"),
                refs: option.None,
                items: option.None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let result = builder.build_schema([embed_images_lexicon, post_lexicon])
  should.be_ok(result)

  case result {
    Ok(schema_val) -> {
      // The type should exist in the schema
      let all_types = introspection.get_all_schema_types(schema_val)
      let serialized = sdl.print_types(all_types)

      // AppBskyEmbedImages should be in the types (referenced from Post.embed)
      string.contains(serialized, "AppBskyEmbedImages")
      |> should.be_true

      // Post type should have embed field
      string.contains(serialized, "embed: AppBskyEmbedImages")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
