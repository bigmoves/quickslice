/// Tests for notifications schema generation
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/schema/database
import lexicon_graphql/types

pub fn builds_record_collection_enum_test() {
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
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let enum_result = database.build_record_collection_enum([lexicon])

  enum_result |> should.be_ok()
}

pub fn builds_record_collection_enum_from_multiple_lexicons_test() {
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
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "ref",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let enum_result =
    database.build_record_collection_enum([post_lexicon, like_lexicon])

  enum_result |> should.be_ok()
}

pub fn nsid_to_enum_value_converts_correctly_test() {
  database.nsid_to_enum_value("app.bsky.feed.post")
  |> should.equal("APP_BSKY_FEED_POST")

  database.nsid_to_enum_value("app.bsky.graph.follow")
  |> should.equal("APP_BSKY_GRAPH_FOLLOW")
}

pub fn enum_value_to_nsid_converts_correctly_test() {
  database.enum_value_to_nsid("APP_BSKY_FEED_POST")
  |> should.equal("app.bsky.feed.post")

  database.enum_value_to_nsid("APP_BSKY_GRAPH_FOLLOW")
  |> should.equal("app.bsky.graph.follow")
}

pub fn builds_notification_record_union_test() {
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
                required: False,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let like_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "ref",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let union_result =
    database.build_notification_record_union([post_lexicon, like_lexicon])

  union_result |> should.be_ok()
}
