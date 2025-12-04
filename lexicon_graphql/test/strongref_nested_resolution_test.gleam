/// Tests for strongRef resolution in nested object types
///
/// Verifies that com.atproto.repo.strongRef refs in "others" object definitions
/// (like #replyRef) resolve to ComAtprotoRepoStrongRef, not String
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/graphql/object_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema

/// Test that strongRef fields in nested objects resolve to object type, not String
/// This reproduces the bug: app.bsky.feed.post#replyRef.parent should be
/// ComAtprotoRepoStrongRef, not String
pub fn strongref_in_nested_object_resolves_to_object_type_test() {
  // Create com.atproto.repo.strongRef lexicon (main-level object type)
  let strongref_lexicon =
    types.Lexicon(
      id: "com.atproto.repo.strongRef",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "uri",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
            #(
              "cid",
              types.Property(
                type_: "string",
                required: True,
                format: Some("cid"),
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

  // Create a post lexicon with #replyRef that references strongRef
  let post_lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: Some("tid"), properties: [
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
              types.ObjectDef(
                type_: "object",
                required_fields: ["parent", "root"],
                properties: [
                  #(
                    "parent",
                    types.Property(
                      type_: "ref",
                      required: True,
                      format: None,
                      ref: Some("com.atproto.repo.strongRef"),
                      refs: None,
                      items: None,
                    ),
                  ),
                  #(
                    "root",
                    types.Property(
                      type_: "ref",
                      required: True,
                      format: None,
                      ref: Some("com.atproto.repo.strongRef"),
                      refs: None,
                      items: None,
                    ),
                  ),
                ],
              ),
            ),
          ),
        ]),
      ),
    )

  // Build registry and object types
  let reg = registry.from_lexicons([strongref_lexicon, post_lexicon])
  let object_types = object_builder.build_all_object_types(reg, None, None)

  // The strongRef type should exist
  let strongref_type_result =
    dict.get(object_types, "com.atproto.repo.strongRef")
  should.be_ok(strongref_type_result)

  // The replyRef type should exist
  let reply_ref_result = dict.get(object_types, "app.bsky.feed.post#replyRef")
  should.be_ok(reply_ref_result)

  // Check the replyRef type's parent field is ComAtprotoRepoStrongRef, not String
  case reply_ref_result {
    Ok(reply_ref_type) -> {
      let type_name = schema.type_name(reply_ref_type)
      should.equal(type_name, "AppBskyFeedPostReplyRef")

      // Get the fields and find "parent"
      let fields = schema.get_fields(reply_ref_type)
      let parent_field =
        list.find(fields, fn(f) { schema.field_name(f) == "parent" })

      case parent_field {
        Ok(field) -> {
          // The field type should be ComAtprotoRepoStrongRef, not String
          let field_type = schema.field_type(field)

          // Unwrap NonNull wrapper to get inner type
          let inner_type = case schema.inner_type(field_type) {
            Some(t) -> t
            None -> field_type
          }

          let inner_type_name = schema.type_name(inner_type)

          // Should NOT be "String"
          should.not_equal(inner_type_name, "String")

          // Should be "ComAtprotoRepoStrongRef"
          should.equal(inner_type_name, "ComAtprotoRepoStrongRef")
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}
