/// Tests for nested forward join resolution in object_builder
///
/// Verifies that object types containing strongRef fields get *Resolved fields
/// when batch_fetcher and generic_record_type are provided
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/graphql/object_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema

/// Test that nested strongRef fields get *Resolved fields when batch_fetcher is provided
pub fn nested_strongref_gets_resolved_field_test() {
  // Create a lexicon with an object type containing strongRef fields
  let lexicon =
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

  // Build a registry from the lexicon
  let reg = registry.from_lexicons([lexicon])

  // Create a mock Record union type (the generic type for *Resolved fields)
  let mock_record_type = schema.object_type("Record", "Mock record union", [])

  // Create a mock batch fetcher that returns empty results
  let mock_batch_fetcher = fn(_uris, _collection, _field) { Ok(dict.new()) }

  // Build object types WITH batch_fetcher and generic_record_type
  let object_types =
    object_builder.build_all_object_types(
      reg,
      Some(mock_batch_fetcher),
      Some(mock_record_type),
    )

  // The replyRef object type should exist
  let assert Ok(reply_ref_type) =
    dict.get(object_types, "app.bsky.feed.post#replyRef")

  // Get the fields from the type
  let fields = schema.get_fields(reply_ref_type)
  let field_names = list.map(fields, schema.field_name)

  // Should have parentResolved and rootResolved fields
  list.contains(field_names, "parentResolved")
  |> should.be_true

  list.contains(field_names, "rootResolved")
  |> should.be_true
}

/// Test that at-uri format fields in nested objects also get *Resolved fields
pub fn nested_at_uri_gets_resolved_field_test() {
  let lexicon =
    types.Lexicon(
      id: "test.record",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: Some("tid"), properties: []),
        ),
        others: dict.from_list([
          #(
            "refObject",
            types.Object(
              types.ObjectDef(
                type_: "object",
                required_fields: ["target"],
                properties: [
                  #(
                    "target",
                    types.Property(
                      type_: "string",
                      required: True,
                      format: Some("at-uri"),
                      ref: None,
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

  // Build a registry from the lexicon
  let reg = registry.from_lexicons([lexicon])

  // Create mock types
  let mock_record_type = schema.object_type("Record", "Mock record union", [])
  let mock_batch_fetcher = fn(_uris, _collection, _field) { Ok(dict.new()) }

  // Build object types WITH batch_fetcher and generic_record_type
  let object_types =
    object_builder.build_all_object_types(
      reg,
      Some(mock_batch_fetcher),
      Some(mock_record_type),
    )

  // The refObject type should exist
  let assert Ok(ref_object_type) =
    dict.get(object_types, "test.record#refObject")

  // Get the fields from the type
  let fields = schema.get_fields(ref_object_type)
  let field_names = list.map(fields, schema.field_name)

  // Should have targetResolved field
  list.contains(field_names, "targetResolved")
  |> should.be_true
}

/// Test that *Resolved fields are NOT added when batch_fetcher is None
pub fn no_resolved_fields_without_batch_fetcher_test() {
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.post",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: Some("tid"), properties: []),
        ),
        others: dict.from_list([
          #(
            "replyRef",
            types.Object(
              types.ObjectDef(
                type_: "object",
                required_fields: ["parent"],
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
                ],
              ),
            ),
          ),
        ]),
      ),
    )

  let reg = registry.from_lexicons([lexicon])

  // Build object types WITHOUT batch_fetcher (None, None)
  let object_types = object_builder.build_all_object_types(reg, None, None)

  let assert Ok(reply_ref_type) =
    dict.get(object_types, "app.bsky.feed.post#replyRef")

  let fields = schema.get_fields(reply_ref_type)
  let field_names = list.map(fields, schema.field_name)

  // Should NOT have parentResolved field (no batch_fetcher)
  list.contains(field_names, "parentResolved")
  |> should.be_false

  // Should still have the regular parent field
  list.contains(field_names, "parent")
  |> should.be_true
}
