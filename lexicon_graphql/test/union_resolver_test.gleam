import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/graphql/object_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema
import swell/value

/// Test the union type resolver with actual $type data
pub fn union_type_resolver_works_test() {
  // Create a lexicon like app.bsky.richtext.facet
  let lexicon =
    types.Lexicon(
      id: "app.bsky.richtext.facet",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "object", key: None, properties: [
            #(
              "features",
              types.Property(
                type_: "array",
                required: True,
                format: None,
                ref: None,
                refs: None,
                items: Some(types.ArrayItems(
                  type_: "union",
                  ref: None,
                  refs: Some(["#mention", "#link"]),
                )),
              ),
            ),
          ]),
        ),
        others: dict.from_list([
          #(
            "mention",
            types.Object(
              types.ObjectDef(
                type_: "object",
                required_fields: ["did"],
                properties: [
                  #(
                    "did",
                    types.Property(
                      type_: "string",
                      required: True,
                      format: Some("did"),
                      ref: None,
                      refs: None,
                      items: None,
                    ),
                  ),
                ],
              ),
            ),
          ),
          #(
            "link",
            types.Object(
              types.ObjectDef(
                type_: "object",
                required_fields: ["uri"],
                properties: [
                  #(
                    "uri",
                    types.Property(
                      type_: "string",
                      required: True,
                      format: Some("uri"),
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

  // Build registry and object types
  let reg = registry.from_lexicons([lexicon])
  let object_types = object_builder.build_all_object_types(reg)

  // Get the main facet type
  let assert Ok(main_type) = dict.get(object_types, "app.bsky.richtext.facet")

  // Get the features field
  let fields = schema.get_fields(main_type)
  let assert Ok(features_field) =
    list.find(fields, fn(f) { schema.field_name(f) == "features" })

  // Get the field type and unwrap to the union
  // Field type is NonNull[List[NonNull[Union]]]
  let field_type = schema.field_type(features_field)

  // Unwrap NonNull -> List -> NonNull -> Union
  let assert Some(list_type) = schema.inner_type(field_type)
  let assert Some(inner_nonnull) = schema.inner_type(list_type)
  let assert Some(union_type) = schema.inner_type(inner_nonnull)

  // Verify it's a union
  should.be_true(schema.is_union(union_type))

  // Now test the type resolver with mock data that has $type
  let mention_data =
    value.Object([
      #("$type", value.String("app.bsky.richtext.facet#mention")),
      #("did", value.String("did:plc:abc123")),
    ])

  let ctx = schema.context(Some(mention_data))

  // Try to resolve the union type
  let assert Ok(resolved_type) = schema.resolve_union_type(union_type, ctx)
  should.equal(schema.type_name(resolved_type), "AppBskyRichtextFacetMention")
}
