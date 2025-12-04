/// Tests for object type build order
///
/// Verifies that #fragment refs are built before main object types
/// so that unions in main types can resolve their member types
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/graphql/object_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema

/// Test that main object types with union array fields resolve correctly
/// when the union members are #fragment refs in the same lexicon
pub fn union_array_refs_resolve_to_object_types_test() {
  // Create a lexicon like app.bsky.richtext.facet with:
  // - main object type with features: array of union [#mention, #link]
  // - others: mention, link object definitions
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

  // The main type should exist
  let main_type_result = dict.get(object_types, "app.bsky.richtext.facet")
  should.be_ok(main_type_result)

  // The #fragment types should exist
  let mention_type_result =
    dict.get(object_types, "app.bsky.richtext.facet#mention")
  should.be_ok(mention_type_result)

  let link_type_result = dict.get(object_types, "app.bsky.richtext.facet#link")
  should.be_ok(link_type_result)

  // Check the main type's features field is a union, not String
  case main_type_result {
    Ok(main_type) -> {
      let type_name = schema.type_name(main_type)
      should.equal(type_name, "AppBskyRichtextFacet")

      // Get the fields and find "features"
      let fields = schema.get_fields(main_type)
      let features_field =
        list.find(fields, fn(f) { schema.field_name(f) == "features" })

      case features_field {
        Ok(field) -> {
          // The field type should be a list containing a union, not [String!]
          let field_type = schema.field_type(field)
          let inner_type_name = get_list_inner_type_name(field_type)

          // Should NOT be "String" - should be a union type name
          should.be_false(inner_type_name == "String")

          // With new naming convention: {ParentType}{CapitalizedField}Union
          // Should be "AppBskyRichtextFacetFeaturesUnion"
          should.equal(inner_type_name, "AppBskyRichtextFacetFeaturesUnion")
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

/// Helper to get the inner type name from a list type
/// NonNull[List[NonNull[Union]]] -> "UnionName"
fn get_list_inner_type_name(t: schema.Type) -> String {
  // Unwrap NonNull -> List -> NonNull -> actual type
  case schema.inner_type(t) {
    Some(list_type) ->
      case schema.inner_type(list_type) {
        Some(inner_nonnull) ->
          case schema.inner_type(inner_nonnull) {
            Some(union_type) -> schema.type_name(union_type)
            None -> schema.type_name(inner_nonnull)
          }
        None -> schema.type_name(list_type)
      }
    None -> schema.type_name(t)
  }
}
