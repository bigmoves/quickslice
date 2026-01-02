/// Tests for Union Input Builder
///
/// Tests the generation of GraphQL input types for AT Protocol union fields
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/graphql/union_input_builder
import lexicon_graphql/internal/lexicon/registry
import lexicon_graphql/types
import swell/schema

// ─── ref_to_input_type_name Tests ──────────────────────────────────

pub fn ref_to_input_type_name_with_hash_test() {
  union_input_builder.ref_to_input_type_name(
    "com.atproto.label.defs#selfLabels",
  )
  |> should.equal("ComAtprotoLabelDefsSelfLabelsInput")
}

pub fn ref_to_input_type_name_without_hash_test() {
  union_input_builder.ref_to_input_type_name("com.atproto.label.defs")
  |> should.equal("ComAtprotoLabelDefsInput")
}

// ─── ref_to_variant_enum_value Tests ───────────────────────────────

pub fn ref_to_variant_enum_value_with_hash_test() {
  union_input_builder.ref_to_variant_enum_value(
    "com.atproto.label.defs#selfLabels",
  )
  |> should.equal("SELF_LABELS")
}

pub fn ref_to_variant_enum_value_camel_case_test() {
  union_input_builder.ref_to_variant_enum_value(
    "com.example.defs#myVariantType",
  )
  |> should.equal("MY_VARIANT_TYPE")
}

pub fn ref_to_variant_enum_value_single_word_test() {
  union_input_builder.ref_to_variant_enum_value("com.example.defs#labels")
  |> should.equal("LABELS")
}

pub fn ref_to_variant_enum_value_with_numbers_test() {
  // Numbers are not treated as word boundaries
  union_input_builder.ref_to_variant_enum_value("com.example.defs#oauth2Client")
  |> should.equal("OAUTH2_CLIENT")
}

// ─── ref_to_variant_field_name Tests ───────────────────────────────

pub fn ref_to_variant_field_name_with_hash_test() {
  union_input_builder.ref_to_variant_field_name(
    "com.atproto.label.defs#selfLabels",
  )
  |> should.equal("selfLabels")
}

pub fn ref_to_variant_field_name_without_hash_test() {
  union_input_builder.ref_to_variant_field_name("com.atproto.label.defs")
  |> should.equal("defs")
}

// ─── enum_value_to_short_name Tests ────────────────────────────────

pub fn enum_value_to_short_name_multi_word_test() {
  union_input_builder.enum_value_to_short_name("SELF_LABELS")
  |> should.equal("selfLabels")
}

pub fn enum_value_to_short_name_single_word_test() {
  union_input_builder.enum_value_to_short_name("LABELS")
  |> should.equal("labels")
}

pub fn enum_value_to_short_name_three_words_test() {
  union_input_builder.enum_value_to_short_name("MY_VARIANT_TYPE")
  |> should.equal("myVariantType")
}

pub fn enum_value_to_short_name_with_numbers_test() {
  // Numbers are preserved correctly
  union_input_builder.enum_value_to_short_name("OAUTH2_CLIENT")
  |> should.equal("oauth2Client")
}

// ─── Round-trip Conversion Tests ──────────────────────────────────

pub fn round_trip_camel_to_snake_and_back_test() {
  // Convert camelCase -> SCREAMING_SNAKE -> camelCase
  let original = "selfLabels"
  let snake =
    union_input_builder.ref_to_variant_enum_value("com.example#" <> original)
  union_input_builder.enum_value_to_short_name(snake)
  |> should.equal(original)
}

pub fn round_trip_with_multiple_words_test() {
  let original = "myVariantType"
  let snake =
    union_input_builder.ref_to_variant_enum_value("com.example#" <> original)
  union_input_builder.enum_value_to_short_name(snake)
  |> should.equal(original)
}

pub fn round_trip_with_numbers_test() {
  let original = "oauth2Client"
  let snake =
    union_input_builder.ref_to_variant_enum_value("com.example#" <> original)
  union_input_builder.enum_value_to_short_name(snake)
  |> should.equal(original)
}

// ─── is_multi_variant_union Tests ──────────────────────────────────

pub fn is_multi_variant_union_with_two_refs_test() {
  union_input_builder.is_multi_variant_union(Some(["ref1", "ref2"]))
  |> should.be_true()
}

pub fn is_multi_variant_union_with_one_ref_test() {
  union_input_builder.is_multi_variant_union(Some(["ref1"]))
  |> should.be_false()
}

pub fn is_multi_variant_union_with_none_test() {
  union_input_builder.is_multi_variant_union(None)
  |> should.be_false()
}

pub fn is_multi_variant_union_with_empty_list_test() {
  union_input_builder.is_multi_variant_union(Some([]))
  |> should.be_false()
}

// ─── enum_value_to_ref Tests ───────────────────────────────────────

pub fn enum_value_to_ref_finds_match_test() {
  let refs = [
    "com.atproto.label.defs#selfLabels",
    "com.example.defs#otherType",
  ]

  union_input_builder.enum_value_to_ref("SELF_LABELS", refs)
  |> should.equal(Some("com.atproto.label.defs#selfLabels"))
}

pub fn enum_value_to_ref_no_match_test() {
  let refs = [
    "com.atproto.label.defs#selfLabels",
    "com.example.defs#otherType",
  ]

  union_input_builder.enum_value_to_ref("UNKNOWN_TYPE", refs)
  |> should.equal(None)
}

// ─── UnionRegistry Tests ───────────────────────────────────────────

pub fn register_union_field_stores_refs_test() {
  let initial_registry =
    union_input_builder.UnionRegistry(
      input_types: dict.new(),
      field_variants: dict.new(),
    )

  let updated_registry =
    union_input_builder.register_union_field(
      initial_registry,
      "social.grain.gallery",
      "labels",
      ["com.atproto.label.defs#selfLabels"],
    )

  union_input_builder.get_union_refs(
    updated_registry,
    "social.grain.gallery",
    "labels",
  )
  |> should.equal(Some(["com.atproto.label.defs#selfLabels"]))
}

pub fn get_union_refs_returns_none_for_missing_field_test() {
  let empty_registry =
    union_input_builder.UnionRegistry(
      input_types: dict.new(),
      field_variants: dict.new(),
    )

  union_input_builder.get_union_refs(empty_registry, "unknown", "field")
  |> should.equal(None)
}

// ─── build_multi_variant_union_input Tests ─────────────────────────

pub fn build_multi_variant_union_input_creates_type_with_discriminator_test() {
  let refs = [
    "com.atproto.label.defs#selfLabels",
    "com.example.defs#otherLabels",
  ]

  let union_type =
    union_input_builder.build_multi_variant_union_input(
      "GalleryInput",
      "labels",
      refs,
      dict.new(),
    )

  // Verify it's an input object type
  schema.is_input_object(union_type)
  |> should.be_true()

  // Verify the type name
  schema.type_name(union_type)
  |> should.equal("GalleryInputLabelsInput")
}

// ─── build_union_input_types with lexicons Tests ───────────────────

pub fn build_union_input_types_from_empty_lexicons_test() {
  // Create a registry from empty lexicons
  let empty_registry = registry.from_lexicons([])

  let union_registry =
    union_input_builder.build_union_input_types(empty_registry)

  // Should have empty input_types and field_variants
  dict.size(union_registry.input_types)
  |> should.equal(0)

  dict.size(union_registry.field_variants)
  |> should.equal(0)
}

pub fn build_union_input_types_generates_input_for_object_def_test() {
  // Create a lexicon with an object def in "others"
  let obj_def =
    types.ObjectDef(type_: "object", required_fields: ["values"], properties: [
      #(
        "values",
        types.Property(
          type_: "array",
          required: True,
          format: None,
          ref: None,
          refs: None,
          items: Some(types.ArrayItems(type_: "string", ref: None, refs: None)),
        ),
      ),
    ])

  let lexicon =
    types.Lexicon(
      id: "com.atproto.label.defs",
      defs: types.Defs(
        main: None,
        others: dict.from_list([#("selfLabels", types.Object(obj_def))]),
      ),
    )

  let reg = registry.from_lexicons([lexicon])
  let union_registry = union_input_builder.build_union_input_types(reg)

  // Should have an input type for the ref
  union_input_builder.get_input_type(
    union_registry,
    "com.atproto.label.defs#selfLabels",
  )
  |> option.is_some
  |> should.be_true()
}
