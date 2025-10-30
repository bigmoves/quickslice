/// Blob Type for GraphQL
///
/// Provides a GraphQL object type for AT Protocol blob references.
///
/// ## Schema
///
/// ```graphql
/// type Blob {
///   ref: String!
///   mimeType: String!
///   size: Int!
///   url(preset: String): String!
/// }
/// ```
///
/// ## Fields
///
/// - `ref`: CID reference to the blob (e.g., "bafyreiabc123...")
/// - `mimeType`: MIME type of the blob (e.g., "image/jpeg", "image/png")
/// - `size`: Size of the blob in bytes
/// - `url(preset)`: Generate a CDN URL with an optional preset parameter
///   - Presets: `avatar`, `banner`, `feed_thumbnail`, `feed_fullsize`
///   - Default preset: `feed_fullsize`
///   - URL format: `https://cdn.bsky.app/img/{preset}/plain/{did}/{ref}@jpeg`
///
/// ## Usage
///
/// The Blob type is automatically used when a lexicon field has type "blob".
/// The resolver expects blob data in this format:
///
/// ```gleam
/// value.Object([
///   #("ref", value.String("bafyreiabc123")),
///   #("mime_type", value.String("image/jpeg")),
///   #("size", value.Int(12345)),
///   #("did", value.String("did:plc:user123")),
/// ])
/// ```
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/result
import graphql/schema
import graphql/value

/// Create the Blob GraphQL object type
pub fn create_blob_type() -> schema.Type {
  schema.object_type("Blob", "A blob reference with metadata and URL generation", [
    // ref field - CID reference
    schema.field(
      "ref",
      schema.non_null(schema.string_type()),
      "CID reference to the blob",
      resolve_ref,
    ),
    // mimeType field
    schema.field(
      "mimeType",
      schema.non_null(schema.string_type()),
      "MIME type of the blob",
      resolve_mime_type,
    ),
    // size field
    schema.field(
      "size",
      schema.non_null(schema.int_type()),
      "Size in bytes",
      resolve_size,
    ),
    // url field with preset argument
    schema.field_with_args(
      "url",
      schema.non_null(schema.string_type()),
      "Generate CDN URL for the blob with the specified preset (avatar, banner, feed_thumbnail, feed_fullsize)",
      [
        schema.argument(
          "preset",
          schema.string_type(),
          "Image preset: avatar, banner, feed_thumbnail, feed_fullsize",
          Some(value.String("feed_fullsize")),
        ),
      ],
      resolve_url,
    ),
  ])
}

/// Check if a type has a field with the given name
pub fn has_field(type_: schema.Type, field_name: String) -> Bool {
  case schema.get_field(type_, field_name) {
    option.Some(_) -> True
    option.None -> False
  }
}

/// Resolve the ref field
pub fn resolve_ref(ctx: schema.Context) -> Result(value.Value, String) {
  extract_blob_field(ctx, "ref")
  |> result.map(value.String)
}

/// Resolve the mimeType field
pub fn resolve_mime_type(ctx: schema.Context) -> Result(value.Value, String) {
  extract_blob_field(ctx, "mime_type")
  |> result.map(value.String)
}

/// Resolve the size field
pub fn resolve_size(ctx: schema.Context) -> Result(value.Value, String) {
  case ctx.data {
    Some(value.Object(fields)) -> {
      case list.key_find(fields, "size") {
        Ok(value.Int(size)) -> Ok(value.Int(size))
        Ok(value.String(s)) ->
          case int.parse(s) {
            Ok(i) -> Ok(value.Int(i))
            Error(_) -> Error("Invalid size value")
          }
        _ -> Error("Size field not found or invalid type")
      }
    }
    _ -> Error("Missing blob data in context")
  }
}

/// Resolve the url field with preset argument
pub fn resolve_url(ctx: schema.Context) -> Result(value.Value, String) {
  // Extract preset argument (with default)
  let preset = case schema.get_argument(ctx, "preset") {
    Some(value.String(p)) -> p
    _ -> "feed_fullsize"
  }

  // Extract blob data from context
  use ref <- result.try(extract_blob_field(ctx, "ref"))
  use did <- result.try(extract_blob_field(ctx, "did"))

  // Build CDN URL: https://cdn.bsky.app/img/{preset}/plain/{did}/{ref}@jpeg
  let cdn_url =
    "https://cdn.bsky.app/img/"
    <> preset
    <> "/plain/"
    <> did
    <> "/"
    <> ref
    <> "@jpeg"

  Ok(value.String(cdn_url))
}

/// Helper to extract a string field from blob data in context
fn extract_blob_field(
  ctx: schema.Context,
  field_name: String,
) -> Result(String, String) {
  case ctx.data {
    Some(value.Object(fields)) -> {
      case list.key_find(fields, field_name) {
        Ok(value.String(val)) -> Ok(val)
        Ok(value.Int(val)) -> Ok(int.to_string(val))
        _ -> Error("Field " <> field_name <> " not found or invalid type")
      }
    }
    _ -> Error("Missing blob data in context")
  }
}
