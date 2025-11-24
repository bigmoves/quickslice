/// Tests for Mutation Builder - uploadBlob mutation
///
/// Tests the uploadBlob mutation and BlobUploadResponse type with flat structure
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/mutation/builder as mutation_builder
import swell/schema
import swell/value

/// Test that uploadBlob mutation is added when factory is provided
pub fn build_mutation_type_includes_upload_blob_test() {
  // Create a simple upload blob resolver factory
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("bafkreiabc123")),
          #("mime_type", value.String("image/jpeg")),
          #("size", value.Int(12_345)),
        ]),
      )
    }
  }

  // Build mutation type with uploadBlob factory
  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Verify the mutation type has uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let has_upload_blob =
    list.any(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  has_upload_blob
  |> should.be_true()
}

/// Test that uploadBlob mutation is NOT added when factory is None
pub fn build_mutation_type_without_upload_blob_test() {
  // Build mutation type without uploadBlob factory
  let mutation_type =
    mutation_builder.build_mutation_type([], dict.new(), None, None, None, None)

  // Verify the mutation type does NOT have uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let has_upload_blob =
    list.any(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  has_upload_blob
  |> should.be_false()
}

/// Test that uploadBlob mutation has correct arguments
pub fn upload_blob_has_correct_arguments_test() {
  // Create a simple factory
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  // Build mutation type
  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let args = schema.field_arguments(field)
      // Should have 2 arguments: data and mimeType
      let has_data =
        list.any(args, fn(arg) { schema.argument_name(arg) == "data" })
      let has_mime_type =
        list.any(args, fn(arg) { schema.argument_name(arg) == "mimeType" })

      has_data |> should.be_true()
      has_mime_type |> should.be_true()
      list.length(args) |> should.equal(2)
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test that data argument is non-null String
pub fn upload_blob_data_argument_is_non_null_string_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let args = schema.field_arguments(field)
      let data_arg =
        list.find(args, fn(arg) { schema.argument_name(arg) == "data" })

      case data_arg {
        Ok(arg) -> {
          let arg_type = schema.argument_type(arg)
          // Check if it's non-null
          schema.is_non_null(arg_type) |> should.be_true()

          // Check if inner type is string
          case schema.inner_type(arg_type) {
            Some(inner) -> {
              schema.type_name(inner) |> should.equal("String")
            }
            None -> should.be_true(False)
          }
        }
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test that mimeType argument is non-null String
pub fn upload_blob_mime_type_argument_is_non_null_string_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let args = schema.field_arguments(field)
      let mime_type_arg =
        list.find(args, fn(arg) { schema.argument_name(arg) == "mimeType" })

      case mime_type_arg {
        Ok(arg) -> {
          let arg_type = schema.argument_type(arg)
          // Check if it's non-null
          schema.is_non_null(arg_type) |> should.be_true()

          // Check if inner type is string
          case schema.inner_type(arg_type) {
            Some(inner) -> {
              schema.type_name(inner) |> should.equal("String")
            }
            None -> should.be_true(False)
          }
        }
        Error(_) -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test that uploadBlob returns non-null BlobUploadResponse
pub fn upload_blob_return_type_is_non_null_blob_upload_response_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let return_type = schema.field_type(field)
      // Check if it's non-null
      schema.is_non_null(return_type) |> should.be_true()

      // Check if inner type is BlobUploadResponse
      case schema.inner_type(return_type) {
        Some(inner) -> {
          schema.type_name(inner) |> should.equal("BlobUploadResponse")
        }
        None -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test BlobUploadResponse type has ref field
pub fn blob_upload_response_has_ref_field_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let return_type = schema.field_type(field)
      case schema.inner_type(return_type) {
        Some(blob_response_type) -> {
          let response_fields = schema.get_fields(blob_response_type)
          let has_ref =
            list.any(response_fields, fn(f) { schema.field_name(f) == "ref" })
          has_ref |> should.be_true()
        }
        None -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test BlobUploadResponse type has mimeType field
pub fn blob_upload_response_has_mime_type_field_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let return_type = schema.field_type(field)
      case schema.inner_type(return_type) {
        Some(blob_response_type) -> {
          let response_fields = schema.get_fields(blob_response_type)
          let has_mime_type =
            list.any(response_fields, fn(f) {
              schema.field_name(f) == "mimeType"
            })
          has_mime_type |> should.be_true()
        }
        None -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test BlobUploadResponse type has size field
pub fn blob_upload_response_has_size_field_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let return_type = schema.field_type(field)
      case schema.inner_type(return_type) {
        Some(blob_response_type) -> {
          let response_fields = schema.get_fields(blob_response_type)
          let has_size =
            list.any(response_fields, fn(f) { schema.field_name(f) == "size" })
          has_size |> should.be_true()
        }
        None -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}

/// Test BlobUploadResponse has exactly 3 fields (no nested blob wrapper)
pub fn blob_upload_response_has_three_fields_test() {
  let upload_factory = fn() {
    fn(_ctx) {
      Ok(
        value.Object([
          #("ref", value.String("test")),
          #("mime_type", value.String("test")),
          #("size", value.Int(0)),
        ]),
      )
    }
  }

  let mutation_type =
    mutation_builder.build_mutation_type(
      [],
      dict.new(),
      None,
      None,
      None,
      Some(upload_factory),
    )

  // Get uploadBlob field
  let fields = schema.get_fields(mutation_type)
  let upload_blob_field =
    list.find(fields, fn(field) { schema.field_name(field) == "uploadBlob" })

  case upload_blob_field {
    Ok(field) -> {
      let return_type = schema.field_type(field)
      case schema.inner_type(return_type) {
        Some(blob_response_type) -> {
          let response_fields = schema.get_fields(blob_response_type)
          list.length(response_fields) |> should.equal(3)
        }
        None -> should.be_true(False)
      }
    }
    Error(_) -> should.be_true(False)
  }
}
