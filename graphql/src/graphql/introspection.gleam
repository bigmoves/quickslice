/// GraphQL Introspection
///
/// Implements the GraphQL introspection system per the GraphQL spec.
/// Provides __schema, __type, and __typename meta-fields.
import gleam/list
import gleam/option
import graphql/schema
import graphql/value

/// Build introspection value for __schema
pub fn schema_introspection(graphql_schema: schema.Schema) -> value.Value {
  let query_type = schema.query_type(graphql_schema)

  // Build list of all types in the schema
  let all_types = get_all_types(graphql_schema)

  value.Object([
    #("queryType", type_ref(query_type)),
    #("mutationType", value.Null),
    #("subscriptionType", value.Null),
    #("types", value.List(all_types)),
    #("directives", value.List([])),
  ])
}

/// Get all types from the schema
fn get_all_types(graphql_schema: schema.Schema) -> List(value.Value) {
  let query_type = schema.query_type(graphql_schema)

  // Collect all types by traversing the schema
  let mut_collected_types = collect_types_from_type(query_type, [])

  // Deduplicate by type name
  let type_names = list.map(mut_collected_types, schema.type_name)
  let unique_types =
    list.zip(type_names, mut_collected_types)
    |> list.unique
    |> list.map(fn(pair) { pair.1 })

  // Add any built-in scalars that aren't already in the list
  let all_built_ins = [
    schema.string_type(),
    schema.int_type(),
    schema.float_type(),
    schema.boolean_type(),
    schema.id_type(),
  ]

  let collected_names = list.map(unique_types, schema.type_name)
  let missing_built_ins =
    list.filter(all_built_ins, fn(built_in) {
      let built_in_name = schema.type_name(built_in)
      !list.contains(collected_names, built_in_name)
    })

  let all_types = list.append(unique_types, missing_built_ins)

  // Convert all types to introspection values
  list.map(all_types, type_introspection)
}

/// Collect all types referenced in a type (recursively)
fn collect_types_from_type(
  t: schema.Type,
  acc: List(schema.Type),
) -> List(schema.Type) {
  case
    list.any(acc, fn(existing) {
      schema.type_name(existing) == schema.type_name(t)
    })
  {
    True -> acc
    // Already collected this type
    False -> {
      let new_acc = [t, ..acc]

      // Recursively collect types from fields if this is an object type
      case schema.is_object(t) {
        True -> {
          let fields = schema.get_fields(t)
          list.fold(fields, new_acc, fn(acc2, field) {
            let field_type = schema.field_type(field)
            collect_types_from_type_deep(field_type, acc2)
          })
        }
        False -> {
          // Check if it's a wrapping type (List or NonNull)
          case schema.inner_type(t) {
            option.Some(inner) -> collect_types_from_type_deep(inner, new_acc)
            option.None -> new_acc
          }
        }
      }
    }
  }
}

/// Helper to unwrap LIST and NON_NULL and collect the inner type
fn collect_types_from_type_deep(
  t: schema.Type,
  acc: List(schema.Type),
) -> List(schema.Type) {
  // Check if this is a wrapping type (List or NonNull)
  case schema.inner_type(t) {
    option.Some(inner) -> collect_types_from_type_deep(inner, acc)
    option.None -> collect_types_from_type(t, acc)
  }
}

/// Build full type introspection value
fn type_introspection(t: schema.Type) -> value.Value {
  let kind = schema.type_kind(t)
  let type_name = schema.type_name(t)

  // Get inner type for LIST and NON_NULL
  let of_type = case schema.inner_type(t) {
    option.Some(inner) -> type_ref(inner)
    option.None -> value.Null
  }

  // Determine fields based on kind
  let fields = case kind {
    "OBJECT" -> value.List(get_fields_for_type(t))
    _ -> value.Null
  }

  // Handle wrapping types (LIST/NON_NULL) differently
  let name = case kind {
    "LIST" -> value.Null
    "NON_NULL" -> value.Null
    _ -> value.String(type_name)
  }

  value.Object([
    #("kind", value.String(kind)),
    #("name", name),
    #("description", value.Null),
    #("fields", fields),
    #("interfaces", value.List([])),
    #("possibleTypes", value.Null),
    #("enumValues", value.Null),
    #("inputFields", value.Null),
    #("ofType", of_type),
  ])
}

/// Get fields for a type (if it's an object type)
fn get_fields_for_type(t: schema.Type) -> List(value.Value) {
  let fields = schema.get_fields(t)

  list.map(fields, fn(field) {
    let field_type_val = schema.field_type(field)
    let args = schema.field_arguments(field)

    value.Object([
      #("name", value.String(schema.field_name(field))),
      #("description", value.String(schema.field_description(field))),
      #("args", value.List(list.map(args, argument_introspection))),
      #("type", type_ref(field_type_val)),
      #("isDeprecated", value.Boolean(False)),
      #("deprecationReason", value.Null),
    ])
  })
}

/// Build introspection for an argument
fn argument_introspection(arg: schema.Argument) -> value.Value {
  value.Object([
    #("name", value.String(schema.argument_name(arg))),
    #("description", value.String(schema.argument_description(arg))),
    #("type", type_ref(schema.argument_type(arg))),
    #("defaultValue", value.Null),
  ])
}

/// Build a type reference (simplified version of type_introspection for field types)
fn type_ref(t: schema.Type) -> value.Value {
  let kind = schema.type_kind(t)
  let type_name = schema.type_name(t)

  // Get inner type for LIST and NON_NULL
  let of_type = case schema.inner_type(t) {
    option.Some(inner) -> type_ref(inner)
    option.None -> value.Null
  }

  // Handle wrapping types (LIST/NON_NULL) differently
  let name = case kind {
    "LIST" -> value.Null
    "NON_NULL" -> value.Null
    _ -> value.String(type_name)
  }

  value.Object([
    #("kind", value.String(kind)),
    #("name", name),
    #("ofType", of_type),
  ])
}
