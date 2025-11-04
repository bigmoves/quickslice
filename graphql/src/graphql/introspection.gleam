/// GraphQL Introspection
///
/// Implements the GraphQL introspection system per the GraphQL spec.
/// Provides __schema, __type, and __typename meta-fields.
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import graphql/schema
import graphql/value

/// Build introspection value for __schema
pub fn schema_introspection(graphql_schema: schema.Schema) -> value.Value {
  let query_type = schema.query_type(graphql_schema)
  let mutation_type_option = schema.get_mutation_type(graphql_schema)

  // Build list of all types in the schema
  let all_types = get_all_types(graphql_schema)

  // Build mutation type ref if it exists
  let mutation_type_value = case mutation_type_option {
    option.Some(mutation_type) -> type_ref(mutation_type)
    option.None -> value.Null
  }

  value.Object([
    #("queryType", type_ref(query_type)),
    #("mutationType", mutation_type_value),
    #("subscriptionType", value.Null),
    #("types", value.List(all_types)),
    #("directives", value.List([])),
  ])
}

/// Build introspection value for __type(name: "TypeName")
/// Returns Some(type_introspection) if the type is found, None otherwise
pub fn type_by_name_introspection(
  graphql_schema: schema.Schema,
  type_name: String,
) -> option.Option(value.Value) {
  let all_types = get_all_schema_types(graphql_schema)

  // Find the type with the matching name
  let found_type =
    list.find(all_types, fn(t) { schema.type_name(t) == type_name })

  case found_type {
    Ok(t) -> option.Some(type_introspection(t))
    Error(_) -> option.None
  }
}

/// Get all types from the schema as schema.Type values
/// Useful for testing and documentation generation
pub fn get_all_schema_types(graphql_schema: schema.Schema) -> List(schema.Type) {
  let query_type = schema.query_type(graphql_schema)
  let mutation_type_option = schema.get_mutation_type(graphql_schema)

  // Collect all types by traversing the query type
  let mut_collected_types = collect_types_from_type(query_type, [])

  // Also collect types from mutation type if it exists
  let all_collected_types = case mutation_type_option {
    option.Some(mutation_type) ->
      collect_types_from_type(mutation_type, mut_collected_types)
    option.None -> mut_collected_types
  }

  // Deduplicate by type name, preferring types with more fields
  // This ensures we get the "most complete" version of each type
  let unique_types = deduplicate_types_by_name(all_collected_types)

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

  list.append(unique_types, missing_built_ins)
}

/// Get all types from the schema
fn get_all_types(graphql_schema: schema.Schema) -> List(value.Value) {
  let all_types = get_all_schema_types(graphql_schema)

  // Convert all types to introspection values
  list.map(all_types, type_introspection)
}

/// Deduplicate types by name, keeping the version with the most fields
/// This ensures we get the "most complete" version of each type when
/// multiple versions exist (e.g., from different passes in schema building)
fn deduplicate_types_by_name(types: List(schema.Type)) -> List(schema.Type) {
  // Group types by name
  types
  |> list.group(schema.type_name)
  |> dict.to_list
  |> list.map(fn(pair) {
    let #(_name, type_list) = pair
    // For each group, find the type with the most content
    type_list
    |> list.reduce(fn(best, current) {
      // Count content: fields for object types, enum values for enums, etc.
      let best_content_count = get_type_content_count(best)
      let current_content_count = get_type_content_count(current)

      // Prefer the type with more content
      case current_content_count > best_content_count {
        True -> current
        False -> best
      }
    })
    |> result.unwrap(
      list.first(type_list)
      |> result.unwrap(schema.string_type()),
    )
  })
}

/// Get the "content count" for a type (fields, enum values, input fields, etc.)
/// This helps us pick the most complete version of a type during deduplication
fn get_type_content_count(t: schema.Type) -> Int {
  // For object types, count fields
  let field_count = list.length(schema.get_fields(t))

  // For enum types, count enum values
  let enum_value_count = list.length(schema.get_enum_values(t))

  // For input object types, count input fields
  let input_field_count = list.length(schema.get_input_fields(t))

  // Return the maximum (types will only have one of these be non-zero)
  [field_count, enum_value_count, input_field_count]
  |> list.reduce(fn(a, b) {
    case a > b {
      True -> a
      False -> b
    }
  })
  |> result.unwrap(0)
}

/// Collect all types referenced in a type (recursively)
/// Note: We collect ALL instances of each type (even duplicates by name)
/// because we want to find the "most complete" version during deduplication
fn collect_types_from_type(
  t: schema.Type,
  acc: List(schema.Type),
) -> List(schema.Type) {
  // Always add this type - we'll deduplicate later by choosing the version with most fields
  let new_acc = [t, ..acc]

  // To prevent infinite recursion, check if we've already traversed this exact type instance
  // We use a simple heuristic: if this type name appears multiple times AND this specific
  // instance has the same or fewer content than what we've seen, skip traversing its children
  let should_traverse_children = case
    schema.is_object(t) || schema.is_enum(t) || schema.is_union(t)
  {
    True -> {
      let current_content_count = get_type_content_count(t)
      let existing_with_same_name =
        list.filter(acc, fn(existing) {
          schema.type_name(existing) == schema.type_name(t)
        })
      let max_existing_content =
        existing_with_same_name
        |> list.map(get_type_content_count)
        |> list.reduce(fn(a, b) {
          case a > b {
            True -> a
            False -> b
          }
        })
        |> result.unwrap(0)

      // Only traverse if this instance has more content than we've seen before
      current_content_count > max_existing_content
    }
    False -> True
  }

  case should_traverse_children {
    False -> new_acc
    True -> {
      // Recursively collect types from fields if this is an object type
      case schema.is_object(t) {
        True -> {
          let fields = schema.get_fields(t)
          list.fold(fields, new_acc, fn(acc2, field) {
            let field_type = schema.field_type(field)
            let acc3 = collect_types_from_type_deep(field_type, acc2)

            // Also collect types from field arguments
            let arguments = schema.field_arguments(field)
            list.fold(arguments, acc3, fn(acc4, arg) {
              let arg_type = schema.argument_type(arg)
              collect_types_from_type_deep(arg_type, acc4)
            })
          })
        }
        False -> {
          // Check if it's a union type
          case schema.is_union(t) {
            True -> {
              // Collect types from union's possible_types
              let possible_types = schema.get_possible_types(t)
              list.fold(possible_types, new_acc, fn(acc2, union_type) {
                collect_types_from_type_deep(union_type, acc2)
              })
            }
            False -> {
              // Check if it's an InputObjectType
              let input_fields = schema.get_input_fields(t)
              case list.is_empty(input_fields) {
                False -> {
                  // This is an InputObjectType, collect types from its fields
                  list.fold(input_fields, new_acc, fn(acc2, input_field) {
                    let field_type = schema.input_field_type(input_field)
                    collect_types_from_type_deep(field_type, acc2)
                  })
                }
                True -> {
                  // Check if it's a wrapping type (List or NonNull)
                  case schema.inner_type(t) {
                    option.Some(inner) ->
                      collect_types_from_type_deep(inner, new_acc)
                    option.None -> new_acc
                  }
                }
              }
            }
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

  // Determine inputFields for INPUT_OBJECT types
  let input_fields = case kind {
    "INPUT_OBJECT" -> value.List(get_input_fields_for_type(t))
    _ -> value.Null
  }

  // Determine enumValues for ENUM types
  let enum_values = case kind {
    "ENUM" -> value.List(get_enum_values_for_type(t))
    _ -> value.Null
  }

  // Determine possibleTypes for UNION types
  let possible_types = case kind {
    "UNION" -> {
      let types = schema.get_possible_types(t)
      value.List(list.map(types, type_ref))
    }
    _ -> value.Null
  }

  // Handle wrapping types (LIST/NON_NULL) differently
  let name = case kind {
    "LIST" -> value.Null
    "NON_NULL" -> value.Null
    _ -> value.String(type_name)
  }

  let description = case schema.type_description(t) {
    "" -> value.Null
    desc -> value.String(desc)
  }

  value.Object([
    #("kind", value.String(kind)),
    #("name", name),
    #("description", description),
    #("fields", fields),
    #("interfaces", value.List([])),
    #("possibleTypes", possible_types),
    #("enumValues", enum_values),
    #("inputFields", input_fields),
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

/// Get input fields for a type (if it's an input object type)
fn get_input_fields_for_type(t: schema.Type) -> List(value.Value) {
  let input_fields = schema.get_input_fields(t)

  list.map(input_fields, fn(input_field) {
    let field_type_val = schema.input_field_type(input_field)

    value.Object([
      #("name", value.String(schema.input_field_name(input_field))),
      #(
        "description",
        value.String(schema.input_field_description(input_field)),
      ),
      #("type", type_ref(field_type_val)),
      #("defaultValue", value.Null),
    ])
  })
}

/// Get enum values for a type (if it's an enum type)
fn get_enum_values_for_type(t: schema.Type) -> List(value.Value) {
  let enum_values = schema.get_enum_values(t)

  list.map(enum_values, fn(enum_value) {
    value.Object([
      #("name", value.String(schema.enum_value_name(enum_value))),
      #("description", value.String(schema.enum_value_description(enum_value))),
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
