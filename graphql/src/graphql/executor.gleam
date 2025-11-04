/// GraphQL Executor
///
/// Executes GraphQL queries against a schema
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import graphql/introspection
import graphql/parser
import graphql/schema
import graphql/value

/// GraphQL Error
pub type GraphQLError {
  GraphQLError(message: String, path: List(String))
}

/// GraphQL Response
pub type Response {
  Response(data: value.Value, errors: List(GraphQLError))
}

/// Get the response key for a field (alias if present, otherwise field name)
fn response_key(field_name: String, alias: option.Option(String)) -> String {
  case alias {
    option.Some(alias_name) -> alias_name
    option.None -> field_name
  }
}

/// Execute a GraphQL query
pub fn execute(
  query: String,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
) -> Result(Response, String) {
  // Parse the query
  case parser.parse(query) {
    Error(parse_error) ->
      Error("Parse error: " <> format_parse_error(parse_error))
    Ok(document) -> {
      // Execute the document
      case execute_document(document, graphql_schema, ctx) {
        Ok(#(data, errors)) -> Ok(Response(data, errors))
        Error(err) -> Error(err)
      }
    }
  }
}

fn format_parse_error(err: parser.ParseError) -> String {
  case err {
    parser.UnexpectedToken(_, msg) -> msg
    parser.UnexpectedEndOfInput(msg) -> msg
    parser.LexerError(_) -> "Lexer error"
  }
}

/// Execute a document
fn execute_document(
  document: parser.Document,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
) -> Result(#(value.Value, List(GraphQLError)), String) {
  case document {
    parser.Document(operations) -> {
      // Separate fragments from executable operations
      let #(fragments, executable_ops) = partition_operations(operations)

      // Build fragments dictionary
      let fragments_dict = build_fragments_dict(fragments)

      // Execute the first executable operation
      case executable_ops {
        [operation, ..] ->
          execute_operation(operation, graphql_schema, ctx, fragments_dict)
        [] -> Error("No executable operations in document")
      }
    }
  }
}

/// Partition operations into fragments and executable operations
fn partition_operations(
  operations: List(parser.Operation),
) -> #(List(parser.Operation), List(parser.Operation)) {
  list.partition(operations, fn(op) {
    case op {
      parser.FragmentDefinition(_, _, _) -> True
      _ -> False
    }
  })
}

/// Build a dictionary of fragments keyed by name
fn build_fragments_dict(
  fragments: List(parser.Operation),
) -> Dict(String, parser.Operation) {
  fragments
  |> list.filter_map(fn(frag) {
    case frag {
      parser.FragmentDefinition(name, _, _) -> Ok(#(name, frag))
      _ -> Error(Nil)
    }
  })
  |> dict.from_list
}

/// Execute an operation
fn execute_operation(
  operation: parser.Operation,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
  fragments: Dict(String, parser.Operation),
) -> Result(#(value.Value, List(GraphQLError)), String) {
  case operation {
    parser.Query(selection_set) -> {
      let root_type = schema.query_type(graphql_schema)
      execute_selection_set(
        selection_set,
        root_type,
        graphql_schema,
        ctx,
        fragments,
        [],
      )
    }
    parser.NamedQuery(_, _, selection_set) -> {
      let root_type = schema.query_type(graphql_schema)
      execute_selection_set(
        selection_set,
        root_type,
        graphql_schema,
        ctx,
        fragments,
        [],
      )
    }
    parser.Mutation(selection_set) -> {
      // Get mutation root type from schema
      case schema.get_mutation_type(graphql_schema) {
        option.Some(mutation_type) ->
          execute_selection_set(
            selection_set,
            mutation_type,
            graphql_schema,
            ctx,
            fragments,
            [],
          )
        option.None ->
          Error("Schema does not define a mutation type")
      }
    }
    parser.NamedMutation(_, _, selection_set) -> {
      // Get mutation root type from schema
      case schema.get_mutation_type(graphql_schema) {
        option.Some(mutation_type) ->
          execute_selection_set(
            selection_set,
            mutation_type,
            graphql_schema,
            ctx,
            fragments,
            [],
          )
        option.None ->
          Error("Schema does not define a mutation type")
      }
    }
    parser.FragmentDefinition(_, _, _) ->
      Error("Fragment definitions are not executable operations")
  }
}

/// Execute a selection set
fn execute_selection_set(
  selection_set: parser.SelectionSet,
  parent_type: schema.Type,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
  fragments: Dict(String, parser.Operation),
  path: List(String),
) -> Result(#(value.Value, List(GraphQLError)), String) {
  case selection_set {
    parser.SelectionSet(selections) -> {
      let results =
        list.map(selections, fn(selection) {
          execute_selection(
            selection,
            parent_type,
            graphql_schema,
            ctx,
            fragments,
            path,
          )
        })

      // Collect all data and errors, merging fragment fields
      let #(data, errors) = collect_and_merge_fields(results)

      Ok(#(value.Object(data), errors))
    }
  }
}

/// Collect and merge fields from selection results, handling fragment fields
fn collect_and_merge_fields(
  results: List(Result(#(String, value.Value, List(GraphQLError)), String)),
) -> #(List(#(String, value.Value)), List(GraphQLError)) {
  let #(data, errors) =
    results
    |> list.fold(#([], []), fn(acc, r) {
      let #(fields_acc, errors_acc) = acc
      case r {
        Ok(#("__fragment_fields", value.Object(fragment_fields), errs)) -> {
          // Merge fragment fields into parent
          #(
            list.append(fields_acc, fragment_fields),
            list.append(errors_acc, errs),
          )
        }
        Ok(#("__fragment_skip", _, _errs)) -> {
          // Skip fragment that didn't match type condition
          acc
        }
        Ok(#(name, val, errs)) -> {
          // Regular field
          #(
            list.append(fields_acc, [#(name, val)]),
            list.append(errors_acc, errs),
          )
        }
        Error(_) -> acc
      }
    })

  #(data, errors)
}

/// Execute a selection
fn execute_selection(
  selection: parser.Selection,
  parent_type: schema.Type,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
  fragments: Dict(String, parser.Operation),
  path: List(String),
) -> Result(#(String, value.Value, List(GraphQLError)), String) {
  case selection {
    parser.FragmentSpread(name) -> {
      // Look up the fragment definition
      case dict.get(fragments, name) {
        Error(_) -> Error("Fragment '" <> name <> "' not found")
        Ok(parser.FragmentDefinition(
          _fname,
          type_condition,
          fragment_selection_set,
        )) -> {
          // Check type condition
          let current_type_name = schema.type_name(parent_type)
          case type_condition == current_type_name {
            False -> {
              // Type condition doesn't match, skip this fragment
              // Return empty object as a placeholder that will be filtered out
              Ok(#("__fragment_skip", value.Null, []))
            }
            True -> {
              // Type condition matches, execute fragment's selections
              case
                execute_selection_set(
                  fragment_selection_set,
                  parent_type,
                  graphql_schema,
                  ctx,
                  fragments,
                  path,
                )
              {
                Ok(#(value.Object(fields), errs)) -> {
                  // Fragment selections should be merged into parent
                  // For now, return as a special marker
                  Ok(#("__fragment_fields", value.Object(fields), errs))
                }
                Ok(#(val, errs)) -> Ok(#("__fragment_fields", val, errs))
                Error(err) -> Error(err)
              }
            }
          }
        }
        Ok(_) -> Error("Invalid fragment definition")
      }
    }
    parser.InlineFragment(type_condition_opt, inline_selections) -> {
      // Check type condition if present
      let current_type_name = schema.type_name(parent_type)
      let should_execute = case type_condition_opt {
        None -> True
        Some(type_condition) -> type_condition == current_type_name
      }

      case should_execute {
        False -> Ok(#("__fragment_skip", value.Null, []))
        True -> {
          let inline_selection_set = parser.SelectionSet(inline_selections)
          case
            execute_selection_set(
              inline_selection_set,
              parent_type,
              graphql_schema,
              ctx,
              fragments,
              path,
            )
          {
            Ok(#(value.Object(fields), errs)) ->
              Ok(#("__fragment_fields", value.Object(fields), errs))
            Ok(#(val, errs)) -> Ok(#("__fragment_fields", val, errs))
            Error(err) -> Error(err)
          }
        }
      }
    }
    parser.Field(name, alias, arguments, nested_selections) -> {
      // Convert arguments to dict (with variable resolution from context)
      let args_dict = arguments_to_dict(arguments, ctx)

      // Determine the response key (use alias if provided, otherwise field name)
      let key = response_key(name, alias)

      // Handle introspection meta-fields
      case name {
        "__typename" -> {
          let type_name = schema.type_name(parent_type)
          Ok(#(key, value.String(type_name), []))
        }
        "__schema" -> {
          let schema_value = introspection.schema_introspection(graphql_schema)
          // Handle nested selections on __schema
          case nested_selections {
            [] -> Ok(#(key, schema_value, []))
            _ -> {
              let selection_set = parser.SelectionSet(nested_selections)
              // We don't have an actual type for __Schema, so we'll handle it specially
              // For now, just return the schema value with nested execution
              case
                execute_introspection_selection_set(
                  selection_set,
                  schema_value,
                  graphql_schema,
                  ctx,
                  fragments,
                  ["__schema", ..path],
                  set.new(),
                )
              {
                Ok(#(nested_data, nested_errors)) ->
                  Ok(#(key, nested_data, nested_errors))
                Error(err) -> {
                  let error = GraphQLError(err, ["__schema", ..path])
                  Ok(#(key, value.Null, [error]))
                }
              }
            }
          }
        }
        "__type" -> {
          // Extract the "name" argument
          case dict.get(args_dict, "name") {
            Ok(value.String(type_name)) -> {
              // Look up the type in the schema
              case introspection.type_by_name_introspection(graphql_schema, type_name) {
                option.Some(type_value) -> {
                  // Handle nested selections on __type
                  case nested_selections {
                    [] -> Ok(#(key, type_value, []))
                    _ -> {
                      let selection_set = parser.SelectionSet(nested_selections)
                      case
                        execute_introspection_selection_set(
                          selection_set,
                          type_value,
                          graphql_schema,
                          ctx,
                          fragments,
                          ["__type", ..path],
                          set.new(),
                        )
                      {
                        Ok(#(nested_data, nested_errors)) ->
                          Ok(#(key, nested_data, nested_errors))
                        Error(err) -> {
                          let error = GraphQLError(err, ["__type", ..path])
                          Ok(#(key, value.Null, [error]))
                        }
                      }
                    }
                  }
                }
                option.None -> {
                  // Type not found, return null (per GraphQL spec)
                  Ok(#(key, value.Null, []))
                }
              }
            }
            Ok(_) -> {
              let error = GraphQLError("__type argument 'name' must be a String", path)
              Ok(#(key, value.Null, [error]))
            }
            Error(_) -> {
              let error = GraphQLError("__type requires a 'name' argument", path)
              Ok(#(key, value.Null, [error]))
            }
          }
        }
        _ -> {
          // Get field from schema
          case schema.get_field(parent_type, name) {
            None -> {
              let error = GraphQLError("Field '" <> name <> "' not found", path)
              Ok(#(key, value.Null, [error]))
            }
            Some(field) -> {
              // Get the field's type for nested selections
              let field_type_def = schema.field_type(field)

              // Create context with arguments (preserve variables from parent context)
              let field_ctx = schema.Context(ctx.data, args_dict, ctx.variables)

              // Resolve the field
              case schema.resolve_field(field, field_ctx) {
                Error(err) -> {
                  let error = GraphQLError(err, [name, ..path])
                  Ok(#(key, value.Null, [error]))
                }
                Ok(field_value) -> {
                  // If there are nested selections, recurse
                  case nested_selections {
                    [] -> Ok(#(key, field_value, []))
                    _ -> {
                      // Need to resolve nested fields
                      case field_value {
                        value.Object(_) -> {
                          // Check if field_type_def is a union type
                          // If so, resolve it to the concrete type first
                          let type_to_use = case schema.is_union(field_type_def) {
                            True -> {
                              // Create context with the field value for type resolution
                              let resolve_ctx =
                                schema.context(option.Some(field_value))
                              case
                                schema.resolve_union_type(field_type_def, resolve_ctx)
                              {
                                Ok(concrete_type) -> concrete_type
                                Error(_) -> field_type_def
                                // Fallback to union type if resolution fails
                              }
                            }
                            False -> field_type_def
                          }

                          // Execute nested selections using the resolved type
                          // Create new context with this object's data
                          let object_ctx = schema.context(option.Some(field_value))
                          let selection_set =
                            parser.SelectionSet(nested_selections)
                          case
                            execute_selection_set(
                              selection_set,
                              type_to_use,
                              graphql_schema,
                              object_ctx,
                              fragments,
                              [name, ..path],
                            )
                          {
                            Ok(#(nested_data, nested_errors)) ->
                              Ok(#(key, nested_data, nested_errors))
                            Error(err) -> {
                              let error = GraphQLError(err, [name, ..path])
                              Ok(#(key, value.Null, [error]))
                            }
                          }
                        }
                        value.List(items) -> {
                          // Handle list with nested selections
                          // Get the inner type from the LIST wrapper, unwrapping NonNull if needed
                          let inner_type = case
                            schema.inner_type(field_type_def)
                          {
                            option.Some(t) -> {
                              // If the result is still wrapped (NonNull), unwrap it too
                              case schema.inner_type(t) {
                                option.Some(unwrapped) -> unwrapped
                                option.None -> t
                              }
                            }
                            option.None -> field_type_def
                          }

                          // Execute nested selections on each item
                          let selection_set =
                            parser.SelectionSet(nested_selections)
                          let results =
                            list.map(items, fn(item) {
                              // Check if inner_type is a union and resolve it
                              let item_type = case schema.is_union(inner_type) {
                                True -> {
                                  // Create context with the item value for type resolution
                                  let resolve_ctx = schema.context(option.Some(item))
                                  case schema.resolve_union_type(inner_type, resolve_ctx) {
                                    Ok(concrete_type) -> concrete_type
                                    Error(_) -> inner_type  // Fallback to union type if resolution fails
                                  }
                                }
                                False -> inner_type
                              }

                              // Create context with this item's data
                              let item_ctx = schema.context(option.Some(item))
                              execute_selection_set(
                                selection_set,
                                item_type,
                                graphql_schema,
                                item_ctx,
                                fragments,
                                [name, ..path],
                              )
                            })

                          // Collect results and errors
                          let processed_items =
                            results
                            |> list.filter_map(fn(r) {
                              case r {
                                Ok(#(val, _)) -> Ok(val)
                                Error(_) -> Error(Nil)
                              }
                            })

                          let all_errors =
                            results
                            |> list.flat_map(fn(r) {
                              case r {
                                Ok(#(_, errs)) -> errs
                                Error(_) -> []
                              }
                            })

                          Ok(#(key, value.List(processed_items), all_errors))
                        }
                        _ -> Ok(#(key, field_value, []))
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
  }
}

/// Execute a selection set on an introspection value (like __schema)
/// This directly reads fields from the value.Object rather than using resolvers
fn execute_introspection_selection_set(
  selection_set: parser.SelectionSet,
  value_obj: value.Value,
  graphql_schema: schema.Schema,
  ctx: schema.Context,
  fragments: Dict(String, parser.Operation),
  path: List(String),
  visited_types: Set(String),
) -> Result(#(value.Value, List(GraphQLError)), String) {
  case selection_set {
    parser.SelectionSet(selections) -> {
      case value_obj {
        value.List(items) -> {
          // For lists, execute the selection set on each item
          let results =
            list.map(items, fn(item) {
              execute_introspection_selection_set(
                selection_set,
                item,
                graphql_schema,
                ctx,
                fragments,
                path,
                visited_types,
              )
            })

          // Collect the data and errors
          let data_items =
            results
            |> list.filter_map(fn(r) {
              case r {
                Ok(#(val, _)) -> Ok(val)
                Error(_) -> Error(Nil)
              }
            })

          let all_errors =
            results
            |> list.flat_map(fn(r) {
              case r {
                Ok(#(_, errs)) -> errs
                Error(_) -> []
              }
            })

          Ok(#(value.List(data_items), all_errors))
        }
        value.Null -> {
          // If the value is null, just return null regardless of selections
          // This handles cases like mutationType and subscriptionType which are null
          Ok(#(value.Null, []))
        }
        value.Object(fields) -> {
          // CYCLE DETECTION: Extract type name from object to detect circular references
          let type_name = case list.key_find(fields, "name") {
            Ok(value.String(name)) -> option.Some(name)
            _ -> option.None
          }

          // Check if we've already visited this type to prevent infinite loops
          let is_cycle = case type_name {
            option.Some(name) -> set.contains(visited_types, name)
            option.None -> False
          }

          // If we detected a cycle, return a minimal object to break the loop
          case is_cycle {
            True -> {
              // Return just the type name and kind to break the cycle
              let minimal_fields = case type_name {
                option.Some(name) -> {
                  let kind_value = case list.key_find(fields, "kind") {
                    Ok(kind) -> kind
                    Error(_) -> value.Null
                  }
                  [#("name", value.String(name)), #("kind", kind_value)]
                }
                option.None -> []
              }
              Ok(#(value.Object(minimal_fields), []))
            }
            False -> {
              // Add current type to visited set before recursing
              let new_visited = case type_name {
                option.Some(name) -> set.insert(visited_types, name)
                option.None -> visited_types
              }

              // For each selection, find the corresponding field in the object
              let results =
            list.map(selections, fn(selection) {
              case selection {
                parser.FragmentSpread(name) -> {
                  // Look up the fragment definition
                  case dict.get(fragments, name) {
                    Error(_) -> {
                      // Fragment not found - return error
                      let error = GraphQLError("Fragment '" <> name <> "' not found", path)
                      Ok(#("__FRAGMENT_ERROR", value.String("Fragment not found: " <> name), [error]))
                    }
                    Ok(parser.FragmentDefinition(
                      _fname,
                      _type_condition,
                      fragment_selection_set,
                    )) -> {
                      // For introspection, we don't check type conditions - just execute the fragment
                      // IMPORTANT: Use visited_types (not new_visited) because we're selecting from
                      // the SAME object, not recursing into it. The current object was already added
                      // to new_visited, but the fragment is just selecting different fields.
                      case
                        execute_introspection_selection_set(
                          fragment_selection_set,
                          value_obj,
                          graphql_schema,
                          ctx,
                          fragments,
                          path,
                          visited_types,
                        )
                      {
                        Ok(#(value.Object(fragment_fields), errs)) ->
                          Ok(#(
                            "__fragment_fields",
                            value.Object(fragment_fields),
                            errs,
                          ))
                        Ok(#(val, errs)) ->
                          Ok(#("__fragment_fields", val, errs))
                        Error(_err) -> Error(Nil)
                      }
                    }
                    Ok(_) -> Error(Nil)
                    // Invalid fragment definition
                  }
                }
                parser.InlineFragment(_type_condition_opt, inline_selections) -> {
                  // For introspection, inline fragments always execute (no type checking needed)
                  // Execute the inline fragment's selections on this object
                  let inline_selection_set =
                    parser.SelectionSet(inline_selections)
                  case
                    execute_introspection_selection_set(
                      inline_selection_set,
                      value_obj,
                      graphql_schema,
                      ctx,
                      fragments,
                      path,
                      new_visited,
                    )
                  {
                    Ok(#(value.Object(fragment_fields), errs)) ->
                      // Return fragment fields to be merged
                      Ok(#(
                        "__fragment_fields",
                        value.Object(fragment_fields),
                        errs,
                      ))
                    Ok(#(val, errs)) -> Ok(#("__fragment_fields", val, errs))
                    Error(_err) -> Error(Nil)
                  }
                }
                parser.Field(name, alias, _arguments, nested_selections) -> {
                  // Determine the response key (use alias if provided, otherwise field name)
                  let key = response_key(name, alias)

                  // Find the field in the object
                  case list.key_find(fields, name) {
                    Ok(field_value) -> {
                      // Handle nested selections
                      case nested_selections {
                        [] -> Ok(#(key, field_value, []))
                        _ -> {
                          let selection_set =
                            parser.SelectionSet(nested_selections)
                          case
                            execute_introspection_selection_set(
                              selection_set,
                              field_value,
                              graphql_schema,
                              ctx,
                              fragments,
                              [name, ..path],
                              new_visited,
                            )
                          {
                            Ok(#(nested_data, nested_errors)) ->
                              Ok(#(key, nested_data, nested_errors))
                            Error(err) -> {
                              let error = GraphQLError(err, [name, ..path])
                              Ok(#(key, value.Null, [error]))
                            }
                          }
                        }
                      }
                    }
                    Error(_) -> {
                      let error =
                        GraphQLError("Field '" <> name <> "' not found", path)
                      Ok(#(key, value.Null, [error]))
                    }
                  }
                }
              }
            })

          // Collect all data and errors, merging fragment fields
          let #(data, errors) =
            results
            |> list.fold(#([], []), fn(acc, r) {
              let #(fields_acc, errors_acc) = acc
              case r {
                Ok(#("__fragment_fields", value.Object(fragment_fields), errs)) -> {
                  // Merge fragment fields into parent
                  #(
                    list.append(fields_acc, fragment_fields),
                    list.append(errors_acc, errs),
                  )
                }
                Ok(#(name, val, errs)) -> {
                  // Regular field
                  #(
                    list.append(fields_acc, [#(name, val)]),
                    list.append(errors_acc, errs),
                  )
                }
                Error(_) -> acc
              }
            })

          Ok(#(value.Object(data), errors))
            }
          }
        }
        _ ->
          Error(
            "Expected object, list, or null for introspection selection set",
          )
      }
    }
  }
}

/// Convert parser ArgumentValue to value.Value
fn argument_value_to_value(
  arg_value: parser.ArgumentValue,
  ctx: schema.Context,
) -> value.Value {
  case arg_value {
    parser.IntValue(s) -> value.String(s)
    parser.FloatValue(s) -> value.String(s)
    parser.StringValue(s) -> value.String(s)
    parser.BooleanValue(b) -> value.Boolean(b)
    parser.NullValue -> value.Null
    parser.EnumValue(s) -> value.String(s)
    parser.ListValue(items) ->
      value.List(list.map(items, fn(item) { argument_value_to_value(item, ctx) }))
    parser.ObjectValue(fields) ->
      value.Object(
        list.map(fields, fn(pair) {
          let #(name, val) = pair
          #(name, argument_value_to_value(val, ctx))
        }),
      )
    parser.VariableValue(name) -> {
      // Look up variable value from context
      case schema.get_variable(ctx, name) {
        option.Some(val) -> val
        option.None -> value.Null
      }
    }
  }
}

/// Convert list of Arguments to a Dict of values
fn arguments_to_dict(
  arguments: List(parser.Argument),
  ctx: schema.Context,
) -> Dict(String, value.Value) {
  list.fold(arguments, dict.new(), fn(acc, arg) {
    case arg {
      parser.Argument(name, arg_value) -> {
        let value = argument_value_to_value(arg_value, ctx)
        dict.insert(acc, name, value)
      }
    }
  })
}
