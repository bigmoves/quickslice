/// GraphQL Executor
///
/// Executes GraphQL queries against a schema
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
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
  let root_type = schema.query_type(graphql_schema)

  case operation {
    parser.Query(selection_set) ->
      execute_selection_set(
        selection_set,
        root_type,
        graphql_schema,
        ctx,
        fragments,
        [],
      )
    parser.NamedQuery(_, _, selection_set) ->
      execute_selection_set(
        selection_set,
        root_type,
        graphql_schema,
        ctx,
        fragments,
        [],
      )
    parser.Mutation(_) -> Error("Mutations not yet implemented")
    parser.NamedMutation(_, _, _) -> Error("Mutations not yet implemented")
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
    parser.Field(name, _alias, _arguments, nested_selections) -> {
      // Handle introspection meta-fields
      case name {
        "__typename" -> {
          let type_name = schema.type_name(parent_type)
          Ok(#("__typename", value.String(type_name), []))
        }
        "__schema" -> {
          let schema_value = introspection.schema_introspection(graphql_schema)
          // Handle nested selections on __schema
          case nested_selections {
            [] -> Ok(#("__schema", schema_value, []))
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
                )
              {
                Ok(#(nested_data, nested_errors)) ->
                  Ok(#("__schema", nested_data, nested_errors))
                Error(err) -> {
                  let error = GraphQLError(err, ["__schema", ..path])
                  Ok(#("__schema", value.Null, [error]))
                }
              }
            }
          }
        }
        _ -> {
          // Get field from schema
          case schema.get_field(parent_type, name) {
            None -> {
              let error = GraphQLError("Field '" <> name <> "' not found", path)
              Ok(#(name, value.Null, [error]))
            }
            Some(field) -> {
              // Get the field's type for nested selections
              let field_type_def = schema.field_type(field)

              // Resolve the field
              case schema.resolve_field(field, ctx) {
                Error(err) -> {
                  let error = GraphQLError(err, [name, ..path])
                  Ok(#(name, value.Null, [error]))
                }
                Ok(field_value) -> {
                  // If there are nested selections, recurse
                  case nested_selections {
                    [] -> Ok(#(name, field_value, []))
                    _ -> {
                      // Need to resolve nested fields
                      case field_value {
                        value.Object(_) -> {
                          // Execute nested selections using the field's type, not parent type
                          let selection_set =
                            parser.SelectionSet(nested_selections)
                          case
                            execute_selection_set(
                              selection_set,
                              field_type_def,
                              graphql_schema,
                              ctx,
                              fragments,
                              [name, ..path],
                            )
                          {
                            Ok(#(nested_data, nested_errors)) ->
                              Ok(#(name, nested_data, nested_errors))
                            Error(err) -> {
                              let error = GraphQLError(err, [name, ..path])
                              Ok(#(name, value.Null, [error]))
                            }
                          }
                        }
                        value.List(items) -> {
                          // Handle list with nested selections
                          // Get the inner type from the LIST wrapper
                          let inner_type = case
                            schema.inner_type(field_type_def)
                          {
                            option.Some(t) -> t
                            option.None -> field_type_def
                          }

                          // Execute nested selections on each item
                          let selection_set =
                            parser.SelectionSet(nested_selections)
                          let results =
                            list.map(items, fn(item) {
                              // Create context with this item's data
                              let item_ctx = schema.Context(option.Some(item))
                              execute_selection_set(
                                selection_set,
                                inner_type,
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

                          Ok(#(name, value.List(processed_items), all_errors))
                        }
                        _ -> Ok(#(name, field_value, []))
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
          // For each selection, find the corresponding field in the object
          let results =
            list.map(selections, fn(selection) {
              case selection {
                parser.FragmentSpread(name) -> {
                  // Look up the fragment definition
                  case dict.get(fragments, name) {
                    Error(_) -> Error(Nil)
                    // Fragment not found, skip it
                    Ok(parser.FragmentDefinition(
                      _fname,
                      _type_condition,
                      fragment_selection_set,
                    )) -> {
                      // For introspection, we don't check type conditions - just execute the fragment
                      case
                        execute_introspection_selection_set(
                          fragment_selection_set,
                          value_obj,
                          graphql_schema,
                          ctx,
                          fragments,
                          path,
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
                parser.Field(name, _alias, _arguments, nested_selections) -> {
                  // Find the field in the object
                  case list.key_find(fields, name) {
                    Ok(field_value) -> {
                      // Handle nested selections
                      case nested_selections {
                        [] -> Ok(#(name, field_value, []))
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
                            )
                          {
                            Ok(#(nested_data, nested_errors)) ->
                              Ok(#(name, nested_data, nested_errors))
                            Error(err) -> {
                              let error = GraphQLError(err, [name, ..path])
                              Ok(#(name, value.Null, [error]))
                            }
                          }
                        }
                      }
                    }
                    Error(_) -> {
                      let error =
                        GraphQLError("Field '" <> name <> "' not found", path)
                      Ok(#(name, value.Null, [error]))
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
        _ ->
          Error(
            "Expected object, list, or null for introspection selection set",
          )
      }
    }
  }
}
