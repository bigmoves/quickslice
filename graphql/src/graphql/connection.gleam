/// GraphQL Connection Types for Relay Cursor Connections
///
/// Implements the Relay Cursor Connections Specification:
/// https://relay.dev/graphql/connections.htm
import gleam/list
import gleam/option.{type Option, None, Some}
import graphql/schema
import graphql/value

/// PageInfo type for connection pagination metadata
pub type PageInfo {
  PageInfo(
    has_next_page: Bool,
    has_previous_page: Bool,
    start_cursor: Option(String),
    end_cursor: Option(String),
  )
}

/// Edge wrapper containing a node and its cursor
pub type Edge(node_type) {
  Edge(node: node_type, cursor: String)
}

/// Connection wrapper containing edges and page info
pub type Connection(node_type) {
  Connection(
    edges: List(Edge(node_type)),
    page_info: PageInfo,
    total_count: Option(Int),
  )
}

/// Creates the PageInfo GraphQL type
pub fn page_info_type() -> schema.Type {
  schema.object_type(
    "PageInfo",
    "Information about pagination in a connection",
    [
      schema.field(
        "hasNextPage",
        schema.non_null(schema.boolean_type()),
        "When paginating forwards, are there more items?",
        fn(ctx) {
          // Extract from context data
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "hasNextPage") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Boolean(False))
              }
            }
            _ -> Ok(value.Boolean(False))
          }
        },
      ),
      schema.field(
        "hasPreviousPage",
        schema.non_null(schema.boolean_type()),
        "When paginating backwards, are there more items?",
        fn(ctx) {
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "hasPreviousPage") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Boolean(False))
              }
            }
            _ -> Ok(value.Boolean(False))
          }
        },
      ),
      schema.field(
        "startCursor",
        schema.string_type(),
        "Cursor corresponding to the first item in the page",
        fn(ctx) {
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "startCursor") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Null)
              }
            }
            _ -> Ok(value.Null)
          }
        },
      ),
      schema.field(
        "endCursor",
        schema.string_type(),
        "Cursor corresponding to the last item in the page",
        fn(ctx) {
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "endCursor") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Null)
              }
            }
            _ -> Ok(value.Null)
          }
        },
      ),
    ],
  )
}

/// Creates an Edge type for a given node type name
pub fn edge_type(node_type_name: String, node_type: schema.Type) -> schema.Type {
  let edge_type_name = node_type_name <> "Edge"

  schema.object_type(
    edge_type_name,
    "An edge in a connection for " <> node_type_name,
    [
      schema.field(
        "node",
        schema.non_null(node_type),
        "The item at the end of the edge",
        fn(ctx) {
          // Extract node from context data
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "node") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Null)
              }
            }
            _ -> Ok(value.Null)
          }
        },
      ),
      schema.field(
        "cursor",
        schema.non_null(schema.string_type()),
        "A cursor for use in pagination",
        fn(ctx) {
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "cursor") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.String(""))
              }
            }
            _ -> Ok(value.String(""))
          }
        },
      ),
    ],
  )
}

/// Creates a Connection type for a given node type name
pub fn connection_type(
  node_type_name: String,
  edge_type: schema.Type,
) -> schema.Type {
  let connection_type_name = node_type_name <> "Connection"

  schema.object_type(
    connection_type_name,
    "A connection to a list of items for " <> node_type_name,
    [
      schema.field(
        "edges",
        schema.non_null(schema.list_type(schema.non_null(edge_type))),
        "A list of edges",
        fn(ctx) {
          // Extract edges from context data
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "edges") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.List([]))
              }
            }
            _ -> Ok(value.List([]))
          }
        },
      ),
      schema.field(
        "pageInfo",
        schema.non_null(page_info_type()),
        "Information to aid in pagination",
        fn(ctx) {
          // Extract pageInfo from context data
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "pageInfo") {
                Ok(val) -> Ok(val)
                Error(_) ->
                  Ok(
                    value.Object([
                      #("hasNextPage", value.Boolean(False)),
                      #("hasPreviousPage", value.Boolean(False)),
                      #("startCursor", value.Null),
                      #("endCursor", value.Null),
                    ]),
                  )
              }
            }
            _ ->
              Ok(
                value.Object([
                  #("hasNextPage", value.Boolean(False)),
                  #("hasPreviousPage", value.Boolean(False)),
                  #("startCursor", value.Null),
                  #("endCursor", value.Null),
                ]),
              )
          }
        },
      ),
      schema.field(
        "totalCount",
        schema.int_type(),
        "Total number of items in the connection",
        fn(ctx) {
          case ctx.data {
            Some(value.Object(fields)) -> {
              case list.key_find(fields, "totalCount") {
                Ok(val) -> Ok(val)
                Error(_) -> Ok(value.Null)
              }
            }
            _ -> Ok(value.Null)
          }
        },
      ),
    ],
  )
}

/// Standard pagination arguments for forward pagination
pub fn forward_pagination_args() -> List(schema.Argument) {
  [
    schema.argument(
      "first",
      schema.int_type(),
      "Returns the first n items from the list",
      None,
    ),
    schema.argument(
      "after",
      schema.string_type(),
      "Returns items after the given cursor",
      None,
    ),
  ]
}

/// Standard pagination arguments for backward pagination
pub fn backward_pagination_args() -> List(schema.Argument) {
  [
    schema.argument(
      "last",
      schema.int_type(),
      "Returns the last n items from the list",
      None,
    ),
    schema.argument(
      "before",
      schema.string_type(),
      "Returns items before the given cursor",
      None,
    ),
  ]
}

/// All standard connection arguments (forward + backward)
/// Note: sortBy is not included yet as it requires InputObject type support
pub fn connection_args() -> List(schema.Argument) {
  list.flatten([forward_pagination_args(), backward_pagination_args()])
}

/// Converts a PageInfo value to a GraphQL value
pub fn page_info_to_value(page_info: PageInfo) -> value.Value {
  value.Object([
    #("hasNextPage", value.Boolean(page_info.has_next_page)),
    #("hasPreviousPage", value.Boolean(page_info.has_previous_page)),
    #(
      "startCursor",
      case page_info.start_cursor {
        Some(cursor) -> value.String(cursor)
        None -> value.Null
      },
    ),
    #(
      "endCursor",
      case page_info.end_cursor {
        Some(cursor) -> value.String(cursor)
        None -> value.Null
      },
    ),
  ])
}

/// Converts an Edge to a GraphQL value
pub fn edge_to_value(edge: Edge(value.Value)) -> value.Value {
  value.Object([
    #("node", edge.node),
    #("cursor", value.String(edge.cursor)),
  ])
}

/// Converts a Connection to a GraphQL value
pub fn connection_to_value(connection: Connection(value.Value)) -> value.Value {
  let edges_value =
    connection.edges
    |> list.map(edge_to_value)
    |> value.List

  let total_count_value = case connection.total_count {
    Some(count) -> value.Int(count)
    None -> value.Null
  }

  value.Object([
    #("edges", edges_value),
    #("pageInfo", page_info_to_value(connection.page_info)),
    #("totalCount", total_count_value),
  ])
}
