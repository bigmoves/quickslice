/// GraphQL WebSocket Handler
///
/// Handles WebSocket connections for GraphQL subscriptions using the graphql-ws protocol
import database
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import graphql/executor
import graphql/parser
import graphql/schema
import graphql/value
import graphql_gleam
import graphql_ws
import logging
import mist.{type Connection, type ResponseData, type WebsocketConnection, type WebsocketMessage}
import pubsub
import sqlight
import websocket_ffi

/// Configuration constants
const max_subscriptions_per_connection = 100

const max_subscriptions_global = 10_000

/// FFI bindings for global atomic subscription counter
@external(erlang, "subscription_counter_ffi", "increment_global")
fn increment_global_subscriptions() -> Int

@external(erlang, "subscription_counter_ffi", "decrement_global")
fn decrement_global_subscriptions() -> Int

@external(erlang, "subscription_counter_ffi", "get_global_count")
fn get_global_subscription_count() -> Int

/// Convert a PubSub RecordEvent to a GraphQL value.Value
/// Similar to record_to_graphql_value but for subscription events
fn event_to_graphql_value(
  event: pubsub.RecordEvent,
  db: sqlight.Connection,
) -> value.Value {
  // Parse the record JSON value
  let value_object = case graphql_gleam.parse_json_to_value(event.value) {
    Ok(val) -> val
    Error(_) -> value.Object([])
  }

  // Look up actor handle from actor table
  let actor_handle = case database.get_actor(db, event.did) {
    Ok([actor, ..]) -> value.String(actor.handle)
    _ -> value.Null
  }

  // Create the full record object with metadata and value
  value.Object([
    #("uri", value.String(event.uri)),
    #("cid", value.String(event.cid)),
    #("did", value.String(event.did)),
    #("collection", value.String(event.collection)),
    #("indexedAt", value.String(event.indexed_at)),
    #("actorHandle", actor_handle),
    #("value", value_object),
  ])
}

/// Execute a subscription query with event data and variables
/// Returns the formatted JSON response
fn execute_subscription_query(
  query: String,
  variables: Dict(String, value.Value),
  graphql_schema: schema.Schema,
  event: pubsub.RecordEvent,
  db: sqlight.Connection,
) -> Result(String, String) {
  // Convert event to GraphQL value
  let event_value = event_to_graphql_value(event, db)

  // Create context with the event data and variables
  let ctx = schema.context_with_variables(Some(event_value), variables)

  // Execute the subscription query directly
  // The executor now natively supports subscription operations
  use response <- result.try(executor.execute(query, graphql_schema, ctx))

  // Format the response as JSON
  Ok(graphql_gleam.format_response(response))
}

/// Convert collection name to GraphQL field name format
/// Example: "xyz.statusphere.status" -> "xyzStatusphereStatus"
fn collection_to_graphql_name(collection: String) -> String {
  collection
  |> string.split(".")
  |> list.index_map(fn(part, index) {
    case index {
      0 -> part  // Keep first segment lowercase
      _ -> {
        // Capitalize first letter of subsequent segments
        case string.pop_grapheme(part) {
          Ok(#(first, rest)) -> string.uppercase(first) <> rest
          Error(_) -> part
        }
      }
    }
  })
  |> string.join("")
}

/// Extract the subscription field name from a parsed GraphQL query
/// Returns the first field name found in the subscription operation
fn extract_subscription_field(query: String) -> Result(String, String) {
  use document <- result.try(
    parser.parse(query)
    |> result.map_error(fn(_) { "Failed to parse subscription query" }),
  )

  // Find the first subscription operation
  let subscription_op =
    document.operations
    |> list.find(fn(op) {
      case op {
        parser.Subscription(_) | parser.NamedSubscription(_, _, _) -> True
        _ -> False
      }
    })

  use op <- result.try(
    subscription_op
    |> result.replace_error("No subscription operation found in query"),
  )

  // Extract the selection set
  let selections = case op {
    parser.Subscription(parser.SelectionSet(sels)) -> Ok(sels)
    parser.NamedSubscription(_, _, parser.SelectionSet(sels)) -> Ok(sels)
    _ -> Error("Invalid subscription operation")
  }

  use sels <- result.try(selections)

  // Get the first field from the selections
  case sels {
    [parser.Field(name, _, _, _), ..] -> Ok(name)
    _ -> Error("No fields found in subscription")
  }
}


/// Subscription metadata
pub type SubscriptionInfo {
  SubscriptionInfo(
    pid: process.Pid,
    field_name: String,
    variables: Dict(String, value.Value),
  )
}

/// State for WebSocket connection
pub type State {
  State(
    db: sqlight.Connection,
    // Map of subscription ID -> subscription info
    subscriptions: Dict(String, SubscriptionInfo),
    // The WebSocket connection for sending frames
    conn: WebsocketConnection,
    // Subject for receiving subscription data
    subscription_subject: process.Subject(websocket_ffi.SubscriptionMessage),
    // GraphQL schema for executing subscription queries
    schema: schema.Schema,
  )
}

/// Handle WebSocket connection request (called from Mist handler)
pub fn handle_websocket(
  req: Request(Connection),
  db: sqlight.Connection,
  auth_base_url: String,
  plc_url: String,
) -> response.Response(ResponseData) {
  mist.websocket(
    request: req,
    on_init: fn(conn) {
      logging.log(logging.Info, "[websocket] Client connected")

      // Build GraphQL schema for subscriptions
      let graphql_schema = case graphql_gleam.build_schema_from_db(
        db,
        auth_base_url,
        plc_url,
      ) {
        Ok(schema) -> schema
        Error(err) -> {
          // Schema build failed - this is a critical error for subscriptions
          logging.log(
            logging.Error,
            "[websocket] FATAL: Failed to build GraphQL schema: " <> err,
          )
          // Panic because we can't continue without a schema
          panic as "Cannot initialize WebSocket subscriptions without GraphQL schema"
        }
      }

      // Create a Subject for receiving subscription data
      let subscription_subject = process.new_subject()

      // Create a selector that listens to the subject
      let selector =
        process.new_selector()
        |> process.select(subscription_subject)

      let state =
        State(
          db: db,
          subscriptions: dict.new(),
          conn: conn,
          subscription_subject: subscription_subject,
          schema: graphql_schema,
        )

      #(state, Some(selector))
    },
    on_close: fn(state) {
      logging.log(logging.Info, "[websocket] Client disconnected")

      // Clean up all active subscriptions
      let subscription_count = dict.size(state.subscriptions)
      dict.each(state.subscriptions, fn(_id, info) {
        // Kill the listener process
        process.kill(info.pid)
        // Decrement global counter
        let _ = decrement_global_subscriptions()
        Nil
      })

      logging.log(
        logging.Info,
        "[websocket] Cleaned up "
          <> string.inspect(subscription_count)
          <> " subscriptions",
      )
      Nil
    },
    handler: handle_ws_message,
  )
}

/// Handle incoming WebSocket messages
fn handle_ws_message(
  state: State,
  message: WebsocketMessage(websocket_ffi.SubscriptionMessage),
  conn: WebsocketConnection,
) {
  case message {
    mist.Text(text) -> {
      handle_text_message(state, conn, text)
    }
    mist.Binary(_) -> {
      logging.log(logging.Warning, "[websocket] Received binary message, ignoring")
      mist.continue(state)
    }
    mist.Closed | mist.Shutdown -> {
      mist.stop()
    }
    mist.Custom(websocket_ffi.SubscriptionData(id, data)) -> {
      // Handle subscription data from listener processes
      let next_msg = graphql_ws.format_message(graphql_ws.Next(id, data))
      let _ = mist.send_text_frame(conn, next_msg)
      mist.continue(state)
    }
  }
}


/// Handle text messages (GraphQL-WS protocol)
fn handle_text_message(
  state: State,
  conn: WebsocketConnection,
  text: String,
) {
  case graphql_ws.parse_message(text) {
    Ok(graphql_ws.ConnectionInit(_payload)) -> {
      // Send connection_ack
      let ack_msg = graphql_ws.format_message(graphql_ws.ConnectionAck)
      let _ = mist.send_text_frame(conn, ack_msg)
      logging.log(logging.Info, "[websocket] Connection initialized")
      mist.continue(state)
    }

    Ok(graphql_ws.Subscribe(id, query, variables_opt)) -> {
      // Check per-connection subscription limit
      let connection_count = dict.size(state.subscriptions)
      case connection_count >= max_subscriptions_per_connection {
        True -> {
          logging.log(
            logging.Warning,
            "[websocket] Subscription limit reached for connection: "
              <> string.inspect(connection_count),
          )
          let error_msg =
            graphql_ws.format_message(graphql_ws.ErrorMessage(
              id,
              "Maximum subscriptions per connection exceeded ("
                <> string.inspect(max_subscriptions_per_connection)
                <> ")",
            ))
          let _ = mist.send_text_frame(conn, error_msg)
          mist.continue(state)
        }
        False -> {
          // Check global subscription limit
          let global_count = get_global_subscription_count()
          case global_count >= max_subscriptions_global {
            True -> {
              logging.log(
                logging.Warning,
                "[websocket] Global subscription limit reached: "
                  <> string.inspect(global_count),
              )
              let error_msg =
                graphql_ws.format_message(graphql_ws.ErrorMessage(
                  id,
                  "Global subscription limit exceeded",
                ))
              let _ = mist.send_text_frame(conn, error_msg)
              mist.continue(state)
            }
            False -> {
              // Parse and validate the subscription query to extract field name
              case extract_subscription_field(query) {
                Error(err) -> {
                  logging.log(
                    logging.Warning,
                    "[websocket] Invalid subscription query: " <> err,
                  )
                  let error_msg =
                    graphql_ws.format_message(graphql_ws.ErrorMessage(
                      id,
                      "Invalid subscription query: " <> err,
                    ))
                  let _ = mist.send_text_frame(conn, error_msg)
                  mist.continue(state)
                }
                Ok(field_name) -> {
                  // Parse variables from JSON
                  let variables = case variables_opt {
                    Some(vars_json) ->
                      graphql_gleam.json_string_to_variables_dict(vars_json)
                    None -> dict.new()
                  }

                  logging.log(
                    logging.Info,
                    "[websocket] Subscription started: " <> id <> " (field: " <> field_name <> ")",
                  )

                  // Spawn an unlinked process to listen for PubSub events
                  let listener_pid = process.spawn_unlinked(fn() {
                    subscription_listener(
                      state.subscription_subject,
                      id,
                      query,
                      field_name,
                      variables,
                      state.db,
                      state.schema,
                    )
                  })

                  // Increment global counter
                  let _ = increment_global_subscriptions()

                  // Store subscription info
                  let subscription_info =
                    SubscriptionInfo(listener_pid, field_name, variables)
                  let new_subscriptions =
                    dict.insert(state.subscriptions, id, subscription_info)
                  let new_state = State(..state, subscriptions: new_subscriptions)

                  mist.continue(new_state)
                }
              }
            }
          }
        }
      }
    }

    Ok(graphql_ws.Complete(id)) -> {
      // Client wants to stop subscription
      case dict.get(state.subscriptions, id) {
        Ok(info) -> {
          // Kill the listener process explicitly
          process.kill(info.pid)

          // Decrement global counter
          let _ = decrement_global_subscriptions()

          let new_subscriptions = dict.delete(state.subscriptions, id)
          let new_state = State(..state, subscriptions: new_subscriptions)

          logging.log(logging.Info, "[websocket] Subscription completed: " <> id)

          mist.continue(new_state)
        }
        Error(_) -> {
          logging.log(
            logging.Warning,
            "[websocket] Complete for unknown subscription: " <> id,
          )
          mist.continue(state)
        }
      }
    }

    Ok(graphql_ws.Ping) -> {
      // Respond with pong
      let pong_msg = graphql_ws.format_message(graphql_ws.Pong)
      let _ = mist.send_text_frame(conn, pong_msg)
      mist.continue(state)
    }

    Ok(graphql_ws.Pong) -> {
      // Client responded to our ping, just continue
      mist.continue(state)
    }

    Ok(_) -> {
      // Other message types (server messages we shouldn't receive)
      logging.log(
        logging.Warning,
        "[websocket] Received unexpected message type",
      )
      mist.continue(state)
    }

    Error(err) -> {
      logging.log(
        logging.Warning,
        "[websocket] Failed to parse message: " <> err,
      )
      mist.continue(state)
    }
  }
}

/// Check if an event matches the subscription field
/// Uses exact field name matching instead of string.contains
fn event_matches_subscription(
  event: pubsub.RecordEvent,
  subscription_field: String,
) -> Bool {
  // Convert collection name to GraphQL field name format
  let graphql_name = collection_to_graphql_name(event.collection)
  let event_field = case event.operation {
    pubsub.Create -> graphql_name <> "Created"
    pubsub.Update -> graphql_name <> "Updated"
    pubsub.Delete -> graphql_name <> "Deleted"
  }

  // Exact field name match
  event_field == subscription_field
}

/// Process an event and send it to the WebSocket client if it matches
fn process_event(
  event: pubsub.RecordEvent,
  subscription_subject: process.Subject(websocket_ffi.SubscriptionMessage),
  subscription_id: String,
  subscription_field: String,
  query: String,
  variables: Dict(String, value.Value),
  db: sqlight.Connection,
  graphql_schema: schema.Schema,
) -> Nil {
  case event_matches_subscription(event, subscription_field) {
    True -> {
      // Execute the GraphQL subscription query with the event data and variables
      case execute_subscription_query(query, variables, graphql_schema, event, db) {
        Ok(result_json) -> {
          // Send message to handler via Subject
          process.send(
            subscription_subject,
            websocket_ffi.SubscriptionData(subscription_id, result_json),
          )
          Nil
        }
        Error(err) -> {
          logging.log(
            logging.Error,
            "[websocket] Failed to execute subscription query: " <> err,
          )
          Nil
        }
      }
    }
    False -> Nil
  }
}

/// Event loop - processes events indefinitely
fn event_loop(
  selector: process.Selector(pubsub.RecordEvent),
  subscription_subject: process.Subject(websocket_ffi.SubscriptionMessage),
  subscription_id: String,
  subscription_field: String,
  query: String,
  variables: Dict(String, value.Value),
  db: sqlight.Connection,
  graphql_schema: schema.Schema,
) -> Nil {
  // Wait for next event
  let event = process.selector_receive_forever(selector)

  // Process the event
  process_event(
    event,
    subscription_subject,
    subscription_id,
    subscription_field,
    query,
    variables,
    db,
    graphql_schema,
  )

  // Continue listening
  event_loop(
    selector,
    subscription_subject,
    subscription_id,
    subscription_field,
    query,
    variables,
    db,
    graphql_schema,
  )
}

/// Listen for PubSub events and forward them to the WebSocket client
fn subscription_listener(
  subscription_subject: process.Subject(websocket_ffi.SubscriptionMessage),
  subscription_id: String,
  query: String,
  subscription_field: String,
  variables: Dict(String, value.Value),
  db: sqlight.Connection,
  graphql_schema: schema.Schema,
) -> Nil {
  // Subscribe to PubSub from this process
  let my_subject = pubsub.subscribe()

  // Create a selector to receive RecordEvent messages
  let selector =
    process.new_selector()
    |> process.select_map(my_subject, fn(msg) { msg })

  // Start the event loop
  event_loop(
    selector,
    subscription_subject,
    subscription_id,
    subscription_field,
    query,
    variables,
    db,
    graphql_schema,
  )
}
