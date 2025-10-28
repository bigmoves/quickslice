import argv
import backfill
import database
import dotenv_gleam
import envoy
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/otp/actor
import graphiql_handler
import graphql_handler
import importer
import jetstream
import jetstream_consumer
import lustre/attribute
import lustre/element
import lustre/element/html
import mist
import sqlight
import wisp
import wisp/wisp_mist
import xrpc_handlers
import xrpc_router

pub type Context {
  Context(db: sqlight.Connection, auth_base_url: String)
}

pub type BackfillMessage {
  StartLexiconBackfill(reply_to: Subject(Nil))
  StartCustomBackfill(collections: List(String), reply_to: Subject(Nil))
}

fn handle_backfill(db: sqlight.Connection, message: BackfillMessage) {
  case message {
    StartLexiconBackfill(client) -> {
      io.println("🔄 Starting lexicon schema backfill...")
      backfill_lexicon_schemas(db)

      // After lexicon backfill, check which collections have lexicons
      io.println("")
      io.println("🔍 Checking collections for lexicons...")

      let collections_to_check = ["xyz.statusphere.status"]

      let collections_with_lexicons =
        collections_to_check
        |> list.filter(fn(collection) {
          case database.has_lexicon_for_collection(db, collection) {
            Ok(True) -> {
              io.println("  ✓ Found lexicon for: " <> collection)
              True
            }
            Ok(False) -> {
              io.println("  ✗ No lexicon for: " <> collection)
              False
            }
            Error(_) -> {
              io.println("  ⚠️ Error checking lexicon for: " <> collection)
              False
            }
          }
        })

      case collections_with_lexicons {
        [] -> {
          io.println("")
          io.println(
            "⚠️ No collections with lexicons found - skipping custom backfill",
          )
        }
        _ -> {
          io.println("")
          io.println(
            "📋 Starting custom backfill for "
            <> int.to_string(list.length(collections_with_lexicons))
            <> " collections with lexicons...",
          )
          run_custom_backfill_for_collections(db, collections_with_lexicons)
        }
      }

      process.send(client, Nil)
      actor.continue(db)
    }
    StartCustomBackfill(collections, client) -> {
      io.println("🔄 Starting custom backfill for specified collections...")
      run_custom_backfill_for_collections(db, collections)
      process.send(client, Nil)
      actor.continue(db)
    }
  }
}

pub fn main() {
  // Check for CLI arguments
  case argv.load().arguments {
    ["import", directory] -> run_import_command(directory)
    ["backfill"] -> run_backfill_command()
    _ -> start_server_normally()
  }
}

fn run_import_command(directory: String) {
  io.println("🔄 Importing lexicons from: " <> directory)
  io.println("")

  case importer.import_lexicons_from_directory(directory) {
    Ok(stats) -> {
      io.println("")
      io.println("✅ Import complete!")
      io.println("   Total files: " <> int.to_string(stats.total))
      io.println("   Imported: " <> int.to_string(stats.imported))
      io.println("   Failed: " <> int.to_string(stats.failed))

      case stats.errors {
        [] -> Nil
        errors -> {
          io.println("")
          io.println("⚠️ Errors:")
          list.each(errors, fn(err) { io.println("   " <> err) })
        }
      }
    }
    Error(err) -> {
      io.println_error("❌ Import failed: " <> err)
    }
  }
}

fn run_backfill_command() {
  io.println("🔄 Starting backfill for record-type lexicon collections")
  io.println("")

  // Initialize the database
  let assert Ok(db) = database.initialize("atproto.db")

  // Get all record-type lexicons
  io.println("📚 Fetching record-type lexicons from database...")
  case database.get_record_type_lexicons(db) {
    Ok(lexicons) -> {
      case lexicons {
        [] -> {
          io.println("⚠️ No record-type lexicons found in database")
          io.println(
            "   Hint: Run 'gleam run -- import priv/lexicons' to import lexicons first",
          )
        }
        _ -> {
          let collections = list.map(lexicons, fn(lex) { lex.id })
          io.println(
            "✓ Found "
            <> int.to_string(list.length(collections))
            <> " record-type collection(s):",
          )
          list.each(collections, fn(col) { io.println("  - " <> col) })

          io.println("")
          let config = backfill.default_config()
          backfill.backfill_collections([], collections, [], config, db)
        }
      }
    }
    Error(_) -> {
      io.println_error("❌ Failed to fetch lexicons from database")
    }
  }
}

fn start_server_normally() {
  // Load environment variables from .env file
  let _ = dotenv_gleam.config()

  // Initialize the database
  let assert Ok(db) = database.initialize("atproto.db")

  // Auto-import lexicons from priv/lexicons if directory exists
  io.println("")
  io.println("🔍 Checking for lexicons in priv/lexicons...")
  case importer.import_lexicons_from_directory("priv/lexicons") {
    Ok(stats) -> {
      case stats.imported {
        0 -> io.println("  ℹ️  No lexicons found to import")
        _ -> {
          io.println(
            "  ✓ Imported " <> int.to_string(stats.imported) <> " lexicon(s)",
          )
        }
      }
    }
    Error(_) -> {
      io.println("  ℹ️  No priv/lexicons directory found, skipping import")
    }
  }

  // Start Jetstream consumer in background
  case jetstream_consumer.start(db) {
    Ok(_) -> Nil
    Error(err) -> {
      io.println_error("❌ Failed to start Jetstream consumer: " <> err)
      io.println("   Server will continue without real-time indexing")
    }
  }

  io.println("")
  io.println("=== ATProto Gleam ===")
  io.println("")

  // Start server immediately (this blocks)
  start_server(db)
}

fn start_server(db: sqlight.Connection) {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  // Get auth_base_url from environment variable or use default
  let auth_base_url = case envoy.get("AIP_BASE_URL") {
    Ok(url) -> url
    Error(_) -> "https://tunnel.chadtmiller.com"
  }

  io.println("🔐 Using AIP server: " <> auth_base_url)

  let ctx = Context(db: db, auth_base_url: auth_base_url)

  let handler = fn(req) { handle_request(req, ctx) }

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(8000)
    |> mist.start

  io.println("Server started on http://localhost:8000")
  process.sleep_forever()
}

fn start_jetstream(db: sqlight.Connection) {
  // Create a configuration for Jetstream
  // Listen to commit events only (posts, likes, reposts, follows)
  let config =
    jetstream.JetstreamConfig(
      endpoint: "wss://jetstream2.us-west.bsky.network/subscribe",
      wanted_collections: [],
      wanted_dids: [],
    )

  // Start the consumer with an event handler that identifies commit events
  jetstream.start_consumer(config, fn(event_json) {
    case jetstream.parse_event(event_json) {
      jetstream.CommitEvent(did, _time_us, commit) -> {
        io.println("✨ COMMIT EVENT")
        io.println("  DID: " <> did)
        io.println("  Operation: " <> commit.operation)
        io.println("  Collection: " <> commit.collection)
        io.println("  Record key: " <> commit.rkey)
        io.println("  Revision: " <> commit.rev)
        io.println("---")
      }
      jetstream.IdentityEvent(did, _time_us, _identity) -> {
        io.println("👤 IDENTITY EVENT: " <> did)
        io.println("---")
      }
      jetstream.AccountEvent(did, _time_us, _account) -> {
        io.println("🔐 ACCOUNT EVENT: " <> did)
        io.println("---")
      }
      jetstream.UnknownEvent(_raw) -> {
        // Ignore unknown events
        Nil
      }
    }
  })
}

fn handle_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  use _req <- middleware(req)

  let segments = wisp.path_segments(req)

  case segments {
    [] -> index_route(ctx)
    ["graphql"] -> graphql_handler.handle_graphql_request(req, ctx.db)
    ["graphiql"] -> graphiql_handler.handle_graphiql_request(req)
    ["xrpc", _] -> {
      // Try to parse the XRPC route
      case xrpc_router.parse_xrpc_path(segments) {
        option.Some(route) -> {
          // Check if lexicon exists for this NSID
          case xrpc_router.validate_nsid(ctx.db, route.nsid) {
            True -> {
              // Route to the appropriate handler based on method
              case xrpc_router.parse_method(route.method) {
                xrpc_router.CreateRecord ->
                  xrpc_handlers.handle_create_record(
                    req,
                    ctx.db,
                    route.nsid,
                    ctx.auth_base_url,
                  )
                xrpc_router.UpdateRecord ->
                  xrpc_handlers.handle_update_record(req, ctx.db, route.nsid)
                xrpc_router.DeleteRecord ->
                  xrpc_handlers.handle_delete_record(req, ctx.db, route.nsid)
                xrpc_router.GetRecord ->
                  xrpc_handlers.handle_get_record(req, ctx.db, route.nsid)
                xrpc_router.UnknownMethod -> {
                  wisp.response(404)
                  |> wisp.set_header("content-type", "application/json")
                  |> wisp.set_body(wisp.Text(
                    "{\"error\": \"MethodNotSupported\", \"message\": \"Unknown XRPC method: "
                    <> route.method
                    <> "\"}",
                  ))
                }
              }
            }
            False -> {
              // No lexicon found for this NSID
              wisp.response(404)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"LexiconNotFound\", \"message\": \"No lexicon found for collection: "
                <> route.nsid
                <> "\"}",
              ))
            }
          }
        }
        option.None -> {
          // Invalid XRPC path format
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"InvalidRequest\", \"message\": \"Invalid XRPC path format\"}",
          ))
        }
      }
    }
    _ -> wisp.html_response("<h1>Not Found</h1>", 404)
  }
}

fn index_route(ctx: Context) -> wisp.Response {
  // Query database stats
  let collection_stats = case database.get_collection_stats(ctx.db) {
    Ok(stats) -> stats
    Error(_) -> []
  }

  let actor_count = case database.get_actor_count(ctx.db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  let lexicon_count = case database.get_lexicon_count(ctx.db) {
    Ok(count) -> count
    Error(_) -> 0
  }

  // Get record-type lexicons (collections)
  let record_lexicons = case database.get_record_type_lexicons(ctx.db) {
    Ok(lexicons) -> lexicons
    Error(_) -> []
  }

  // Build collection rows from actual records
  let record_rows =
    collection_stats
    |> list.map(fn(stat) {
      html.tr([attribute.class("hover:bg-gray-50 transition-colors")], [
        html.td([attribute.class("px-4 py-3 text-sm text-gray-900")], [
          element.text(stat.collection),
        ]),
        html.td([attribute.class("px-4 py-3 text-sm text-gray-700")], [
          element.text(int.to_string(stat.count)),
        ]),
      ])
    })

  // Build lexicon rows (show lexicons that don't have records yet)
  let lexicon_rows =
    record_lexicons
    |> list.filter(fn(lexicon) {
      // Only show lexicons that don't already appear in collection_stats
      !list.any(collection_stats, fn(stat) { stat.collection == lexicon.id })
    })
    |> list.map(fn(lexicon) {
      html.tr([attribute.class("hover:bg-gray-50 transition-colors")], [
        html.td([attribute.class("px-4 py-3 text-sm text-gray-900")], [
          element.text(lexicon.id),
        ]),
        html.td([attribute.class("px-4 py-3 text-sm text-gray-500 italic")], [
          element.text("0"),
        ]),
      ])
    })

  // Combine both types of rows
  let collection_rows = list.append(record_rows, lexicon_rows)

  let page =
    html.html([attribute.class("h-full")], [
      html.head([], [
        html.title([], "ATProto Database Stats"),
        element.element("meta", [attribute.attribute("charset", "UTF-8")], []),
        element.element(
          "meta",
          [
            attribute.attribute("name", "viewport"),
            attribute.attribute(
              "content",
              "width=device-width, initial-scale=1.0",
            ),
          ],
          [],
        ),
        element.element(
          "script",
          [attribute.attribute("src", "https://cdn.tailwindcss.com")],
          [],
        ),
      ]),
      html.body([attribute.class("bg-gray-50 min-h-screen p-8")], [
        html.div([attribute.class("max-w-4xl mx-auto")], [
          html.div([attribute.class("flex justify-between items-center mb-8")], [
            html.h1([attribute.class("text-4xl font-bold text-gray-900")], [
              element.text("quickslice"),
            ]),
            html.a(
              [
                attribute.href("/graphiql"),
                attribute.class(
                  "bg-purple-600 hover:bg-purple-700 text-white font-semibold py-2 px-4 rounded-lg transition-colors shadow-sm",
                ),
              ],
              [element.text("Open GraphiQL")],
            ),
          ]),
          // Lexicons section
          html.div([attribute.class("mb-8")], [
            html.h2(
              [attribute.class("text-2xl font-semibold text-gray-700 mb-4")],
              [element.text("Lexicons")],
            ),
            html.div(
              [
                attribute.class(
                  "bg-purple-50 rounded-lg p-6 border border-purple-100 shadow-sm",
                ),
              ],
              [
                html.div(
                  [attribute.class("text-4xl font-bold text-purple-600 mb-2")],
                  [element.text(int.to_string(lexicon_count))],
                ),
                html.div([attribute.class("text-gray-600")], [
                  element.text("Lexicon schemas loaded"),
                ]),
              ],
            ),
          ]),
          // Actors section
          html.div([attribute.class("mb-8")], [
            html.h2(
              [attribute.class("text-2xl font-semibold text-gray-700 mb-4")],
              [element.text("Actors")],
            ),
            html.div(
              [
                attribute.class(
                  "bg-blue-50 rounded-lg p-6 border border-blue-100 shadow-sm",
                ),
              ],
              [
                html.div(
                  [attribute.class("text-4xl font-bold text-blue-600 mb-2")],
                  [element.text(int.to_string(actor_count))],
                ),
                html.div([attribute.class("text-gray-600")], [
                  element.text("Total actors indexed"),
                ]),
              ],
            ),
          ]),
          // Collections section
          html.div([], [
            html.h2(
              [attribute.class("text-2xl font-semibold text-gray-700 mb-4")],
              [element.text("Collections")],
            ),
            html.div(
              [
                attribute.class(
                  "bg-white rounded-lg shadow-sm border border-gray-200 overflow-hidden",
                ),
              ],
              [
                html.table(
                  [attribute.class("min-w-full divide-y divide-gray-200")],
                  [
                    html.thead([attribute.class("bg-gray-50")], [
                      html.tr([], [
                        html.th(
                          [
                            attribute.class(
                              "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider",
                            ),
                          ],
                          [element.text("Collection")],
                        ),
                        html.th(
                          [
                            attribute.class(
                              "px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider",
                            ),
                          ],
                          [element.text("Record Count")],
                        ),
                      ]),
                    ]),
                    html.tbody(
                      [attribute.class("bg-white divide-y divide-gray-200")],
                      collection_rows,
                    ),
                  ],
                ),
              ],
            ),
          ]),
        ]),
      ]),
    ])

  let html_string = element.to_document_string(page)
  wisp.html_response(html_string, 200)
}

fn middleware(
  req: wisp.Request,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  use <- wisp.rescue_crashes
  use <- wisp.log_request(req)
  use req <- wisp.handle_head(req)

  handle_request(req)
}

/// Backfills com.atproto.lexicon.schema collections on startup.
/// This function auto-discovers repositories from the relay that have lexicon schemas
/// and indexes them into the local database.
/// Note: Actor indexing is disabled for lexicon schemas.
fn backfill_lexicon_schemas(db: sqlight.Connection) {
  let repos = []
  let collections = ["com.atproto.lexicon.schema"]
  let external_collections = []
  let config =
    backfill.BackfillConfig(
      plc_directory_url: "https://plc.directory",
      index_actors: False,
      max_workers: 10,
    )

  backfill.backfill_collections(
    repos,
    collections,
    external_collections,
    config,
    db,
  )

  io.println("✅ Lexicon schema backfill complete")
}

/// Run a custom backfill for specific collections that have lexicons.
/// This backfills actual records (posts, follows, etc.) after verifying lexicons exist.
fn run_custom_backfill_for_collections(
  db: sqlight.Connection,
  collections: List(String),
) {
  let repos = []
  let external_collections = []
  let config = backfill.default_config()

  backfill.backfill_collections(
    repos,
    collections,
    external_collections,
    config,
    db,
  )

  io.println("✅ Custom backfill complete")
}
