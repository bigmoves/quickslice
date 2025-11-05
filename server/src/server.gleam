import argv
import backfill
import database
import dotenv_gleam
import envoy
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import graphiql_handler
import graphql_handler
import importer
import jetstream_consumer
import lustre/element
import mist
import oauth/handlers
import oauth/session
import pages/index
import sqlight
import upload_handler
import wisp
import wisp/wisp_mist
import xrpc_handlers
import xrpc_router

pub type Context {
  Context(
    db: sqlight.Connection,
    auth_base_url: String,
    oauth_config: handlers.OAuthConfig,
    admin_dids: List(String),
  )
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

  // Get database URL from environment variable or use default
  let database_url = case envoy.get("DATABASE_URL") {
    Ok(url) -> url
    Error(_) -> "quickslice.db"
  }

  // Initialize the database
  let assert Ok(db) = database.initialize(database_url)

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
          // Separate lexicons by domain authority
          let #(local_lexicons, external_lexicons) =
            lexicons
            |> list.partition(fn(lex) {
              backfill.nsid_matches_domain_authority(lex.id)
            })

          let collections = list.map(local_lexicons, fn(lex) { lex.id })
          let external_collections =
            list.map(external_lexicons, fn(lex) { lex.id })

          io.println(
            "✓ Found "
            <> int.to_string(list.length(collections))
            <> " local collection(s):",
          )
          list.each(collections, fn(col) { io.println("  - " <> col) })

          case external_collections {
            [] -> Nil
            _ -> {
              io.println("")
              io.println(
                "✓ Found "
                <> int.to_string(list.length(external_collections))
                <> " external collection(s):",
              )
              list.each(external_collections, fn(col) {
                io.println("  - " <> col)
              })
            }
          }

          io.println("")
          let config = backfill.default_config()
          backfill.backfill_collections(
            [],
            collections,
            external_collections,
            config,
            db,
          )
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

  // Get database URL from environment variable or use default
  let database_url = case envoy.get("DATABASE_URL") {
    Ok(url) -> url
    Error(_) -> "quickslice.db"
  }

  // Initialize the database
  let assert Ok(db) = database.initialize(database_url)

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

  // Get secret_key_base from environment or generate one
  let secret_key_base = case envoy.get("SECRET_KEY_BASE") {
    Ok(key) -> {
      io.println("✓ Using SECRET_KEY_BASE from environment")
      key
    }
    Error(_) -> {
      io.println(
        "⚠️  WARNING: SECRET_KEY_BASE not set, generating random key",
      )
      io.println(
        "   Sessions will be invalidated on server restart. Set SECRET_KEY_BASE in .env for persistence.",
      )
      wisp.random_string(64)
    }
  }

  // Get auth_base_url from environment variable or use default
  let auth_base_url = case envoy.get("AIP_BASE_URL") {
    Ok(url) -> url
    Error(_) -> "https://tunnel.chadtmiller.com"
  }

  // OAuth configuration
  let oauth_client_id = case envoy.get("OAUTH_CLIENT_ID") {
    Ok(id) -> id
    Error(_) -> ""
  }

  let oauth_client_secret = case envoy.get("OAUTH_CLIENT_SECRET") {
    Ok(secret) -> secret
    Error(_) -> ""
  }

  let oauth_redirect_uri = case envoy.get("OAUTH_REDIRECT_URI") {
    Ok(uri) -> uri
    Error(_) -> "http://localhost:8000/oauth/callback"
  }

  let oauth_config =
    handlers.OAuthConfig(
      client_id: oauth_client_id,
      client_secret: oauth_client_secret,
      redirect_uri: oauth_redirect_uri,
      auth_url: auth_base_url,
    )

  // Get HOST and PORT from environment variables or use defaults
  let host = case envoy.get("HOST") {
    Ok(h) -> h
    Error(_) -> "127.0.0.1"
  }

  let port = case envoy.get("PORT") {
    Ok(p) ->
      case int.parse(p) {
        Ok(port_num) -> port_num
        Error(_) -> 8000
      }
    Error(_) -> 8000
  }

  io.println("🔐 Using AIP server: " <> auth_base_url)

  // Parse ADMIN_DIDS from environment variable (comma-separated list)
  let admin_dids = case envoy.get("ADMIN_DIDS") {
    Ok(dids_str) -> {
      dids_str
      |> string.split(",")
      |> list.map(string.trim)
      |> list.filter(fn(did) { !string.is_empty(did) })
    }
    Error(_) -> []
  }

  let ctx =
    Context(
      db: db,
      auth_base_url: auth_base_url,
      oauth_config: oauth_config,
      admin_dids: admin_dids,
    )

  let handler = fn(req) { handle_request(req, ctx) }

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.bind(host)
    |> mist.port(port)
    |> mist.start

  io.println("Server started on http://" <> host <> ":" <> int.to_string(port))
  process.sleep_forever()
}

/// Check if a DID has admin access based on the ADMIN_DIDS list
fn is_admin(did: String, admin_dids: List(String)) -> Bool {
  list.contains(admin_dids, did)
}

fn handle_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  use _req <- middleware(req)

  let segments = wisp.path_segments(req)

  case segments {
    [] -> index_route(req, ctx)
    ["health"] -> handle_health_check(ctx)
    ["oauth", "authorize"] ->
      handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
    ["oauth", "callback"] ->
      handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
    ["logout"] -> handlers.handle_logout(req, ctx.db)
    ["backfill"] -> handle_backfill_request(req, ctx.db)
    ["graphql"] ->
      graphql_handler.handle_graphql_request(req, ctx.db, ctx.auth_base_url)
    ["graphiql"] ->
      graphiql_handler.handle_graphiql_request(req, ctx.db, ctx.oauth_config)
    ["upload"] ->
      upload_handler.handle_upload_request(req, ctx.db, ctx.oauth_config)
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

fn handle_backfill_request(
  req: wisp.Request,
  db: sqlight.Connection,
) -> wisp.Response {
  case req.method {
    gleam_http.Post -> {
      // Get all record-type lexicons
      case database.get_record_type_lexicons(db) {
        Ok(lexicons) -> {
          case lexicons {
            [] -> {
              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"no_lexicons\", \"message\": \"No record-type lexicons found\"}",
              ))
            }
            _ -> {
              // Separate lexicons by domain authority
              let #(collections, external_collections) =
                lexicons
                |> list.partition(fn(lex) {
                  backfill.nsid_matches_domain_authority(lex.id)
                })

              let collection_ids = list.map(collections, fn(lex) { lex.id })
              let external_collection_ids =
                list.map(external_collections, fn(lex) { lex.id })

              // Run backfill in background process
              let config = backfill.default_config()
              process.spawn_unlinked(fn() {
                backfill.backfill_collections(
                  [],
                  collection_ids,
                  external_collection_ids,
                  config,
                  db,
                )
              })

              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"status\": \"started\", \"collections\": "
                <> int.to_string(list.length(collection_ids))
                <> ", \"external_collections\": "
                <> int.to_string(list.length(external_collection_ids))
                <> "}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(500)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"database_error\", \"message\": \"Failed to fetch lexicons\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"method_not_allowed\", \"message\": \"Use POST to trigger backfill\"}",
      ))
    }
  }
}

fn handle_health_check(ctx: Context) -> wisp.Response {
  // Try a simple database query to verify connectivity
  case database.get_lexicon_count(ctx.db) {
    Ok(_) -> {
      // Database is accessible
      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text("{\"status\": \"healthy\"}"))
    }
    Error(_) -> {
      // Database is not accessible
      wisp.response(503)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"status\": \"unhealthy\", \"message\": \"Database connection failed\"}",
      ))
    }
  }
}

fn index_route(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Get current user from session (with automatic token refresh)
  let refresh_fn = fn(refresh_token) {
    handlers.refresh_access_token(ctx.oauth_config, refresh_token)
  }

  let #(current_user, user_is_admin) =
    case session.get_current_user(req, ctx.db, refresh_fn) {
      Ok(#(did, handle, _access_token)) -> {
        let admin = is_admin(did, ctx.admin_dids)
        #(option.Some(#(did, handle)), admin)
      }
      Error(_) -> #(option.None, False)
    }

  index.view(ctx.db, current_user, user_is_admin)
  |> element.to_document_string
  |> wisp.html_response(200)
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
