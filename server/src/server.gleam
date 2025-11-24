import activity_cleanup
import argv
import backfill
import backfill_state
import config
import database/connection
import database/repositories/lexicons
import dotenv_gleam
import envoy
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/http/request
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import handlers/backfill as backfill_handler
import handlers/client_graphql as client_graphql_handler
import handlers/graphiql as graphiql_handler
import handlers/graphql as graphql_handler
import handlers/graphql_ws as graphql_ws_handler
import handlers/health as health_handler
import handlers/index as index_handler
import handlers/upload as upload_handler
import importer
import jetstream_consumer
import logging
import mist
import oauth/handlers
import oauth/registration
import pubsub
import sqlight
import stats_pubsub
import wisp
import wisp/wisp_mist

pub type Context {
  Context(
    db: sqlight.Connection,
    auth_base_url: String,
    plc_url: String,
    oauth_config: handlers.OAuthConfig,
    admin_dids: List(String),
    backfill_state: process.Subject(backfill_state.Message),
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
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
  logging.log(logging.Info, "Importing lexicons from: " <> directory)
  logging.log(logging.Info, "")

  // Get database URL from environment variable or use default
  let database_url = case envoy.get("DATABASE_URL") {
    Ok(url) -> url
    Error(_) -> "quickslice.db"
  }

  // Initialize the database
  let assert Ok(db) = connection.initialize(database_url)

  case importer.import_lexicons_from_directory(directory, db) {
    Ok(stats) -> {
      logging.log(logging.Info, "")
      logging.log(logging.Info, "Import complete!")
      logging.log(
        logging.Info,
        "   Total files: " <> int.to_string(stats.total),
      )
      logging.log(
        logging.Info,
        "   Imported: " <> int.to_string(stats.imported),
      )
      logging.log(logging.Info, "   Failed: " <> int.to_string(stats.failed))

      case stats.errors {
        [] -> Nil
        errors -> {
          logging.log(logging.Info, "")
          logging.log(logging.Warning, "Errors:")
          list.each(errors, fn(err) {
            logging.log(logging.Warning, "   " <> err)
          })
        }
      }
    }
    Error(err) -> {
      logging.log(logging.Error, "Import failed: " <> err)
    }
  }
}

fn run_backfill_command() {
  logging.log(
    logging.Info,
    "Starting backfill for record-type lexicon collections",
  )
  logging.log(logging.Info, "")

  // Get database URL from environment variable or use default
  let database_url = case envoy.get("DATABASE_URL") {
    Ok(url) -> url
    Error(_) -> "quickslice.db"
  }

  // Initialize the database
  let assert Ok(db) = connection.initialize(database_url)

  // Start config cache actor
  let assert Ok(config_subject) = config.start(db)

  // Get domain authority from config
  let domain_authority = case config.get_domain_authority(config_subject) {
    option.Some(authority) -> authority
    option.None -> {
      logging.log(
        logging.Warning,
        "No domain_authority configured. All collections will be treated as external.",
      )
      ""
    }
  }

  // Get all record-type lexicons
  logging.log(logging.Info, "Fetching record-type lexicons from database...")
  case lexicons.get_record_types(db) {
    Ok(lexicons) -> {
      case lexicons {
        [] -> {
          logging.log(
            logging.Warning,
            "No record-type lexicons found in database",
          )
          logging.log(
            logging.Info,
            "   Hint: Run 'gleam run -- import priv/lexicons' to import lexicons first",
          )
        }
        _ -> {
          // Separate lexicons by domain authority
          let #(local_lexicons, external_lexicons) =
            lexicons
            |> list.partition(fn(lex) {
              backfill.nsid_matches_domain_authority(lex.id, domain_authority)
            })

          let collections = list.map(local_lexicons, fn(lex) { lex.id })
          let external_collections =
            list.map(external_lexicons, fn(lex) { lex.id })

          logging.log(
            logging.Info,
            "Found "
              <> int.to_string(list.length(collections))
              <> " local collection(s):",
          )
          list.each(collections, fn(col) {
            logging.log(logging.Info, "  - " <> col)
          })

          case external_collections {
            [] -> Nil
            _ -> {
              logging.log(logging.Info, "")
              logging.log(
                logging.Info,
                "Found "
                  <> int.to_string(list.length(external_collections))
                  <> " external collection(s):",
              )
              list.each(external_collections, fn(col) {
                logging.log(logging.Info, "  - " <> col)
              })
            }
          }

          logging.log(logging.Info, "")
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
      logging.log(logging.Error, "Failed to fetch lexicons from database")
    }
  }
}

fn start_server_normally() {
  // Initialize logging
  logging.configure()
  logging.set_level(logging.Info)

  // Load environment variables from .env file
  let _ = dotenv_gleam.config()

  // Get database URL from environment variable or use default
  let database_url = case envoy.get("DATABASE_URL") {
    Ok(url) -> url
    Error(_) -> "quickslice.db"
  }

  // Initialize the database
  let assert Ok(db) = connection.initialize(database_url)

  // Note: Lexicon import has been moved to the settings page (ZIP upload)
  // Use the /settings page to upload a ZIP file containing lexicon JSON files

  // Initialize PubSub registry for subscriptions
  pubsub.start()
  logging.log(logging.Info, "[server] PubSub registry initialized")

  // Initialize Stats PubSub registry for real-time stats
  stats_pubsub.start()
  logging.log(logging.Info, "[server] Stats PubSub registry initialized")

  // Start activity cleanup scheduler
  case activity_cleanup.start(db) {
    Ok(_cleanup_subject) ->
      logging.log(
        logging.Info,
        "[server] Activity cleanup scheduler started (runs hourly)",
      )
    Error(err) ->
      logging.log(
        logging.Warning,
        "[server] Failed to start activity cleanup scheduler: "
          <> string.inspect(err),
      )
  }

  // Start Jetstream consumer in background
  let jetstream_subject = case jetstream_consumer.start(db) {
    Ok(subject) -> option.Some(subject)
    Error(err) -> {
      logging.log(
        logging.Error,
        "[server] Failed to start Jetstream consumer: " <> err,
      )
      logging.log(
        logging.Warning,
        "[server]    Server will continue without real-time indexing",
      )
      option.None
    }
  }

  logging.log(logging.Info, "")
  logging.log(logging.Info, "[server] === quickslice ===")
  logging.log(logging.Info, "")

  // Start server immediately (this blocks)
  start_server(db, jetstream_subject)
}

fn start_server(
  db: sqlight.Connection,
  jetstream_subject: option.Option(
    process.Subject(jetstream_consumer.ManagerMessage),
  ),
) {
  wisp.configure_logger()

  // Get priv directory for serving static files
  let assert Ok(priv_directory) = wisp.priv_directory("server")
  let static_directory = priv_directory <> "/static"

  // Get secret_key_base from environment or generate one
  let secret_key_base = case envoy.get("SECRET_KEY_BASE") {
    Ok(key) -> {
      logging.log(
        logging.Info,
        "[server] Using SECRET_KEY_BASE from environment",
      )
      key
    }
    Error(_) -> {
      logging.log(
        logging.Warning,
        "[server] WARNING: SECRET_KEY_BASE not set, generating random key",
      )
      logging.log(
        logging.Warning,
        "[server]    Sessions will be invalidated on server restart. Set SECRET_KEY_BASE in .env for persistence.",
      )
      wisp.random_string(64)
    }
  }

  // Get auth_base_url from environment variable or use default
  let auth_base_url = case envoy.get("AIP_BASE_URL") {
    Ok(url) -> url
    Error(_) -> "https://auth.example.com"
  }

  // Get HOST and PORT from environment variables or use defaults
  let host = case envoy.get("HOST") {
    Ok(h) -> h
    Error(_) -> "localhost"
  }

  let port = case envoy.get("PORT") {
    Ok(p) ->
      case int.parse(p) {
        Ok(port_num) -> port_num
        Error(_) -> 8000
      }
    Error(_) -> 8000
  }

  // OAuth configuration - check environment variables first for backwards compatibility
  let enable_auto_register = case envoy.get("ENABLE_OAUTH_AUTO_REGISTER") {
    Ok(val) -> val == "true" || val == "1"
    Error(_) -> False
  }

  // Determine redirect URI from EXTERNAL_BASE_URL environment variable
  let oauth_redirect_uri = case envoy.get("EXTERNAL_BASE_URL") {
    Ok(base_url) -> base_url <> "/oauth/callback"
    Error(_) ->
      "http://" <> host <> ":" <> int.to_string(port) <> "/oauth/callback"
  }

  let oauth_config = case
    envoy.get("OAUTH_CLIENT_ID"),
    envoy.get("OAUTH_CLIENT_SECRET")
  {
    Ok(id), Ok(secret) if id != "" && secret != "" -> {
      // Use environment variables if provided (backwards compatibility)
      logging.log(
        logging.Info,
        "[oauth] Using OAuth credentials from environment variables",
      )
      handlers.OAuthConfig(
        client_id: id,
        client_secret: secret,
        redirect_uri: oauth_redirect_uri,
        auth_url: auth_base_url,
      )
    }
    _, _ -> {
      // Try auto-registration if enabled
      case enable_auto_register {
        True -> {
          logging.log(
            logging.Info,
            "[oauth] Auto-registration enabled, checking for stored credentials...",
          )

          case
            registration.ensure_oauth_client(
              db,
              auth_base_url,
              oauth_redirect_uri,
              "Quickslice Server",
            )
          {
            Ok(config) -> config
            Error(err) -> {
              logging.log(
                logging.Error,
                "[oauth] OAuth auto-registration failed: " <> err,
              )
              logging.log(
                logging.Warning,
                "[oauth] Starting retry actor to attempt registration in background...",
              )

              // Start retry actor in background
              case
                registration.start_retry_actor(
                  db,
                  auth_base_url,
                  oauth_redirect_uri,
                  "Quickslice Server",
                )
              {
                Ok(retry_subject) -> {
                  logging.log(
                    logging.Info,
                    "[oauth] Retry actor started successfully",
                  )
                  // Trigger first retry attempt immediately
                  registration.trigger_retry(retry_subject)
                }
                Error(_) -> {
                  logging.log(
                    logging.Error,
                    "[oauth] Failed to start retry actor",
                  )
                }
              }

              // Return empty credentials for now
              logging.log(
                logging.Warning,
                "[oauth] Server will start without OAuth support until registration succeeds",
              )
              handlers.OAuthConfig(
                client_id: "",
                client_secret: "",
                redirect_uri: oauth_redirect_uri,
                auth_url: auth_base_url,
              )
            }
          }
        }
        False -> {
          // Auto-registration disabled, use empty credentials
          logging.log(
            logging.Info,
            "[oauth] Auto-registration disabled, OAuth will not be available",
          )
          handlers.OAuthConfig(
            client_id: "",
            client_secret: "",
            redirect_uri: oauth_redirect_uri,
            auth_url: auth_base_url,
          )
        }
      }
    }
  }

  logging.log(logging.Info, "[server] Using AIP server: " <> auth_base_url)

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

  // Get PLC directory URL from environment variable or use default
  let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
    Ok(url) -> url
    Error(_) -> "https://plc.directory"
  }

  // Start backfill state actor to track backfill status across requests
  let assert Ok(backfill_state_subject) = backfill_state.start()
  logging.log(logging.Info, "[server] Backfill state actor initialized")

  // Start config cache actor
  let assert Ok(config_subject) = config.start(db)
  logging.log(logging.Info, "[server] Config cache actor initialized")

  let ctx =
    Context(
      db: db,
      auth_base_url: auth_base_url,
      plc_url: plc_url,
      oauth_config: oauth_config,
      admin_dids: admin_dids,
      backfill_state: backfill_state_subject,
      config: config_subject,
      jetstream_consumer: jetstream_subject,
    )

  let handler = fn(req) { handle_request(req, ctx, static_directory) }

  logging.log(
    logging.Info,
    "[server] Server started on http://" <> host <> ":" <> int.to_string(port),
  )

  // Create Wisp handler converted to Mist format
  let wisp_handler = wisp_mist.handler(handler, secret_key_base)

  // Wrap it to intercept WebSocket upgrades for GraphQL subscriptions
  let mist_handler = fn(req: request.Request(mist.Connection)) {
    let upgrade_header = request.get_header(req, "upgrade")
    let path = request.path_segments(req)

    case path {
      // GraphQL WebSocket for subscriptions
      ["graphql"] | ["", "graphql"] -> {
        case upgrade_header {
          Ok(upgrade_value) -> {
            case string.lowercase(upgrade_value) {
              "websocket" -> {
                logging.log(
                  logging.Info,
                  "[server] Handling WebSocket upgrade for /graphql",
                )
                let domain_authority = case
                  config.get_domain_authority(ctx.config)
                {
                  option.Some(authority) -> authority
                  option.None -> ""
                }
                graphql_ws_handler.handle_websocket(
                  req,
                  ctx.db,
                  ctx.auth_base_url,
                  ctx.plc_url,
                  domain_authority,
                )
              }
              _ -> wisp_handler(req)
            }
          }
          _ -> wisp_handler(req)
        }
      }

      _ -> wisp_handler(req)
    }
  }

  let assert Ok(_) =
    mist.new(mist_handler)
    |> mist.bind(host)
    |> mist.port(port)
    |> mist.start

  process.sleep_forever()
}

fn handle_request(
  req: wisp.Request,
  ctx: Context,
  static_directory: String,
) -> wisp.Response {
  use _req <- middleware(req, static_directory)

  let segments = wisp.path_segments(req)

  case segments {
    [] -> index_handler.handle()
    ["health"] -> health_handler.handle(ctx.db)
    ["oauth", "authorize"] ->
      handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
    ["oauth", "callback"] ->
      handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
    ["logout"] -> handlers.handle_logout(req, ctx.db)
    ["backfill"] -> backfill_handler.handle(req, ctx.db, ctx.config)
    ["admin", "graphql"] ->
      client_graphql_handler.handle_client_graphql_request(
        req,
        ctx.db,
        ctx.admin_dids,
        ctx.jetstream_consumer,
      )
    ["graphql"] ->
      graphql_handler.handle_graphql_request(
        req,
        ctx.db,
        ctx.auth_base_url,
        ctx.plc_url,
      )
    ["graphiql"] ->
      graphiql_handler.handle_graphiql_request(req, ctx.db, ctx.oauth_config)
    ["upload"] ->
      upload_handler.handle_upload_request(req, ctx.db, ctx.oauth_config)
    // Fallback: serve SPA index.html for client-side routing
    _ -> index_handler.handle()
  }
}

fn middleware(
  req: wisp.Request,
  static_directory: String,
  handle_request: fn(wisp.Request) -> wisp.Response,
) -> wisp.Response {
  use <- wisp.rescue_crashes
  use <- wisp.log_request(req)
  use req <- wisp.handle_head(req)
  use <- wisp.serve_static(req, under: "/", from: static_directory)

  // Get origin from request headers
  let origin = case request.get_header(req, "origin") {
    Ok(o) -> o
    Error(_) -> "http://localhost:8000"
  }

  // Handle CORS preflight requests
  case req.method {
    gleam_http.Options -> {
      wisp.response(200)
      |> wisp.set_header("access-control-allow-origin", origin)
      |> wisp.set_header("access-control-allow-credentials", "true")
      |> wisp.set_header("access-control-allow-methods", "GET, POST, OPTIONS")
      |> wisp.set_header("access-control-allow-headers", "Content-Type")
      |> wisp.set_body(wisp.Text(""))
    }
    _ -> {
      // Add CORS headers to all responses
      handle_request(req)
      |> wisp.set_header("access-control-allow-origin", origin)
      |> wisp.set_header("access-control-allow-credentials", "true")
      |> wisp.set_header("access-control-allow-methods", "GET, POST, OPTIONS")
      |> wisp.set_header("access-control-allow-headers", "Content-Type")
    }
  }
}
