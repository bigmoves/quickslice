import activity_cleanup
import argv
import backfill
import backfill_state
import database/connection
import database/repositories/config as config_repo
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
import gleam/uri
import handlers/admin_oauth_authorize as admin_oauth_authorize_handler
import handlers/admin_oauth_callback as admin_oauth_callback_handler
import handlers/client_graphql as client_graphql_handler
import handlers/graphiql as graphiql_handler
import handlers/graphql as graphql_handler
import handlers/graphql_ws as graphql_ws_handler
import handlers/health as health_handler
import handlers/index as index_handler
import handlers/logout as logout_handler
import handlers/mcp as mcp_handler
import handlers/oauth/atp_callback as oauth_atp_callback_handler
import handlers/oauth/atp_session as oauth_atp_session_handler
import handlers/oauth/authorize as oauth_authorize_handler
import handlers/oauth/client_metadata as oauth_client_metadata_handler
import handlers/oauth/dpop_nonce as oauth_dpop_nonce_handler
import handlers/oauth/jwks as oauth_jwks_handler
import handlers/oauth/metadata as oauth_metadata_handler
import handlers/oauth/par as oauth_par_handler
import handlers/oauth/register as oauth_register_handler
import handlers/oauth/token as oauth_token_handler
import handlers/upload as upload_handler
import importer
import jetstream_consumer
import lib/oauth/did_cache
import logging
import mist
import pubsub
import sqlight
import stats_pubsub
import wisp
import wisp/wisp_mist

pub type Context {
  Context(
    db: sqlight.Connection,
    external_base_url: String,
    backfill_state: process.Subject(backfill_state.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
    oauth_signing_key: option.Option(String),
    oauth_loopback_mode: Bool,
    /// AT Protocol client_id for OAuth (metadata URL or loopback client_id)
    atp_client_id: String,
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

  // Get domain authority from database
  let domain_authority = case config_repo.get(db, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> {
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
          let config = backfill.default_config(db)
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

  // Initialize config defaults
  let _ = config_repo.initialize_config_defaults(db)

  // Initialize HTTP connection pool for backfill/DID resolution
  backfill.configure_hackney_pool(150)

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

  // Get HOST and PORT from environment variables or use defaults
  let host = case envoy.get("HOST") {
    Ok(h) -> h
    Error(_) -> "localhost"
  }

  let port = case envoy.get("PORT") {
    Ok(p) ->
      case int.parse(p) {
        Ok(port_num) -> port_num
        Error(_) -> 8080
      }
    Error(_) -> 8080
  }

  // Determine external base URL from EXTERNAL_BASE_URL environment variable
  let external_base_url = case envoy.get("EXTERNAL_BASE_URL") {
    Ok(base_url) -> base_url
    Error(_) -> "http://" <> host <> ":" <> int.to_string(port)
  }

  // Get OAuth signing key from environment variable (multibase format)
  let oauth_signing_key = case envoy.get("OAUTH_SIGNING_KEY") {
    Ok(key) if key != "" -> {
      logging.log(
        logging.Info,
        "[oauth] Using OAUTH_SIGNING_KEY from environment",
      )
      option.Some(key)
    }
    _ -> {
      logging.log(
        logging.Warning,
        "[oauth] OAUTH_SIGNING_KEY not set, JWT signing and JWKS will be unavailable",
      )
      option.None
    }
  }

  // Get OAuth loopback mode from environment variable
  // When true, uses loopback client IDs (http://localhost/?redirect_uri=...)
  // instead of client metadata URLs, allowing local development without ngrok
  let oauth_loopback_mode = case envoy.get("OAUTH_LOOPBACK_MODE") {
    Ok("true") -> {
      logging.log(
        logging.Info,
        "[oauth] Loopback mode enabled - using loopback client IDs",
      )
      True
    }
    _ -> False
  }

  // Start backfill state actor to track backfill status across requests
  let assert Ok(backfill_state_subject) = backfill_state.start()
  logging.log(logging.Info, "[server] Backfill state actor initialized")

  // Start DID cache actor
  let assert Ok(did_cache_subject) = did_cache.start()
  logging.log(logging.Info, "[server] DID cache actor initialized")

  // Compute ATP client_id once (used for token refresh)
  let atp_client_id = case oauth_loopback_mode {
    True ->
      build_loopback_client_id(
        external_base_url <> "/oauth/atp/callback",
        "atproto transition:generic",
      )
    False -> external_base_url <> "/oauth-client-metadata.json"
  }

  let ctx =
    Context(
      db: db,
      external_base_url: external_base_url,
      backfill_state: backfill_state_subject,
      jetstream_consumer: jetstream_subject,
      did_cache: did_cache_subject,
      oauth_signing_key: oauth_signing_key,
      oauth_loopback_mode: oauth_loopback_mode,
      atp_client_id: atp_client_id,
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
                  config_repo.get(ctx.db, "domain_authority")
                {
                  Ok(authority) -> authority
                  Error(_) -> ""
                }
                graphql_ws_handler.handle_websocket(
                  req,
                  ctx.db,
                  ctx.did_cache,
                  ctx.oauth_signing_key,
                  ctx.atp_client_id,
                  config_repo.get_plc_directory_url(ctx.db),
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

/// Build a loopback client ID for OAuth with native apps
/// Format: http://localhost/?redirect_uri=...&scope=...
/// Per RFC 8252, redirect_uri must use 127.0.0.1 (not localhost)
fn build_loopback_client_id(redirect_uri: String, scope: String) -> String {
  "http://localhost/?redirect_uri="
  <> uri.percent_encode(redirect_uri)
  <> "&scope="
  <> uri.percent_encode(scope)
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
    ["logout"] -> logout_handler.handle(req, ctx.db)
    ["admin", "oauth", "authorize"] -> {
      let redirect_uri = ctx.external_base_url <> "/admin/oauth/callback"
      let client_id = case ctx.oauth_loopback_mode {
        True ->
          build_loopback_client_id(
            redirect_uri,
            config_repo.get_oauth_supported_scopes(ctx.db),
          )
        False -> ctx.external_base_url <> "/oauth-client-metadata.json"
      }
      admin_oauth_authorize_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        redirect_uri,
        client_id,
        ctx.oauth_signing_key,
        config_repo.get_oauth_supported_scopes_list(ctx.db),
      )
    }
    ["admin", "oauth", "callback"] -> {
      let redirect_uri = ctx.external_base_url <> "/admin/oauth/callback"
      let client_id = case ctx.oauth_loopback_mode {
        True ->
          build_loopback_client_id(
            redirect_uri,
            config_repo.get_oauth_supported_scopes(ctx.db),
          )
        False -> ctx.external_base_url <> "/oauth-client-metadata.json"
      }
      admin_oauth_callback_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        redirect_uri,
        client_id,
        ctx.oauth_signing_key,
      )
    }
    ["admin", "graphql"] ->
      client_graphql_handler.handle_client_graphql_request(
        req,
        ctx.db,
        ctx.jetstream_consumer,
        ctx.did_cache,
        config_repo.get_oauth_supported_scopes_list(ctx.db),
        ctx.backfill_state,
      )
    ["graphql"] ->
      graphql_handler.handle_graphql_request(
        req,
        ctx.db,
        ctx.did_cache,
        ctx.oauth_signing_key,
        ctx.atp_client_id,
        config_repo.get_plc_directory_url(ctx.db),
      )
    ["graphiql"] ->
      graphiql_handler.handle_graphiql_request(req, ctx.db, ctx.did_cache)
    ["upload"] ->
      upload_handler.handle_upload_request(req, ctx.db, ctx.did_cache)
    // MCP endpoint for AI assistant introspection
    ["mcp"] -> {
      let mcp_ctx =
        mcp_handler.McpContext(
          db: ctx.db,
          external_base_url: ctx.external_base_url,
          did_cache: ctx.did_cache,
          signing_key: ctx.oauth_signing_key,
          plc_url: config_repo.get_plc_directory_url(ctx.db),
          supported_scopes: config_repo.get_oauth_supported_scopes_list(ctx.db),
        )
      mcp_handler.handle(req, mcp_ctx)
    }
    // New OAuth 2.0 endpoints
    [".well-known", "oauth-authorization-server"] ->
      oauth_metadata_handler.handle(
        ctx.external_base_url,
        config_repo.get_oauth_supported_scopes_list(ctx.db),
      )
    [".well-known", "jwks.json"] ->
      oauth_jwks_handler.handle(ctx.oauth_signing_key)
    ["oauth-client-metadata.json"] ->
      oauth_client_metadata_handler.handle(
        ctx.external_base_url,
        "Quickslice Server",
        [
          ctx.external_base_url <> "/admin/oauth/callback",
          ctx.external_base_url <> "/oauth/atp/callback",
        ],
        config_repo.get_oauth_supported_scopes(ctx.db),
        option.None,
        option.Some(ctx.external_base_url <> "/.well-known/jwks.json"),
      )
    ["oauth", "dpop", "nonce"] -> oauth_dpop_nonce_handler.handle(ctx.db)
    ["oauth", "register"] -> oauth_register_handler.handle(req, ctx.db)
    ["oauth", "par"] -> oauth_par_handler.handle(req, ctx.db)
    ["oauth", "authorize"] -> {
      let redirect_uri = ctx.external_base_url <> "/oauth/atp/callback"
      let client_id = case ctx.oauth_loopback_mode {
        True ->
          build_loopback_client_id(
            redirect_uri,
            config_repo.get_oauth_supported_scopes(ctx.db),
          )
        False -> ctx.external_base_url <> "/oauth-client-metadata.json"
      }
      oauth_authorize_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        redirect_uri,
        client_id,
        ctx.oauth_signing_key,
      )
    }

    ["oauth", "token"] ->
      oauth_token_handler.handle(req, ctx.db, ctx.external_base_url)
    ["oauth", "atp", "callback"] -> {
      let redirect_uri = ctx.external_base_url <> "/oauth/atp/callback"
      let client_id = case ctx.oauth_loopback_mode {
        True ->
          build_loopback_client_id(
            redirect_uri,
            config_repo.get_oauth_supported_scopes(ctx.db),
          )
        False -> ctx.external_base_url <> "/oauth-client-metadata.json"
      }
      oauth_atp_callback_handler.handle(
        req,
        ctx.db,
        ctx.did_cache,
        redirect_uri,
        client_id,
        ctx.oauth_signing_key,
      )
    }
    ["api", "atp", "sessions", session_id] ->
      oauth_atp_session_handler.handle(req, ctx.db, session_id)
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
    Error(_) -> "http://localhost:8080"
  }

  // Handle CORS preflight requests
  case req.method {
    gleam_http.Options -> {
      wisp.response(200)
      |> wisp.set_header("access-control-allow-origin", origin)
      |> wisp.set_header("access-control-allow-credentials", "true")
      |> wisp.set_header("access-control-allow-methods", "GET, POST, OPTIONS")
      |> wisp.set_header(
        "access-control-allow-headers",
        "Content-Type, Authorization, DPoP",
      )
      |> wisp.set_body(wisp.Text(""))
    }
    _ -> {
      // Add CORS headers to all responses
      handle_request(req)
      |> wisp.set_header("access-control-allow-origin", origin)
      |> wisp.set_header("access-control-allow-credentials", "true")
      |> wisp.set_header("access-control-allow-methods", "GET, POST, OPTIONS")
      |> wisp.set_header(
        "access-control-allow-headers",
        "Content-Type, Authorization, DPoP",
      )
    }
  }
}
