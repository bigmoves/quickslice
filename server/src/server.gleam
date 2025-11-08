import argv
import backfill
import backfill_state
import config
import database
import dotenv_gleam
import envoy
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/http/request
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/string
import graphiql_handler
import graphql_handler
import graphql_ws_handler
import importer
import jetstream_consumer
import logging
import lustre/element
import lustre_handlers
import mist
import oauth/handlers
import oauth/registration
import oauth/session
import pages/index
import pubsub
import settings_handler
import sqlight
import stats_pubsub
import upload_handler
import wisp
import wisp/wisp_mist
import xrpc_handlers
import xrpc_router

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
      process.Subject(jetstream_consumer.Message),
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
  let assert Ok(db) = database.initialize(database_url)

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
  let assert Ok(db) = database.initialize(database_url)

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
  case database.get_record_type_lexicons(db) {
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
  let assert Ok(db) = database.initialize(database_url)

  // Note: Lexicon import has been moved to the settings page (ZIP upload)
  // Use the /settings page to upload a ZIP file containing lexicon JSON files

  // Initialize PubSub registry for subscriptions
  pubsub.start()
  logging.log(logging.Info, "[server] PubSub registry initialized")

  // Initialize Stats PubSub registry for real-time stats
  stats_pubsub.start()
  logging.log(logging.Info, "[server] Stats PubSub registry initialized")

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
  jetstream_subject: option.Option(process.Subject(jetstream_consumer.Message)),
) {
  wisp.configure_logger()

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

  let handler = fn(req) { handle_request(req, ctx) }

  logging.log(
    logging.Info,
    "[server] Server started on http://" <> host <> ":" <> int.to_string(port),
  )

  // Create Wisp handler converted to Mist format
  let wisp_handler = wisp_mist.handler(handler, secret_key_base)

  // Wrap it to intercept WebSocket upgrades and serve Lustre runtime
  let mist_handler = fn(req: request.Request(mist.Connection)) {
    let upgrade_header = request.get_header(req, "upgrade")
    let path = request.path_segments(req)

    case path {
      // Serve Lustre client runtime
      ["lustre", "runtime.mjs"] -> lustre_handlers.serve_lustre_runtime()

      // Serve styles CSS
      ["styles.css"] -> lustre_handlers.serve_tailwind_css()

      // Backfill button WebSocket
      ["backfill-ws"] -> {
        case upgrade_header {
          Ok(upgrade_value) -> {
            case string.lowercase(upgrade_value) {
              "websocket" -> {
                lustre_handlers.serve_backfill_button(
                  req,
                  ctx.db,
                  ctx.backfill_state,
                  ctx.config,
                )
              }
              _ -> wisp_handler(req)
            }
          }
          _ -> wisp_handler(req)
        }
      }

      // Stats cards WebSocket
      ["stats-ws"] -> {
        case upgrade_header {
          Ok(upgrade_value) -> {
            case string.lowercase(upgrade_value) {
              "websocket" -> {
                lustre_handlers.serve_stats_cards(req, ctx.db)
              }
              _ -> wisp_handler(req)
            }
          }
          _ -> wisp_handler(req)
        }
      }

      // GraphQL WebSocket
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
    ["settings"] ->
      settings_handler.handle(
        req,
        settings_handler.Context(
          db: ctx.db,
          oauth_config: ctx.oauth_config,
          admin_dids: ctx.admin_dids,
          config: ctx.config,
          jetstream_consumer: ctx.jetstream_consumer,
        ),
      )
    ["oauth", "authorize"] ->
      handlers.handle_oauth_authorize(req, ctx.db, ctx.oauth_config)
    ["oauth", "callback"] ->
      handlers.handle_oauth_callback(req, ctx.db, ctx.oauth_config)
    ["logout"] -> handlers.handle_logout(req, ctx.db)
    ["backfill"] -> handle_backfill_request(req, ctx)
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

fn handle_backfill_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  case req.method {
    gleam_http.Post -> {
      // Get domain authority from config
      let domain_authority = case config.get_domain_authority(ctx.config) {
        option.Some(authority) -> authority
        option.None -> ""
      }

      // Get all record-type lexicons
      case database.get_record_type_lexicons(ctx.db) {
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
                  backfill.nsid_matches_domain_authority(
                    lex.id,
                    domain_authority,
                  )
                })

              let collection_ids = list.map(collections, fn(lex) { lex.id })
              let external_collection_ids =
                list.map(external_collections, fn(lex) { lex.id })

              // Run backfill in background process
              let backfill_config = backfill.default_config()
              process.spawn_unlinked(fn() {
                backfill.backfill_collections(
                  [],
                  collection_ids,
                  external_collection_ids,
                  backfill_config,
                  ctx.db,
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

  let #(current_user, user_is_admin) = case
    session.get_current_user(req, ctx.db, refresh_fn)
  {
    Ok(#(did, handle, _access_token)) -> {
      let admin = is_admin(did, ctx.admin_dids)
      #(option.Some(#(did, handle)), admin)
    }
    Error(_) -> #(option.None, False)
  }

  // Get domain authority from config cache
  let domain_authority = config.get_domain_authority(ctx.config)

  // Get backfill state
  let backfilling =
    actor.call(
      ctx.backfill_state,
      waiting: 100,
      sending: backfill_state.IsBackfilling,
    )

  index.view(
    ctx.db,
    current_user,
    user_is_admin,
    domain_authority,
    backfilling,
  )
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
