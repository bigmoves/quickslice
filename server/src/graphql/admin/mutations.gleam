/// Mutation resolvers for admin GraphQL API
import admin_session as session
import backfill
import backfill_state
import database/repositories/actors
import database/repositories/config as config_repo
import database/repositories/jetstream_activity
import database/repositories/lexicons
import database/repositories/oauth_clients
import database/repositories/records
import database/types
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import graphql/admin/converters
import graphql/admin/types as admin_types
import importer
import jetstream_consumer
import lib/oauth/did_cache
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/token_generator
import lib/oauth/validator
import logging
import sqlight
import swell/schema
import swell/value
import wisp

/// Validate that a string is a valid DID format
/// Valid format: did:<method>:<identifier>
fn is_valid_did(s: String) -> Bool {
  case string.starts_with(s, "did:") {
    False -> False
    True -> {
      let parts = string.split(s, ":")
      case parts {
        ["did", method, identifier, ..] -> method != "" && identifier != ""
        _ -> False
      }
    }
  }
}

/// Validate that all requested scopes are in the supported list
fn validate_scope_against_supported(
  requested_scope: String,
  supported_scopes: List(String),
) -> Result(Nil, String) {
  let requested =
    requested_scope
    |> string.split(" ")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  let invalid =
    list.filter(requested, fn(s) { !list.contains(supported_scopes, s) })

  case invalid {
    [] -> Ok(Nil)
    _ ->
      Error(
        "Unsupported scope(s): "
        <> string.join(invalid, ", ")
        <> ". Supported: "
        <> string.join(supported_scopes, ", "),
      )
  }
}

/// Build the Mutation root type with all mutation resolvers
pub fn mutation_type(
  conn: sqlight.Connection,
  req: wisp.Request,
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Type {
  schema.object_type("Mutation", "Root mutation type", [
    // updateSettings mutation - consolidated settings update
    schema.field_with_args(
      "updateSettings",
      schema.non_null(admin_types.settings_type()),
      "Update system settings (domain authority and/or admin DIDs)",
      [
        schema.argument(
          "domainAuthority",
          schema.string_type(),
          "New domain authority value (optional)",
          None,
        ),
        schema.argument(
          "adminDids",
          schema.list_type(schema.non_null(schema.string_type())),
          "New admin DIDs list (optional)",
          None,
        ),
        schema.argument(
          "relayUrl",
          schema.string_type(),
          "New relay URL (optional)",
          None,
        ),
        schema.argument(
          "plcDirectoryUrl",
          schema.string_type(),
          "New PLC directory URL (optional)",
          None,
        ),
        schema.argument(
          "jetstreamUrl",
          schema.string_type(),
          "New Jetstream URL (optional)",
          None,
        ),
        schema.argument(
          "oauthSupportedScopes",
          schema.string_type(),
          "New OAuth supported scopes space-separated (optional)",
          None,
        ),
      ],
      fn(ctx) {
        // Check admin privileges
        case session.get_current_session(req, conn, did_cache) {
          Error(_) -> Error("Authentication required")
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              False -> Error("Admin privileges required")
              True -> {
                // Update domain authority if provided
                let domain_authority_result = case
                  schema.get_argument(ctx, "domainAuthority")
                {
                  Some(value.String(authority)) -> {
                    // Validate not empty
                    case string.trim(authority) {
                      "" -> Error("Domain authority cannot be empty")
                      trimmed_authority -> {
                        case
                          config_repo.set(
                            conn,
                            "domain_authority",
                            trimmed_authority,
                          )
                        {
                          Ok(_) -> {
                            // Restart Jetstream consumer
                            case jetstream_subject {
                              Some(consumer) -> {
                                logging.log(
                                  logging.Info,
                                  "[updateSettings] Restarting Jetstream consumer...",
                                )
                                let _ = jetstream_consumer.restart(consumer)
                                Nil
                              }
                              None -> Nil
                            }
                            Ok(Nil)
                          }
                          Error(_) -> Error("Failed to update domain authority")
                        }
                      }
                    }
                  }
                  _ -> Ok(Nil)
                }

                case domain_authority_result {
                  Error(err) -> Error(err)
                  Ok(_) -> {
                    // Update admin DIDs if provided
                    let admin_dids_result = case
                      schema.get_argument(ctx, "adminDids")
                    {
                      Some(value.List(dids)) -> {
                        let did_strings =
                          list.filter_map(dids, fn(d) {
                            case d {
                              value.String(s) -> Ok(s)
                              _ -> Error(Nil)
                            }
                          })
                        // Validate at least one admin
                        case did_strings {
                          [] -> Error("Cannot have zero admins")
                          _ -> {
                            // Validate all DIDs have correct format
                            let invalid_dids =
                              list.filter(did_strings, fn(d) {
                                !is_valid_did(d)
                              })
                            case invalid_dids {
                              [first_invalid, ..] ->
                                Error(
                                  "Invalid DID format: "
                                  <> first_invalid
                                  <> ". DIDs must start with 'did:' followed by method and identifier (e.g., did:plc:abc123)",
                                )
                              [] -> {
                                case
                                  config_repo.set_admin_dids(conn, did_strings)
                                {
                                  Ok(_) -> Ok(Nil)
                                  Error(_) ->
                                    Error("Failed to update admin DIDs")
                                }
                              }
                            }
                          }
                        }
                      }
                      _ -> Ok(Nil)
                    }

                    case admin_dids_result {
                      Error(err) -> Error(err)
                      Ok(_) -> {
                        // Update relay URL if provided
                        let relay_url_result = case
                          schema.get_argument(ctx, "relayUrl")
                        {
                          Some(value.String(url)) -> {
                            case string.trim(url) {
                              "" -> Error("Relay URL cannot be empty")
                              trimmed_url -> {
                                case
                                  config_repo.set_relay_url(conn, trimmed_url)
                                {
                                  Ok(_) -> Ok(Nil)
                                  Error(_) ->
                                    Error("Failed to update relay URL")
                                }
                              }
                            }
                          }
                          _ -> Ok(Nil)
                        }

                        case relay_url_result {
                          Error(err) -> Error(err)
                          Ok(_) -> {
                            // Update PLC directory URL if provided
                            let plc_url_result = case
                              schema.get_argument(ctx, "plcDirectoryUrl")
                            {
                              Some(value.String(url)) -> {
                                case string.trim(url) {
                                  "" ->
                                    Error("PLC directory URL cannot be empty")
                                  trimmed_url -> {
                                    case
                                      config_repo.set_plc_directory_url(
                                        conn,
                                        trimmed_url,
                                      )
                                    {
                                      Ok(_) -> Ok(True)
                                      Error(_) ->
                                        Error(
                                          "Failed to update PLC directory URL",
                                        )
                                    }
                                  }
                                }
                              }
                              _ -> Ok(False)
                            }

                            case plc_url_result {
                              Error(err) -> Error(err)
                              Ok(plc_changed) -> {
                                // Restart Jetstream if PLC URL changed
                                case plc_changed {
                                  True -> {
                                    case jetstream_subject {
                                      Some(consumer) -> {
                                        logging.log(
                                          logging.Info,
                                          "[updateSettings] Restarting Jetstream consumer due to PLC URL change",
                                        )
                                        case
                                          jetstream_consumer.restart(consumer)
                                        {
                                          Ok(_) ->
                                            logging.log(
                                              logging.Info,
                                              "[updateSettings] Jetstream consumer restarted",
                                            )
                                          Error(err) ->
                                            logging.log(
                                              logging.Error,
                                              "[updateSettings] Failed to restart Jetstream: "
                                                <> err,
                                            )
                                        }
                                      }
                                      None -> Nil
                                    }
                                  }
                                  False -> Nil
                                }
                                // Update Jetstream URL if provided and restart consumer
                                let jetstream_url_result = case
                                  schema.get_argument(ctx, "jetstreamUrl")
                                {
                                  Some(value.String(url)) -> {
                                    case string.trim(url) {
                                      "" ->
                                        Error("Jetstream URL cannot be empty")
                                      trimmed_url -> {
                                        case
                                          config_repo.set_jetstream_url(
                                            conn,
                                            trimmed_url,
                                          )
                                        {
                                          Ok(_) -> Ok(True)
                                          Error(_) ->
                                            Error(
                                              "Failed to update Jetstream URL",
                                            )
                                        }
                                      }
                                    }
                                  }
                                  _ -> Ok(False)
                                }

                                case jetstream_url_result {
                                  Error(err) -> Error(err)
                                  Ok(jetstream_url_changed) -> {
                                    // If Jetstream URL changed, restart consumer
                                    case jetstream_url_changed {
                                      True -> {
                                        case jetstream_subject {
                                          Some(consumer) -> {
                                            logging.log(
                                              logging.Info,
                                              "[updateSettings] Restarting Jetstream consumer due to URL change",
                                            )
                                            case
                                              jetstream_consumer.restart(
                                                consumer,
                                              )
                                            {
                                              Ok(_) ->
                                                logging.log(
                                                  logging.Info,
                                                  "[updateSettings] Jetstream consumer restarted",
                                                )
                                              Error(err) ->
                                                logging.log(
                                                  logging.Error,
                                                  "[updateSettings] Failed to restart Jetstream: "
                                                    <> err,
                                                )
                                            }
                                          }
                                          None -> Nil
                                        }
                                      }
                                      False -> Nil
                                    }

                                    // Update OAuth supported scopes if provided (with validation)
                                    let oauth_scopes_result = case
                                      schema.get_argument(
                                        ctx,
                                        "oauthSupportedScopes",
                                      )
                                    {
                                      Some(value.String(scopes)) -> {
                                        case string.trim(scopes) {
                                          "" ->
                                            Error(
                                              "OAuth supported scopes cannot be empty",
                                            )
                                          trimmed_scopes -> {
                                            // Validate scope format (accepts any valid ATProto scope)
                                            case
                                              scope_validator.validate_scope_format(
                                                trimmed_scopes,
                                              )
                                            {
                                              Ok(_) -> {
                                                // Validation passed, save to database
                                                case
                                                  config_repo.set_oauth_supported_scopes(
                                                    conn,
                                                    trimmed_scopes,
                                                  )
                                                {
                                                  Ok(_) -> Ok(Nil)
                                                  Error(_) ->
                                                    Error(
                                                      "Failed to save OAuth scopes",
                                                    )
                                                }
                                              }
                                              Error(err) -> {
                                                logging.log(
                                                  logging.Error,
                                                  "[updateSettings] Invalid OAuth scope: "
                                                    <> string.inspect(err),
                                                )
                                                Error("Invalid OAuth scope")
                                              }
                                            }
                                          }
                                        }
                                      }
                                      _ -> Ok(Nil)
                                    }

                                    case oauth_scopes_result {
                                      Error(err) -> Error(err)
                                      Ok(_) -> {
                                        // Return updated settings
                                        let final_authority = case
                                          config_repo.get(
                                            conn,
                                            "domain_authority",
                                          )
                                        {
                                          Ok(a) -> a
                                          Error(_) -> ""
                                        }
                                        let final_admin_dids =
                                          config_repo.get_admin_dids(conn)
                                        let final_relay_url =
                                          config_repo.get_relay_url(conn)
                                        let final_plc_directory_url =
                                          config_repo.get_plc_directory_url(
                                            conn,
                                          )
                                        let final_jetstream_url =
                                          config_repo.get_jetstream_url(conn)
                                        let final_oauth_scopes =
                                          config_repo.get_oauth_supported_scopes(
                                            conn,
                                          )

                                        Ok(converters.settings_to_value(
                                          final_authority,
                                          final_admin_dids,
                                          final_relay_url,
                                          final_plc_directory_url,
                                          final_jetstream_url,
                                          final_oauth_scopes,
                                        ))
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
              }
            }
          }
        }
      },
    ),
    // uploadLexicons mutation
    schema.field_with_args(
      "uploadLexicons",
      schema.non_null(schema.boolean_type()),
      "Upload and import lexicons from base64-encoded ZIP",
      [
        schema.argument(
          "zipBase64",
          schema.non_null(schema.string_type()),
          "Base64-encoded ZIP file containing lexicon JSON files",
          None,
        ),
      ],
      fn(ctx) {
        case schema.get_argument(ctx, "zipBase64") {
          Some(value.String(zip_base64)) -> {
            // Import lexicons from base64-encoded ZIP
            case importer.import_lexicons_from_base64_zip(zip_base64, conn) {
              Ok(_stats) -> {
                // Restart Jetstream consumer to pick up newly imported collections
                case jetstream_subject {
                  Some(consumer) -> {
                    logging.log(
                      logging.Info,
                      "[uploadLexicons] Restarting Jetstream consumer with new lexicons...",
                    )
                    case jetstream_consumer.restart(consumer) {
                      Ok(_) -> {
                        logging.log(
                          logging.Info,
                          "[uploadLexicons] Jetstream consumer restarted successfully",
                        )
                        Ok(value.Boolean(True))
                      }
                      Error(err) -> {
                        logging.log(
                          logging.Error,
                          "[uploadLexicons] Failed to restart Jetstream consumer: "
                            <> err,
                        )
                        Error(
                          "Lexicons imported but failed to restart Jetstream consumer: "
                          <> err,
                        )
                      }
                    }
                  }
                  None -> {
                    logging.log(
                      logging.Info,
                      "[uploadLexicons] Jetstream consumer not running, skipping restart",
                    )
                    Ok(value.Boolean(True))
                  }
                }
              }
              Error(err) -> Error("Failed to import lexicons: " <> err)
            }
          }
          _ -> Error("Invalid zipBase64 argument")
        }
      },
    ),
    // resetAll mutation
    schema.field_with_args(
      "resetAll",
      schema.non_null(schema.boolean_type()),
      "Reset all data (requires RESET confirmation and admin privileges)",
      [
        schema.argument(
          "confirm",
          schema.non_null(schema.string_type()),
          "Must be the string 'RESET' to confirm",
          None,
        ),
      ],
      fn(ctx) {
        // Check if user is authenticated and admin
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "confirm") {
                  Some(value.String("RESET")) -> {
                    // Call multiple database functions to reset all data
                    let _ = records.delete_all(conn)
                    let _ = actors.delete_all(conn)
                    let _ = lexicons.delete_all(conn)
                    let _ = config_repo.delete_domain_authority(conn)
                    let _ = jetstream_activity.delete_all(conn)

                    // Restart Jetstream consumer after reset
                    case jetstream_subject {
                      Some(consumer) -> {
                        logging.log(
                          logging.Info,
                          "[resetAll] Restarting Jetstream consumer after reset...",
                        )
                        let _ = jetstream_consumer.restart(consumer)
                        Nil
                      }
                      None -> Nil
                    }

                    Ok(value.Boolean(True))
                  }
                  Some(value.String(_)) -> Error("Confirmation must be 'RESET'")
                  _ -> Error("Invalid confirm argument")
                }
              }
              False -> Error("Admin privileges required to reset all data")
            }
          }
          Error(_) -> Error("Authentication required to reset all data")
        }
      },
    ),
    // triggerBackfill mutation
    schema.field(
      "triggerBackfill",
      schema.non_null(schema.boolean_type()),
      "Trigger a background backfill operation for all collections (admin only)",
      fn(_ctx) {
        // Check if user is authenticated and admin
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                // Mark backfill as started
                process.send(
                  backfill_state_subject,
                  backfill_state.StartBackfill,
                )

                // Spawn background process to run backfill
                process.spawn_unlinked(fn() {
                  logging.log(
                    logging.Info,
                    "[triggerBackfill] Starting background backfill...",
                  )

                  // Get all record-type collections from database (only backfill records, not queries/procedures)
                  let collections = case lexicons.get_record_types(conn) {
                    Ok(lexicon_list) ->
                      list.map(lexicon_list, fn(lex) { lex.id })
                    Error(_) -> []
                  }

                  // Get domain authority to determine external collections
                  let domain_authority = case
                    config_repo.get(conn, "domain_authority")
                  {
                    Ok(authority) -> authority
                    Error(_) -> ""
                  }

                  // Split collections into primary and external
                  let #(primary_collections, external_collections) =
                    list.partition(collections, fn(collection) {
                      backfill.nsid_matches_domain_authority(
                        collection,
                        domain_authority,
                      )
                    })

                  // Run backfill with default config and empty repo list (fetches from relay)
                  let config = backfill.default_config(conn)
                  backfill.backfill_collections(
                    [],
                    primary_collections,
                    external_collections,
                    config,
                    conn,
                  )

                  logging.log(
                    logging.Info,
                    "[triggerBackfill] Background backfill completed",
                  )

                  // Mark backfill as stopped
                  process.send(
                    backfill_state_subject,
                    backfill_state.StopBackfill,
                  )
                })

                // Return immediately
                Ok(value.Boolean(True))
              }
              False -> Error("Admin privileges required to trigger backfill")
            }
          }
          Error(_) -> Error("Authentication required to trigger backfill")
        }
      },
    ),
    // backfillActor mutation - sync a specific actor's collections
    schema.field_with_args(
      "backfillActor",
      schema.non_null(schema.boolean_type()),
      "Trigger a background backfill for a specific actor's collections",
      [
        schema.argument(
          "did",
          schema.non_null(schema.string_type()),
          "The DID of the actor to backfill",
          None,
        ),
      ],
      fn(ctx) {
        // Check if user is authenticated (any logged-in user can trigger)
        case session.get_current_session(req, conn, did_cache) {
          Ok(_sess) -> {
            case schema.get_argument(ctx, "did") {
              Some(value.String(did)) -> {
                // Get all record-type collections from database
                let collections = case lexicons.get_record_types(conn) {
                  Ok(lexicon_list) -> list.map(lexicon_list, fn(lex) { lex.id })
                  Error(_) -> []
                }

                // Get domain authority to determine external collections
                let domain_authority = case
                  config_repo.get(conn, "domain_authority")
                {
                  Ok(authority) -> authority
                  Error(_) -> ""
                }

                // Split collections into primary and external
                let #(primary_collections, external_collections) =
                  list.partition(collections, fn(collection) {
                    backfill.nsid_matches_domain_authority(
                      collection,
                      domain_authority,
                    )
                  })

                // Get PLC URL from database config
                let plc_url = config_repo.get_plc_directory_url(conn)

                // Spawn background process to run backfill for this actor
                process.spawn_unlinked(fn() {
                  logging.log(
                    logging.Info,
                    "[backfillActor] Starting background backfill for " <> did,
                  )

                  case
                    backfill.rescue(fn() {
                      backfill.backfill_collections_for_actor(
                        conn,
                        did,
                        primary_collections,
                        external_collections,
                        plc_url,
                      )
                    })
                  {
                    Ok(_) ->
                      logging.log(
                        logging.Info,
                        "[backfillActor] Background backfill completed for "
                          <> did,
                      )
                    Error(err) ->
                      logging.log(
                        logging.Error,
                        "[backfillActor] Background backfill FAILED for "
                          <> did
                          <> ": "
                          <> string.inspect(err),
                      )
                  }
                })

                // Return immediately
                Ok(value.Boolean(True))
              }
              _ -> Error("DID argument is required")
            }
          }
          Error(_) -> Error("Authentication required to trigger backfill")
        }
      },
    ),
    // createOAuthClient mutation
    schema.field_with_args(
      "createOAuthClient",
      schema.non_null(admin_types.oauth_client_type()),
      "Create a new OAuth client (admin only)",
      [
        schema.argument(
          "clientName",
          schema.non_null(schema.string_type()),
          "Client display name",
          None,
        ),
        schema.argument(
          "clientType",
          schema.non_null(schema.string_type()),
          "PUBLIC or CONFIDENTIAL",
          None,
        ),
        schema.argument(
          "redirectUris",
          schema.non_null(
            schema.list_type(schema.non_null(schema.string_type())),
          ),
          "Allowed redirect URIs",
          None,
        ),
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "clientType"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(name)),
                    Some(value.String(type_str)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
                    // Validate client name
                    let trimmed_name = string.trim(name)
                    case trimmed_name {
                      "" -> Error("Client name cannot be empty")
                      _ -> {
                        let client_type = case string.uppercase(type_str) {
                          "CONFIDENTIAL" -> types.Confidential
                          _ -> types.Public
                        }
                        let redirect_uris =
                          list.filter_map(uris, fn(u) {
                            case u {
                              value.String(s) ->
                                case string.trim(s) {
                                  "" -> Error(Nil)
                                  trimmed -> Ok(trimmed)
                                }
                              _ -> Error(Nil)
                            }
                          })
                        // Validate at least one redirect URI
                        case redirect_uris {
                          [] -> Error("At least one redirect URI is required")
                          _ -> {
                            // Validate each redirect URI format
                            let invalid_uri =
                              list.find(redirect_uris, fn(uri) {
                                case validator.validate_redirect_uri(uri) {
                                  Ok(_) -> False
                                  Error(_) -> True
                                }
                              })
                            case invalid_uri {
                              Ok(uri) ->
                                Error(
                                  "Invalid redirect URI: "
                                  <> uri
                                  <> ". URIs must use https://, or http:// only for localhost.",
                                )
                              Error(_) -> {
                                // Validate scope against supported scopes
                                case
                                  validate_scope_against_supported(
                                    scope,
                                    oauth_supported_scopes,
                                  )
                                {
                                  Error(err) -> Error(err)
                                  Ok(_) -> {
                                    let now =
                                      token_generator.current_timestamp()
                                    let client_id =
                                      token_generator.generate_client_id()
                                    let client_secret = case client_type {
                                      types.Confidential ->
                                        Some(
                                          token_generator.generate_client_secret(),
                                        )
                                      types.Public -> None
                                    }
                                    let client =
                                      types.OAuthClient(
                                        client_id: client_id,
                                        client_secret: client_secret,
                                        client_name: trimmed_name,
                                        redirect_uris: redirect_uris,
                                        grant_types: [
                                          types.AuthorizationCode,
                                          types.RefreshToken,
                                        ],
                                        response_types: [types.Code],
                                        scope: case string.trim(scope) {
                                          "" -> None
                                          s -> Some(s)
                                        },
                                        token_endpoint_auth_method: case
                                          client_type
                                        {
                                          types.Confidential ->
                                            types.ClientSecretPost
                                          types.Public -> types.AuthNone
                                        },
                                        client_type: client_type,
                                        created_at: now,
                                        updated_at: now,
                                        metadata: "{}",
                                        access_token_expiration: 3600,
                                        refresh_token_expiration: 86_400 * 30,
                                        require_redirect_exact: True,
                                        registration_access_token: None,
                                        jwks: None,
                                      )
                                    case oauth_clients.insert(conn, client) {
                                      Ok(_) ->
                                        Ok(converters.oauth_client_to_value(
                                          client,
                                        ))
                                      Error(_) ->
                                        Error("Failed to create OAuth client")
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
                  _, _, _, _ -> Error("Invalid arguments")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // updateOAuthClient mutation
    schema.field_with_args(
      "updateOAuthClient",
      schema.non_null(admin_types.oauth_client_type()),
      "Update an existing OAuth client (admin only)",
      [
        schema.argument(
          "clientId",
          schema.non_null(schema.string_type()),
          "Client ID to update",
          None,
        ),
        schema.argument(
          "clientName",
          schema.non_null(schema.string_type()),
          "New client display name",
          None,
        ),
        schema.argument(
          "redirectUris",
          schema.non_null(
            schema.list_type(schema.non_null(schema.string_type())),
          ),
          "New redirect URIs",
          None,
        ),
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case
                  schema.get_argument(ctx, "clientId"),
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(client_id)),
                    Some(value.String(name)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
                    // Validate client name
                    let trimmed_name = string.trim(name)
                    case trimmed_name {
                      "" -> Error("Client name cannot be empty")
                      _ -> {
                        case oauth_clients.get(conn, client_id) {
                          Ok(Some(existing)) -> {
                            let redirect_uris =
                              list.filter_map(uris, fn(u) {
                                case u {
                                  value.String(s) ->
                                    case string.trim(s) {
                                      "" -> Error(Nil)
                                      trimmed -> Ok(trimmed)
                                    }
                                  _ -> Error(Nil)
                                }
                              })
                            // Validate at least one redirect URI
                            case redirect_uris {
                              [] ->
                                Error("At least one redirect URI is required")
                              _ -> {
                                // Validate each redirect URI format
                                let invalid_uri =
                                  list.find(redirect_uris, fn(uri) {
                                    case validator.validate_redirect_uri(uri) {
                                      Ok(_) -> False
                                      Error(_) -> True
                                    }
                                  })
                                case invalid_uri {
                                  Ok(uri) ->
                                    Error(
                                      "Invalid redirect URI: "
                                      <> uri
                                      <> ". URIs must use https://, or http:// only for localhost.",
                                    )
                                  Error(_) -> {
                                    // Validate scope against supported scopes
                                    case
                                      validate_scope_against_supported(
                                        scope,
                                        oauth_supported_scopes,
                                      )
                                    {
                                      Error(err) -> Error(err)
                                      Ok(_) -> {
                                        let updated =
                                          types.OAuthClient(
                                            ..existing,
                                            client_name: trimmed_name,
                                            redirect_uris: redirect_uris,
                                            scope: case string.trim(scope) {
                                              "" -> None
                                              s -> Some(s)
                                            },
                                            updated_at: token_generator.current_timestamp(),
                                          )
                                        case
                                          oauth_clients.update(conn, updated)
                                        {
                                          Ok(_) ->
                                            Ok(converters.oauth_client_to_value(
                                              updated,
                                            ))
                                          Error(_) ->
                                            Error(
                                              "Failed to update OAuth client",
                                            )
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          Ok(None) -> Error("OAuth client not found")
                          Error(_) -> Error("Failed to fetch OAuth client")
                        }
                      }
                    }
                  }
                  _, _, _, _ -> Error("Invalid arguments")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
    // deleteOAuthClient mutation
    schema.field_with_args(
      "deleteOAuthClient",
      schema.non_null(schema.boolean_type()),
      "Delete an OAuth client (admin only)",
      [
        schema.argument(
          "clientId",
          schema.non_null(schema.string_type()),
          "Client ID to delete",
          None,
        ),
      ],
      fn(ctx) {
        case session.get_current_session(req, conn, did_cache) {
          Ok(sess) -> {
            case config_repo.is_admin(conn, sess.did) {
              True -> {
                case schema.get_argument(ctx, "clientId") {
                  Some(value.String(client_id)) -> {
                    case client_id {
                      "admin" -> Error("Cannot delete internal admin client")
                      _ -> {
                        case oauth_clients.delete(conn, client_id) {
                          Ok(_) -> Ok(value.Boolean(True))
                          Error(_) -> Error("Failed to delete OAuth client")
                        }
                      }
                    }
                  }
                  _ -> Error("Invalid clientId argument")
                }
              }
              False -> Error("Admin privileges required")
            }
          }
          Error(_) -> Error("Authentication required")
        }
      },
    ),
  ])
}
