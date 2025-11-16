import config
import database
import gleam/erlang/process
import gleam/http as gleam_http
import gleam/int
import gleam/list
import gleam/option
import gleam/string
import importer
import jetstream_consumer
import logging
import oauth/handlers
import oauth/session
import simplifile
import sqlight
import wisp
import wisp_flash
import zip_helper

pub type Context {
  Context(
    db: sqlight.Connection,
    oauth_config: handlers.OAuthConfig,
    admin_dids: List(String),
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
  )
}

pub fn handle(req: wisp.Request, ctx: Context) -> wisp.Response {
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

  // Require admin access for the entire settings page
  case user_is_admin {
    False -> {
      logging.log(
        logging.Warning,
        "[settings] Non-admin user attempted to access settings page",
      )
      wisp.redirect("/")
    }
    True -> handle_admin_request(req, ctx, current_user)
  }
}

fn handle_admin_request(
  req: wisp.Request,
  ctx: Context,
  _current_user: option.Option(#(String, String)),
) -> wisp.Response {
  case req.method {
    gleam_http.Get -> {
      // TODO: Migrate settings page to client SPA
      wisp.html_response(
        "<h1>Settings</h1><p>Settings page will be migrated to the client SPA</p>",
        200,
      )
    }
    gleam_http.Post -> {
      // Handle form submission (domain authority or lexicons upload or reset)
      use form_data <- wisp.require_form(req)

      // Check if this is a reset action
      case list.key_find(form_data.values, "action") {
        Ok("reset") -> {
          // Handle reset action (admin-only, already verified)
          handle_reset(req, form_data, ctx)
        }
        _ -> {
          // Check if this is a lexicons upload
          case list.key_find(form_data.files, "lexicons_zip") {
            Ok(uploaded_file) -> {
              // Handle lexicons ZIP upload
              handle_lexicons_upload(req, uploaded_file, ctx)
            }
            Error(_) -> {
              // Not a file upload, check for domain_authority field
              case list.key_find(form_data.values, "domain_authority") {
                Ok(domain_authority) -> {
                  // Validate domain authority format
                  case validate_domain_authority(domain_authority) {
                    Ok(_) -> {
                      // Save domain_authority to database and update cache
                      case
                        config.set_domain_authority(
                          ctx.config,
                          ctx.db,
                          domain_authority,
                        )
                      {
                        Ok(_) -> {
                          wisp.redirect("/settings")
                          |> wisp_flash.set_flash(
                            req,
                            "success",
                            "Domain authority saved successfully",
                          )
                        }
                        Error(_) -> {
                          logging.log(
                            logging.Error,
                            "[settings] Failed to save domain_authority",
                          )
                          wisp.redirect("/settings")
                          |> wisp_flash.set_flash(
                            req,
                            "error",
                            "Failed to save domain authority",
                          )
                        }
                      }
                    }
                    Error(error_message) -> {
                      logging.log(
                        logging.Warning,
                        "[settings] Invalid domain authority: " <> error_message,
                      )
                      wisp.redirect("/settings")
                      |> wisp_flash.set_flash(req, "error", error_message)
                    }
                  }
                }
                Error(_) -> {
                  logging.log(
                    logging.Warning,
                    "[settings] No form data received",
                  )
                  wisp.redirect("/settings")
                }
              }
            }
          }
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "text/html")
      |> wisp.set_body(wisp.Text("<h1>Method Not Allowed</h1>"))
    }
  }
}

fn is_admin(did: String, admin_dids: List(String)) -> Bool {
  list.contains(admin_dids, did)
}

fn handle_lexicons_upload(
  req: wisp.Request,
  uploaded_file: wisp.UploadedFile,
  ctx: Context,
) -> wisp.Response {
  logging.log(
    logging.Info,
    "[settings] Processing lexicons ZIP upload: " <> uploaded_file.file_name,
  )

  // Create temporary directory for extraction with random suffix
  let temp_dir = "tmp/lexicon_upload_" <> wisp.random_string(16)

  case simplifile.create_directory_all(temp_dir) {
    Ok(_) -> {
      logging.log(
        logging.Info,
        "[settings] Created temp directory: " <> temp_dir,
      )

      // Extract ZIP file to temp directory
      case zip_helper.extract_zip(uploaded_file.path, temp_dir) {
        Ok(_) -> {
          logging.log(
            logging.Info,
            "[settings] Extracted ZIP file to: " <> temp_dir,
          )

          // Import lexicons from extracted directory
          case importer.import_lexicons_from_directory(temp_dir, ctx.db) {
            Ok(stats) -> {
              // Clean up temp directory
              let _ = simplifile.delete(temp_dir)

              logging.log(
                logging.Info,
                "[settings] Lexicon import complete: "
                  <> int.to_string(stats.imported)
                  <> " imported, "
                  <> int.to_string(stats.failed)
                  <> " failed",
              )

              // Log any errors
              case stats.errors {
                [] -> Nil
                errors -> {
                  list.each(errors, fn(err) {
                    logging.log(
                      logging.Warning,
                      "[settings] Import error: " <> err,
                    )
                  })
                }
              }

              // Restart Jetstream consumer to pick up newly imported collections
              let restart_status = case ctx.jetstream_consumer {
                option.Some(consumer) -> {
                  logging.log(
                    logging.Info,
                    "[settings] Restarting Jetstream consumer with new lexicons...",
                  )
                  case jetstream_consumer.restart(consumer) {
                    Ok(_) -> {
                      logging.log(
                        logging.Info,
                        "[settings] Jetstream consumer restarted successfully",
                      )
                      "success"
                    }
                    Error(err) -> {
                      logging.log(
                        logging.Error,
                        "[settings] Failed to restart Jetstream consumer: "
                          <> err,
                      )
                      "failed"
                    }
                  }
                }
                option.None -> {
                  logging.log(
                    logging.Info,
                    "[settings] Jetstream consumer not running, skipping restart",
                  )
                  "not_running"
                }
              }

              // Build success message with import stats and restart status
              let base_message =
                "Imported "
                <> int.to_string(stats.imported)
                <> " lexicon(s) successfully"
              let message = case restart_status {
                "success" -> base_message <> ". Jetstream consumer restarted."
                "failed" ->
                  base_message
                  <> ". Warning: Jetstream consumer restart failed."
                "not_running" -> base_message <> "."
                _ -> base_message
              }

              let flash_kind = case restart_status {
                "failed" -> "warning"
                _ -> "success"
              }

              wisp.redirect("/settings")
              |> wisp_flash.set_flash(req, flash_kind, message)
            }
            Error(err) -> {
              // Clean up temp directory
              let _ = simplifile.delete(temp_dir)

              logging.log(
                logging.Error,
                "[settings] Failed to import lexicons: " <> err,
              )
              wisp.redirect("/settings")
              |> wisp_flash.set_flash(
                req,
                "error",
                "Failed to import lexicons: " <> err,
              )
            }
          }
        }
        Error(err) -> {
          // Clean up temp directory
          let _ = simplifile.delete(temp_dir)

          logging.log(
            logging.Error,
            "[settings] Failed to extract ZIP: " <> err,
          )
          wisp.redirect("/settings")
          |> wisp_flash.set_flash(
            req,
            "error",
            "Failed to extract ZIP file: " <> err,
          )
        }
      }
    }
    Error(_) -> {
      logging.log(logging.Error, "[settings] Failed to create temp directory")
      wisp.redirect("/settings")
      |> wisp_flash.set_flash(
        req,
        "error",
        "Failed to create temporary directory for upload",
      )
    }
  }
}

/// Validates domain authority format (e.g., "com.example")
fn validate_domain_authority(domain_authority: String) -> Result(Nil, String) {
  // Check for http:// or https://
  case
    string.contains(domain_authority, "http://")
    || string.contains(domain_authority, "https://")
  {
    True ->
      Error(
        "Domain authority should not contain http:// or https:// (e.g., com.example)",
      )
    False -> {
      // Check that it has exactly two parts separated by a dot
      let parts = string.split(domain_authority, ".")
      case list.length(parts) == 2 {
        False ->
          Error(
            "Domain authority must have exactly two parts separated by a dot (e.g., com.example)",
          )
        True -> {
          // Check that no parts are empty
          case list.all(parts, fn(part) { string.length(part) > 0 }) {
            False ->
              Error(
                "Domain authority parts cannot be empty (e.g., com.example)",
              )
            True -> Ok(Nil)
          }
        }
      }
    }
  }
}

fn handle_reset(
  req: wisp.Request,
  form_data: wisp.FormData,
  ctx: Context,
) -> wisp.Response {
  // Admin access already verified by page-level check
  // Verify confirmation
  case list.key_find(form_data.values, "confirm") {
    Ok("RESET") -> {
      // Delete all data
      let domain_result = database.delete_domain_authority(ctx.db)
      let lexicons_result = database.delete_all_lexicons(ctx.db)
      let records_result = database.delete_all_records(ctx.db)
      let actors_result = database.delete_all_actors(ctx.db)
      let oauth_result = database.delete_oauth_credentials(ctx.db)

      case
        domain_result,
        lexicons_result,
        records_result,
        actors_result,
        oauth_result
      {
        Ok(_), Ok(_), Ok(_), Ok(_), Ok(_) -> {
          logging.log(
            logging.Info,
            "[settings] OAuth credentials deleted, re-registration will occur on next interaction",
          )
          // Reload config cache
          let _ = config.reload(ctx.config, ctx.db)

          // Restart Jetstream consumer if it exists
          let restart_message = case ctx.jetstream_consumer {
            option.Some(consumer) -> {
              case jetstream_consumer.restart(consumer) {
                Ok(_) -> "All data has been reset successfully"
                Error(_) ->
                  "Data reset completed (Jetstream consumer may need manual restart)"
              }
            }
            option.None -> "All data has been reset successfully"
          }

          logging.log(logging.Info, "[settings] System reset completed")
          wisp.redirect("/settings")
          |> wisp_flash.set_flash(req, "success", restart_message)
        }
        _, _, _, _, _ -> {
          logging.log(logging.Error, "[settings] Failed to reset some data")
          wisp.redirect("/settings")
          |> wisp_flash.set_flash(req, "error", "Failed to reset all data")
        }
      }
    }
    _ -> {
      logging.log(
        logging.Warning,
        "[settings] Reset attempted without proper confirmation",
      )
      wisp.redirect("/settings")
      |> wisp_flash.set_flash(
        req,
        "error",
        "Confirmation failed: Please type RESET exactly",
      )
    }
  }
}
