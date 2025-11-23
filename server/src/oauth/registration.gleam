import database/repositories/config as config_repo
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http.{Post}
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/option
import gleam/otp/actor
import gleam/result
import gleam/string
import logging
import oauth/handlers
import sqlight

pub type RegistrationStatus {
  NotRegistered
  Registered(client_id: String)
  RegistrationFailed(error: String)
}

type ClientRegistrationRequest {
  ClientRegistrationRequest(
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(String),
    response_types: List(String),
    token_endpoint_auth_method: String,
    scope: String,
  )
}

type ClientRegistrationResponse {
  ClientRegistrationResponse(client_id: String, client_secret: String)
}

/// Check registration status by looking at database
pub fn check_registration_status(db: sqlight.Connection) -> RegistrationStatus {
  case config_repo.get_oauth_credentials(db) {
    Ok(option.Some(#(client_id, _client_secret, _redirect_uri))) ->
      Registered(client_id)
    Ok(option.None) -> NotRegistered
    Error(_) -> NotRegistered
  }
}

/// Ensure OAuth client credentials exist, registering if needed
/// Returns OAuthConfig or Error
pub fn ensure_oauth_client(
  db: sqlight.Connection,
  aip_base_url: String,
  redirect_uri: String,
  client_name: String,
) -> Result(handlers.OAuthConfig, String) {
  // Check if credentials exist in database
  case config_repo.get_oauth_credentials(db) {
    Ok(option.Some(#(client_id, client_secret, _stored_uri))) -> {
      logging.log(logging.Info, "[oauth] Using stored OAuth credentials")
      Ok(handlers.OAuthConfig(
        client_id: client_id,
        client_secret: client_secret,
        redirect_uri: redirect_uri,
        auth_url: aip_base_url,
      ))
    }
    Ok(option.None) | Error(_) -> {
      // No credentials found, register new client
      logging.log(
        logging.Info,
        "[oauth] No OAuth credentials found, registering new client...",
      )

      case register_new_client(aip_base_url, redirect_uri, client_name) {
        Ok(#(client_id, client_secret)) -> {
          logging.log(
            logging.Info,
            "[oauth] OAuth client registered: " <> client_id,
          )

          // Store credentials in database
          case
            store_oauth_credentials(db, client_id, client_secret, redirect_uri)
          {
            Ok(_) -> {
              logging.log(
                logging.Info,
                "[oauth] OAuth credentials stored in database",
              )
              Ok(handlers.OAuthConfig(
                client_id: client_id,
                client_secret: client_secret,
                redirect_uri: redirect_uri,
                auth_url: aip_base_url,
              ))
            }
            Error(err) -> {
              logging.log(
                logging.Error,
                "[oauth] Failed to store credentials: " <> string.inspect(err),
              )
              // Return credentials anyway, but warn
              Ok(handlers.OAuthConfig(
                client_id: client_id,
                client_secret: client_secret,
                redirect_uri: redirect_uri,
                auth_url: aip_base_url,
              ))
            }
          }
        }
        Error(err) -> {
          logging.log(
            logging.Error,
            "[oauth] Client registration failed: " <> err,
          )
          Error("Client registration failed: " <> err)
        }
      }
    }
  }
}

/// Register a new OAuth client with the AIP server
/// Returns (client_id, client_secret) or Error
pub fn register_new_client(
  aip_base_url: String,
  redirect_uri: String,
  client_name: String,
) -> Result(#(String, String), String) {
  let registration_url = aip_base_url <> "/oauth/clients/register"

  // Build registration request body (RFC 7591)
  let registration_request =
    ClientRegistrationRequest(
      client_name: client_name,
      redirect_uris: [redirect_uri],
      grant_types: ["authorization_code", "refresh_token"],
      response_types: ["code"],
      token_endpoint_auth_method: "client_secret_basic",
      scope: "profile openid atproto transition:generic",
    )

  let body = encode_registration_request(registration_request)
  let body_string = json.to_string(body)

  // Make HTTP request
  case request.to(registration_url) {
    Ok(req) -> {
      let req =
        req
        |> request.set_method(Post)
        |> request.set_header("content-type", "application/json")
        |> request.set_body(body_string)

      case httpc.send(req) {
        Ok(resp) -> {
          case resp.status {
            200 | 201 -> {
              // Parse response (both 200 OK and 201 Created are valid)
              case json.parse(resp.body, decode.dynamic) {
                Ok(parsed) -> {
                  case decode_registration_response(parsed) {
                    Ok(response) ->
                      Ok(#(response.client_id, response.client_secret))
                    Error(err) -> Error(err)
                  }
                }
                Error(_) -> Error("Failed to parse registration response JSON")
              }
            }
            _ ->
              Error(
                "Registration failed with status "
                <> int.to_string(resp.status)
                <> ": "
                <> resp.body,
              )
          }
        }
        Error(_) -> Error("Failed to send registration request to AIP")
      }
    }
    Error(_) -> Error("Invalid registration URL: " <> registration_url)
  }
}

/// Store OAuth credentials in the database config table
pub fn store_oauth_credentials(
  db: sqlight.Connection,
  client_id: String,
  client_secret: String,
  redirect_uri: String,
) -> Result(Nil, sqlight.Error) {
  use _ <- result.try(config_repo.set(db, "oauth_client_id", client_id))
  use _ <- result.try(config_repo.set(db, "oauth_client_secret", client_secret))
  use _ <- result.try(config_repo.set(db, "oauth_redirect_uri", redirect_uri))
  Ok(Nil)
}

// Helper Functions ---------------------------------------------------------------

fn encode_registration_request(req: ClientRegistrationRequest) -> json.Json {
  json.object([
    #("client_name", json.string(req.client_name)),
    #(
      "redirect_uris",
      json.array(req.redirect_uris, fn(uri) { json.string(uri) }),
    ),
    #(
      "grant_types",
      json.array(req.grant_types, fn(grant) { json.string(grant) }),
    ),
    #(
      "response_types",
      json.array(req.response_types, fn(response) { json.string(response) }),
    ),
    #("token_endpoint_auth_method", json.string(req.token_endpoint_auth_method)),
    #("scope", json.string(req.scope)),
  ])
}

fn decode_registration_response(
  parsed: decode.Dynamic,
) -> Result(ClientRegistrationResponse, String) {
  let client_id =
    decode.run(parsed, decode.at(["client_id"], decode.string))
    |> result.map_error(fn(_) { "Missing client_id in response" })

  let client_secret =
    decode.run(parsed, decode.at(["client_secret"], decode.string))
    |> result.map_error(fn(_) { "Missing client_secret in response" })

  case client_id, client_secret {
    Ok(id), Ok(secret) ->
      Ok(ClientRegistrationResponse(client_id: id, client_secret: secret))
    Error(err), _ -> Error(err)
    _, Error(err) -> Error(err)
  }
}

// Retry Logic -------------------------------------------------------------------

pub type RetryState {
  RetryState(
    db: sqlight.Connection,
    aip_base_url: String,
    redirect_uri: String,
    client_name: String,
    attempt: Int,
    max_backoff_minutes: Int,
  )
}

pub type RetryMessage {
  AttemptRegistration(actor_subject: process.Subject(RetryMessage))
  Stop
}

/// Start a retry actor that attempts registration with exponential backoff
/// Returns a Subject that can be used to send messages to the actor
pub fn start_retry_actor(
  db: sqlight.Connection,
  aip_base_url: String,
  redirect_uri: String,
  client_name: String,
) -> Result(process.Subject(RetryMessage), actor.StartError) {
  let initial_state =
    RetryState(
      db: db,
      aip_base_url: aip_base_url,
      redirect_uri: redirect_uri,
      client_name: client_name,
      attempt: 0,
      max_backoff_minutes: 30,
    )

  let result =
    actor.new(initial_state)
    |> actor.on_message(handle_retry_message)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

fn handle_retry_message(
  state: RetryState,
  message: RetryMessage,
) -> actor.Next(RetryState, RetryMessage) {
  case message {
    Stop -> {
      logging.log(logging.Info, "[oauth] Retry actor stopped")
      actor.stop()
    }
    AttemptRegistration(actor_subject) -> {
      // Check if already registered
      case check_registration_status(state.db) {
        Registered(client_id) -> {
          logging.log(
            logging.Info,
            "[oauth] Registration successful! Client ID: " <> client_id,
          )
          actor.stop()
        }
        NotRegistered | RegistrationFailed(_) -> {
          // Attempt registration
          logging.log(
            logging.Info,
            "[oauth] Retry attempt "
              <> int.to_string(state.attempt + 1)
              <> " - attempting registration...",
          )

          case
            register_new_client(
              state.aip_base_url,
              state.redirect_uri,
              state.client_name,
            )
          {
            Ok(#(client_id, client_secret)) -> {
              logging.log(
                logging.Info,
                "[oauth] Registration successful: " <> client_id,
              )

              // Store credentials
              case
                store_oauth_credentials(
                  state.db,
                  client_id,
                  client_secret,
                  state.redirect_uri,
                )
              {
                Ok(_) -> {
                  logging.log(
                    logging.Info,
                    "[oauth] Credentials stored successfully",
                  )
                  actor.stop()
                }
                Error(err) -> {
                  logging.log(
                    logging.Error,
                    "[oauth] Failed to store credentials: "
                      <> string.inspect(err),
                  )
                  // Schedule next retry
                  schedule_next_retry(state, actor_subject)
                }
              }
            }
            Error(err) -> {
              logging.log(
                logging.Error,
                "[oauth] Registration attempt failed: " <> err,
              )
              // Schedule next retry
              schedule_next_retry(state, actor_subject)
            }
          }
        }
      }
    }
  }
}

fn schedule_next_retry(
  state: RetryState,
  actor_subject: process.Subject(RetryMessage),
) -> actor.Next(RetryState, RetryMessage) {
  let new_attempt = state.attempt + 1

  // Calculate backoff: 2^attempt minutes, capped at max_backoff_minutes
  let backoff_minutes =
    int.min(int.bitwise_shift_left(1, new_attempt), state.max_backoff_minutes)
  let backoff_ms = backoff_minutes * 60 * 1000

  logging.log(
    logging.Info,
    "[oauth] Scheduling next retry in "
      <> int.to_string(backoff_minutes)
      <> " minutes",
  )

  // Spawn a timer process that will send a message after the delay
  let _ =
    process.spawn_unlinked(fn() {
      process.sleep(backoff_ms)
      process.send(actor_subject, AttemptRegistration(actor_subject))
    })

  // Continue with updated attempt count
  actor.continue(RetryState(..state, attempt: new_attempt))
}

/// Trigger an immediate retry attempt
pub fn trigger_retry(subject: process.Subject(RetryMessage)) -> Nil {
  process.send(subject, AttemptRegistration(subject))
}

/// Stop the retry actor
pub fn stop_retry(subject: process.Subject(RetryMessage)) -> Nil {
  process.send(subject, Stop)
}
