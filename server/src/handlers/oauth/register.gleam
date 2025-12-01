/// Client registration endpoint
/// POST /oauth/register
import database/repositories/oauth_clients
import database/types.{
  type GrantType, AuthNone, AuthorizationCode, ClientSecretBasic,
  ClientSecretPost, Code, Confidential, OAuthClient, PrivateKeyJwt, Public,
  RefreshToken,
}
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/scopes/validator as scope_validator
import lib/oauth/types/error
import lib/oauth/validator
import sqlight
import wisp

/// Registration request parsed from JSON
pub type RegistrationRequest {
  RegistrationRequest(
    client_name: Option(String),
    redirect_uris: List(String),
    grant_types: Option(List(String)),
    response_types: Option(List(String)),
    token_endpoint_auth_method: Option(String),
    scope: Option(String),
  )
}

/// Registration response
pub type RegistrationResponse {
  RegistrationResponse(
    client_id: String,
    client_secret: Option(String),
    client_name: String,
    redirect_uris: List(String),
    grant_types: List(String),
    response_types: List(String),
    token_endpoint_auth_method: String,
    scope: String,
    client_id_issued_at: Int,
    client_type: String,
  )
}

/// Handle POST /oauth/register
pub fn handle(req: wisp.Request, conn: sqlight.Connection) -> wisp.Response {
  use body <- wisp.require_string_body(req)

  case parse_and_register(body, conn) {
    Ok(response) -> {
      let json_response = encode_response(response)

      wisp.response(201)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_header("cache-control", "no-store")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
    Error(#(status, error, description)) -> {
      let json_response =
        json.object([
          #("error", json.string(error)),
          #("error_description", json.string(description)),
        ])

      wisp.response(status)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(json.to_string(json_response)))
    }
  }
}

fn parse_and_register(
  body: String,
  conn: sqlight.Connection,
) -> Result(RegistrationResponse, #(Int, String, String)) {
  // Parse JSON request
  use req <- result.try(
    json.parse(body, decode_registration_request())
    |> result.map_error(fn(_) {
      #(400, "invalid_request", "Invalid JSON or missing required fields")
    }),
  )

  // Validate redirect_uris (required)
  use _ <- result.try(case req.redirect_uris {
    [] -> Error(#(400, "invalid_request", "redirect_uris cannot be empty"))
    uris -> validate_redirect_uris(uris)
  })

  // Validate scope format if provided
  use _ <- result.try(case req.scope {
    Some(scope_str) ->
      scope_validator.validate_scope_format(scope_str)
      |> result.map(fn(_) { Nil })
      |> result.map_error(fn(e) {
        #(400, "invalid_scope", error.error_description(e))
      })
    None -> Ok(Nil)
  })

  // Parse grant_types or use defaults
  let grant_types = case req.grant_types {
    Some(types_list) -> parse_grant_types(types_list)
    None -> [AuthorizationCode, RefreshToken]
  }

  // Parse response_types or use defaults
  let response_types = case req.response_types {
    Some(types_list) -> parse_response_types(types_list)
    None -> [Code]
  }

  // Parse token_endpoint_auth_method or use default
  let auth_method = case req.token_endpoint_auth_method {
    Some(method) -> parse_auth_method(method)
    None -> ClientSecretPost
  }

  // Determine client_type based on auth method
  let client_type = case auth_method {
    AuthNone -> Public
    _ -> Confidential
  }

  // Generate client_id and client_secret
  let client_id = generate_client_id()
  let client_secret = case client_type {
    Confidential -> Some(generate_client_secret())
    Public -> None
  }

  let now = current_timestamp()

  // Create client record
  let oauth_client =
    OAuthClient(
      client_id: client_id,
      client_secret: client_secret,
      client_name: req.client_name |> option.unwrap("OAuth Client"),
      redirect_uris: req.redirect_uris,
      grant_types: grant_types,
      response_types: response_types,
      scope: req.scope,
      token_endpoint_auth_method: auth_method,
      client_type: client_type,
      created_at: now,
      updated_at: now,
      metadata: "{}",
      access_token_expiration: 3600,
      refresh_token_expiration: 2_592_000,
      require_redirect_exact: True,
      registration_access_token: None,
      jwks: None,
    )

  // Store client
  use _ <- result.try(
    oauth_clients.insert(conn, oauth_client)
    |> result.map_error(fn(_) {
      #(500, "server_error", "Failed to store client")
    }),
  )

  // Build response
  Ok(RegistrationResponse(
    client_id: client_id,
    client_secret: client_secret,
    client_name: oauth_client.client_name,
    redirect_uris: oauth_client.redirect_uris,
    grant_types: list.map(oauth_client.grant_types, types.grant_type_to_string),
    response_types: list.map(
      oauth_client.response_types,
      types.response_type_to_string,
    ),
    token_endpoint_auth_method: types.client_auth_method_to_string(
      oauth_client.token_endpoint_auth_method,
    ),
    scope: oauth_client.scope |> option.unwrap("atproto"),
    client_id_issued_at: oauth_client.created_at,
    client_type: types.client_type_to_string(oauth_client.client_type),
  ))
}

fn decode_registration_request() -> decode.Decoder(RegistrationRequest) {
  use client_name <- decode.optional_field(
    "client_name",
    None,
    decode.optional(decode.string),
  )
  use redirect_uris <- decode.field("redirect_uris", decode.list(decode.string))
  use grant_types <- decode.optional_field(
    "grant_types",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use response_types <- decode.optional_field(
    "response_types",
    None,
    decode.optional(decode.list(decode.string)),
  )
  use token_endpoint_auth_method <- decode.optional_field(
    "token_endpoint_auth_method",
    None,
    decode.optional(decode.string),
  )
  use scope <- decode.optional_field(
    "scope",
    None,
    decode.optional(decode.string),
  )

  decode.success(RegistrationRequest(
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: grant_types,
    response_types: response_types,
    token_endpoint_auth_method: token_endpoint_auth_method,
    scope: scope,
  ))
}

fn validate_redirect_uris(
  uris: List(String),
) -> Result(Nil, #(Int, String, String)) {
  // Find any invalid URI using the validator
  let invalid =
    list.find(uris, fn(uri) {
      case validator.validate_redirect_uri(uri) {
        Ok(_) -> False
        Error(_) -> True
      }
    })

  case invalid {
    Ok(uri) ->
      Error(#(
        400,
        "invalid_request",
        "Invalid redirect_uri: "
          <> uri
          <> ". URIs must use https://, or http:// only for localhost.",
      ))
    Error(_) -> Ok(Nil)
  }
}

fn parse_grant_types(types_list: List(String)) -> List(GrantType) {
  list.filter_map(types_list, fn(t) {
    case t {
      "authorization_code" -> Ok(AuthorizationCode)
      "refresh_token" -> Ok(RefreshToken)
      _ -> Error(Nil)
    }
  })
}

fn parse_response_types(types_list: List(String)) -> List(types.ResponseType) {
  list.filter_map(types_list, fn(t) {
    case t {
      "code" -> Ok(Code)
      _ -> Error(Nil)
    }
  })
}

fn parse_auth_method(method: String) -> types.ClientAuthMethod {
  case method {
    "client_secret_post" -> ClientSecretPost
    "client_secret_basic" -> ClientSecretBasic
    "private_key_jwt" -> PrivateKeyJwt
    "none" -> AuthNone
    _ -> ClientSecretPost
  }
}

fn generate_client_id() -> String {
  "client_" <> random_string(16)
}

fn generate_client_secret() -> String {
  random_string(32)
}

fn random_string(bytes: Int) -> String {
  crypto.strong_random_bytes(bytes)
  |> bit_array.base64_url_encode(False)
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int

fn encode_response(response: RegistrationResponse) -> json.Json {
  json.object([
    #("client_id", json.string(response.client_id)),
    #("client_secret", case response.client_secret {
      Some(secret) -> json.string(secret)
      None -> json.null()
    }),
    #("client_name", json.string(response.client_name)),
    #("redirect_uris", json.array(response.redirect_uris, json.string)),
    #("grant_types", json.array(response.grant_types, json.string)),
    #("response_types", json.array(response.response_types, json.string)),
    #(
      "token_endpoint_auth_method",
      json.string(response.token_endpoint_auth_method),
    ),
    #("scope", json.string(response.scope)),
    #("client_id_issued_at", json.int(response.client_id_issued_at)),
    #("client_type", json.string(response.client_type)),
  ])
}
