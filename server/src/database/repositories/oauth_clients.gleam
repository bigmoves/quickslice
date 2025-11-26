/// OAuth client repository operations
import database/types.{type OAuthClient, OAuthClient}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/token_generator
import logging
import sqlight

/// Insert a new OAuth client
pub fn insert(
  conn: sqlight.Connection,
  client: OAuthClient,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    INSERT INTO oauth_client (
      client_id, client_secret, client_name, redirect_uris,
      grant_types, response_types, scope, token_endpoint_auth_method,
      client_type, created_at, updated_at, metadata,
      access_token_expiration, refresh_token_expiration,
      require_redirect_exact, registration_access_token, jwks
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  "

  let redirect_uris_json =
    json.to_string(json.array(client.redirect_uris, json.string))
  let grant_types_json =
    json.to_string(
      json.array(client.grant_types, fn(gt) {
        json.string(types.grant_type_to_string(gt))
      }),
    )
  let response_types_json =
    json.to_string(
      json.array(client.response_types, fn(rt) {
        json.string(types.response_type_to_string(rt))
      }),
    )

  let params = [
    sqlight.text(client.client_id),
    sqlight.nullable(sqlight.text, client.client_secret),
    sqlight.text(client.client_name),
    sqlight.text(redirect_uris_json),
    sqlight.text(grant_types_json),
    sqlight.text(response_types_json),
    sqlight.nullable(sqlight.text, client.scope),
    sqlight.text(types.client_auth_method_to_string(
      client.token_endpoint_auth_method,
    )),
    sqlight.text(types.client_type_to_string(client.client_type)),
    sqlight.int(client.created_at),
    sqlight.int(client.updated_at),
    sqlight.text(client.metadata),
    sqlight.int(client.access_token_expiration),
    sqlight.int(client.refresh_token_expiration),
    sqlight.bool(client.require_redirect_exact),
    sqlight.nullable(sqlight.text, client.registration_access_token),
    sqlight.nullable(sqlight.text, client.jwks),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Get an OAuth client by client_id
pub fn get(
  conn: sqlight.Connection,
  client_id: String,
) -> Result(Option(OAuthClient), sqlight.Error) {
  let sql =
    "SELECT client_id, client_secret, client_name, redirect_uris,
            grant_types, response_types, scope, token_endpoint_auth_method,
            client_type, created_at, updated_at, metadata,
            access_token_expiration, refresh_token_expiration,
            require_redirect_exact, registration_access_token, jwks
     FROM oauth_client WHERE client_id = ?"

  use rows <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(client_id)],
    expecting: decoder(),
  ))

  case list.first(rows) {
    Ok(client) -> Ok(Some(client))
    Error(_) -> Ok(None)
  }
}

/// Get all OAuth clients (excludes internal 'admin' client)
pub fn get_all(
  conn: sqlight.Connection,
) -> Result(List(OAuthClient), sqlight.Error) {
  let sql =
    "SELECT client_id, client_secret, client_name, redirect_uris,
            grant_types, response_types, scope, token_endpoint_auth_method,
            client_type, created_at, updated_at, metadata,
            access_token_expiration, refresh_token_expiration,
            require_redirect_exact, registration_access_token, jwks
     FROM oauth_client WHERE client_id != 'admin'
     ORDER BY created_at DESC"

  sqlight.query(
    sql,
    on: conn,
    with: [],
    expecting: decoder(),
  )
}

/// Update an existing OAuth client
pub fn update(
  conn: sqlight.Connection,
  client: OAuthClient,
) -> Result(Nil, sqlight.Error) {
  let sql =
    "
    UPDATE oauth_client SET
      client_secret = ?,
      client_name = ?,
      redirect_uris = ?,
      grant_types = ?,
      response_types = ?,
      scope = ?,
      token_endpoint_auth_method = ?,
      updated_at = ?,
      metadata = ?,
      access_token_expiration = ?,
      refresh_token_expiration = ?,
      require_redirect_exact = ?,
      jwks = ?
    WHERE client_id = ?
  "

  let redirect_uris_json =
    json.to_string(json.array(client.redirect_uris, json.string))
  let grant_types_json =
    json.to_string(
      json.array(client.grant_types, fn(gt) {
        json.string(types.grant_type_to_string(gt))
      }),
    )
  let response_types_json =
    json.to_string(
      json.array(client.response_types, fn(rt) {
        json.string(types.response_type_to_string(rt))
      }),
    )

  let params = [
    sqlight.nullable(sqlight.text, client.client_secret),
    sqlight.text(client.client_name),
    sqlight.text(redirect_uris_json),
    sqlight.text(grant_types_json),
    sqlight.text(response_types_json),
    sqlight.nullable(sqlight.text, client.scope),
    sqlight.text(types.client_auth_method_to_string(
      client.token_endpoint_auth_method,
    )),
    sqlight.int(client.updated_at),
    sqlight.text(client.metadata),
    sqlight.int(client.access_token_expiration),
    sqlight.int(client.refresh_token_expiration),
    sqlight.bool(client.require_redirect_exact),
    sqlight.nullable(sqlight.text, client.jwks),
    sqlight.text(client.client_id),
  ]

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: params,
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Delete an OAuth client
pub fn delete(
  conn: sqlight.Connection,
  client_id: String,
) -> Result(Nil, sqlight.Error) {
  let sql = "DELETE FROM oauth_client WHERE client_id = ?"

  use _ <- result.try(sqlight.query(
    sql,
    on: conn,
    with: [sqlight.text(client_id)],
    expecting: decode.dynamic,
  ))
  Ok(Nil)
}

/// Ensure the internal "admin" client exists for admin UI tokens
/// Call this at startup after migrations
pub fn ensure_admin_client(conn: sqlight.Connection) -> Result(Nil, sqlight.Error) {
  case get(conn, "admin") {
    Ok(Some(_)) -> {
      // Admin client already exists
      Ok(Nil)
    }
    Ok(None) -> {
      // Create the admin client
      let now = token_generator.current_timestamp()
      let admin_client =
        OAuthClient(
          client_id: "admin",
          client_secret: None,
          client_name: "Admin UI",
          redirect_uris: [],
          grant_types: [],
          response_types: [],
          scope: None,
          token_endpoint_auth_method: types.AuthNone,
          client_type: types.Confidential,
          created_at: now,
          updated_at: now,
          metadata: "{}",
          access_token_expiration: 86_400 * 7,
          // 7 days
          refresh_token_expiration: 86_400 * 30,
          // 30 days
          require_redirect_exact: False,
          registration_access_token: None,
          jwks: None,
        )
      case insert(conn, admin_client) {
        Ok(_) -> {
          logging.log(logging.Info, "Created internal 'admin' OAuth client")
          Ok(Nil)
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(err)
  }
}

/// Decode OAuth client from database row
fn decoder() -> decode.Decoder(OAuthClient) {
  use client_id <- decode.field(0, decode.string)
  use client_secret <- decode.field(1, decode.optional(decode.string))
  use client_name <- decode.field(2, decode.string)
  use redirect_uris_json <- decode.field(3, decode.string)
  use grant_types_json <- decode.field(4, decode.string)
  use response_types_json <- decode.field(5, decode.string)
  use scope <- decode.field(6, decode.optional(decode.string))
  use auth_method_str <- decode.field(7, decode.string)
  use client_type_str <- decode.field(8, decode.string)
  use created_at <- decode.field(9, decode.int)
  use updated_at <- decode.field(10, decode.int)
  use metadata <- decode.field(11, decode.string)
  use access_exp <- decode.field(12, decode.int)
  use refresh_exp <- decode.field(13, decode.int)
  use require_exact <- decode.field(14, decode.int)
  use reg_token <- decode.field(15, decode.optional(decode.string))
  use jwks <- decode.field(16, decode.optional(decode.string))

  // Parse JSON arrays
  let redirect_uris =
    json.parse(redirect_uris_json, decode.list(decode.string))
    |> result.unwrap([])

  let grant_types =
    json.parse(grant_types_json, decode.list(decode.string))
    |> result.unwrap([])
    |> list.filter_map(fn(s) {
      case types.grant_type_from_string(s) {
        Some(gt) -> Ok(gt)
        None -> Error(Nil)
      }
    })

  let response_types =
    json.parse(response_types_json, decode.list(decode.string))
    |> result.unwrap([])
    |> list.filter_map(fn(s) {
      case types.response_type_from_string(s) {
        Some(rt) -> Ok(rt)
        None -> Error(Nil)
      }
    })

  let auth_method =
    types.client_auth_method_from_string(auth_method_str)
    |> option.unwrap(types.AuthNone)

  let client_type =
    types.client_type_from_string(client_type_str)
    |> option.unwrap(types.Public)

  decode.success(OAuthClient(
    client_id: client_id,
    client_secret: client_secret,
    client_name: client_name,
    redirect_uris: redirect_uris,
    grant_types: grant_types,
    response_types: response_types,
    scope: scope,
    token_endpoint_auth_method: auth_method,
    client_type: client_type,
    created_at: created_at,
    updated_at: updated_at,
    metadata: metadata,
    access_token_expiration: access_exp,
    refresh_token_expiration: refresh_exp,
    require_redirect_exact: require_exact == 1,
    registration_access_token: reg_token,
    jwks: jwks,
  ))
}
