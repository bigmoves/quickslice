/// OAuth client repository operations
import database/executor.{type DbError, type Executor, Int as DbInt, Text}
import database/types.{type OAuthClient, OAuthClient}
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lib/oauth/token_generator
import logging

/// Insert a new OAuth client
pub fn insert(exec: Executor, client: OAuthClient) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)
  let p8 = executor.placeholder(exec, 8)
  let p9 = executor.placeholder(exec, 9)
  let p10 = executor.placeholder(exec, 10)
  let p11 = executor.placeholder(exec, 11)
  let p12 = executor.placeholder(exec, 12)
  let p13 = executor.placeholder(exec, 13)
  let p14 = executor.placeholder(exec, 14)
  let p15 = executor.placeholder(exec, 15)
  let p16 = executor.placeholder(exec, 16)
  let p17 = executor.placeholder(exec, 17)

  let sql = case executor.dialect(exec) {
    executor.SQLite -> "INSERT INTO oauth_client (
        client_id, client_secret, client_name, redirect_uris,
        grant_types, response_types, scope, token_endpoint_auth_method,
        client_type, created_at, updated_at, metadata,
        access_token_expiration, refresh_token_expiration,
        require_redirect_exact, registration_access_token, jwks
      ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> ", " <> p13 <> ", " <> p14 <> ", " <> p15 <> ", " <> p16 <> ", " <> p17 <> ")"
    executor.PostgreSQL -> "INSERT INTO oauth_client (
        client_id, client_secret, client_name, redirect_uris,
        grant_types, response_types, scope, token_endpoint_auth_method,
        client_type, created_at, updated_at, metadata,
        access_token_expiration, refresh_token_expiration,
        require_redirect_exact, registration_access_token, jwks
      ) VALUES (" <> p1 <> ", " <> p2 <> ", " <> p3 <> ", " <> p4 <> ", " <> p5 <> ", " <> p6 <> ", " <> p7 <> ", " <> p8 <> ", " <> p9 <> ", " <> p10 <> ", " <> p11 <> ", " <> p12 <> "::jsonb, " <> p13 <> ", " <> p14 <> ", " <> p15 <> ", " <> p16 <> ", " <> p17 <> "::jsonb)"
  }

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
    Text(client.client_id),
    executor.nullable_text(client.client_secret),
    Text(client.client_name),
    Text(redirect_uris_json),
    Text(grant_types_json),
    Text(response_types_json),
    executor.nullable_text(client.scope),
    Text(types.client_auth_method_to_string(client.token_endpoint_auth_method)),
    Text(types.client_type_to_string(client.client_type)),
    DbInt(client.created_at),
    DbInt(client.updated_at),
    Text(client.metadata),
    DbInt(client.access_token_expiration),
    DbInt(client.refresh_token_expiration),
    executor.bool_value(client.require_redirect_exact),
    executor.nullable_text(client.registration_access_token),
    executor.nullable_text(client.jwks),
  ]

  executor.exec(exec, sql, params)
}

/// Get an OAuth client by client_id
pub fn get(
  exec: Executor,
  client_id: String,
) -> Result(Option(OAuthClient), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT client_id, client_secret, client_name, redirect_uris,
              grant_types, response_types, scope, token_endpoint_auth_method,
              client_type, created_at, updated_at, metadata,
              access_token_expiration, refresh_token_expiration,
              require_redirect_exact, registration_access_token, jwks
       FROM oauth_client WHERE client_id = " <> executor.placeholder(exec, 1)
    executor.PostgreSQL ->
      "SELECT client_id, client_secret, client_name, redirect_uris,
              grant_types, response_types, scope, token_endpoint_auth_method,
              client_type, created_at, updated_at, metadata::text,
              access_token_expiration, refresh_token_expiration,
              require_redirect_exact::int, registration_access_token, jwks::text
       FROM oauth_client WHERE client_id = " <> executor.placeholder(exec, 1)
  }

  case executor.query(exec, sql, [Text(client_id)], decoder()) {
    Ok(rows) ->
      case list.first(rows) {
        Ok(client) -> Ok(Some(client))
        Error(_) -> Ok(None)
      }
    Error(err) -> Error(err)
  }
}

/// Get all OAuth clients (excludes internal 'admin' client)
pub fn get_all(exec: Executor) -> Result(List(OAuthClient), DbError) {
  let sql = case executor.dialect(exec) {
    executor.SQLite ->
      "SELECT client_id, client_secret, client_name, redirect_uris,
              grant_types, response_types, scope, token_endpoint_auth_method,
              client_type, created_at, updated_at, metadata,
              access_token_expiration, refresh_token_expiration,
              require_redirect_exact, registration_access_token, jwks
       FROM oauth_client WHERE client_id != 'admin'
       ORDER BY created_at DESC"
    executor.PostgreSQL ->
      "SELECT client_id, client_secret, client_name, redirect_uris,
              grant_types, response_types, scope, token_endpoint_auth_method,
              client_type, created_at, updated_at, metadata::text,
              access_token_expiration, refresh_token_expiration,
              require_redirect_exact::int, registration_access_token, jwks::text
       FROM oauth_client WHERE client_id != 'admin'
       ORDER BY created_at DESC"
  }

  executor.query(exec, sql, [], decoder())
}

/// Update an existing OAuth client
pub fn update(exec: Executor, client: OAuthClient) -> Result(Nil, DbError) {
  let p1 = executor.placeholder(exec, 1)
  let p2 = executor.placeholder(exec, 2)
  let p3 = executor.placeholder(exec, 3)
  let p4 = executor.placeholder(exec, 4)
  let p5 = executor.placeholder(exec, 5)
  let p6 = executor.placeholder(exec, 6)
  let p7 = executor.placeholder(exec, 7)
  let p8 = executor.placeholder(exec, 8)
  let p9 = executor.placeholder(exec, 9)
  let p10 = executor.placeholder(exec, 10)
  let p11 = executor.placeholder(exec, 11)
  let p12 = executor.placeholder(exec, 12)
  let p13 = executor.placeholder(exec, 13)
  let p14 = executor.placeholder(exec, 14)

  let sql = case executor.dialect(exec) {
    executor.SQLite -> "UPDATE oauth_client SET
        client_secret = " <> p1 <> ",
        client_name = " <> p2 <> ",
        redirect_uris = " <> p3 <> ",
        grant_types = " <> p4 <> ",
        response_types = " <> p5 <> ",
        scope = " <> p6 <> ",
        token_endpoint_auth_method = " <> p7 <> ",
        updated_at = " <> p8 <> ",
        metadata = " <> p9 <> ",
        access_token_expiration = " <> p10 <> ",
        refresh_token_expiration = " <> p11 <> ",
        require_redirect_exact = " <> p12 <> ",
        jwks = " <> p13 <> "
      WHERE client_id = " <> p14
    executor.PostgreSQL -> "UPDATE oauth_client SET
        client_secret = " <> p1 <> ",
        client_name = " <> p2 <> ",
        redirect_uris = " <> p3 <> ",
        grant_types = " <> p4 <> ",
        response_types = " <> p5 <> ",
        scope = " <> p6 <> ",
        token_endpoint_auth_method = " <> p7 <> ",
        updated_at = " <> p8 <> ",
        metadata = " <> p9 <> "::jsonb,
        access_token_expiration = " <> p10 <> ",
        refresh_token_expiration = " <> p11 <> ",
        require_redirect_exact = " <> p12 <> ",
        jwks = " <> p13 <> "::jsonb
      WHERE client_id = " <> p14
  }

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
    executor.nullable_text(client.client_secret),
    Text(client.client_name),
    Text(redirect_uris_json),
    Text(grant_types_json),
    Text(response_types_json),
    executor.nullable_text(client.scope),
    Text(types.client_auth_method_to_string(client.token_endpoint_auth_method)),
    DbInt(client.updated_at),
    Text(client.metadata),
    DbInt(client.access_token_expiration),
    DbInt(client.refresh_token_expiration),
    executor.bool_value(client.require_redirect_exact),
    executor.nullable_text(client.jwks),
    Text(client.client_id),
  ]

  executor.exec(exec, sql, params)
}

/// Delete an OAuth client
pub fn delete(exec: Executor, client_id: String) -> Result(Nil, DbError) {
  let sql =
    "DELETE FROM oauth_client WHERE client_id = "
    <> executor.placeholder(exec, 1)

  executor.exec(exec, sql, [Text(client_id)])
}

/// Ensure the internal "admin" client exists for admin UI tokens
/// Call this at startup after migrations
pub fn ensure_admin_client(exec: Executor) -> Result(Nil, DbError) {
  case get(exec, "admin") {
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
      case insert(exec, admin_client) {
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
