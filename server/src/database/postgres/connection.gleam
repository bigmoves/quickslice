// server/src/database/postgres/connection.gleam

import database/executor.{type DbError, type Executor, ConnectionError}
import database/postgres/executor as postgres_executor
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/uri
import logging
import pog

/// Default connection pool size
const default_pool_size = 10

/// Connect to PostgreSQL database and return an Executor
pub fn connect(url: String) -> Result(Executor, DbError) {
  use config <- result.try(parse_url(url))

  // Generate a unique pool name
  let pool_name = process.new_name("pog_pool")

  let pog_config =
    pog.default_config(pool_name: pool_name)
    |> pog.host(config.host)
    |> pog.port(config.port)
    |> pog.database(config.database)
    |> pog.user(config.user)
    |> apply_password(config.password)
    |> pog.pool_size(config.pool_size)

  case pog.start(pog_config) {
    Ok(started) -> {
      logging.log(
        logging.Info,
        "Connected to PostgreSQL database: "
          <> config.host
          <> "/"
          <> config.database,
      )
      Ok(postgres_executor.new(started.data))
    }
    Error(actor.InitTimeout) ->
      Error(ConnectionError("PostgreSQL connection timeout"))
    Error(actor.InitFailed(_reason)) ->
      Error(ConnectionError("Failed to connect to PostgreSQL"))
    Error(actor.InitExited(_reason)) ->
      Error(ConnectionError("PostgreSQL connection process exited"))
  }
}

/// Apply password to config if present
fn apply_password(
  config: pog.Config,
  password: option.Option(String),
) -> pog.Config {
  case password {
    Some(p) -> pog.password(config, Some(p))
    None -> config
  }
}

/// Parsed PostgreSQL connection config
type PgConfig {
  PgConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: option.Option(String),
    pool_size: Int,
  )
}

/// Parse a PostgreSQL URL into connection config
/// Format: postgres://user:password@host:port/database?pool_size=N
fn parse_url(url: String) -> Result(PgConfig, DbError) {
  case uri.parse(url) {
    Ok(parsed) -> {
      let host = option.unwrap(parsed.host, "localhost")
      let port = option.unwrap(parsed.port, 5432)
      let database = case parsed.path {
        "/" <> db -> db
        db -> db
      }

      // Parse userinfo (user:password)
      let #(user, password) = case parsed.userinfo {
        Some(userinfo) ->
          case string.split_once(userinfo, ":") {
            Ok(#(u, p)) -> #(u, Some(p))
            Error(_) -> #(userinfo, None)
          }
        None -> #("postgres", None)
      }

      // Parse query params for pool_size
      let pool_size = case parsed.query {
        Some(query) -> parse_pool_size(query)
        None -> default_pool_size
      }

      case database {
        "" -> Error(ConnectionError("No database specified in PostgreSQL URL"))
        _ ->
          Ok(PgConfig(
            host: host,
            port: port,
            database: database,
            user: user,
            password: password,
            pool_size: pool_size,
          ))
      }
    }
    Error(_) -> Error(ConnectionError("Invalid PostgreSQL URL: " <> url))
  }
}

/// Parse pool_size from query string
fn parse_pool_size(query: String) -> Int {
  query
  |> string.split("&")
  |> list.find_map(fn(param) {
    case string.split_once(param, "=") {
      Ok(#("pool_size", value)) ->
        case int.parse(value) {
          Ok(n) -> Ok(n)
          Error(_) -> Error(Nil)
        }
      _ -> Error(Nil)
    }
  })
  |> result.unwrap(default_pool_size)
}
