/// Lexicon GraphQL schema entry point
///
/// Public API for building and executing the lexicon-driven GraphQL schema.
/// External code should import this module for all lexicon GraphQL operations.
import backfill
import database/repositories/config as config_repo
import database/repositories/lexicons
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import graphql/lexicon/converters
import graphql/lexicon/fetchers
import graphql/lexicon/mutations
import lexicon_graphql
import lexicon_graphql/schema/database
import lib/oauth/did_cache
import sqlight
import swell/executor
import swell/schema
import swell/value

/// Build a GraphQL schema from database lexicons
///
/// This is exposed for WebSocket subscriptions to build the schema once
/// and reuse it for multiple subscription executions.
pub fn build_schema_from_db(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
  domain_authority: String,
) -> Result(schema.Schema, String) {
  // Step 1: Fetch lexicons from database
  use lexicon_records <- result.try(
    lexicons.get_all(db)
    |> result.map_error(fn(_) { "Failed to fetch lexicons from database" }),
  )

  // Step 2: Parse lexicon JSON into structured Lexicon types
  let parsed_lexicons =
    lexicon_records
    |> list.filter_map(fn(lex) {
      case lexicon_graphql.parse_lexicon(lex.json) {
        Ok(parsed) -> Ok(parsed)
        Error(_) -> Error(Nil)
      }
    })

  // Check if we got any valid lexicons
  case parsed_lexicons {
    [] -> Error("No valid lexicons found in database")
    _ -> {
      // Step 3: Create fetchers
      let record_fetcher = fetchers.record_fetcher(db)
      let batch_fetcher = fetchers.batch_fetcher(db)
      let paginated_batch_fetcher = fetchers.paginated_batch_fetcher(db)
      let aggregate_fetcher = fetchers.aggregate_fetcher(db)
      let viewer_fetcher = fetchers.viewer_fetcher(db)

      // Step 4: Determine local and external collections for backfill
      let collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case
            backfill.nsid_matches_domain_authority(lex.id, domain_authority)
          {
            True -> Ok(lex.id)
            False -> Error(Nil)
          }
        })

      let external_collection_ids =
        parsed_lexicons
        |> list.filter_map(fn(lex) {
          case
            backfill.nsid_matches_domain_authority(lex.id, domain_authority)
          {
            True -> Error(Nil)
            False -> Ok(lex.id)
          }
        })

      // Step 5: Create mutation resolver factories
      let mutation_ctx =
        mutations.MutationContext(
          db: db,
          did_cache: did_cache,
          signing_key: signing_key,
          atp_client_id: atp_client_id,
          plc_url: plc_url,
          collection_ids: collection_ids,
          external_collection_ids: external_collection_ids,
        )

      let create_factory =
        option.Some(fn(collection) {
          mutations.create_resolver_factory(collection, mutation_ctx)
        })

      let update_factory =
        option.Some(fn(collection) {
          mutations.update_resolver_factory(collection, mutation_ctx)
        })

      let delete_factory =
        option.Some(fn(collection) {
          mutations.delete_resolver_factory(collection, mutation_ctx)
        })

      let upload_blob_factory =
        option.Some(fn() {
          mutations.upload_blob_resolver_factory(mutation_ctx)
        })

      // Step 6: Build schema with database-backed resolvers, mutations, and subscriptions
      database.build_schema_with_subscriptions(
        parsed_lexicons,
        record_fetcher,
        option.Some(batch_fetcher),
        option.Some(paginated_batch_fetcher),
        create_factory,
        update_factory,
        delete_factory,
        upload_blob_factory,
        option.Some(aggregate_fetcher),
        option.Some(viewer_fetcher),
      )
    }
  }
}

/// Execute a GraphQL query against lexicons in the database
///
/// This fetches lexicons, builds a schema with database resolvers,
/// executes the query, and returns the result as JSON.
pub fn execute_query_with_db(
  db: sqlight.Connection,
  query_string: String,
  variables_json_str: String,
  auth_token: Result(String, Nil),
  did_cache: Subject(did_cache.Message),
  signing_key: option.Option(String),
  atp_client_id: String,
  plc_url: String,
) -> Result(String, String) {
  // Get domain authority from database
  let domain_authority = case config_repo.get(db, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  // Build the schema
  use graphql_schema <- result.try(build_schema_from_db(
    db,
    did_cache,
    signing_key,
    atp_client_id,
    plc_url,
    domain_authority,
  ))

  // Create context with auth token if provided
  let ctx_data = case auth_token {
    Ok(token) -> {
      // Add auth token to context for mutation resolvers
      option.Some(value.Object([#("auth_token", value.String(token))]))
    }
    Error(_) -> option.None
  }

  // Convert json variables to Dict(String, value.Value)
  let variables_dict = json_string_to_variables_dict(variables_json_str)

  let ctx = schema.context_with_variables(ctx_data, variables_dict)

  // Execute the query
  use response <- result.try(executor.execute(query_string, graphql_schema, ctx))

  // Format the response as JSON
  Ok(format_response(response))
}

/// Format an executor.Response as JSON string
/// Per GraphQL spec, only include "errors" field when there are actual errors
pub fn format_response(response: executor.Response) -> String {
  let data_json = value_to_json(response.data)

  case response.errors {
    [] -> "{\"data\": " <> data_json <> "}"
    errors -> {
      let error_strings =
        list.map(errors, fn(err) {
          let message_json = json.string(err.message) |> json.to_string
          let path_json =
            json.array(err.path, of: json.string) |> json.to_string
          "{\"message\": " <> message_json <> ", \"path\": " <> path_json <> "}"
        })

      let errors_json = "[" <> string.join(error_strings, ",") <> "]"
      "{\"data\": " <> data_json <> ", \"errors\": " <> errors_json <> "}"
    }
  }
}

/// Convert JSON string variables to Dict(String, value.Value)
/// Exported for use by subscription handlers
pub fn json_string_to_variables_dict(
  json_string: String,
) -> dict.Dict(String, value.Value) {
  // First try to extract the "variables" field from the JSON
  let variables_decoder = {
    use vars <- decode.field("variables", decode.dynamic)
    decode.success(vars)
  }

  case json.parse(json_string, variables_decoder) {
    Ok(dyn) -> {
      // Convert dynamic to value.Value
      case converters.json_dynamic_to_value(dyn) {
        value.Object(fields) -> dict.from_list(fields)
        _ -> dict.new()
      }
    }
    Error(_) -> dict.new()
  }
}

/// Re-export parse_json_to_value for WebSocket handler
pub fn parse_json_to_value(json_str: String) -> Result(value.Value, String) {
  converters.parse_json_to_value(json_str)
}

// ─── Private Helpers ───────────────────────────────────────────────

/// Convert a GraphQL value to JSON string
fn value_to_json(val: value.Value) -> String {
  case val {
    value.Null -> "null"
    value.Int(i) -> json.int(i) |> json.to_string
    value.Float(f) -> json.float(f) |> json.to_string
    value.String(s) -> json.string(s) |> json.to_string
    value.Boolean(b) -> json.bool(b) |> json.to_string
    value.Enum(e) -> json.string(e) |> json.to_string
    value.List(items) -> {
      let item_jsons = list.map(items, value_to_json)
      "[" <> string.join(item_jsons, ",") <> "]"
    }
    value.Object(fields) -> {
      let field_jsons =
        list.map(fields, fn(field) {
          let #(key, v) = field
          let key_json = json.string(key) |> json.to_string
          let value_json = value_to_json(v)
          key_json <> ": " <> value_json
        })
      "{" <> string.join(field_jsons, ",") <> "}"
    }
  }
}
