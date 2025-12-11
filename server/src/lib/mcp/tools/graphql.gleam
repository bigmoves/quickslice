import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/option.{type Option}
import gleam/result
import graphql/lexicon/schema as lexicon_schema
import lib/oauth/did_cache
import sqlight

/// Execute a GraphQL query
pub fn execute_query(
  db: sqlight.Connection,
  query: String,
  variables_json: String,
  did_cache: Subject(did_cache.Message),
  signing_key: Option(String),
  plc_url: String,
) -> Result(json.Json, String) {
  use result_str <- result.try(lexicon_schema.execute_query_with_db(
    db,
    query,
    variables_json,
    Error(Nil),
    // No auth token for MCP queries
    did_cache,
    signing_key,
    "",
    // Empty atp_client_id - MCP queries don't do mutations that need ATP refresh
    plc_url,
  ))

  // Return the result string wrapped in a JSON object
  Ok(json.object([#("result", json.string(result_str))]))
}

/// Get full GraphQL schema introspection
pub fn introspect_schema(
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  signing_key: Option(String),
  plc_url: String,
) -> Result(json.Json, String) {
  let introspection_query =
    "
    query IntrospectionQuery {
      __schema {
        queryType { name }
        mutationType { name }
        subscriptionType { name }
        types {
          name
          kind
          description
          fields {
            name
            description
            args { name type { name } }
            type { name kind ofType { name kind } }
          }
        }
      }
    }
  "

  execute_query(db, introspection_query, "{}", did_cache, signing_key, plc_url)
}
