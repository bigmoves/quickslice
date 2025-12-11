/// Admin GraphQL schema entry point
///
/// This is the public API for the admin GraphQL schema.
/// External code should import this module to build the schema.
import backfill_state
import database/executor.{type Executor}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, Some}
import graphql/admin/mutations
import graphql/admin/queries
import jetstream_consumer
import lib/oauth/did_cache
import swell/schema
import wisp

/// Build the complete GraphQL schema for admin queries
pub fn build_schema(
  conn: Executor,
  req: wisp.Request,
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
  backfill_state_subject: Subject(backfill_state.Message),
) -> schema.Schema {
  schema.schema(
    queries.query_type(conn, req, did_cache, backfill_state_subject),
    Some(mutations.mutation_type(
      conn,
      req,
      jetstream_subject,
      did_cache,
      oauth_supported_scopes,
      backfill_state_subject,
    )),
  )
}
