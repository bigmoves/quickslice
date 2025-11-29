/// Backfill status polling module
///
/// Handles polling the server for backfill status and managing
/// the polling interval lifecycle.
import generated/queries/is_backfilling
import gleam/json
import lustre/effect.{type Effect}
import squall/unstable_registry as registry
import squall_cache.{type Cache}

/// Backfill status states
pub type BackfillStatus {
  /// No backfill running, none completed this session
  Idle
  /// Backfill triggered locally, waiting for server confirmation
  Triggered
  /// Server confirmed backfill is running
  InProgress
  /// Backfill completed this session
  Completed
}

/// Messages for backfill polling
pub type Msg {
  /// Timer tick - time to poll
  PollTick
  /// Response received from server
  PollResponse(Result(is_backfilling.IsBackfillingResponse, String))
}

/// Poll for backfill status
/// Returns updated cache and effects
pub fn poll(
  cache: Cache,
  registry: registry.Registry,
  wrap_msg: fn(String, json.Json, Result(String, String)) -> msg,
) -> #(Cache, List(Effect(msg))) {
  let variables = json.object([])

  // Invalidate to force fresh request
  let cache_invalidated =
    squall_cache.invalidate(cache, "IsBackfilling", variables)

  let #(cache_with_lookup, _) =
    squall_cache.lookup(
      cache_invalidated,
      "IsBackfilling",
      variables,
      is_backfilling.parse_is_backfilling_response,
    )

  squall_cache.process_pending(cache_with_lookup, registry, wrap_msg, fn() { 0 })
}

/// Determine new status based on server response and current status
pub fn update_status(
  current_status: BackfillStatus,
  server_is_backfilling: Bool,
) -> BackfillStatus {
  case current_status, server_is_backfilling {
    // Server confirms backfill is running -> in progress
    Triggered, True -> InProgress
    InProgress, True -> InProgress
    // Was confirmed in progress, now done -> completed
    InProgress, False -> Completed
    // Still waiting for server to start (triggered but server says false)
    Triggered, False -> Triggered
    // Otherwise keep current status
    status, _ -> status
  }
}

/// Check if we should continue polling
pub fn should_poll(status: BackfillStatus) -> Bool {
  case status {
    Triggered -> True
    InProgress -> True
    _ -> False
  }
}
