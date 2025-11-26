/// DID document caching with ETS backend
import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/otp/actor

/// Cache state containing the ETS table reference
pub type CacheState {
  CacheState(table: dynamic.Dynamic)
}

/// Cache message types
pub type Message {
  Get(did: String, reply_to: Subject(Result(String, String)))
  Put(did: String, document: String, ttl_seconds: Int)
  Delete(did: String)
  Invalidate(did: String)
  Cleanup
}

/// Start the DID cache actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  let initial_state = CacheState(table: new_table())

  let result =
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Get a DID document from cache
pub fn get(cache: Subject(Message), did: String) -> Result(String, String) {
  actor.call(cache, waiting: 5000, sending: Get(did, _))
}

/// Put a DID document in cache with TTL
pub fn put(
  cache: Subject(Message),
  did: String,
  document: String,
  ttl_seconds: Int,
) -> Nil {
  actor.send(cache, Put(did, document, ttl_seconds))
}

/// Delete a DID from cache
pub fn delete(cache: Subject(Message), did: String) -> Nil {
  actor.send(cache, Delete(did))
}

/// Invalidate (same as delete)
pub fn invalidate(cache: Subject(Message), did: String) -> Nil {
  actor.send(cache, Invalidate(did))
}

/// Handle cache messages
fn handle_message(
  state: CacheState,
  message: Message,
) -> actor.Next(CacheState, Message) {
  case message {
    Get(did, reply_to) -> {
      let result = lookup_internal(state.table, did)
      actor.send(reply_to, result)
      actor.continue(state)
    }
    Put(did, document, ttl_seconds) -> {
      let expires_at = current_timestamp() + ttl_seconds
      insert_internal(state.table, did, document, expires_at)
      actor.continue(state)
    }
    Delete(did) -> {
      delete_internal(state.table, did)
      actor.continue(state)
    }
    Invalidate(did) -> {
      delete_internal(state.table, did)
      actor.continue(state)
    }
    Cleanup -> {
      cleanup_expired_internal(state.table)
      actor.continue(state)
    }
  }
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int

@external(erlang, "did_cache_ffi", "new_table")
fn new_table() -> dynamic.Dynamic

@external(erlang, "did_cache_ffi", "insert")
fn insert_internal(
  table: dynamic.Dynamic,
  did: String,
  document: String,
  expires_at: Int,
) -> Nil

@external(erlang, "did_cache_ffi", "lookup")
fn lookup_internal(
  table: dynamic.Dynamic,
  did: String,
) -> Result(String, String)

@external(erlang, "did_cache_ffi", "delete")
fn delete_internal(table: dynamic.Dynamic, did: String) -> Nil

@external(erlang, "did_cache_ffi", "cleanup_expired")
fn cleanup_expired_internal(table: dynamic.Dynamic) -> Nil
