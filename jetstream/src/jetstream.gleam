import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/string

/// Jetstream event types
pub type JetstreamEvent {
  CommitEvent(did: String, time_us: Int, commit: CommitData)
  IdentityEvent(did: String, time_us: Int, identity: IdentityData)
  AccountEvent(did: String, time_us: Int, account: AccountData)
  UnknownEvent(raw: String)
}

pub type CommitData {
  CommitData(
    rev: String,
    operation: String,
    collection: String,
    rkey: String,
    record: Option(Dynamic),
    cid: Option(String),
  )
}

pub type IdentityData {
  IdentityData(did: String, handle: String, seq: Int, time: String)
}

pub type AccountData {
  AccountData(active: Bool, did: String, seq: Int, time: String)
}

/// Configuration for Jetstream consumer
pub type JetstreamConfig {
  JetstreamConfig(
    endpoint: String,
    wanted_collections: List(String),
    wanted_dids: List(String),
  )
}

/// Create a default configuration for US East endpoint
pub fn default_config() -> JetstreamConfig {
  JetstreamConfig(
    endpoint: "wss://jetstream2.us-east.bsky.network/subscribe",
    wanted_collections: [],
    wanted_dids: [],
  )
}

/// Build the WebSocket URL with query parameters
pub fn build_url(config: JetstreamConfig) -> String {
  let base = config.endpoint
  let mut_params = []

  // Add wanted collections (each as a separate query parameter)
  let mut_params = case config.wanted_collections {
    [] -> mut_params
    collections -> {
      let collection_params =
        list.map(collections, fn(col) { "wantedCollections=" <> col })
      list.append(collection_params, mut_params)
    }
  }

  // Add wanted DIDs (each as a separate query parameter)
  let mut_params = case config.wanted_dids {
    [] -> mut_params
    dids -> {
      let did_params = list.map(dids, fn(did) { "wantedDids=" <> did })
      list.append(did_params, mut_params)
    }
  }

  case mut_params {
    [] -> base
    params -> base <> "?" <> string.join(list.reverse(params), "&")
  }
}

/// Connect to Jetstream WebSocket using Erlang gun library
@external(erlang, "jetstream_ws_ffi", "connect")
pub fn connect(url: String, handler_pid: Pid) -> Result(Pid, Dynamic)

/// Start consuming the Jetstream feed
pub fn start_consumer(
  config: JetstreamConfig,
  on_event: fn(String) -> Nil,
) -> Nil {
  let url = build_url(config)
  io.println("🔗 Jetstream URL: " <> url)
  let self = process.self()
  let result = connect(url, self)

  case result {
    Ok(_conn_pid) -> {
      receive_loop(on_event)
    }
    Error(err) -> {
      io.println("Failed to connect to Jetstream")
      io.println_error(string.inspect(err))
    }
  }
}

/// Receive loop for WebSocket messages
fn receive_loop(on_event: fn(String) -> Nil) -> Nil {
  // Call Erlang to receive one message
  case receive_ws_message() {
    Ok(text) -> {
      on_event(text)
      receive_loop(on_event)
    }
    Error(_) -> {
      // Timeout or error, continue loop
      receive_loop(on_event)
    }
  }
}

/// Receive a WebSocket message from the message queue
@external(erlang, "jetstream_ffi", "receive_ws_message")
fn receive_ws_message() -> Result(String, Nil)

/// Parse a JSON event string into a JetstreamEvent
pub fn parse_event(json_string: String) -> JetstreamEvent {
  // Try to parse as commit event first
  case json.parse(json_string, commit_event_decoder()) {
    Ok(event) -> event
    Error(_) -> {
      // Try identity event
      case json.parse(json_string, identity_event_decoder()) {
        Ok(event) -> event
        Error(_) -> {
          // Try account event
          case json.parse(json_string, account_event_decoder()) {
            Ok(event) -> event
            Error(_) -> UnknownEvent(json_string)
          }
        }
      }
    }
  }
}

/// Decoder for commit events
fn commit_event_decoder() {
  use did <- decode.field("did", decode.string)
  use time_us <- decode.field("time_us", decode.int)
  use commit <- decode.field("commit", commit_data_decoder())
  decode.success(CommitEvent(did: did, time_us: time_us, commit: commit))
}

/// Decoder for commit data - handles both create/update (with record) and delete (without)
fn commit_data_decoder() {
  // Try decoder with record and cid fields first (for create/update)
  // If that fails, try without (for delete)
  decode.one_of(commit_with_record_decoder(), or: [
    commit_without_record_decoder(),
  ])
}

/// Decoder for commit with record (create/update operations)
fn commit_with_record_decoder() {
  use rev <- decode.field("rev", decode.string)
  use operation <- decode.field("operation", decode.string)
  use collection <- decode.field("collection", decode.string)
  use rkey <- decode.field("rkey", decode.string)
  use record <- decode.field("record", decode.dynamic)
  use cid <- decode.field("cid", decode.string)
  decode.success(CommitData(
    rev: rev,
    operation: operation,
    collection: collection,
    rkey: rkey,
    record: option.Some(record),
    cid: option.Some(cid),
  ))
}

/// Decoder for commit without record (delete operations)
fn commit_without_record_decoder() {
  use rev <- decode.field("rev", decode.string)
  use operation <- decode.field("operation", decode.string)
  use collection <- decode.field("collection", decode.string)
  use rkey <- decode.field("rkey", decode.string)
  decode.success(CommitData(
    rev: rev,
    operation: operation,
    collection: collection,
    rkey: rkey,
    record: option.None,
    cid: option.None,
  ))
}

/// Decoder for identity events
fn identity_event_decoder() {
  use did <- decode.field("did", decode.string)
  use time_us <- decode.field("time_us", decode.int)
  use identity <- decode.field("identity", identity_data_decoder())
  decode.success(IdentityEvent(did: did, time_us: time_us, identity: identity))
}

/// Decoder for identity data
fn identity_data_decoder() {
  use did <- decode.field("did", decode.string)
  use handle <- decode.field("handle", decode.string)
  use seq <- decode.field("seq", decode.int)
  use time <- decode.field("time", decode.string)
  decode.success(IdentityData(did: did, handle: handle, seq: seq, time: time))
}

/// Decoder for account events
fn account_event_decoder() {
  use did <- decode.field("did", decode.string)
  use time_us <- decode.field("time_us", decode.int)
  use account <- decode.field("account", account_data_decoder())
  decode.success(AccountEvent(did: did, time_us: time_us, account: account))
}

/// Decoder for account data
fn account_data_decoder() {
  use active <- decode.field("active", decode.bool)
  use did <- decode.field("did", decode.string)
  use seq <- decode.field("seq", decode.int)
  use time <- decode.field("time", decode.string)
  decode.success(AccountData(active: active, did: did, seq: seq, time: time))
}
