import gleam/erlang/process.{type Subject}
import gleam/list
import group_registry

/// Represents the operation type for a record event
pub type RecordOperation {
  Create
  Update
  Delete
}

/// Event published when a record is created, updated, or deleted
pub type RecordEvent {
  RecordEvent(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    value: String,
    indexed_at: String,
    operation: RecordOperation,
  )
}

/// The group name for all record event subscriptions
const group_name = "record_events"

// Global registry name - must be created once and reused
@external(erlang, "pubsub_ffi", "get_registry_name")
fn registry_name() -> process.Name(group_registry.Message(RecordEvent))

/// Initialize the PubSub registry
/// Must be called once when the server starts
pub fn start() -> Nil {
  // Start the registry (idempotent - safe to call multiple times)
  let _ = group_registry.start(registry_name())
  Nil
}

fn get_registry() -> group_registry.GroupRegistry(RecordEvent) {
  // Get existing registry
  group_registry.get_registry(registry_name())
}

/// Subscribe to all record events
/// Returns a Subject that will receive RecordEvent messages
pub fn subscribe() -> Subject(RecordEvent) {
  let registry = get_registry()
  let my_pid = process.self()
  group_registry.join(registry, group_name, my_pid)
}

/// Unsubscribe from record events
pub fn unsubscribe(subscriber: Subject(RecordEvent)) -> Nil {
  let registry = get_registry()
  case process.subject_owner(subscriber) {
    Ok(my_pid) -> group_registry.leave(registry, group_name, [my_pid])
    Error(_) -> Nil
  }
}

/// Publish a record event to all subscribers
pub fn publish(event: RecordEvent) -> Nil {
  let registry = get_registry()
  let subscribers = group_registry.members(registry, group_name)
  list.each(subscribers, fn(sub) { process.send(sub, event) })
}
