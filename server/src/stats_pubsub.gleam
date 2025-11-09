import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/option
import group_registry

/// Event types for stats updates
pub type StatsEvent {
  RecordCreated
  RecordDeleted
  ActorCreated
  ActivityLogged(
    id: Int,
    timestamp: String,
    operation: String,
    collection: String,
    did: String,
    status: String,
    error_message: option.Option(String),
    event_json: String,
  )
}

/// The group name for all stats event subscriptions
const group_name = "stats_events"

// Global registry name - must be created once and reused
@external(erlang, "stats_pubsub_ffi", "get_registry_name")
fn registry_name() -> process.Name(group_registry.Message(StatsEvent))

/// Initialize the Stats PubSub registry
/// Must be called once when the server starts
pub fn start() -> Nil {
  // Start the registry (idempotent - safe to call multiple times)
  let _ = group_registry.start(registry_name())
  Nil
}

fn get_registry() -> group_registry.GroupRegistry(StatsEvent) {
  // Get existing registry
  group_registry.get_registry(registry_name())
}

/// Subscribe to stats events
/// Returns a Subject that will receive StatsEvent messages
pub fn subscribe() -> Subject(StatsEvent) {
  let registry = get_registry()
  let my_pid = process.self()
  group_registry.join(registry, group_name, my_pid)
}

/// Unsubscribe from stats events
pub fn unsubscribe(subscriber: Subject(StatsEvent)) -> Nil {
  let registry = get_registry()
  case process.subject_owner(subscriber) {
    Ok(my_pid) -> group_registry.leave(registry, group_name, [my_pid])
    Error(_) -> Nil
  }
}

/// Publish a stats event to all subscribers
pub fn publish(event: StatsEvent) -> Nil {
  let registry = get_registry()
  let subscribers = group_registry.members(registry, group_name)
  list.each(subscribers, fn(sub) { process.send(sub, event) })
}
