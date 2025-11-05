import gleam/erlang/process

pub type SubscriptionMessage {
  SubscriptionData(id: String, data: String)
}

/// Send a subscription data message to the handler process
@external(erlang, "websocket_ffi_impl", "send_to_handler")
pub fn send_to_handler(
  handler_pid: process.Pid,
  subscription_id: String,
  data: String,
) -> Nil

/// Send a ping to trigger mailbox check
@external(erlang, "websocket_ffi_impl", "send_ping")
pub fn send_ping(handler_pid: process.Pid) -> Nil

/// Check for subscription data messages in the process mailbox
/// Returns the subscription_id and data if a message is available
@external(erlang, "websocket_ffi_impl", "receive_subscription_data")
pub fn receive_subscription_data() -> Result(#(String, String), Nil)

/// Add a selector branch that matches {subscription_data, Id, Data} tuples
@external(erlang, "websocket_ffi_impl", "select_subscription_data")
pub fn select_subscription_data(
  selector: process.Selector(SubscriptionMessage),
) -> process.Selector(SubscriptionMessage)
