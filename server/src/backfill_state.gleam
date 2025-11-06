/// Global state management for backfill operations.
///
/// This module provides a singleton OTP actor that tracks whether a backfill
/// operation is currently running. The state persists across WebSocket
/// reconnections and page refreshes, allowing the UI to show accurate backfill
/// status even if the user refreshes the page during a long-running backfill.
///
/// ## Architecture
///
/// - Single global actor instance started at server boot
/// - State is shared across all client connections
/// - Backfill process updates state when starting/stopping
/// - UI components poll state to update their displays
///
/// ## Example Usage
///
/// ```gleam
/// // Start the actor (done once at server startup)
/// let assert Ok(backfill_state) = backfill_state.start()
///
/// // Start a backfill operation
/// process.send(backfill_state, backfill_state.StartBackfill)
///
/// // Query current state
/// let is_backfilling = actor.call(
///   backfill_state,
///   waiting: 100,
///   sending: backfill_state.IsBackfilling,
/// )
///
/// // Stop the backfill
/// process.send(backfill_state, backfill_state.StopBackfill)
/// ```
import gleam/erlang/process
import gleam/otp/actor

/// Internal state of the backfill actor
pub type State {
  State(backfilling: Bool)
}

/// Messages that can be sent to the backfill state actor
pub type Message {
  /// Query whether a backfill is currently running.
  /// The actor will send the current state to the provided Subject.
  IsBackfilling(reply_with: process.Subject(Bool))
  /// Set backfilling state to True (sent when backfill begins)
  StartBackfill
  /// Set backfilling state to False (sent when backfill completes)
  StopBackfill
}

/// Start the backfill state actor.
///
/// This should be called once during server initialization.
/// Returns a Subject that can be used to send messages to the actor.
pub fn start() -> Result(process.Subject(Message), actor.StartError) {
  let result =
    actor.new(State(backfilling: False))
    |> actor.on_message(handle_message)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Handle incoming messages to update or query the backfill state
fn handle_message(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    IsBackfilling(client) -> {
      process.send(client, state.backfilling)
      actor.continue(state)
    }

    StartBackfill -> {
      actor.continue(State(backfilling: True))
    }

    StopBackfill -> {
      actor.continue(State(backfilling: False))
    }
  }
}
