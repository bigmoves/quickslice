import gleam/erlang/process
import gleam/otp/actor
import gleam/string
import jetstream_activity
import logging
import sqlight

/// Message types for the cleanup actor
pub type Message {
  Cleanup
  Shutdown
}

type State {
  State(db: sqlight.Connection, self: process.Subject(Message))
}

/// Start the cleanup scheduler
/// Returns a Subject that can be used to send messages to the scheduler
pub fn start(
  db: sqlight.Connection,
) -> Result(process.Subject(Message), actor.StartError) {
  let initial_state = State(db: db, self: process.new_subject())

  let result =
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start

  // Schedule first cleanup after 1 hour
  case result {
    Ok(started) -> {
      let _ = process.send_after(started.data, 3_600_000, Cleanup)
      Ok(started.data)
    }
    Error(reason) -> Error(reason)
  }
}

fn handle_message(
  state: State,
  message: Message,
) -> actor.Next(State, Message) {
  case message {
    Cleanup -> {
      // Clean up activity entries older than 7 days (168 hours)
      case jetstream_activity.cleanup_old_activity(state.db, 168) {
        Ok(_) -> Nil
        Error(err) -> {
          logging.log(
            logging.Error,
            "[cleanup] Failed to cleanup old activity: "
              <> string.inspect(err),
          )
        }
      }

      // Schedule next cleanup in 1 hour (3600000 milliseconds)
      let _ = process.send_after(state.self, 3_600_000, Cleanup)

      actor.continue(state)
    }
    Shutdown -> {
      logging.log(logging.Info, "[cleanup] Shutting down cleanup scheduler")
      actor.stop()
    }
  }
}
