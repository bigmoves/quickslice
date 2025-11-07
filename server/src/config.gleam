import database
import gleam/erlang/process
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import logging
import sqlight

// ===== Config Cache Actor =====

pub type ConfigCache {
  ConfigCache(domain_authority: Option(String))
}

pub type Message {
  GetDomainAuthority(reply_with: process.Subject(Option(String)))
  SetDomainAuthority(value: String, reply_with: process.Subject(Nil))
  Reload(db: sqlight.Connection, reply_with: process.Subject(Nil))
}

fn handle_message(
  state: ConfigCache,
  message: Message,
) -> actor.Next(ConfigCache, Message) {
  case message {
    GetDomainAuthority(client) -> {
      process.send(client, state.domain_authority)
      actor.continue(state)
    }
    SetDomainAuthority(value, client) -> {
      process.send(client, Nil)
      actor.continue(ConfigCache(domain_authority: Some(value)))
    }
    Reload(db, client) -> {
      let new_state = case database.get_config(db, "domain_authority") {
        Ok(value) -> ConfigCache(domain_authority: Some(value))
        Error(_) -> ConfigCache(domain_authority: None)
      }
      process.send(client, Nil)
      actor.continue(new_state)
    }
  }
}

/// Start the config cache actor
pub fn start(
  db: sqlight.Connection,
) -> Result(process.Subject(Message), actor.StartError) {
  // Load initial domain authority from database
  let initial_state = case database.get_config(db, "domain_authority") {
    Ok(value) -> ConfigCache(domain_authority: Some(value))
    Error(_) -> {
      logging.log(
        logging.Info,
        "[config] No domain_authority found in database",
      )
      ConfigCache(domain_authority: None)
    }
  }

  let result =
    actor.new(initial_state)
    |> actor.on_message(handle_message)
    |> actor.start

  case result {
    Ok(started) -> Ok(started.data)
    Error(err) -> Error(err)
  }
}

/// Get the current domain authority from cache
pub fn get_domain_authority(config: process.Subject(Message)) -> Option(String) {
  actor.call(config, waiting: 100, sending: GetDomainAuthority)
}

/// Set the domain authority (updates both database and cache)
pub fn set_domain_authority(
  config: process.Subject(Message),
  db: sqlight.Connection,
  value: String,
) -> Result(Nil, sqlight.Error) {
  // Update database first
  case database.set_config(db, "domain_authority", value) {
    Ok(_) -> {
      // Update cache
      actor.call(config, waiting: 100, sending: SetDomainAuthority(value, _))
      logging.log(logging.Info, "[config] Updated domain_authority: " <> value)
      Ok(Nil)
    }
    Error(err) -> Error(err)
  }
}

/// Reload config from database (useful after external updates)
pub fn reload(config: process.Subject(Message), db: sqlight.Connection) -> Nil {
  actor.call(config, waiting: 100, sending: Reload(db, _))
}
