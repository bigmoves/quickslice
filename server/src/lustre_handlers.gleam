/// WebSocket handlers and setup for Lustre server components.
///
/// This module contains all the WebSocket lifecycle handlers and routing
/// for Lustre server components, including serving the client runtime and
/// managing component WebSocket connections.
import backfill_state
import components/backfill_button
import config
import gleam/bytes_tree
import gleam/erlang/application
import gleam/erlang/process
import gleam/http/request
import gleam/http/response
import gleam/json
import gleam/option
import gleam/otp/actor
import lustre
import lustre/server_component
import mist
import sqlight

// LUSTRE RUNTIME

/// Serve the Lustre client runtime JavaScript
pub fn serve_lustre_runtime() -> response.Response(mist.ResponseData) {
  let assert Ok(lustre_priv) = application.priv_directory("lustre")
  let file_path = lustre_priv <> "/static/lustre-server-component.mjs"

  case mist.send_file(file_path, offset: 0, limit: option.None) {
    Ok(file) ->
      response.new(200)
      |> response.prepend_header("content-type", "application/javascript")
      |> response.set_body(file)

    Error(_) ->
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}

// BACKFILL BUTTON COMPONENT

/// WebSocket handler for backfill button component
pub fn serve_backfill_button(
  req: request.Request(mist.Connection),
  db: sqlight.Connection,
  backfill_state_subject: process.Subject(backfill_state.Message),
  config_subject: process.Subject(config.Message),
) -> response.Response(mist.ResponseData) {
  mist.websocket(
    request: req,
    on_init: init_backfill_button_socket(
      db,
      backfill_state_subject,
      config_subject,
      _,
    ),
    handler: loop_backfill_button_socket,
    on_close: close_backfill_button_socket,
  )
}

type BackfillButtonSocket {
  BackfillButtonSocket(
    component: lustre.Runtime(backfill_button.Msg),
    self: process.Subject(server_component.ClientMessage(backfill_button.Msg)),
  )
}

type BackfillButtonSocketMessage =
  server_component.ClientMessage(backfill_button.Msg)

type BackfillButtonSocketInit =
  #(
    BackfillButtonSocket,
    option.Option(process.Selector(BackfillButtonSocketMessage)),
  )

fn init_backfill_button_socket(
  db: sqlight.Connection,
  backfill_state_subject: process.Subject(backfill_state.Message),
  config_subject: process.Subject(config.Message),
  _connection: mist.WebsocketConnection,
) -> BackfillButtonSocketInit {
  // TODO: Get is_admin from session
  let is_admin = True

  // Query current backfill state
  let backfilling =
    actor.call(
      backfill_state_subject,
      waiting: 100,
      sending: backfill_state.IsBackfilling,
    )

  let component =
    backfill_button.component(db, backfill_state_subject, config_subject)
  let assert Ok(runtime) =
    lustre.start_server_component(component, #(is_admin, backfilling))

  let self = process.new_subject()
  let selector = process.new_selector() |> process.select(self)

  server_component.register_subject(self)
  |> lustre.send(to: runtime)

  #(BackfillButtonSocket(component: runtime, self: self), option.Some(selector))
}

fn loop_backfill_button_socket(
  state: BackfillButtonSocket,
  message: mist.WebsocketMessage(BackfillButtonSocketMessage),
  connection: mist.WebsocketConnection,
) -> mist.Next(BackfillButtonSocket, BackfillButtonSocketMessage) {
  case message {
    mist.Text(json_string) -> {
      case json.parse(json_string, server_component.runtime_message_decoder()) {
        Ok(runtime_message) -> lustre.send(state.component, runtime_message)
        Error(_) -> Nil
      }

      mist.continue(state)
    }

    mist.Binary(_) -> mist.continue(state)

    mist.Custom(client_message) -> {
      let json_obj = server_component.client_message_to_json(client_message)
      let assert Ok(_) =
        mist.send_text_frame(connection, json.to_string(json_obj))

      mist.continue(state)
    }

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}

fn close_backfill_button_socket(state: BackfillButtonSocket) -> Nil {
  lustre.shutdown()
  |> lustre.send(to: state.component)
}
