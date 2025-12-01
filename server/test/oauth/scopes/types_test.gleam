import gleeunit/should
import lib/oauth/scopes/types

pub fn static_scope_atproto_test() {
  types.Atproto
  |> types.static_scope_to_string
  |> should.equal("atproto")
}

pub fn static_scope_transition_generic_test() {
  types.TransitionGeneric
  |> types.static_scope_to_string
  |> should.equal("transition:generic")
}

pub fn static_scope_transition_email_test() {
  types.TransitionEmail
  |> types.static_scope_to_string
  |> should.equal("transition:email")
}

pub fn static_scope_transition_chat_bsky_test() {
  types.TransitionChatBsky
  |> types.static_scope_to_string
  |> should.equal("transition:chat.bsky")
}

pub fn action_to_string_create_test() {
  types.Create
  |> types.action_to_string
  |> should.equal("create")
}

pub fn action_to_string_read_test() {
  types.Read
  |> types.action_to_string
  |> should.equal("read")
}

pub fn action_to_string_manage_test() {
  types.Manage
  |> types.action_to_string
  |> should.equal("manage")
}
