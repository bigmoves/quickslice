import gleam/option.{None, Some}
import gleeunit/should
import lib/oauth/scopes/parser
import lib/oauth/scopes/types

pub fn parse_atproto_test() {
  parser.parse_scope("atproto")
  |> should.be_ok
  |> should.equal(types.Static(types.Atproto))
}

pub fn parse_transition_generic_test() {
  parser.parse_scope("transition:generic")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionGeneric))
}

pub fn parse_transition_email_test() {
  parser.parse_scope("transition:email")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionEmail))
}

pub fn parse_transition_chat_bsky_test() {
  parser.parse_scope("transition:chat.bsky")
  |> should.be_ok
  |> should.equal(types.Static(types.TransitionChatBsky))
}

pub fn parse_account_email_test() {
  parser.parse_scope("account:email")
  |> should.be_ok
  |> should.equal(
    types.Account(types.AccountScope(
      attribute: types.EmailAttr,
      action: types.Read,
    )),
  )
}

pub fn parse_account_email_with_action_test() {
  parser.parse_scope("account:email?action=manage")
  |> should.be_ok
  |> should.equal(
    types.Account(types.AccountScope(
      attribute: types.EmailAttr,
      action: types.Manage,
    )),
  )
}

pub fn parse_account_repo_test() {
  parser.parse_scope("account:repo")
  |> should.be_ok
  |> should.equal(
    types.Account(types.AccountScope(
      attribute: types.RepoAttr,
      action: types.Read,
    )),
  )
}

pub fn parse_account_status_test() {
  parser.parse_scope("account:status?action=manage")
  |> should.be_ok
  |> should.equal(
    types.Account(types.AccountScope(
      attribute: types.StatusAttr,
      action: types.Manage,
    )),
  )
}

pub fn parse_identity_handle_test() {
  parser.parse_scope("identity:handle")
  |> should.be_ok
  |> should.equal(types.Identity(types.IdentityScope(attribute: types.Handle)))
}

pub fn parse_identity_all_test() {
  parser.parse_scope("identity:*")
  |> should.be_ok
  |> should.equal(types.Identity(types.IdentityScope(attribute: types.All)))
}

pub fn parse_repo_wildcard_test() {
  parser.parse_scope("repo:*")
  |> should.be_ok
  |> should.equal(
    types.Repo(
      types.RepoScope(collection: "*", actions: [
        types.Create,
        types.Update,
        types.Delete,
      ]),
    ),
  )
}

pub fn parse_repo_specific_collection_test() {
  parser.parse_scope("repo:app.bsky.feed.post")
  |> should.be_ok
  |> should.equal(
    types.Repo(
      types.RepoScope(collection: "app.bsky.feed.post", actions: [
        types.Create,
        types.Update,
        types.Delete,
      ]),
    ),
  )
}

pub fn parse_repo_with_single_action_test() {
  parser.parse_scope("repo:app.bsky.feed.post?action=create")
  |> should.be_ok
  |> should.equal(
    types.Repo(
      types.RepoScope(collection: "app.bsky.feed.post", actions: [types.Create]),
    ),
  )
}

pub fn parse_repo_with_multiple_actions_test() {
  parser.parse_scope("repo:*?action=create&action=delete")
  |> should.be_ok
  |> should.equal(
    types.Repo(
      types.RepoScope(collection: "*", actions: [types.Create, types.Delete]),
    ),
  )
}

pub fn parse_blob_wildcard_test() {
  parser.parse_scope("blob:*/*")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "*/*")))
}

pub fn parse_blob_image_wildcard_test() {
  parser.parse_scope("blob:image/*")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "image/*")))
}

pub fn parse_blob_specific_type_test() {
  parser.parse_scope("blob:image/png")
  |> should.be_ok
  |> should.equal(types.Blob(types.BlobScope(mime_type: "image/png")))
}

pub fn parse_blob_invalid_mime_test() {
  parser.parse_scope("blob:invalid")
  |> should.be_error
}

pub fn parse_rpc_specific_method_test() {
  parser.parse_scope("rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app")
  |> should.be_ok
  |> should.equal(
    types.Rpc(types.RpcScope(
      methods: ["app.bsky.feed.getFeed"],
      audience: "did:web:bsky.app",
    )),
  )
}

pub fn parse_rpc_wildcard_method_specific_aud_test() {
  parser.parse_scope("rpc:*?aud=did:web:api.bsky.app")
  |> should.be_ok
  |> should.equal(
    types.Rpc(types.RpcScope(methods: ["*"], audience: "did:web:api.bsky.app")),
  )
}

pub fn parse_rpc_missing_aud_test() {
  parser.parse_scope("rpc:app.bsky.feed.getFeed")
  |> should.be_error
}

pub fn parse_rpc_wildcard_both_test() {
  // rpc:* with aud=* is invalid
  parser.parse_scope("rpc:*?aud=*")
  |> should.be_error
}

pub fn parse_include_simple_test() {
  parser.parse_scope("include:app.bsky.feed")
  |> should.be_ok
  |> should.equal(
    types.Include(types.IncludeScope(nsid: "app.bsky.feed", audience: None)),
  )
}

pub fn parse_include_with_aud_test() {
  parser.parse_scope("include:chat.bsky.moderation?aud=did:web:bsky.chat")
  |> should.be_ok
  |> should.equal(
    types.Include(types.IncludeScope(
      nsid: "chat.bsky.moderation",
      audience: Some("did:web:bsky.chat"),
    )),
  )
}

pub fn parse_scopes_multiple_test() {
  parser.parse_scopes("atproto repo:* account:email")
  |> should.be_ok
  |> should.equal([
    types.Static(types.Atproto),
    types.Repo(
      types.RepoScope(collection: "*", actions: [
        types.Create,
        types.Update,
        types.Delete,
      ]),
    ),
    types.Account(types.AccountScope(
      attribute: types.EmailAttr,
      action: types.Read,
    )),
  ])
}

pub fn parse_scopes_empty_test() {
  parser.parse_scopes("")
  |> should.be_ok
  |> should.equal([])
}

pub fn parse_scopes_single_invalid_fails_test() {
  parser.parse_scopes("atproto invalid::: repo:*")
  |> should.be_error
}
