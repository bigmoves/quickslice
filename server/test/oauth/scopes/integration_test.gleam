/// Integration tests for OAuth scope validation across all endpoints
import database/schema/tables
import gleam/http
import gleam/json
import gleam/string
import gleeunit/should
import handlers/oauth/register
import sqlight
import wisp
import wisp/simulate

/// Test that all ATProto scope types are accepted
pub fn all_atproto_scopes_accepted_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let all_scopes =
    "atproto transition:generic transition:email transition:chat.bsky "
    <> "account:email account:repo?action=manage account:status "
    <> "identity:handle identity:* "
    <> "repo:* repo:app.bsky.feed.post?action=create "
    <> "blob:*/* blob:image/* blob:image/png "
    <> "rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app "
    <> "include:app.bsky.feed include:chat.bsky?aud=did:web:bsky.chat"

  let body =
    json.object([
      #("client_name", json.string("Full Scope Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string(all_scopes)),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)

  // Verify the scope was stored in the response
  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("atproto") |> should.be_true
      response_body |> string.contains("transition:generic") |> should.be_true
      response_body |> string.contains("account:email") |> should.be_true
      response_body |> string.contains("identity:handle") |> should.be_true
      response_body |> string.contains("repo:*") |> should.be_true
      response_body |> string.contains("blob:*/*") |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test that complex multi-action repo scope is accepted
pub fn multi_action_repo_scope_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Multi Action Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto repo:*?action=create&action=delete")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)
}

/// Test that minimal atproto scope is accepted
pub fn minimal_atproto_scope_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Minimal Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(201)
}

/// Test that malformed scope prefix is rejected
pub fn malformed_scope_prefix_rejected_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Invalid Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto invalid:scope")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test that blob with invalid MIME type is rejected
pub fn invalid_blob_mime_type_rejected_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Invalid Blob Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto blob:invalid")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}

/// Test that RPC scope without audience is rejected
pub fn rpc_without_audience_rejected_test() {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_oauth_client_table(conn)

  let body =
    json.object([
      #("client_name", json.string("Invalid RPC Test")),
      #(
        "redirect_uris",
        json.array([json.string("https://example.com/callback")], fn(x) { x }),
      ),
      #("scope", json.string("atproto rpc:app.bsky.feed.getFeed")),
    ])
    |> json.to_string

  let req =
    simulate.request(http.Post, "/oauth/register")
    |> simulate.string_body(body)
    |> simulate.header("content-type", "application/json")

  let response = register.handle(req, conn)

  response.status |> should.equal(400)

  case response.body {
    wisp.Text(response_body) -> {
      response_body |> string.contains("invalid_scope") |> should.be_true
    }
    _ -> should.fail()
  }
}
