import database/executor
import gleam/string
import gleeunit/should
import handlers/oauth/dpop_nonce
import test_helpers
import wisp

pub fn handle_generates_nonce_test() {
  // Create in-memory database with schema
  let assert Ok(exec) = test_helpers.create_test_db()

  // Create the oauth_dpop_nonce table
  let assert Ok(_) =
    executor.exec(
      exec,
      "CREATE TABLE oauth_dpop_nonce (
        nonce TEXT PRIMARY KEY,
        expires_at INTEGER NOT NULL
      )",
      [],
    )

  let response = dpop_nonce.handle(exec)

  // Should return 200
  response.status |> should.equal(200)

  // Body should contain nonce
  case response.body {
    wisp.Text(body) -> {
      body |> string.contains("nonce") |> should.be_true
      body |> string.contains("expires_in") |> should.be_true
    }
    _ -> should.fail()
  }
}
