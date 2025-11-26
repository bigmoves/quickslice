import gleam/string
import gleeunit/should
import handlers/oauth/dpop_nonce
import sqlight
import wisp

pub fn handle_generates_nonce_test() {
  // Create in-memory database with schema
  let assert Ok(conn) = sqlight.open(":memory:")

  // Create the oauth_dpop_nonce table
  let assert Ok(_) =
    sqlight.exec(
      "CREATE TABLE oauth_dpop_nonce (
        nonce TEXT PRIMARY KEY,
        expires_at INTEGER NOT NULL
      )",
      conn,
    )

  let response = dpop_nonce.handle(conn)

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
