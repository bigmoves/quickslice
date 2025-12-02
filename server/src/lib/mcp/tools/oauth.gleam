import gleam/json

/// Get OAuth configuration info
pub fn get_oauth_info(
  base_url: String,
  supported_scopes: List(String),
) -> Result(json.Json, String) {
  Ok(
    json.object([
      #(
        "flows",
        json.array(["authorization_code", "refresh_token"], json.string),
      ),
      #("scopes", json.array(supported_scopes, json.string)),
      #(
        "endpoints",
        json.object([
          #("authorize", json.string(base_url <> "/oauth/authorize")),
          #("token", json.string(base_url <> "/oauth/token")),
          #("register", json.string(base_url <> "/oauth/register")),
          #("par", json.string(base_url <> "/oauth/par")),
          #("jwks", json.string(base_url <> "/.well-known/jwks.json")),
          #(
            "metadata",
            json.string(base_url <> "/.well-known/oauth-authorization-server"),
          ),
        ]),
      ),
      #("clientTypes", json.array(["public", "confidential"], json.string)),
      #(
        "authMethods",
        json.array(
          ["none", "client_secret_post", "client_secret_basic"],
          json.string,
        ),
      ),
      #("pkceRequired", json.bool(True)),
      #("dpopSupported", json.bool(True)),
    ]),
  )
}
