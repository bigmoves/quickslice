import gleam/bytes_tree.{type BytesTree}
import gleam/hackney
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/httpc

const user_agent = "quickslice"

/// Send an HTTP request with the quickslice user-agent header.
/// Use this instead of httpc.send directly.
pub fn send(req: Request(String)) -> Result(Response(String), httpc.HttpError) {
  req
  |> request.set_header("user-agent", user_agent)
  |> httpc.send
}

/// Send an HTTP request with binary body using httpc.
/// Use this instead of httpc.send_bits directly.
pub fn send_bits(
  req: Request(BitArray),
) -> Result(Response(BitArray), httpc.HttpError) {
  req
  |> request.set_header("user-agent", user_agent)
  |> httpc.send_bits
}

/// Send an HTTP request using hackney.
/// Use this instead of hackney.send directly.
pub fn hackney_send(
  req: Request(String),
) -> Result(Response(String), hackney.Error) {
  req
  |> request.set_header("user-agent", user_agent)
  |> hackney.send
}

/// Send an HTTP request with binary body using hackney.
/// Use this instead of hackney.send_bits directly.
pub fn hackney_send_bits(
  req: Request(BytesTree),
) -> Result(Response(BitArray), hackney.Error) {
  req
  |> request.set_header("user-agent", user_agent)
  |> hackney.send_bits
}
