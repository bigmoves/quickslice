import gleeunit/should
import lib/mcp/protocol

pub fn decode_initialize_request_test() {
  let json_str =
    "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("initialize")
  req.id |> should.equal(protocol.IntId(1))
}

pub fn decode_tools_list_request_test() {
  let json_str = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":2}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("tools/list")
}

pub fn decode_tools_call_request_test() {
  let json_str =
    "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"list_lexicons\",\"arguments\":{}},\"id\":3}"

  let result = protocol.decode_request(json_str)

  result |> should.be_ok
  let assert Ok(req) = result
  req.method |> should.equal("tools/call")
}
