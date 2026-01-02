import gleeunit/should
import graphql/admin/cursor

pub fn encode_cursor_test() {
  cursor.encode("Label", 42)
  |> should.equal("TGFiZWw6NDI=")
}

pub fn encode_cursor_with_large_id_test() {
  cursor.encode("Report", 12_345)
  |> should.equal("UmVwb3J0OjEyMzQ1")
}

pub fn decode_cursor_test() {
  cursor.decode("TGFiZWw6NDI=")
  |> should.equal(Ok(#("Label", 42)))
}

pub fn decode_cursor_with_large_id_test() {
  cursor.decode("UmVwb3J0OjEyMzQ1")
  |> should.equal(Ok(#("Report", 12_345)))
}

pub fn decode_invalid_cursor_test() {
  cursor.decode("not-valid-base64!!!")
  |> should.be_error()
}

pub fn decode_malformed_cursor_test() {
  // Valid base64 but wrong format (no colon)
  cursor.decode("bm9jb2xvbg==")
  |> should.be_error()
}
