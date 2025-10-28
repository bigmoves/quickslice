import gleam/option
import gleeunit/should
import xrpc_router

// Test XRPC path parsing

pub fn parse_valid_xrpc_path_test() {
  let segments = ["xrpc", "xyz.statusphere.status.createRecord"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(
    option.Some(xrpc_router.XrpcRoute(
      nsid: "xyz.statusphere.status",
      method: "createRecord",
    )),
  )
}

pub fn parse_xrpc_path_with_longer_nsid_test() {
  let segments = ["xrpc", "com.example.app.post.getRecord"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(
    option.Some(xrpc_router.XrpcRoute(
      nsid: "com.example.app.post",
      method: "getRecord",
    )),
  )
}

pub fn parse_xrpc_path_update_record_test() {
  let segments = ["xrpc", "xyz.statusphere.status.updateRecord"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(
    option.Some(xrpc_router.XrpcRoute(
      nsid: "xyz.statusphere.status",
      method: "updateRecord",
    )),
  )
}

pub fn parse_xrpc_path_delete_record_test() {
  let segments = ["xrpc", "xyz.statusphere.status.deleteRecord"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(
    option.Some(xrpc_router.XrpcRoute(
      nsid: "xyz.statusphere.status",
      method: "deleteRecord",
    )),
  )
}

pub fn parse_invalid_xrpc_path_too_short_test() {
  let segments = ["xrpc", "invalid"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(option.None)
}

pub fn parse_invalid_xrpc_path_wrong_prefix_test() {
  let segments = ["api", "xyz.statusphere.status.createRecord"]
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(option.None)
}

pub fn parse_invalid_xrpc_path_empty_test() {
  let segments = []
  let result = xrpc_router.parse_xrpc_path(segments)

  result
  |> should.equal(option.None)
}

// Test method parsing

pub fn parse_method_create_record_test() {
  let result = xrpc_router.parse_method("createRecord")

  result
  |> should.equal(xrpc_router.CreateRecord)
}

pub fn parse_method_update_record_test() {
  let result = xrpc_router.parse_method("updateRecord")

  result
  |> should.equal(xrpc_router.UpdateRecord)
}

pub fn parse_method_delete_record_test() {
  let result = xrpc_router.parse_method("deleteRecord")

  result
  |> should.equal(xrpc_router.DeleteRecord)
}

pub fn parse_method_get_record_test() {
  let result = xrpc_router.parse_method("getRecord")

  result
  |> should.equal(xrpc_router.GetRecord)
}

pub fn parse_method_unknown_test() {
  let result = xrpc_router.parse_method("unknownMethod")

  result
  |> should.equal(xrpc_router.UnknownMethod)
}

pub fn parse_method_empty_test() {
  let result = xrpc_router.parse_method("")

  result
  |> should.equal(xrpc_router.UnknownMethod)
}
