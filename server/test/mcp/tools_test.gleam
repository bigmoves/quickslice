import gleam/list
import gleeunit/should
import lib/mcp/tools

pub fn list_tools_returns_all_tools_test() {
  let tool_list = tools.list_tools()

  // Should have 7 tools
  list.length(tool_list) |> should.equal(7)
}

pub fn list_tools_has_list_lexicons_test() {
  let tool_list = tools.list_tools()

  let has_list_lexicons =
    list.any(tool_list, fn(t) { t.name == "list_lexicons" })
  has_list_lexicons |> should.be_true
}

pub fn get_tool_returns_tool_test() {
  let result = tools.get_tool("list_lexicons")

  result |> should.be_ok
  let assert Ok(tool) = result
  tool.name |> should.equal("list_lexicons")
}

pub fn get_tool_returns_error_for_unknown_test() {
  let result = tools.get_tool("unknown_tool")

  result |> should.be_error
}
