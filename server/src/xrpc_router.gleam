import database
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import sqlight

/// Type representing a parsed XRPC request
pub type XrpcRoute {
  XrpcRoute(nsid: String, method: String)
}

/// XRPC method types
pub type XrpcMethod {
  CreateRecord
  UpdateRecord
  DeleteRecord
  GetRecord
  UnknownMethod
}

/// Parse an XRPC path into NSID and method
/// Expected format: ["xrpc", "nsid.method"]
/// Example: ["xrpc", "xyz.statusphere.status.createRecord"]
pub fn parse_xrpc_path(segments: List(String)) -> Option(XrpcRoute) {
  case segments {
    ["xrpc", combined] -> {
      // Split the combined string by the last dot to separate NSID from method
      case split_nsid_and_method(combined) {
        Ok(#(nsid, method)) -> Some(XrpcRoute(nsid: nsid, method: method))
        Error(_) -> None
      }
    }
    _ -> None
  }
}

/// Split a combined NSID.method string into separate parts
/// Example: "xyz.statusphere.status.createRecord" -> ("xyz.statusphere.status", "createRecord")
fn split_nsid_and_method(combined: String) -> Result(#(String, String), Nil) {
  // Split by dots
  let parts = string.split(combined, ".")

  // We need at least 4 parts for a valid NSID (e.g., xyz.statusphere.status.createRecord)
  // NSID typically has 3 parts (authority.name.record), method is the 4th+
  case list_reverse_split_at(parts, 1) {
    Ok(#(method_parts, nsid_parts)) -> {
      let nsid = string.join(nsid_parts, ".")
      let method = string.join(method_parts, ".")
      Ok(#(nsid, method))
    }
    Error(_) -> Error(Nil)
  }
}

/// Helper to split a list from the right side
fn list_reverse_split_at(
  lst: List(a),
  n: Int,
) -> Result(#(List(a), List(a)), Nil) {
  let reversed = list.reverse(lst)
  case list.split(reversed, n) {
    #(suffix_rev, prefix_rev) -> {
      let prefix = list.reverse(prefix_rev)
      let suffix = list.reverse(suffix_rev)
      case list.is_empty(prefix) || list.is_empty(suffix) {
        True -> Error(Nil)
        False -> Ok(#(suffix, prefix))
      }
    }
  }
}

/// Parse method name into XrpcMethod type
pub fn parse_method(method: String) -> XrpcMethod {
  case method {
    "createRecord" -> CreateRecord
    "updateRecord" -> UpdateRecord
    "deleteRecord" -> DeleteRecord
    "getRecord" -> GetRecord
    _ -> UnknownMethod
  }
}

/// Check if a lexicon exists for the given NSID
pub fn validate_nsid(db: sqlight.Connection, nsid: String) -> Bool {
  case database.has_lexicon_for_collection(db, nsid) {
    Ok(True) -> True
    _ -> False
  }
}
