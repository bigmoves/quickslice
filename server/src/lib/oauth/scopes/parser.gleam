/// ATProto OAuth scope parser
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/scopes/parse_error.{
  type ParseError, InvalidAction, InvalidAttribute, InvalidMimeType,
  InvalidRpcScope, InvalidScopeFormat,
}
import lib/oauth/scopes/types.{
  type AccountAttribute, type Action, type Scope, type StaticScope, Account,
  AccountScope, All, Atproto, Blob, BlobScope, Create, Delete, EmailAttr, Handle,
  Identity, IdentityScope, Include, IncludeScope, Manage, Read, Repo, RepoAttr,
  RepoScope, Rpc, RpcScope, Static, StatusAttr, TransitionChatBsky,
  TransitionEmail, TransitionGeneric, Update,
}

/// Parse a single scope token
pub fn parse_scope(token: String) -> Result(Scope, ParseError) {
  case parse_static(token) {
    Ok(scope) -> Ok(Static(scope))
    Error(Nil) -> parse_parameterized(token)
  }
}

/// Parse a space-separated scope string into list of Scope
pub fn parse_scopes(scope_string: String) -> Result(List(Scope), ParseError) {
  case string.is_empty(string.trim(scope_string)) {
    True -> Ok([])
    False -> {
      scope_string
      |> string.split(" ")
      |> list.filter(fn(s) { !string.is_empty(s) })
      |> list.try_map(parse_scope)
    }
  }
}

/// Try to parse as static scope
fn parse_static(token: String) -> Result(StaticScope, Nil) {
  case token {
    "atproto" -> Ok(Atproto)
    "transition:email" -> Ok(TransitionEmail)
    "transition:generic" -> Ok(TransitionGeneric)
    "transition:chat.bsky" -> Ok(TransitionChatBsky)
    _ -> Error(Nil)
  }
}

/// Parse parameterized scope (prefix:value?params)
fn parse_parameterized(token: String) -> Result(Scope, ParseError) {
  case string.split_once(token, ":") {
    Ok(#("account", rest)) -> parse_account_scope(token, rest)
    Ok(#("identity", rest)) -> parse_identity_scope(token, rest)
    Ok(#("repo", rest)) -> parse_repo_scope(token, rest)
    Ok(#("blob", rest)) -> parse_blob_scope(token, rest)
    Ok(#("rpc", rest)) -> parse_rpc_scope(token, rest)
    Ok(#("include", rest)) -> parse_include_scope(token, rest)
    Ok(#(prefix, _)) ->
      Error(InvalidScopeFormat(token, "unknown prefix: " <> prefix))
    Error(Nil) -> Error(InvalidScopeFormat(token, "missing scope prefix"))
  }
}

/// Parse account scope: account:email?action=manage
fn parse_account_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(attr_str, query) = split_query(rest)

  use attribute <- result.try(parse_account_attribute(original, attr_str))

  let action = case get_query_param(query, "action") {
    Some("read") -> Ok(Read)
    Some("manage") -> Ok(Manage)
    Some(other) -> Error(InvalidAction(other))
    None -> Ok(Read)
  }

  use act <- result.try(action)

  Ok(Account(AccountScope(attribute: attribute, action: act)))
}

/// Parse account attribute
fn parse_account_attribute(
  _original: String,
  attr: String,
) -> Result(AccountAttribute, ParseError) {
  case attr {
    "email" -> Ok(EmailAttr)
    "repo" -> Ok(RepoAttr)
    "status" -> Ok(StatusAttr)
    _ -> Error(InvalidAttribute(attr))
  }
}

/// Parse identity scope: identity:handle or identity:*
fn parse_identity_scope(
  _original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(attr_str, _query) = split_query(rest)

  case attr_str {
    "handle" -> Ok(Identity(IdentityScope(attribute: Handle)))
    "*" -> Ok(Identity(IdentityScope(attribute: All)))
    _ -> Error(InvalidAttribute(attr_str))
  }
}

/// Parse repo scope: repo:app.bsky.feed.post?action=create
fn parse_repo_scope(original: String, rest: String) -> Result(Scope, ParseError) {
  let #(collection, query) = split_query(rest)

  case string.is_empty(collection) {
    True -> Error(InvalidScopeFormat(original, "missing collection"))
    False -> {
      let actions = case query {
        None -> [Create, Update, Delete]
        Some(q) -> parse_repo_actions(q)
      }

      case list.is_empty(actions) {
        True -> Error(InvalidScopeFormat(original, "no valid actions"))
        False -> Ok(Repo(RepoScope(collection: collection, actions: actions)))
      }
    }
  }
}

/// Parse actions from query string for repo scope
fn parse_repo_actions(query: String) -> List(Action) {
  case uri.parse_query(query) {
    Ok(params) ->
      params
      |> list.filter_map(fn(p) {
        case p.0 {
          "action" ->
            case p.1 {
              "create" -> Ok(Create)
              "update" -> Ok(Update)
              "delete" -> Ok(Delete)
              _ -> Error(Nil)
            }
          _ -> Error(Nil)
        }
      })
    Error(_) -> []
  }
}

/// Parse blob scope: blob:image/* or blob:*/*
fn parse_blob_scope(
  _original: String,
  mime_type: String,
) -> Result(Scope, ParseError) {
  case validate_mime_type(mime_type) {
    True -> Ok(Blob(BlobScope(mime_type: mime_type)))
    False -> Error(InvalidMimeType(mime_type))
  }
}

/// Validate MIME type format
fn validate_mime_type(mime: String) -> Bool {
  case string.split(mime, "/") {
    [type_part, subtype] ->
      !string.is_empty(type_part) && !string.is_empty(subtype)
    _ -> False
  }
}

/// Parse rpc scope: rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app
fn parse_rpc_scope(original: String, rest: String) -> Result(Scope, ParseError) {
  let #(method, query) = split_query(rest)

  case string.is_empty(method) {
    True -> Error(InvalidScopeFormat(original, "missing method"))
    False -> {
      case get_query_param(query, "aud") {
        None -> Error(InvalidRpcScope("aud parameter is required"))
        Some(aud) -> {
          // Validate: can't have wildcard method with wildcard audience
          case method == "*" && aud == "*" {
            True ->
              Error(InvalidRpcScope(
                "wildcard method requires specific audience",
              ))
            False -> Ok(Rpc(RpcScope(methods: [method], audience: aud)))
          }
        }
      }
    }
  }
}

/// Parse include scope: include:app.bsky.feed?aud=did:web:...
fn parse_include_scope(
  original: String,
  rest: String,
) -> Result(Scope, ParseError) {
  let #(nsid, query) = split_query(rest)

  case string.is_empty(nsid) {
    True -> Error(InvalidScopeFormat(original, "missing NSID"))
    False -> {
      let audience = get_query_param(query, "aud")
      Ok(Include(IncludeScope(nsid: nsid, audience: audience)))
    }
  }
}

/// Split value from query string
fn split_query(s: String) -> #(String, Option(String)) {
  case string.split_once(s, "?") {
    Ok(#(value, query)) -> #(value, Some(query))
    Error(Nil) -> #(s, None)
  }
}

/// Get a query parameter value
fn get_query_param(query: Option(String), key: String) -> Option(String) {
  case query {
    None -> None
    Some(q) -> {
      case uri.parse_query(q) {
        Ok(params) ->
          params
          |> list.find(fn(p) { p.0 == key })
          |> result.map(fn(p) { p.1 })
          |> option.from_result
        Error(_) -> None
      }
    }
  }
}
