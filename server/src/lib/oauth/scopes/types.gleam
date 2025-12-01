/// ATProto OAuth scope types
/// Reference: https://github.com/bluesky-social/atproto/tree/main/packages/oauth/oauth-scopes
import gleam/option.{type Option}

/// Static ATProto scopes
pub type StaticScope {
  Atproto
  TransitionEmail
  TransitionGeneric
  TransitionChatBsky
}

/// Actions for repo and account scopes
pub type Action {
  Create
  Update
  Delete
  Read
  Manage
}

/// Account attributes
pub type AccountAttribute {
  EmailAttr
  RepoAttr
  StatusAttr
}

/// Identity attributes
pub type IdentityAttribute {
  Handle
  All
}

/// Account scope: account:email?action=read
pub type AccountScope {
  AccountScope(attribute: AccountAttribute, action: Action)
}

/// Identity scope: identity:handle or identity:*
pub type IdentityScope {
  IdentityScope(attribute: IdentityAttribute)
}

/// Repo scope: repo:app.bsky.feed.post?action=create
pub type RepoScope {
  RepoScope(collection: String, actions: List(Action))
}

/// Blob scope: blob:image/* or blob:*/*
pub type BlobScope {
  BlobScope(mime_type: String)
}

/// RPC scope: rpc:app.bsky.feed.getFeed?aud=did:web:bsky.app
pub type RpcScope {
  RpcScope(methods: List(String), audience: String)
}

/// Include scope: include:app.bsky.feed?aud=did:web:...
pub type IncludeScope {
  IncludeScope(nsid: String, audience: Option(String))
}

/// Union of all scope types
pub type Scope {
  Static(StaticScope)
  Account(AccountScope)
  Identity(IdentityScope)
  Repo(RepoScope)
  Blob(BlobScope)
  Rpc(RpcScope)
  Include(IncludeScope)
}

/// Convert static scope to string
pub fn static_scope_to_string(scope: StaticScope) -> String {
  case scope {
    Atproto -> "atproto"
    TransitionEmail -> "transition:email"
    TransitionGeneric -> "transition:generic"
    TransitionChatBsky -> "transition:chat.bsky"
  }
}

/// Convert action to string
pub fn action_to_string(action: Action) -> String {
  case action {
    Create -> "create"
    Update -> "update"
    Delete -> "delete"
    Read -> "read"
    Manage -> "manage"
  }
}
