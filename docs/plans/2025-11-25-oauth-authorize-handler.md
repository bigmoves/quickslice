# OAuth Authorize Handler Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `["oauth", "authorize"]` route in server by copying functionality from oauth_server into `server/src/lib/oauth/`.

**Architecture:** Copy core OAuth modules from oauth_server to server's new `lib/oauth/` directory. Adapt all modules to use server's existing repositories (`database/repositories/oauth_*.gleam`) instead of oauth_server's Storage abstraction. Wire up the authorize endpoint in server.gleam.

**Tech Stack:** Gleam, SQLite (sqlight), wisp HTTP framework, jose (JWT/DPoP), Erlang FFI

---

## Task 1: Create lib/oauth directory structure

**Files:**
- Create: `server/src/lib/oauth/.gitkeep` (placeholder)

**Step 1: Create directory structure**

```bash
mkdir -p server/src/lib/oauth/atproto
mkdir -p server/src/lib/oauth/dpop
mkdir -p server/src/lib/oauth/crypto
mkdir -p server/src/lib/oauth/types
```

**Step 2: Verify directories exist**

Run: `ls -la server/src/lib/oauth/`
Expected: Shows atproto, dpop, crypto, types subdirectories

**Step 3: Commit**

```bash
git add server/src/lib/
git commit -m "chore: create lib/oauth directory structure"
```

---

## Task 2: Extend jose_ffi.erl with missing functions

**Files:**
- Modify: `server/src/jose_ffi.erl`

**Step 1: Read current jose_ffi.erl**

Current exports: `generate_dpop_proof/5`, `sha256_hash/1`
Need to add: `compute_jwk_thumbprint/1`, `sha256_base64url/1`

**Step 2: Add new exports and functions**

Add to `-export` line:
```erlang
-export([generate_dpop_proof/5, sha256_hash/1, compute_jwk_thumbprint/1, sha256_base64url/1]).
```

Add after existing functions:
```erlang
%% Compute JWK thumbprint (SHA-256 hash of the canonical JWK)
compute_jwk_thumbprint(JWKJson) when is_binary(JWKJson) ->
    try
        case catch json:decode(JWKJson) of
            {'EXIT', Reason} ->
                {error, iolist_to_binary([<<"Invalid JWK JSON: ">>,
                                           io_lib:format("~p", [Reason])])};
            JWKMap when is_map(JWKMap) ->
                JWK = jose_jwk:from_map(JWKMap),
                {_Module, Thumbprint} = jose_jwk:thumbprint(JWK),
                {ok, Thumbprint};
            Other ->
                {error, iolist_to_binary([<<"JWK decode returned unexpected type: ">>,
                                           io_lib:format("~p", [Other])])}
        end
    catch
        error:ErrorReason:ErrorStack ->
            {error, iolist_to_binary([<<"JWK thumbprint error: ">>,
                                       io_lib:format("~p at ~p", [ErrorReason, ErrorStack])])};
        _:OtherError ->
            {error, iolist_to_binary([<<"Unexpected error: ">>,
                                       io_lib:format("~p", [OtherError])])}
    end;
compute_jwk_thumbprint(JWKJson) when is_list(JWKJson) ->
    compute_jwk_thumbprint(list_to_binary(JWKJson)).

%% SHA-256 hash with base64url encoding (no padding) - exported version
sha256_base64url(Data) when is_binary(Data) ->
    Hash = crypto:hash(sha256, Data),
    base64url_encode(Hash);
sha256_base64url(Data) when is_list(Data) ->
    sha256_base64url(list_to_binary(Data)).
```

**Step 3: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 4: Commit**

```bash
git add server/src/jose_ffi.erl
git commit -m "feat: add compute_jwk_thumbprint and sha256_base64url to jose_ffi"
```

---

## Task 3: Create dpop_keygen_ffi.erl

**Files:**
- Create: `server/src/dpop_keygen_ffi.erl`

**Step 1: Create the FFI file**

```erlang
-module(dpop_keygen_ffi).
-export([secp256r1_params/0, ec_key_to_jwk_json/1]).

%% Return secp256r1 (P-256) curve parameters for key generation
secp256r1_params() ->
    {namedCurve, secp256r1}.

%% Convert EC private key to JWK JSON string (includes private key 'd' component)
ec_key_to_jwk_json(ECPrivateKey) ->
    %% Extract the key components from the ECPrivateKey record
    %% ECPrivateKey = {ECPrivateKey, Version, PrivateKey, Parameters, PublicKey}
    {_, _, PrivateKeyBin, _, PublicKeyBin} = ECPrivateKey,

    %% PublicKey is {0, 4, X, Y} format (uncompressed point)
    %% First byte is 0x04 indicating uncompressed format
    <<4, XY/binary>> = PublicKeyBin,
    KeySize = byte_size(XY) div 2,
    <<X:KeySize/binary, Y:KeySize/binary>> = XY,

    %% Pad private key to 32 bytes if needed
    PaddedD = pad_to_32(PrivateKeyBin),

    %% Build JWK map
    JWK = #{
        <<"kty">> => <<"EC">>,
        <<"crv">> => <<"P-256">>,
        <<"x">> => base64url_encode(X),
        <<"y">> => base64url_encode(Y),
        <<"d">> => base64url_encode(PaddedD)
    },

    %% Convert to JSON string
    json:encode(JWK).

%% Pad binary to 32 bytes (P-256 key size)
pad_to_32(Bin) when byte_size(Bin) >= 32 -> Bin;
pad_to_32(Bin) ->
    PadSize = 32 - byte_size(Bin),
    <<0:(PadSize * 8), Bin/binary>>.

%% Base64 URL-safe encoding (no padding)
base64url_encode(Bin) ->
    Base64 = base64:encode(Bin),
    NoPlus = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<"">>, [global]).
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/dpop_keygen_ffi.erl
git commit -m "feat: add dpop_keygen_ffi for EC key generation"
```

---

## Task 4: Create did_cache_ffi.erl

**Files:**
- Create: `server/src/did_cache_ffi.erl`

**Step 1: Create the FFI file**

```erlang
-module(did_cache_ffi).
-export([new_table/0, insert/4, lookup/2, delete/2, cleanup_expired/1, stats/1]).

%% Create a new ETS table for DID document caching
new_table() ->
    ets:new(did_cache, [set, public, {read_concurrency, true}]).

%% Insert a DID document with expiration timestamp
%% Args: Table, DID (binary), Document (binary), ExpiresAt (integer, unix timestamp)
insert(Table, DID, Document, ExpiresAt) ->
    true = ets:insert(Table, {DID, Document, ExpiresAt}),
    ok.

%% Lookup a DID document, checking expiration
%% Returns: {ok, Document} | {error, not_found} | {error, expired}
lookup(Table, DID) ->
    case ets:lookup(Table, DID) of
        [] ->
            {error, not_found};
        [{_DID, Document, ExpiresAt}] ->
            Now = erlang:system_time(second),
            case Now < ExpiresAt of
                true -> {ok, Document};
                false ->
                    ets:delete(Table, DID),
                    {error, expired}
            end
    end.

%% Delete a DID from the cache
delete(Table, DID) ->
    ets:delete(Table, DID),
    ok.

%% Remove all expired entries from the cache
cleanup_expired(Table) ->
    Now = erlang:system_time(second),
    %% Select and delete expired entries
    ets:select_delete(Table, [{{{'$1', '$2', '$3'}, [], [{'<', '$3', Now}]}}]),
    ok.

%% Get cache statistics
%% Returns: {EntryCount, MemoryBytes}
stats(Table) ->
    Info = ets:info(Table),
    Size = proplists:get_value(size, Info, 0),
    Memory = proplists:get_value(memory, Info, 0) * erlang:system_info(wordsize),
    {Size, Memory}.
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/did_cache_ffi.erl
git commit -m "feat: add did_cache_ffi for ETS-based DID caching"
```

---

## Task 5: Create jwt_ffi.erl

**Files:**
- Create: `server/src/jwt_ffi.erl`

**Step 1: Create the FFI file**

```erlang
-module(jwt_ffi).
-export([sign_jwt/3, derive_public_did_key/1]).

%% Sign a JWT with ES256 using a multibase-encoded private key
%% Args: ClaimsJson (binary), Kid (binary), PrivateKeyMultibase (binary)
%% Returns: {ok, JWT} | {error, Reason}
sign_jwt(ClaimsJson, Kid, PrivateKeyMultibase) ->
    try
        %% Parse multibase key (z-prefixed base58btc)
        case parse_multibase_key(PrivateKeyMultibase) of
            {ok, PrivateKeyBytes} ->
                %% Generate EC key from private key bytes
                {PubX, PubY} = derive_public_coords(PrivateKeyBytes),

                %% Build JWK for signing
                JWK = jose_jwk:from_map(#{
                    <<"kty">> => <<"EC">>,
                    <<"crv">> => <<"P-256">>,
                    <<"x">> => base64url_encode(PubX),
                    <<"y">> => base64url_encode(PubY),
                    <<"d">> => base64url_encode(PrivateKeyBytes)
                }),

                %% Parse claims
                Claims = json:decode(ClaimsJson),

                %% Create JWT with header
                JWS = jose_jws:from_map(#{
                    <<"alg">> => <<"ES256">>,
                    <<"kid">> => Kid
                }),
                JWT = jose_jwt:from_map(Claims),

                %% Sign
                Signed = jose_jwt:sign(JWK, JWS, JWT),
                {_JWS2, CompactToken} = jose_jws:compact(Signed),

                {ok, CompactToken};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error:Stack ->
            {error, iolist_to_binary([<<"JWT signing error: ">>,
                                       io_lib:format("~p at ~p", [Error, Stack])])}
    end.

%% Derive public did:key from private key multibase
derive_public_did_key(PrivateKeyMultibase) ->
    try
        case parse_multibase_key(PrivateKeyMultibase) of
            {ok, PrivateKeyBytes} ->
                {PubX, PubY} = derive_public_coords(PrivateKeyBytes),
                %% Compressed public key format
                CompressedPub = compress_public_key(PubX, PubY),
                %% Add multicodec prefix for P-256 public key (0x1200)
                Prefixed = <<16#80, 16#24, CompressedPub/binary>>,
                %% Encode as base58btc with 'z' prefix
                Encoded = base58_encode(Prefixed),
                {ok, <<"did:key:z", Encoded/binary>>};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:_ -> {error, <<"Failed to derive public key">>}
    end.

%% Parse multibase key (z-prefixed base58btc)
parse_multibase_key(<<"z", Rest/binary>>) ->
    try
        Decoded = base58_decode(Rest),
        %% Skip multicodec prefix (2 bytes for P-256 private key)
        <<_Prefix:2/binary, PrivateKey/binary>> = Decoded,
        {ok, PrivateKey}
    catch
        _:_ -> {error, <<"Invalid multibase key format">>}
    end;
parse_multibase_key(_) ->
    {error, <<"Unsupported multibase prefix">>}.

%% Derive public key coordinates from private key
derive_public_coords(PrivateKeyBytes) ->
    %% Use crypto to compute public key from private key
    PublicKey = crypto:generate_key(ecdh, secp256r1, PrivateKeyBytes),
    {_Priv, <<4, XY/binary>>} = PublicKey,
    <<X:32/binary, Y:32/binary>> = XY,
    {X, Y}.

%% Compress public key (02/03 prefix based on Y parity)
compress_public_key(X, Y) ->
    <<YLast>> = binary:part(Y, 31, 1),
    Prefix = case YLast band 1 of
        0 -> <<2>>;
        1 -> <<3>>
    end,
    <<Prefix/binary, X/binary>>.

%% Base58 Bitcoin alphabet
-define(BASE58_ALPHABET, <<"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz">>).

base58_encode(Bin) ->
    base58_encode(binary:decode_unsigned(Bin), <<>>).

base58_encode(0, Acc) -> Acc;
base58_encode(N, Acc) ->
    Rem = N rem 58,
    Char = binary:at(?BASE58_ALPHABET, Rem),
    base58_encode(N div 58, <<Char, Acc/binary>>).

base58_decode(Bin) ->
    base58_decode(Bin, 0).

base58_decode(<<>>, Acc) -> binary:encode_unsigned(Acc);
base58_decode(<<C, Rest/binary>>, Acc) ->
    case binary:match(?BASE58_ALPHABET, <<C>>) of
        {Pos, 1} -> base58_decode(Rest, Acc * 58 + Pos);
        nomatch -> error(invalid_base58)
    end.

%% Base64 URL-safe encoding (no padding)
base64url_encode(Bin) ->
    Base64 = base64:encode(Bin),
    NoPlus = binary:replace(Base64, <<"+">>, <<"-">>, [global]),
    NoSlash = binary:replace(NoPlus, <<"/">>, <<"_">>, [global]),
    binary:replace(NoSlash, <<"=">>, <<"">>, [global]).
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/jwt_ffi.erl
git commit -m "feat: add jwt_ffi for JWT signing with multibase keys"
```

---

## Task 6: Copy and adapt lib/oauth/types/error.gleam

**Files:**
- Create: `server/src/lib/oauth/types/error.gleam`
- Reference: `oauth_server/src/oauth_server/types/error.gleam`

**Step 1: Create the error types file**

```gleam
/// OAuth error types
/// Reference: RFC 6749 Section 5.2

pub type OAuthError {
  /// Invalid request parameters
  InvalidRequest(String)
  /// Client authentication failed
  InvalidClient(String)
  /// Invalid or expired authorization grant
  InvalidGrant(String)
  /// Client not authorized to use this grant type
  UnauthorizedClient(String)
  /// Grant type not supported
  UnsupportedGrantType(String)
  /// Requested scope not allowed
  InvalidScope(String)
  /// Response type not supported
  UnsupportedResponseType(String)
  /// Server error
  ServerError(String)
  /// Service temporarily unavailable
  TemporarilyUnavailable(String)
  /// Access denied by resource owner
  AccessDenied(String)
}

pub type StorageError {
  DatabaseError(String)
  NotFound(String)
  DuplicateKey(String)
}

/// Convert OAuth error to standard error code
pub fn error_code(error: OAuthError) -> String {
  case error {
    InvalidRequest(_) -> "invalid_request"
    InvalidClient(_) -> "invalid_client"
    InvalidGrant(_) -> "invalid_grant"
    UnauthorizedClient(_) -> "unauthorized_client"
    UnsupportedGrantType(_) -> "unsupported_grant_type"
    InvalidScope(_) -> "invalid_scope"
    UnsupportedResponseType(_) -> "unsupported_response_type"
    ServerError(_) -> "server_error"
    TemporarilyUnavailable(_) -> "temporarily_unavailable"
    AccessDenied(_) -> "access_denied"
  }
}

/// Get error description
pub fn error_description(error: OAuthError) -> String {
  case error {
    InvalidRequest(msg) -> msg
    InvalidClient(msg) -> msg
    InvalidGrant(msg) -> msg
    UnauthorizedClient(msg) -> msg
    UnsupportedGrantType(msg) -> msg
    InvalidScope(msg) -> msg
    UnsupportedResponseType(msg) -> msg
    ServerError(msg) -> msg
    TemporarilyUnavailable(msg) -> msg
    AccessDenied(msg) -> msg
  }
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/types/error.gleam
git commit -m "feat: add OAuth error types"
```

---

## Task 7: Copy and adapt lib/oauth/types/request.gleam

**Files:**
- Create: `server/src/lib/oauth/types/request.gleam`
- Reference: `oauth_server/src/oauth_server/types/request.gleam`

**Step 1: Create the request types file**

```gleam
/// OAuth request types
import gleam/option.{type Option}

/// Authorization request parameters
pub type AuthorizationRequest {
  AuthorizationRequest(
    response_type: String,
    client_id: String,
    redirect_uri: String,
    scope: Option(String),
    state: Option(String),
    code_challenge: Option(String),
    code_challenge_method: Option(String),
    nonce: Option(String),
    login_hint: Option(String),
    request_uri: Option(String),
  )
}

/// OAuth request stored for ATP callback (PKCE verifier, DPoP key, etc.)
pub type OAuthRequest {
  OAuthRequest(
    oauth_state: String,
    authorization_server: String,
    nonce: String,
    pkce_verifier: String,
    signing_public_key: String,
    dpop_private_key: String,
    created_at: Int,
    expires_at: Int,
  )
}

/// Token request parameters
pub type TokenRequest {
  TokenRequest(
    grant_type: String,
    code: Option(String),
    redirect_uri: Option(String),
    client_id: Option(String),
    code_verifier: Option(String),
    refresh_token: Option(String),
    scope: Option(String),
  )
}

/// PAR (Pushed Authorization Request) response
pub type PARResponse {
  PARResponse(request_uri: String, expires_in: Int)
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/types/request.gleam
git commit -m "feat: add OAuth request types"
```

---

## Task 8: Copy and adapt lib/oauth/token_generator.gleam

**Files:**
- Create: `server/src/lib/oauth/token_generator.gleam`
- Reference: `oauth_server/src/oauth_server/internal/core/token_generator.gleam`

**Step 1: Create the token generator module**

```gleam
/// Secure token generation utilities
import gleam/bit_array
import gleam/crypto

/// Generate a secure random authorization code
pub fn generate_authorization_code() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random access token
pub fn generate_access_token() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random refresh token
pub fn generate_refresh_token() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a secure random client ID
pub fn generate_client_id() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  let encoded = bit_array.base64_url_encode(random_bytes, False)
  "client_" <> encoded
}

/// Generate a secure random client secret
pub fn generate_client_secret() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a PAR request URI
pub fn generate_par_request_uri() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  let encoded = bit_array.base64_url_encode(random_bytes, False)
  "urn:ietf:params:oauth:request_uri:" <> encoded
}

/// Generate a DPoP nonce
pub fn generate_dpop_nonce() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a session ID
pub fn generate_session_id() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate an OAuth state parameter
pub fn generate_state() -> String {
  let random_bytes = crypto.strong_random_bytes(16)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Compute JWK thumbprint (JKT) from a signing key
pub fn compute_jkt(key: String) -> String {
  let key_bytes = bit_array.from_string(key)
  let hash = crypto.hash(crypto.Sha256, key_bytes)
  bit_array.base64_url_encode(hash, False)
}

/// Get current timestamp (seconds since epoch)
pub fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

/// Calculate expiration timestamp
pub fn expiration_timestamp(lifetime_seconds: Int) -> Int {
  current_timestamp() + lifetime_seconds
}

/// Check if a timestamp is expired
pub fn is_expired(expires_at: Int) -> Bool {
  current_timestamp() >= expires_at
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/token_generator.gleam
git commit -m "feat: add OAuth token generator"
```

---

## Task 9: Copy and adapt lib/oauth/pkce.gleam

**Files:**
- Create: `server/src/lib/oauth/pkce.gleam`
- Reference: `oauth_server/src/oauth_server/internal/core/pkce.gleam`

**Step 1: Create the PKCE module**

```gleam
/// PKCE (Proof Key for Code Exchange) implementation
/// Reference: RFC 7636
import gleam/bit_array
import gleam/crypto

/// Generate a cryptographically random code verifier
/// Returns a 43-128 character URL-safe string
pub fn generate_code_verifier() -> String {
  let random_bytes = crypto.strong_random_bytes(32)
  bit_array.base64_url_encode(random_bytes, False)
}

/// Generate a code challenge from a code verifier using S256 method
/// SHA-256 hash of verifier, base64url encoded
pub fn generate_code_challenge(verifier: String) -> String {
  let verifier_bytes = bit_array.from_string(verifier)
  let hash = crypto.hash(crypto.Sha256, verifier_bytes)
  bit_array.base64_url_encode(hash, False)
}

/// Verify a code verifier against a code challenge
/// Returns True if the verifier matches the challenge using S256
pub fn verify_code_challenge(
  verifier: String,
  challenge: String,
  method: String,
) -> Bool {
  case method {
    "S256" -> {
      let computed_challenge = generate_code_challenge(verifier)
      computed_challenge == challenge
    }
    "plain" -> verifier == challenge
    _ -> False
  }
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/pkce.gleam
git commit -m "feat: add PKCE implementation"
```

---

## Task 10: Copy and adapt lib/oauth/validator.gleam

**Files:**
- Create: `server/src/lib/oauth/validator.gleam`
- Reference: `oauth_server/src/oauth_server/internal/core/validator.gleam`

**Step 1: Create the validator module**

```gleam
/// OAuth request validation utilities
import gleam/list
import gleam/option.{type Option}
import gleam/string
import gleam/uri
import lib/oauth/types/error.{type OAuthError, InvalidRequest, InvalidScope}

/// Validate redirect URI format
pub fn validate_redirect_uri(uri_string: String) -> Result(Nil, OAuthError) {
  case uri.parse(uri_string) {
    Ok(parsed) -> {
      case parsed.scheme {
        option.Some(scheme) -> {
          case scheme {
            "https" -> Ok(Nil)
            "http" -> {
              case parsed.host {
                option.Some("localhost") -> Ok(Nil)
                option.Some("127.0.0.1") -> Ok(Nil)
                option.Some("[::1]") -> Ok(Nil)
                _ ->
                  Error(InvalidRequest(
                    "HTTP redirect URIs only allowed for localhost",
                  ))
              }
            }
            _ -> {
              case parsed.fragment {
                option.None -> Ok(Nil)
                option.Some(_) ->
                  Error(InvalidRequest(
                    "Redirect URI must not contain fragment",
                  ))
              }
            }
          }
        }
        option.None ->
          Error(InvalidRequest("Redirect URI must have a scheme"))
      }
    }
    Error(_) -> Error(InvalidRequest("Invalid redirect URI format"))
  }
}

/// Check if redirect URI matches registered URI
pub fn matches_redirect_uri(
  requested: String,
  registered: String,
  require_exact: Bool,
) -> Bool {
  case require_exact {
    True -> requested == registered
    False -> string.starts_with(requested, registered)
  }
}

/// Validate that redirect URI matches one of the registered URIs
pub fn validate_redirect_uri_match(
  requested: String,
  registered_uris: List(String),
  require_exact: Bool,
) -> Result(Nil, OAuthError) {
  let matches =
    list.any(registered_uris, fn(registered) {
      matches_redirect_uri(requested, registered, require_exact)
    })

  case matches {
    True -> Ok(Nil)
    False ->
      Error(InvalidRequest(
        "Redirect URI does not match any registered URIs",
      ))
  }
}

/// Validate PKCE code challenge method
pub fn validate_code_challenge_method(method: String) -> Result(Nil, OAuthError) {
  case method {
    "S256" -> Ok(Nil)
    "plain" ->
      Error(InvalidRequest(
        "PKCE method 'plain' is not allowed, use S256",
      ))
    _ ->
      Error(InvalidRequest(
        "Invalid code_challenge_method: " <> method,
      ))
  }
}

/// Validate scope format and allowed scopes
pub fn validate_scope(
  requested: Option(String),
  allowed: Option(String),
) -> Result(Nil, OAuthError) {
  case requested {
    option.None -> Ok(Nil)
    option.Some(req) -> {
      case string.is_empty(req) {
        True -> Error(InvalidScope("Scope cannot be empty string"))
        False -> {
          case allowed {
            option.None ->
              Error(InvalidScope("No scopes allowed for this client"))
            option.Some(allow) -> {
              let req_scopes = string.split(req, " ")
              let allow_scopes = string.split(allow, " ")

              let all_allowed =
                list.all(req_scopes, fn(scope) {
                  list.contains(allow_scopes, scope)
                })

              case all_allowed {
                True -> Ok(Nil)
                False -> Error(InvalidScope("Requested scope not allowed"))
              }
            }
          }
        }
      }
    }
  }
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/validator.gleam
git commit -m "feat: add OAuth request validator"
```

---

## Task 11: Copy and adapt lib/oauth/dpop/types.gleam

**Files:**
- Create: `server/src/lib/oauth/dpop/types.gleam`

**Step 1: Create the DPoP types module**

```gleam
/// DPoP (Demonstrating Proof-of-Possession) types

/// DPoP proof claims
pub type DPoPClaims {
  DPoPClaims(
    jti: String,
    htm: String,
    htu: String,
    iat: Int,
    ath: String,
    nonce: String,
  )
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/dpop/types.gleam
git commit -m "feat: add DPoP types"
```

---

## Task 12: Copy and adapt lib/oauth/dpop/keygen.gleam

**Files:**
- Create: `server/src/lib/oauth/dpop/keygen.gleam`
- Reference: `oauth_server/src/oauth_server/internal/dpop/keygen.gleam`

**Step 1: Create the DPoP keygen module**

```gleam
/// DPoP key generation for ES256 (P-256) keys
import gleam/dynamic
import gleam/io

/// Generate a complete P-256 EC JWK for DPoP use (including private key)
/// Returns JWK as JSON string with kty, crv, x, y, d fields
pub fn generate_dpop_jwk() -> String {
  io.println("[keygen] Calling erlang_secp256r1_params()")
  let key_params = erlang_secp256r1_params()

  io.println("[keygen] Calling erlang_generate_key() with params")
  let ec_private_key = erlang_generate_key(key_params)

  io.println("[keygen] Calling erlang_ec_key_to_jwk_json()")
  let jwk_json = erlang_ec_key_to_jwk_json(ec_private_key)
  io.println("[keygen] JWK JSON generated successfully")
  jwk_json
}

@external(erlang, "public_key", "generate_key")
fn erlang_generate_key(params: dynamic.Dynamic) -> dynamic.Dynamic

@external(erlang, "dpop_keygen_ffi", "secp256r1_params")
fn erlang_secp256r1_params() -> dynamic.Dynamic

@external(erlang, "dpop_keygen_ffi", "ec_key_to_jwk_json")
fn erlang_ec_key_to_jwk_json(ec_key: dynamic.Dynamic) -> String
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/dpop/keygen.gleam
git commit -m "feat: add DPoP key generation"
```

---

## Task 13: Copy and adapt lib/oauth/dpop/generator.gleam

**Files:**
- Create: `server/src/lib/oauth/dpop/generator.gleam`
- Reference: `oauth_server/src/oauth_server/internal/dpop/generator.gleam`

**Step 1: Create the DPoP generator module**

```gleam
/// DPoP proof generation
import gleam/option.{type Option}

/// Generate a DPoP proof JWT token with optional nonce
pub fn generate_dpop_proof_with_nonce(
  method: String,
  url: String,
  access_token: String,
  jwk_json: String,
  nonce: Option(String),
) -> Result(String, String) {
  case nonce {
    option.Some(n) ->
      generate_dpop_proof_internal(method, url, access_token, jwk_json, n)
    option.None ->
      generate_dpop_proof_internal(method, url, access_token, jwk_json, "")
  }
}

@external(erlang, "jose_ffi", "generate_dpop_proof")
fn generate_dpop_proof_internal(
  method: String,
  url: String,
  access_token: String,
  jwk_json: String,
  nonce: String,
) -> Result(String, String)

/// Hash a string using SHA-256
@external(erlang, "jose_ffi", "sha256_hash")
pub fn sha256_hash(data: String) -> String
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/dpop/generator.gleam
git commit -m "feat: add DPoP proof generator"
```

---

## Task 14: Copy and adapt lib/oauth/crypto/jwt.gleam

**Files:**
- Create: `server/src/lib/oauth/crypto/jwt.gleam`
- Reference: `oauth_server/src/oauth_server/internal/crypto/jwt.gleam`

**Step 1: Create the JWT module**

```gleam
/// JWT signing utilities
import gleam/json
import gleam/option.{type Option, None, Some}
import lib/oauth/token_generator

/// Create a client assertion JWT for token endpoint authentication
pub fn create_client_assertion(
  client_id: String,
  audience: String,
  signing_key: String,
) -> Result(String, String) {
  let now = token_generator.current_timestamp()
  let exp = now + 300  // 5 minutes

  let claims =
    json.object([
      #("iss", json.string(client_id)),
      #("sub", json.string(client_id)),
      #("aud", json.string(audience)),
      #("iat", json.int(now)),
      #("exp", json.int(exp)),
      #("jti", json.string(token_generator.generate_state())),
    ])

  let claims_json = json.to_string(claims)

  sign_jwt_internal(claims_json, client_id, signing_key)
}

@external(erlang, "jwt_ffi", "sign_jwt")
fn sign_jwt_internal(
  claims_json: String,
  kid: String,
  private_key: String,
) -> Result(String, String)
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/crypto/jwt.gleam
git commit -m "feat: add JWT signing utilities"
```

---

## Task 15: Copy and adapt lib/oauth/did_cache.gleam

**Files:**
- Create: `server/src/lib/oauth/did_cache.gleam`
- Reference: `oauth_server/src/oauth_server/internal/did_cache.gleam`

**Step 1: Create the DID cache module**

```gleam
/// DID document caching with ETS backend
import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/otp/actor

/// Cache message types
pub type Message {
  Get(did: String, reply_to: Subject(Result(String, String)))
  Put(did: String, document: String, ttl_seconds: Int)
  Delete(did: String)
  Invalidate(did: String)
  Cleanup
}

/// Start the DID cache actor
pub fn start() -> Result(Subject(Message), actor.StartError) {
  actor.start(new_table(), handle_message)
}

/// Get a DID document from cache
pub fn get(
  cache: Subject(Message),
  did: String,
) -> Result(String, String) {
  actor.call(cache, fn(reply_to) { Get(did, reply_to) }, 5000)
}

/// Put a DID document in cache with TTL
pub fn put(
  cache: Subject(Message),
  did: String,
  document: String,
  ttl_seconds: Int,
) -> Nil {
  actor.send(cache, Put(did, document, ttl_seconds))
}

/// Delete a DID from cache
pub fn delete(cache: Subject(Message), did: String) -> Nil {
  actor.send(cache, Delete(did))
}

/// Invalidate (same as delete)
pub fn invalidate(cache: Subject(Message), did: String) -> Nil {
  actor.send(cache, Invalidate(did))
}

/// Handle cache messages
fn handle_message(
  message: Message,
  table: dynamic.Dynamic,
) -> actor.Next(Message, dynamic.Dynamic) {
  case message {
    Get(did, reply_to) -> {
      let result = lookup_internal(table, did)
      actor.send(reply_to, result)
      actor.continue(table)
    }
    Put(did, document, ttl_seconds) -> {
      let expires_at = current_timestamp() + ttl_seconds
      insert_internal(table, did, document, expires_at)
      actor.continue(table)
    }
    Delete(did) -> {
      delete_internal(table, did)
      actor.continue(table)
    }
    Invalidate(did) -> {
      delete_internal(table, did)
      actor.continue(table)
    }
    Cleanup -> {
      cleanup_expired_internal(table)
      actor.continue(table)
    }
  }
}

fn current_timestamp() -> Int {
  get_system_time_native() / 1_000_000_000
}

@external(erlang, "os", "system_time")
fn get_system_time_native() -> Int

@external(erlang, "did_cache_ffi", "new_table")
fn new_table() -> dynamic.Dynamic

@external(erlang, "did_cache_ffi", "insert")
fn insert_internal(
  table: dynamic.Dynamic,
  did: String,
  document: String,
  expires_at: Int,
) -> Nil

@external(erlang, "did_cache_ffi", "lookup")
fn lookup_internal(
  table: dynamic.Dynamic,
  did: String,
) -> Result(String, String)

@external(erlang, "did_cache_ffi", "delete")
fn delete_internal(table: dynamic.Dynamic, did: String) -> Nil

@external(erlang, "did_cache_ffi", "cleanup_expired")
fn cleanup_expired_internal(table: dynamic.Dynamic) -> Nil
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/did_cache.gleam
git commit -m "feat: add DID document cache"
```

---

## Task 16: Copy and adapt lib/oauth/atproto/types.gleam

**Files:**
- Create: `server/src/lib/oauth/atproto/types.gleam`
- Reference: `oauth_server/src/oauth_server/internal/atproto/types.gleam`

**Step 1: Create the ATProto types module**

```gleam
/// ATProto-specific types

/// ATProto error types
pub type ATProtoError {
  DIDResolutionFailed(String)
  PDSNotFound(String)
  InvalidDIDDocument(String)
  HTTPError(String)
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/atproto/types.gleam
git commit -m "feat: add ATProto types"
```

---

## Task 17: Copy and adapt lib/oauth/atproto/did_resolver.gleam

**Files:**
- Create: `server/src/lib/oauth/atproto/did_resolver.gleam`
- Reference: `oauth_server/src/oauth_server/internal/atproto/did_resolver.gleam`

**Step 1: Create the DID resolver module**

```gleam
/// DID resolution for ATProtocol
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lib/oauth/atproto/types.{type ATProtoError}
import lib/oauth/did_cache

/// DID Document structure
pub type DIDDocument {
  DIDDocument(
    id: String,
    service: List(Service),
  )
}

pub type Service {
  Service(
    id: String,
    service_type: String,
    service_endpoint: String,
  )
}

/// Resolve a DID to its document, using cache
pub fn resolve_did_with_cache(
  cache: Subject(did_cache.Message),
  did: String,
  invalidate_first: Bool,
) -> Result(DIDDocument, ATProtoError) {
  // Invalidate cache if requested
  case invalidate_first {
    True -> did_cache.invalidate(cache, did)
    False -> Nil
  }

  // Try cache first
  case did_cache.get(cache, did) {
    Ok(doc_json) -> {
      case parse_did_document(doc_json) {
        Ok(doc) -> Ok(doc)
        Error(_) -> resolve_did_fresh(cache, did)
      }
    }
    Error(_) -> resolve_did_fresh(cache, did)
  }
}

/// Resolve DID without cache
fn resolve_did_fresh(
  cache: Subject(did_cache.Message),
  did: String,
) -> Result(DIDDocument, ATProtoError) {
  use doc <- result.try(resolve_did(did))

  // Cache the result
  let doc_json = encode_did_document(doc)
  did_cache.put(cache, did, doc_json, 3600)  // 1 hour TTL

  Ok(doc)
}

/// Resolve a DID directly (no cache)
pub fn resolve_did(did: String) -> Result(DIDDocument, ATProtoError) {
  case string.starts_with(did, "did:plc:") {
    True -> resolve_plc_did(did)
    False -> {
      case string.starts_with(did, "did:web:") {
        True -> resolve_web_did(did)
        False -> Error(types.DIDResolutionFailed("Unsupported DID method"))
      }
    }
  }
}

/// Resolve a did:plc DID
fn resolve_plc_did(did: String) -> Result(DIDDocument, ATProtoError) {
  let url = "https://plc.directory/" <> did

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> parse_did_document(resp.body)
    404 -> Error(types.DIDResolutionFailed("DID not found"))
    status ->
      Error(types.HTTPError(
        "PLC resolution failed with status " <> string.inspect(status),
      ))
  }
}

/// Resolve a did:web DID
fn resolve_web_did(did: String) -> Result(DIDDocument, ATProtoError) {
  // Extract domain from did:web:domain
  let domain = string.drop_start(did, 8)
  let url = "https://" <> domain <> "/.well-known/did.json"

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> parse_did_document(resp.body)
    404 -> Error(types.DIDResolutionFailed("DID not found"))
    status ->
      Error(types.HTTPError(
        "Web DID resolution failed with status " <> string.inspect(status),
      ))
  }
}

/// Resolve a handle to a DID
pub fn resolve_handle_to_did(handle: String) -> Result(String, ATProtoError) {
  let url =
    "https://bsky.social/xrpc/com.atproto.identity.resolveHandle?handle="
    <> handle

  use req <- result.try(
    request.to(url)
    |> result.map_error(fn(_) { types.HTTPError("Invalid URL") }),
  )

  use resp <- result.try(
    httpc.send(req)
    |> result.map_error(fn(_) { types.HTTPError("Request failed") }),
  )

  case resp.status {
    200 -> {
      let decoder = decode.at(["did"], decode.string)
      case json.parse(resp.body, decoder) {
        Ok(did) -> Ok(did)
        Error(_) -> Error(types.DIDResolutionFailed("Invalid response format"))
      }
    }
    _ -> Error(types.DIDResolutionFailed("Handle resolution failed"))
  }
}

/// Get PDS endpoint from DID document
pub fn get_pds_endpoint(doc: DIDDocument) -> Option(String) {
  doc.service
  |> list.find(fn(s) { s.service_type == "AtprotoPersonalDataServer" })
  |> result.map(fn(s) { s.service_endpoint })
  |> option.from_result
}

/// Parse DID document from JSON
fn parse_did_document(json_str: String) -> Result(DIDDocument, ATProtoError) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use service <- decode.field("service", decode.list(service_decoder()))
    decode.success(DIDDocument(id: id, service: service))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { types.InvalidDIDDocument("Parse failed") })
}

fn service_decoder() -> decode.Decoder(Service) {
  use id <- decode.field("id", decode.string)
  use service_type <- decode.field("type", decode.string)
  use endpoint <- decode.field("serviceEndpoint", decode.string)
  decode.success(Service(
    id: id,
    service_type: service_type,
    service_endpoint: endpoint,
  ))
}

/// Encode DID document to JSON
fn encode_did_document(doc: DIDDocument) -> String {
  let services =
    doc.service
    |> list.map(fn(s) {
      json.object([
        #("id", json.string(s.id)),
        #("type", json.string(s.service_type)),
        #("serviceEndpoint", json.string(s.service_endpoint)),
      ])
    })

  json.object([
    #("id", json.string(doc.id)),
    #("service", json.preprocessed_array(services)),
  ])
  |> json.to_string
}
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 3: Commit**

```bash
git add server/src/lib/oauth/atproto/did_resolver.gleam
git commit -m "feat: add ATProto DID resolver"
```

---

## Task 18: Create simplified authorize handler

**Files:**
- Create: `server/src/handlers/oauth/authorize.gleam`

**Step 1: Create the authorize handler**

This is a simplified version that implements the core authorize flow, using server's existing repositories.

```gleam
/// OAuth authorization endpoint handler
/// GET /oauth/authorize
import database/repositories/oauth_auth_requests
import database/repositories/oauth_atp_requests
import database/repositories/oauth_atp_sessions
import database/repositories/oauth_clients
import database/types.{
  OAuthAtpRequest, OAuthAtpSession, OAuthAuthRequest,
}
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lib/oauth/atproto/did_resolver
import lib/oauth/did_cache
import lib/oauth/dpop/keygen
import lib/oauth/pkce
import lib/oauth/token_generator
import lib/oauth/types/error.{InvalidRequest, UnsupportedResponseType}
import lib/oauth/types/request.{type AuthorizationRequest, AuthorizationRequest}
import lib/oauth/validator
import sqlight
import wisp

/// Authorization response type
pub type AuthorizeResponse {
  RedirectToATProtocol(authorization_url: String)
  RedirectWithError(
    redirect_uri: String,
    error: String,
    error_description: String,
    state: Option(String),
  )
}

/// Handle GET /oauth/authorize
pub fn handle(
  req: wisp.Request,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  redirect_uri: String,
  client_id: String,
  signing_key: Option(String),
) -> wisp.Response {
  case req.method {
    http.Get | http.Post -> {
      case req.query {
        Some(query) -> {
          case handle_authorize(query, conn, did_cache, redirect_uri, client_id, signing_key) {
            Ok(response) -> build_redirect_response(response)
            Error(err) -> {
              wisp.log_error("Authorization error: " <> err)
              wisp.response(400)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text("{\"error\": \"" <> err <> "\"}"))
            }
          }
        }
        None -> {
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text("{\"error\": \"Missing query parameters\"}"))
        }
      }
    }
    _ -> wisp.method_not_allowed([http.Get, http.Post])
  }
}

/// Main authorization handler
fn handle_authorize(
  query: String,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Parse query parameters
  use params <- result.try(
    uri.parse_query(query)
    |> result.map_error(fn(_) { "Failed to parse query string" }),
  )

  // Check for PAR request_uri
  case get_param(params, "request_uri") {
    Some(_request_uri) -> {
      // PAR flow - not implemented yet
      Error("PAR flow not yet implemented")
    }
    None -> {
      // Standard flow
      handle_standard_flow(
        params,
        conn,
        did_cache,
        server_redirect_uri,
        server_client_id,
        signing_key,
      )
    }
  }
}

/// Handle standard authorization flow
fn handle_standard_flow(
  params: List(#(String, String)),
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Parse authorization request
  use auth_request <- result.try(parse_authorization_request(params))

  // Get client
  use client_opt <- result.try(
    oauth_clients.get(conn, auth_request.client_id)
    |> result.map_error(fn(_) { "Failed to retrieve client" }),
  )

  use client <- result.try(case client_opt {
    Some(c) -> Ok(c)
    None -> Error("Client not found")
  })

  // Validate request
  use _ <- result.try(validate_authorization_request(auth_request, client))

  // Process authorization
  process_authorization(
    auth_request,
    conn,
    did_cache,
    server_redirect_uri,
    server_client_id,
    signing_key,
  )
}

/// Parse authorization request from query parameters
fn parse_authorization_request(
  params: List(#(String, String)),
) -> Result(AuthorizationRequest, String) {
  use response_type <- result.try(case get_param(params, "response_type") {
    Some(rt) -> Ok(rt)
    None -> Error("response_type is required")
  })

  use client_id <- result.try(case get_param(params, "client_id") {
    Some(id) -> Ok(id)
    None -> Error("client_id is required")
  })

  use redirect_uri <- result.try(case get_param(params, "redirect_uri") {
    Some(uri) -> Ok(uri)
    None -> Error("redirect_uri is required")
  })

  Ok(AuthorizationRequest(
    response_type: response_type,
    client_id: client_id,
    redirect_uri: redirect_uri,
    scope: get_param(params, "scope"),
    state: get_param(params, "state"),
    code_challenge: get_param(params, "code_challenge"),
    code_challenge_method: get_param(params, "code_challenge_method"),
    nonce: get_param(params, "nonce"),
    login_hint: get_param(params, "login_hint"),
    request_uri: None,
  ))
}

/// Validate authorization request against client
fn validate_authorization_request(
  req: AuthorizationRequest,
  client: types.OAuthClient,
) -> Result(Nil, String) {
  // Validate response_type
  use _ <- result.try(case req.response_type {
    "code" -> Ok(Nil)
    _ -> Error("Unsupported response_type")
  })

  // Validate redirect_uri
  use _ <- result.try(
    validator.validate_redirect_uri(req.redirect_uri)
    |> result.map_error(fn(e) { error.error_description(e) }),
  )

  use _ <- result.try(
    validator.validate_redirect_uri_match(
      req.redirect_uri,
      client.redirect_uris,
      client.require_redirect_exact,
    )
    |> result.map_error(fn(e) { error.error_description(e) }),
  )

  // Validate PKCE if provided
  case req.code_challenge, req.code_challenge_method {
    Some(_), Some(method) ->
      validator.validate_code_challenge_method(method)
      |> result.map_error(fn(e) { error.error_description(e) })
    Some(_), None -> Error("code_challenge_method required")
    None, Some(_) -> Error("code_challenge required")
    None, None -> Ok(Nil)
  }
}

/// Process the authorization - store request and redirect to ATP
fn process_authorization(
  req: AuthorizationRequest,
  conn: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
  server_redirect_uri: String,
  server_client_id: String,
  signing_key: Option(String),
) -> Result(AuthorizeResponse, String) {
  // Extract DID from login_hint
  use did <- result.try(case req.login_hint {
    Some(hint) -> {
      case string.starts_with(hint, "did:") {
        True -> Ok(hint)
        False -> {
          // Resolve handle to DID
          did_resolver.resolve_handle_to_did(hint)
          |> result.map_error(fn(_) { "Failed to resolve handle" })
        }
      }
    }
    None -> Error("login_hint (DID or handle) is required")
  })

  // Generate session_id
  let session_id = token_generator.generate_session_id()
  let now = token_generator.current_timestamp()
  let expires_at = token_generator.expiration_timestamp(600)

  // Store client authorization request
  let auth_req = OAuthAuthRequest(
    session_id: session_id,
    client_id: req.client_id,
    redirect_uri: req.redirect_uri,
    scope: req.scope,
    state: req.state,
    code_challenge: req.code_challenge,
    code_challenge_method: req.code_challenge_method,
    response_type: req.response_type,
    nonce: req.nonce,
    login_hint: req.login_hint,
    created_at: now,
    expires_at: expires_at,
  )

  use _ <- result.try(
    oauth_auth_requests.insert(conn, auth_req)
    |> result.map_error(fn(_) { "Failed to store authorization request" }),
  )

  // Generate ATP OAuth state
  let atp_oauth_state = token_generator.generate_state()

  // Generate DPoP key pair
  let dpop_key = keygen.generate_dpop_jwk()

  // Generate signing key JKT
  let signing_key_jkt = case signing_key {
    Some(key) -> token_generator.compute_jkt(key)
    None -> token_generator.generate_state()
  }

  // Generate PKCE for ATP OAuth
  let pkce_verifier = pkce.generate_code_verifier()
  let code_challenge = pkce.generate_code_challenge(pkce_verifier)

  // Store OAuth request for callback
  let oauth_req = OAuthAtpRequest(
    oauth_state: atp_oauth_state,
    authorization_server: "https://unknown-pds.example.com",
    nonce: token_generator.generate_state(),
    pkce_verifier: pkce_verifier,
    signing_public_key: signing_key_jkt,
    dpop_private_key: dpop_key,
    created_at: now,
    expires_at: expires_at,
  )

  use _ <- result.try(
    oauth_atp_requests.insert(conn, oauth_req)
    |> result.map_error(fn(_) { "Failed to store OAuth request" }),
  )

  // Create ATP session
  let atp_session = OAuthAtpSession(
    session_id: session_id,
    iteration: 0,
    did: Some(did),
    session_created_at: now,
    atp_oauth_state: atp_oauth_state,
    signing_key_jkt: signing_key_jkt,
    dpop_key: dpop_key,
    access_token: None,
    refresh_token: None,
    access_token_created_at: None,
    access_token_expires_at: None,
    access_token_scopes: None,
    session_exchanged_at: None,
    exchange_error: None,
  )

  use _ <- result.try(
    oauth_atp_sessions.insert(conn, atp_session)
    |> result.map_error(fn(_) { "Failed to store ATP session" }),
  )

  // Resolve DID to get PDS endpoint
  use did_doc <- result.try(
    did_resolver.resolve_did_with_cache(did_cache, did, True)
    |> result.map_error(fn(_) { "Failed to resolve DID" }),
  )

  use pds_endpoint <- result.try(case did_resolver.get_pds_endpoint(did_doc) {
    Some(endpoint) -> Ok(endpoint)
    None -> Error("No PDS endpoint in DID document")
  })

  // Get authorization server metadata
  use auth_server <- result.try(
    fetch_authorization_server_metadata(pds_endpoint)
    |> result.map_error(fn(_) { "Failed to get authorization server metadata" }),
  )

  // Build authorization URL
  let scope = case req.scope {
    Some(s) -> s
    None -> "atproto transition:generic"
  }

  let auth_url =
    auth_server.authorization_endpoint
    <> "?client_id=" <> uri.percent_encode(server_client_id)
    <> "&redirect_uri=" <> uri.percent_encode(server_redirect_uri)
    <> "&response_type=code"
    <> "&code_challenge=" <> uri.percent_encode(code_challenge)
    <> "&code_challenge_method=S256"
    <> "&state=" <> uri.percent_encode(atp_oauth_state)
    <> "&scope=" <> uri.percent_encode(scope)
    <> "&login_hint=" <> uri.percent_encode(did)

  Ok(RedirectToATProtocol(authorization_url: auth_url))
}

/// Authorization server metadata
pub type AuthServerMetadata {
  AuthServerMetadata(
    issuer: String,
    authorization_endpoint: String,
    token_endpoint: String,
  )
}

/// Fetch authorization server metadata from PDS
fn fetch_authorization_server_metadata(
  pds_endpoint: String,
) -> Result(AuthServerMetadata, String) {
  // First get protected resource metadata
  let pr_url = pds_endpoint <> "/.well-known/oauth-protected-resource"

  use pr_req <- result.try(
    request.to(pr_url)
    |> result.map_error(fn(_) { "Invalid URL" }),
  )

  use pr_resp <- result.try(
    httpc.send(pr_req)
    |> result.map_error(fn(_) { "Request failed" }),
  )

  use auth_server_url <- result.try(case pr_resp.status {
    200 -> {
      let decoder = decode.at(["authorization_servers"], decode.list(decode.string))
      case json.parse(pr_resp.body, decoder) {
        Ok([first, ..]) -> Ok(first)
        _ -> Error("No authorization servers found")
      }
    }
    _ -> Error("Failed to get protected resource metadata")
  })

  // Now get authorization server metadata
  let as_url = auth_server_url <> "/.well-known/oauth-authorization-server"

  use as_req <- result.try(
    request.to(as_url)
    |> result.map_error(fn(_) { "Invalid URL" }),
  )

  use as_resp <- result.try(
    httpc.send(as_req)
    |> result.map_error(fn(_) { "Request failed" }),
  )

  case as_resp.status {
    200 -> {
      let decoder = {
        use issuer <- decode.field("issuer", decode.string)
        use auth_ep <- decode.field("authorization_endpoint", decode.string)
        use token_ep <- decode.field("token_endpoint", decode.string)
        decode.success(AuthServerMetadata(
          issuer: issuer,
          authorization_endpoint: auth_ep,
          token_endpoint: token_ep,
        ))
      }
      json.parse(as_resp.body, decoder)
      |> result.map_error(fn(_) { "Invalid metadata response" })
    }
    _ -> Error("Failed to get authorization server metadata")
  }
}

/// Build redirect response
fn build_redirect_response(response: AuthorizeResponse) -> wisp.Response {
  case response {
    RedirectToATProtocol(url) -> wisp.redirect(url)
    RedirectWithError(redirect_uri, error, description, state) -> {
      let query =
        "error=" <> uri.percent_encode(error)
        <> "&error_description=" <> uri.percent_encode(description)
        <> case state {
          Some(s) -> "&state=" <> uri.percent_encode(s)
          None -> ""
        }
      wisp.redirect(redirect_uri <> "?" <> query)
    }
  }
}

/// Helper to get parameter from list
fn get_param(
  params: List(#(String, String)),
  key: String,
) -> Option(String) {
  params
  |> list.find(fn(param) { param.0 == key })
  |> result.map(fn(param) { param.1 })
  |> option.from_result
}

// Missing imports at top - need to add
import gleam/dynamic/decode
import gleam/http/request
import gleam/httpc
import gleam/json
```

**Step 2: Verify it compiles**

Run: `cd server && gleam build`
Expected: May have errors - fix import issues

**Step 3: Commit**

```bash
git add server/src/handlers/oauth/authorize.gleam
git commit -m "feat: add OAuth authorize handler"
```

---

## Task 19: Wire up route in server.gleam

**Files:**
- Modify: `server/src/server.gleam`

**Step 1: Add import for authorize handler**

Add near other handler imports:
```gleam
import handlers/oauth/authorize as oauth_authorize_handler
```

**Step 2: Add did_cache to Context type**

Modify Context type to include did_cache:
```gleam
import lib/oauth/did_cache

pub type Context {
  Context(
    db: sqlight.Connection,
    auth_base_url: String,
    plc_url: String,
    oauth_config: handlers.OAuthConfig,
    admin_dids: List(String),
    backfill_state: process.Subject(backfill_state.Message),
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
  )
}
```

**Step 3: Initialize did_cache at startup**

In `start_server` function, after other initializations:
```gleam
// Start DID cache actor
let assert Ok(did_cache_subject) = did_cache.start()
logging.log(logging.Info, "[server] DID cache actor initialized")
```

And include in Context creation:
```gleam
let ctx =
  Context(
    // ... existing fields ...
    did_cache: did_cache_subject,
  )
```

**Step 4: Add route in handle_request**

Add to case statement in handle_request:
```gleam
["oauth", "authorize"] ->
  oauth_authorize_handler.handle(
    req,
    ctx.db,
    ctx.did_cache,
    ctx.oauth_config.redirect_uri,
    ctx.oauth_config.client_id,
    option.None,  // signing_key - could come from config
  )
```

**Step 5: Verify it compiles**

Run: `cd server && gleam build`
Expected: Compiles without errors

**Step 6: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat: wire up /oauth/authorize route"
```

---

## Task 20: Test compilation and fix issues

**Files:**
- Various files may need fixes

**Step 1: Run full build**

Run: `cd server && gleam build`
Expected: Either passes or shows specific errors

**Step 2: Fix any import/type errors**

Common issues:
- Missing imports
- Type mismatches between database/types.gleam and lib/oauth/types
- FFI function signatures

**Step 3: Run tests**

Run: `cd server && gleam test`
Expected: Existing tests still pass

**Step 4: Final commit**

```bash
git add -A
git commit -m "fix: resolve compilation issues for OAuth authorize"
```

---

## Summary

**Files Created:**
- `server/src/lib/oauth/types/error.gleam`
- `server/src/lib/oauth/types/request.gleam`
- `server/src/lib/oauth/token_generator.gleam`
- `server/src/lib/oauth/pkce.gleam`
- `server/src/lib/oauth/validator.gleam`
- `server/src/lib/oauth/did_cache.gleam`
- `server/src/lib/oauth/dpop/types.gleam`
- `server/src/lib/oauth/dpop/keygen.gleam`
- `server/src/lib/oauth/dpop/generator.gleam`
- `server/src/lib/oauth/crypto/jwt.gleam`
- `server/src/lib/oauth/atproto/types.gleam`
- `server/src/lib/oauth/atproto/did_resolver.gleam`
- `server/src/handlers/oauth/authorize.gleam`
- `server/src/dpop_keygen_ffi.erl`
- `server/src/did_cache_ffi.erl`
- `server/src/jwt_ffi.erl`

**Files Modified:**
- `server/src/jose_ffi.erl` (added 2 functions)
- `server/src/server.gleam` (added route and did_cache)

**Total: 16 new files, 2 modified files**
