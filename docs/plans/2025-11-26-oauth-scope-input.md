# OAuth Scope Input Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a scope input field to the OAuth client registration form with backend validation against `OAUTH_SUPPORTED_SCOPES` environment variable.

**Architecture:** Add scope field to client UI form (prefilled with "atproto transition:generic"), pass through GraphQL mutation to backend, validate against env var whitelist, store in existing OAuthClient.scope field.

**Tech Stack:** Gleam (server), Lustre (client UI), GraphQL (Swell), SQLite

---

### Task 1: Add OAUTH_SUPPORTED_SCOPES to Server Context

**Files:**
- Modify: `server/src/server.gleam:49-63` (Context type)
- Modify: `server/src/server.gleam:360-375` (env loading)
- Modify: `server/src/server.gleam:389-400` (Context constructor)

**Step 1: Add field to Context type**

In `server/src/server.gleam`, add `oauth_supported_scopes` to Context type after `oauth_signing_key`:

```gleam
pub type Context {
  Context(
    db: sqlight.Connection,
    external_base_url: String,
    plc_url: String,
    admin_dids: List(String),
    backfill_state: process.Subject(backfill_state.Message),
    config: process.Subject(config.Message),
    jetstream_consumer: option.Option(
      process.Subject(jetstream_consumer.ManagerMessage),
    ),
    did_cache: process.Subject(did_cache.Message),
    oauth_signing_key: option.Option(String),
    oauth_supported_scopes: List(String),
  )
}
```

**Step 2: Load env var in start_server function**

After the `oauth_signing_key` loading block (around line 375), add:

```gleam
  // Get OAuth supported scopes from environment variable (space-separated)
  let oauth_supported_scopes = case envoy.get("OAUTH_SUPPORTED_SCOPES") {
    Ok(scopes_str) -> {
      scopes_str
      |> string.split(" ")
      |> list.map(string.trim)
      |> list.filter(fn(s) { !string.is_empty(s) })
    }
    Error(_) -> ["atproto", "transition:generic"]
  }
```

**Step 3: Add to Context constructor**

Update the Context constructor (around line 389-400) to include the new field:

```gleam
  let ctx =
    Context(
      db: db,
      external_base_url: external_base_url,
      plc_url: plc_url,
      admin_dids: admin_dids,
      backfill_state: backfill_state_subject,
      config: config_subject,
      jetstream_consumer: jetstream_subject,
      did_cache: did_cache_subject,
      oauth_signing_key: oauth_signing_key,
      oauth_supported_scopes: oauth_supported_scopes,
    )
```

**Step 4: Build to verify no compile errors**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 5: Commit**

```bash
git add server/src/server.gleam
git commit -m "feat(oauth): add OAUTH_SUPPORTED_SCOPES to server context"
```

---

### Task 2: Expose scope in GraphQL Type and Conversion

**Files:**
- Modify: `server/src/client_schema.gleam:344-442` (oauth_client_type)
- Modify: `server/src/client_schema.gleam:614-630` (oauth_client_to_value)

**Step 1: Add scope field to oauth_client_type**

In `server/src/client_schema.gleam`, add scope field to `oauth_client_type()` after `redirectUris` field (around line 425):

```gleam
    schema.field(
      "scope",
      schema.string_type(),
      "OAuth scopes for this client (space-separated)",
      fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "scope") {
              Ok(v) -> Ok(v)
              Error(_) -> Ok(value.Null)
            }
          }
          _ -> Ok(value.Null)
        }
      },
    ),
```

**Step 2: Add scope to oauth_client_to_value**

In `oauth_client_to_value` function (around line 614), add scope to the returned object:

```gleam
fn oauth_client_to_value(client: types.OAuthClient) -> value.Value {
  let secret_value = case client.client_secret {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  let scope_value = case client.scope {
    Some(s) -> value.String(s)
    None -> value.Null
  }
  value.Object([
    #("clientId", value.String(client.client_id)),
    #("clientSecret", secret_value),
    #("clientName", value.String(client.client_name)),
    #(
      "clientType",
      value.String(types.client_type_to_string(client.client_type)),
    ),
    #("redirectUris", value.List(list.map(client.redirect_uris, value.String))),
    #("scope", scope_value),
    #("createdAt", value.Int(client.created_at)),
  ])
}
```

**Step 3: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 4: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(oauth): expose scope field in GraphQL OAuthClient type"
```

---

### Task 3: Add Scope Validation Helper

**Files:**
- Modify: `server/src/client_schema.gleam` (add helper function near top, after imports)

**Step 1: Add validation helper function**

Add this function after the `is_admin` helper (around line 34):

```gleam
/// Validate that all requested scopes are in the supported list
fn validate_scope_against_supported(
  requested_scope: String,
  supported_scopes: List(String),
) -> Result(Nil, String) {
  let requested =
    requested_scope
    |> string.split(" ")
    |> list.map(string.trim)
    |> list.filter(fn(s) { !string.is_empty(s) })

  let invalid =
    list.filter(requested, fn(s) { !list.contains(supported_scopes, s) })

  case invalid {
    [] -> Ok(Nil)
    _ ->
      Error(
        "Unsupported scope(s): "
        <> string.join(invalid, ", ")
        <> ". Supported: "
        <> string.join(supported_scopes, ", "),
      )
  }
}
```

**Step 2: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles (warning about unused function is ok for now)

**Step 3: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(oauth): add scope validation helper"
```

---

### Task 4: Update createOAuthClient Mutation

**Files:**
- Modify: `server/src/client_schema.gleam:1049-1184` (createOAuthClient mutation)

**Step 1: Update mutation_type signature to accept supported_scopes**

The `mutation_type` function needs to receive `oauth_supported_scopes`. Update its signature (around line 816):

```gleam
pub fn mutation_type(
  conn: sqlight.Connection,
  req: wisp.Request,
  admin_dids: List(String),
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
) -> schema.Type {
```

**Step 2: Add scope argument to createOAuthClient**

In the `createOAuthClient` field_with_args (around line 1067), add scope argument after redirectUris:

```gleam
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
```

**Step 3: Update the mutation resolver to handle scope**

Update the pattern match to include scope (around line 1081):

```gleam
                case
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "clientType"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(name)),
                    Some(value.String(type_str)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
```

**Step 4: Add scope validation before creating client**

After redirect URI validation succeeds (around line 1130), add:

```gleam
                                // Validate scope against supported scopes
                                case validate_scope_against_supported(scope, oauth_supported_scopes) {
                                  Error(err) -> Error(err)
                                  Ok(_) -> {
```

**Step 5: Update OAuthClient constructor to use scope**

Update the client constructor (around line 1149) to use the scope:

```gleam
                                scope: case string.trim(scope) {
                                  "" -> None
                                  s -> Some(s)
                                },
```

**Step 6: Close the new case block**

Add closing braces to match the new scope validation case.

**Step 7: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compile error about mutation_type signature change

**Step 8: Update build_schema to pass oauth_supported_scopes**

Update `build_schema` function (around line 1341) to accept and pass the new parameter:

```gleam
pub fn build_schema(
  conn: sqlight.Connection,
  req: wisp.Request,
  admin_dids: List(String),
  jetstream_subject: Option(Subject(jetstream_consumer.ManagerMessage)),
  did_cache: Subject(did_cache.Message),
  oauth_supported_scopes: List(String),
) -> schema.Schema {
  schema.schema(
    query_type(conn, req, admin_dids, did_cache),
    Some(mutation_type(conn, req, admin_dids, jetstream_subject, did_cache, oauth_supported_scopes)),
  )
}
```

**Step 9: Update handler that calls build_schema**

In `server/src/handlers/client_graphql.gleam`, update the call to `build_schema` to pass supported scopes. This requires also updating the handler signature.

**Step 10: Build and fix any remaining issues**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 11: Commit**

```bash
git add server/src/client_schema.gleam server/src/handlers/client_graphql.gleam
git commit -m "feat(oauth): add scope argument to createOAuthClient mutation with validation"
```

---

### Task 5: Update updateOAuthClient Mutation

**Files:**
- Modify: `server/src/client_schema.gleam:1186-1297` (updateOAuthClient mutation)

**Step 1: Add scope argument**

Add scope argument to updateOAuthClient (around line 1203):

```gleam
        schema.argument(
          "scope",
          schema.non_null(schema.string_type()),
          "OAuth scopes (space-separated)",
          None,
        ),
```

**Step 2: Update pattern match**

Update the case to match scope (around line 1217):

```gleam
                case
                  schema.get_argument(ctx, "clientId"),
                  schema.get_argument(ctx, "clientName"),
                  schema.get_argument(ctx, "redirectUris"),
                  schema.get_argument(ctx, "scope")
                {
                  Some(value.String(client_id)),
                    Some(value.String(name)),
                    Some(value.List(uris)),
                    Some(value.String(scope))
                  -> {
```

**Step 3: Add scope validation**

After redirect URI validation succeeds (around line 1264), add scope validation:

```gleam
                                case validate_scope_against_supported(scope, oauth_supported_scopes) {
                                  Error(err) -> Error(err)
                                  Ok(_) -> {
```

**Step 4: Update client record with scope**

In the updated OAuthClient (around line 1265):

```gleam
                                let updated =
                                  types.OAuthClient(
                                    ..existing,
                                    client_name: trimmed_name,
                                    redirect_uris: redirect_uris,
                                    scope: case string.trim(scope) {
                                      "" -> None
                                      s -> Some(s)
                                    },
                                    updated_at: token_generator.current_timestamp(),
                                  )
```

**Step 5: Build to verify**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam build`
Expected: Compiles successfully

**Step 6: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat(oauth): add scope argument to updateOAuthClient mutation"
```

---

### Task 6: Update Client UI - Model and Msg Types

**Files:**
- Modify: `client/src/pages/settings.gleam:92-114` (Msg type)
- Modify: `client/src/pages/settings.gleam:116-134` (Model type)
- Modify: `client/src/pages/settings.gleam:152-169` (init function)

**Step 1: Add Msg variants**

Add after `UpdateNewClientRedirectUris(String)` (around line 103):

```gleam
  UpdateNewClientScope(String)
```

Add after `UpdateEditClientRedirectUris(String)` (around line 108):

```gleam
  UpdateEditClientScope(String)
```

**Step 2: Add Model fields**

Add after `new_client_redirect_uris: String,` (around line 126):

```gleam
    new_client_scope: String,
```

Add after `edit_client_redirect_uris: String,` (around line 129):

```gleam
    edit_client_scope: String,
```

**Step 3: Update init function**

Add after `new_client_redirect_uris: "",` (around line 161):

```gleam
    new_client_scope: "atproto transition:generic",
```

Add after `edit_client_redirect_uris: "",` (around line 164):

```gleam
    edit_client_scope: "",
```

**Step 4: Build client to verify**

Run: `cd /Users/chadmiller/code/quickslice/client && gleam build`
Expected: Compile errors about unhandled Msg variants (expected)

**Step 5: Commit**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(oauth): add scope fields to client Model and Msg types"
```

---

### Task 7: Update Client UI - Form Views

**Files:**
- Modify: `client/src/pages/settings.gleam:470-520` (new_client_form)
- Modify: `client/src/pages/settings.gleam:605-664` (edit_client_form)
- Modify: `client/src/pages/settings.gleam:522-603` (oauth_client_card)

**Step 1: Add scope input to new_client_form**

After the redirect URIs div (around line 500), add:

```gleam
      // Scope input
      html.div([], [
        html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
          element.text("Scope"),
        ]),
        html.input([
          attribute.type_("text"),
          attribute.class("font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full"),
          attribute.placeholder("atproto transition:generic"),
          attribute.value(model.new_client_scope),
          event.on_input(UpdateNewClientScope),
        ]),
        html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
          element.text("Space-separated OAuth scopes"),
        ]),
      ]),
```

**Step 2: Add scope input to edit_client_form**

After redirect URIs textarea in edit form (around line 643), add:

```gleam
      // Scope input
      html.div([], [
        html.label([attribute.class("block text-sm text-zinc-400 mb-1")], [
          element.text("Scope"),
        ]),
        html.input([
          attribute.type_("text"),
          attribute.class("font-mono px-3 py-2 text-sm text-zinc-300 bg-zinc-800 border border-zinc-700 rounded w-full"),
          attribute.value(model.edit_client_scope),
          event.on_input(UpdateEditClientScope),
        ]),
      ]),
```

**Step 3: Display scope in oauth_client_card**

After redirect URIs display (around line 599), add:

```gleam
        // Scope
        case client.scope {
          Some(scope) ->
            html.div([attribute.class("mt-2")], [
              html.span([attribute.class("text-xs text-zinc-500")], [element.text("Scope: ")]),
              html.code([attribute.class("text-xs text-zinc-400 font-mono")], [
                element.text(scope),
              ]),
            ])
          None -> element.none()
        },
```

**Step 4: Build client**

Run: `cd /Users/chadmiller/code/quickslice/client && gleam build`
Expected: Still has errors about unhandled Msg and missing scope in OAuthClient type

**Step 5: Commit partial progress**

```bash
git add client/src/pages/settings.gleam
git commit -m "feat(oauth): add scope input fields to client forms"
```

---

### Task 8: Update GraphQL Queries in Client

**Files:**
- Modify: `client/src/pages/settings.gleam:36-78` (GraphQL query docstrings)
- Modify: `client/src/generated/queries/get_o_auth_clients.gleam` (add scope field)
- Modify: `client/src/generated/queries/create_o_auth_client.gleam` (add scope argument)
- Modify: `client/src/generated/queries/update_o_auth_client.gleam` (add scope argument)

**Step 1: Update GetOAuthClients query docstring**

Update the GraphQL query docstring (lines 36-46) to include scope:

```gleam
/// ```graphql
/// query GetOAuthClients {
///   oauthClients {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
```

**Step 2: Update CreateOAuthClient mutation docstring**

Update lines 49-58 to include scope argument:

```gleam
/// ```graphql
/// mutation CreateOAuthClient($clientName: String!, $clientType: String!, $redirectUris: [String!]!, $scope: String!) {
///   createOAuthClient(clientName: $clientName, clientType: $clientType, redirectUris: $redirectUris, scope: $scope) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
```

**Step 3: Update UpdateOAuthClient mutation docstring**

Update lines 62-72 to include scope:

```gleam
/// ```graphql
/// mutation UpdateOAuthClient($clientId: String!, $clientName: String!, $redirectUris: [String!]!, $scope: String!) {
///   updateOAuthClient(clientId: $clientId, clientName: $clientName, redirectUris: $redirectUris, scope: $scope) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     scope
///     createdAt
///   }
/// }
/// ```
```

**Step 4: Update generated query files**

These files are in `client/src/generated/queries/`. Add scope field to OAuthClient type and mutation arguments. (The client uses code generation - may need to regenerate or manually update.)

**Step 5: Build and test**

Run: `cd /Users/chadmiller/code/quickslice && make build`
Expected: Full build succeeds

**Step 6: Commit**

```bash
git add client/
git commit -m "feat(oauth): update GraphQL queries to include scope field"
```

---

### Task 9: Final Integration and Testing

**Step 1: Run full build**

Run: `cd /Users/chadmiller/code/quickslice && make build`
Expected: Compiles successfully

**Step 2: Start server**

Run: `cd /Users/chadmiller/code/quickslice/server && gleam run`
Expected: Server starts without errors

**Step 3: Manual test - create client with valid scope**

1. Navigate to Settings page
2. Click "Register New Client"
3. Fill in name, redirect URI, leave scope as default "atproto transition:generic"
4. Click Create
5. Expected: Client created successfully, scope displayed

**Step 4: Manual test - create client with invalid scope**

1. Click "Register New Client"
2. Fill in name, redirect URI, change scope to "invalid_scope"
3. Click Create
4. Expected: Error message "Unsupported scope(s): invalid_scope"

**Step 5: Manual test - edit client scope**

1. Click Edit on existing client
2. Change scope value
3. Click Save
4. Expected: Scope updated, validation applied

**Step 6: Final commit**

```bash
git add .
git commit -m "feat(oauth): complete scope input implementation with validation"
```

---

## Environment Variable

Add to `.env` or deployment config:

```bash
# Space-separated list of supported OAuth scopes
OAUTH_SUPPORTED_SCOPES="atproto transition:generic"
```

Default if not set: `["atproto", "transition:generic"]`
