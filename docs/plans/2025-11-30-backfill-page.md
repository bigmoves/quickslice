# Backfill Actor Page Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a `/backfill` page in the client that allows any authenticated user to trigger a CAR file repoSync for a specific actor DID.

**Architecture:** New GraphQL mutation `backfillActor(did: String!)` that calls existing `backfill_collections_for_actor()` function. New Lustre page component following the settings page pattern with DID input and submit button.

**Tech Stack:** Gleam, Lustre (client), Swell (GraphQL), squall_cache (client GraphQL)

---

## Task 1: Add GraphQL Mutation

**Files:**
- Modify: `server/src/client_schema.gleam` (after line ~1127, after triggerBackfill mutation)

**Step 1: Add backfillActor mutation to mutation_type function**

Add after the `triggerBackfill` mutation block (around line 1127):

```gleam
    // backfillActor mutation - sync a specific actor's collections
    schema.field_with_args(
      "backfillActor",
      schema.non_null(schema.boolean_type()),
      "Trigger a background backfill for a specific actor's collections",
      [
        schema.argument(
          "did",
          schema.non_null(schema.string_type()),
          "The DID of the actor to backfill",
          None,
        ),
      ],
      fn(ctx) {
        // Check if user is authenticated (any logged-in user can trigger)
        case session.get_current_session(req, conn, did_cache) {
          Ok(_sess) -> {
            case schema.get_argument(ctx, "did") {
              Some(value.String(did)) -> {
                // Get all record-type collections from database
                let collections = case lexicons.get_record_types(conn) {
                  Ok(lexicon_list) ->
                    list.map(lexicon_list, fn(lex) { lex.id })
                  Error(_) -> []
                }

                // Get domain authority to determine external collections
                let domain_authority = case
                  config_repo.get(conn, "domain_authority")
                {
                  Ok(authority) -> authority
                  Error(_) -> ""
                }

                // Split collections into primary and external
                let #(primary_collections, external_collections) =
                  list.partition(collections, fn(collection) {
                    backfill.nsid_matches_domain_authority(
                      collection,
                      domain_authority,
                    )
                  })

                // Get PLC URL from environment
                let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
                  Ok(url) -> url
                  Error(_) -> "https://plc.directory"
                }

                // Spawn background process to run backfill for this actor
                process.spawn_unlinked(fn() {
                  logging.log(
                    logging.Info,
                    "[backfillActor] Starting background backfill for " <> did,
                  )

                  backfill.backfill_collections_for_actor(
                    conn,
                    did,
                    primary_collections,
                    external_collections,
                    plc_url,
                  )

                  logging.log(
                    logging.Info,
                    "[backfillActor] Background backfill completed for " <> did,
                  )
                })

                // Return immediately
                Ok(value.Boolean(True))
              }
              _ -> Error("DID argument is required")
            }
          }
          Error(_) -> Error("Authentication required to trigger backfill")
        }
      },
    ),
```

**Step 2: Add envoy import if not present**

Check if `envoy` is imported at the top of the file. If not, add:

```gleam
import envoy
```

**Step 3: Build and verify**

Run: `gleam build`
Expected: Build succeeds

**Step 4: Commit**

```bash
git add server/src/client_schema.gleam
git commit -m "feat: add backfillActor GraphQL mutation"
```

---

## Task 2: Add Client Query Parser

**Files:**
- Create: `client/src/generated/queries/backfill_actor.gleam`

**Step 1: Create the parser module**

```gleam
import gleam/dynamic/decode
import gleam/http/request.{type Request}
import gleam/json
import squall

pub type BackfillActorResponse {
  BackfillActorResponse(backfill_actor: Bool)
}

pub fn backfill_actor_response_decoder() -> decode.Decoder(
  BackfillActorResponse,
) {
  use backfill_actor <- decode.field("backfillActor", decode.bool)
  decode.success(BackfillActorResponse(backfill_actor: backfill_actor))
}

pub fn backfill_actor_response_to_json(
  input: BackfillActorResponse,
) -> json.Json {
  json.object([#("backfillActor", json.bool(input.backfill_actor))])
}

pub fn backfill_actor(
  client: squall.Client,
  did: String,
) -> Result(Request(String), String) {
  squall.prepare_request(
    client,
    "mutation BackfillActor($did: String!) {\n  backfillActor(did: $did)\n}",
    json.object([#("did", json.string(did))]),
  )
}

pub fn parse_backfill_actor_response(
  body: String,
) -> Result(BackfillActorResponse, String) {
  squall.parse_response(body, backfill_actor_response_decoder())
}
```

**Step 2: Build and verify**

Run: `gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add client/src/generated/queries/backfill_actor.gleam
git commit -m "feat: add backfillActor query parser"
```

---

## Task 3: Register Query in Registry

**Files:**
- Modify: `client/src/generated/queries.gleam`

**Step 1: Add registration for BackfillActor**

Find the `init_registry` function and add a new registration (after the existing registrations):

```gleam
  let reg =
    unstable_registry.register(
      reg,
      "BackfillActor",
      "mutation BackfillActor($did: String!) {\n  backfillActor(did: $did)\n}",
      "generated/queries/backfill_actor",
    )
```

**Step 2: Build and verify**

Run: `gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add client/src/generated/queries.gleam
git commit -m "feat: register BackfillActor mutation in query registry"
```

---

## Task 4: Create Backfill Page Component

**Files:**
- Create: `client/src/pages/backfill.gleam`

**Step 1: Create the page module**

```gleam
/// Backfill Page Component
///
/// Allows authenticated users to trigger a backfill for a specific actor DID
///
/// ```graphql
/// mutation BackfillActor($did: String!) {
///   backfillActor(did: $did)
/// }
/// ```
import components/alert
import components/button
import gleam/option.{type Option, None, Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

pub type Msg {
  UpdateDidInput(String)
  SubmitBackfill
}

pub type Model {
  Model(
    did_input: String,
    is_submitting: Bool,
    alert: Option(#(String, String)),
  )
}

pub fn init() -> Model {
  Model(did_input: "", is_submitting: False, alert: None)
}

pub fn set_alert(model: Model, kind: String, message: String) -> Model {
  Model(..model, alert: Some(#(kind, message)))
}

pub fn clear_alert(model: Model) -> Model {
  Model(..model, alert: None)
}

pub fn set_submitting(model: Model, submitting: Bool) -> Model {
  Model(..model, is_submitting: submitting)
}

pub fn view(model: Model) -> Element(Msg) {
  let input_classes =
    "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full"
  let label_classes = "block text-sm font-medium text-zinc-400 mb-2"

  html.div([attribute.class("space-y-6")], [
    // Page header
    html.div([], [
      html.h1([attribute.class("text-xl font-bold text-zinc-100 mb-2")], [
        html.text("Backfill Actor"),
      ]),
      html.p([attribute.class("text-zinc-400 text-sm")], [
        html.text(
          "Sync all collections for a specific actor via CAR file repository sync.",
        ),
      ]),
    ]),
    // Alert
    case model.alert {
      Some(#(kind, message)) -> {
        let alert_kind = case kind {
          "success" -> alert.Success
          "error" -> alert.Error
          _ -> alert.Info
        }
        alert.alert(alert_kind, message)
      }
      None -> element.none()
    },
    // Form card
    html.div(
      [
        attribute.class(
          "bg-zinc-900/50 border border-zinc-800 rounded-lg p-6 max-w-xl",
        ),
      ],
      [
        html.div([attribute.class("mb-4")], [
          html.label([attribute.class(label_classes)], [
            html.text("Actor DID"),
          ]),
          html.input([
            attribute.type_("text"),
            attribute.class(input_classes),
            attribute.placeholder("did:plc:... or did:web:..."),
            attribute.value(model.did_input),
            event.on_input(UpdateDidInput),
          ]),
          html.p([attribute.class("text-xs text-zinc-500 mt-1")], [
            html.text(
              "Enter the DID of the actor whose records you want to sync.",
            ),
          ]),
        ]),
        html.div([attribute.class("flex items-center gap-4")], [
          button.button(
            disabled: model.is_submitting
              || model.did_input == "",
            on_click: SubmitBackfill,
            text: case model.is_submitting {
              True -> "Syncing..."
              False -> "Sync Actor"
            },
          ),
        ]),
      ],
    ),
  ])
}
```

**Step 2: Build and verify**

Run: `gleam build`
Expected: Build succeeds

**Step 3: Commit**

```bash
git add client/src/pages/backfill.gleam
git commit -m "feat: add backfill page component"
```

---

## Task 5: Wire Up Routing and Messages

**Files:**
- Modify: `client/src/quickslice_client.gleam`

**Step 1: Add import for backfill page and query parser**

Add to the imports section (around line 80-100):

```gleam
import pages/backfill
import generated/queries/backfill_actor
```

**Step 2: Add Backfill to Route type**

Find `pub type Route {` (around line 122) and add:

```gleam
pub type Route {
  Home
  Settings
  Lexicons
  Upload
  Backfill
}
```

**Step 3: Add backfill_page_model to Model**

Find `pub type Model {` (around line 134) and add after `settings_page_model`:

```gleam
    backfill_page_model: backfill.Model,
```

**Step 4: Add BackfillPageMsg to Msg type**

Find the `pub type Msg {` definition (around line 280) and add:

```gleam
  BackfillPageMsg(backfill.Msg)
```

**Step 5: Initialize backfill_page_model in init function**

Find the Model construction in the `init` function and add:

```gleam
    backfill_page_model: backfill.init(),
```

**Step 6: Update parse_route function**

Find `fn parse_route` (around line 1856) and add the backfill route:

```gleam
fn parse_route(uri: uri.Uri) -> Route {
  case uri.path {
    "/" -> Home
    "/settings" -> Settings
    "/lexicons" -> Lexicons
    "/upload" -> Upload
    "/backfill" -> Backfill
    _ -> Home
  }
}
```

**Step 7: Handle OnRouteChange for Backfill**

In the update function, find the `OnRouteChange(route)` case (around line 785). Add handling for Backfill route - redirect if not authenticated:

In the route-specific handling section (after line 880), add:

```gleam
        Backfill -> {
          // Check if user is authenticated
          case model.auth_state {
            NotAuthenticated -> {
              // Not authenticated - redirect to home
              #(model, modem.push("/", option.None, option.None))
            }
            Authenticated(_, _, _) -> {
              // Authenticated - clear alert and stay on page
              let new_backfill_model = backfill.clear_alert(model.backfill_page_model)
              #(
                Model(
                  ..model,
                  route: Backfill,
                  backfill_page_model: new_backfill_model,
                  mobile_menu_open: False,
                ),
                effect.none(),
              )
            }
          }
        }
```

**Step 8: Handle BackfillPageMsg in update**

Add a new case for BackfillPageMsg (after SettingsPageMsg handling, around line 1030):

```gleam
    BackfillPageMsg(backfill_msg) -> {
      case backfill_msg {
        backfill.UpdateDidInput(value) -> {
          let new_backfill_model =
            backfill.Model(
              ..model.backfill_page_model,
              did_input: value,
            )
            |> backfill.clear_alert
          #(
            Model(..model, backfill_page_model: new_backfill_model),
            effect.none(),
          )
        }

        backfill.SubmitBackfill -> {
          let did = model.backfill_page_model.did_input
          let variables = json.object([#("did", json.string(did))])

          // Mark as submitting
          let new_backfill_model =
            model.backfill_page_model
            |> backfill.set_submitting(True)
            |> backfill.clear_alert

          // Invalidate any cached result
          let cache_invalidated =
            squall_cache.invalidate(model.cache, "BackfillActor", variables)

          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              cache_invalidated,
              "BackfillActor",
              variables,
              backfill_actor.parse_backfill_actor_response,
            )

          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache_with_lookup,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          #(
            Model(
              ..model,
              cache: final_cache,
              backfill_page_model: new_backfill_model,
            ),
            effect.batch(effects),
          )
        }
      }
    }
```

**Step 9: Handle BackfillActor in HandleQueryResponse success**

Find the `HandleQueryResponse(query_name, variables, Ok(response_body))` case (around line 650) and add handling for BackfillActor success:

In the success handling section, add a case for "BackfillActor":

```gleam
        "BackfillActor" -> {
          let new_backfill_model =
            model.backfill_page_model
            |> backfill.set_submitting(False)
            |> backfill.set_alert(
              "success",
              "Backfill started for " <> model.backfill_page_model.did_input,
            )
            |> fn(m) { backfill.Model(..m, did_input: "") }
          #(
            Model(..model, cache: updated_cache, backfill_page_model: new_backfill_model),
            effect.none(),
          )
        }
```

**Step 10: Handle BackfillActor in HandleQueryResponse error**

Find the `HandleQueryResponse(query_name, _variables, Error(err))` case (around line 740) and add "BackfillActor" to the error handling:

```gleam
        "UpdateDomainAuthority"
        | "UploadLexicons"
        | "ResetAll"
        | "TriggerBackfill"
        | "BackfillActor" ->
```

And update the error handler to also reset backfill submitting state:

After the settings_page_model update, add:

```gleam
      let new_backfill_model = case query_name {
        "BackfillActor" ->
          model.backfill_page_model
          |> backfill.set_submitting(False)
          |> backfill.set_alert("error", "Error: " <> err)
        _ -> model.backfill_page_model
      }
```

And update the Model return to include `backfill_page_model: new_backfill_model`.

**Step 11: Add view_backfill function and route rendering**

Add after view_upload function (around line 1848):

```gleam
fn view_backfill(model: Model) -> Element(Msg) {
  element.map(backfill.view(model.backfill_page_model), BackfillPageMsg)
}
```

And update the main view function's route case (around line 1795) to include Backfill:

```gleam
            Home -> view_home(model)
            Settings -> view_settings(model)
            Lexicons -> view_lexicons(model)
            Upload -> view_upload(model)
            Backfill -> view_backfill(model)
```

**Step 12: Build and verify**

Run: `gleam build`
Expected: Build succeeds

**Step 13: Commit**

```bash
git add client/src/quickslice_client.gleam
git commit -m "feat: wire up /backfill route and page"
```

---

## Task 6: Add GraphQL Schema Comment

**Files:**
- Modify: `client/src/quickslice_client.gleam`

**Step 1: Add GraphQL comment at top of file**

Add the BackfillActor mutation to the GraphQL schema comments at the top of the file (after the TriggerBackfill comment):

```gleam
/// ```graphql
/// mutation BackfillActor($did: String!) {
///   backfillActor(did: $did)
/// }
/// ```
```

**Step 2: Commit**

```bash
git add client/src/quickslice_client.gleam
git commit -m "docs: add BackfillActor GraphQL schema comment"
```

---

## Task 7: Test End-to-End

**Step 1: Start the server**

Run: `gleam run`

**Step 2: Navigate to /backfill in browser**

1. Login with valid credentials
2. Navigate to `/backfill`
3. Enter a valid DID (e.g., `did:plc:z72i7hdynmk6r22z27h6tvur`)
4. Click "Sync Actor"
5. Verify success message appears
6. Check server logs for backfill activity

**Step 3: Test authentication redirect**

1. Logout
2. Navigate to `/backfill`
3. Verify redirect to home page

**Step 4: Final commit if any fixes needed**

```bash
git add -A
git commit -m "fix: backfill page adjustments"
```
