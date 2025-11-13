/// ```graphql
/// mutation TriggerBackfill {
///   triggerBackfill
/// }
/// ```
/// ```graphql
/// query GetCurrentSession {
///   currentSession {
///     did
///     handle
///     isAdmin
///   }
/// }
/// ```
import components/layout
import file_upload
import generated/queries
import generated/queries/get_activity_buckets.{ONEDAY}
import generated/queries/get_current_session
import generated/queries/get_lexicons
import generated/queries/get_recent_activity
import generated/queries/get_settings
import generated/queries/get_statistics
import generated/queries/reset_all
import generated/queries/trigger_backfill
import generated/queries/update_domain_authority
import generated/queries/upload_lexicons
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/uri
import lustre
import lustre/attribute
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import modem
import navigation
import pages/home
import pages/lexicons
import pages/settings
import squall/unstable_registry as registry
import squall_cache

@external(javascript, "./quickslice_client.ffi.mjs", "getWindowOrigin")
fn window_origin() -> String

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}

// MODEL

pub type Route {
  Home
  Settings
  Lexicons
  Upload
}

pub type AuthState {
  NotAuthenticated
  Authenticated(did: String, handle: String, is_admin: Bool)
}

pub type Model {
  Model(
    cache: squall_cache.Cache,
    registry: registry.Registry,
    route: Route,
    time_range: get_activity_buckets.TimeRange,
    settings_page_model: settings.Model,
    is_backfilling: Bool,
    auth_state: AuthState,
  )
}

fn init(_flags) -> #(Model, Effect(Msg)) {
  let api_url = window_origin() <> "/admin/graphql"
  let cache = squall_cache.new(api_url)

  // Initialize registry with all extracted queries
  let reg = queries.init_registry()

  // Parse the initial route from the current URL
  let initial_route = case modem.initial_uri() {
    Ok(uri) -> parse_route(uri)
    Error(_) -> Home
  }

  // Fetch current session first (needed for all routes)
  let #(cache_with_session, _) =
    squall_cache.lookup(
      cache,
      "GetCurrentSession",
      json.object([]),
      get_current_session.parse_get_current_session_response,
    )

  // Trigger initial data fetches for the route
  let #(initial_cache, data_effects) = case initial_route {
    Home -> {
      // GetStatistics query
      let #(cache1, _) =
        squall_cache.lookup(
          cache_with_session,
          "GetStatistics",
          json.object([]),
          get_statistics.parse_get_statistics_response,
        )

      // GetSettings query (for configuration alerts)
      let #(cache2, _) =
        squall_cache.lookup(
          cache1,
          "GetSettings",
          json.object([]),
          get_settings.parse_get_settings_response,
        )

      // GetActivityBuckets query
      let #(cache3, _) =
        squall_cache.lookup(
          cache2,
          "GetActivityBuckets",
          json.object([#("range", json.string("ONE_DAY"))]),
          get_activity_buckets.parse_get_activity_buckets_response,
        )

      // GetRecentActivity query
      let #(cache4, _) =
        squall_cache.lookup(
          cache3,
          "GetRecentActivity",
          json.object([#("hours", json.int(24))]),
          get_recent_activity.parse_get_recent_activity_response,
        )

      // Process all pending fetches
      let #(final_cache, fx) =
        squall_cache.process_pending(cache4, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Settings -> {
      // GetSettings query
      let #(cache1, _) =
        squall_cache.lookup(
          cache_with_session,
          "GetSettings",
          json.object([]),
          get_settings.parse_get_settings_response,
        )

      // Process pending fetches
      let #(final_cache, fx) =
        squall_cache.process_pending(cache1, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Lexicons -> {
      // GetLexicons query
      let #(cache1, _) =
        squall_cache.lookup(
          cache_with_session,
          "GetLexicons",
          json.object([]),
          get_lexicons.parse_get_lexicons_response,
        )

      // Process pending fetches
      let #(final_cache, fx) =
        squall_cache.process_pending(cache1, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    _ -> #(cache_with_session, [])
  }

  // Combine modem effect with data fetching effects
  let modem_effect = modem.init(on_url_change)
  let combined_effects = effect.batch([modem_effect, ..data_effects])

  #(
    Model(
      cache: initial_cache,
      registry: reg,
      route: initial_route,
      time_range: ONEDAY,
      settings_page_model: settings.init(),
      is_backfilling: False,
      auth_state: NotAuthenticated,
    ),
    combined_effects,
  )
}

// UPDATE

pub type Msg {
  HandleQueryResponse(String, Json, Result(String, String))
  HandleOptimisticMutationSuccess(String, String)
  HandleOptimisticMutationFailure(String, String)
  OnRouteChange(Route)
  HomePageMsg(home.Msg)
  SettingsPageMsg(settings.Msg)
  LexiconsPageMsg(lexicons.Msg)
  FileRead(Result(String, String))
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    HandleOptimisticMutationSuccess(mutation_id, response_body) -> {
      // Mutation succeeded - commit the optimistic update
      let cache_after_commit =
        squall_cache.commit_optimistic(model.cache, mutation_id, response_body)

      let new_settings_model =
        settings.set_alert(
          model.settings_page_model,
          "success",
          "Domain authority updated successfully",
        )

      #(
        Model(
          ..model,
          cache: cache_after_commit,
          settings_page_model: new_settings_model,
        ),
        effect.none(),
      )
    }

    HandleOptimisticMutationFailure(mutation_id, error_message) -> {
      // Mutation failed - rollback the optimistic update
      let cache_after_rollback =
        squall_cache.rollback_optimistic(model.cache, mutation_id)

      // Get the actual saved value from cache to reset the input field
      let saved_domain_authority = case
        squall_cache.lookup(
          cache_after_rollback,
          "GetSettings",
          json.object([]),
          get_settings.parse_get_settings_response,
        )
      {
        #(_, squall_cache.Data(data)) -> data.settings.domain_authority
        _ -> model.settings_page_model.domain_authority_input
      }

      let new_settings_model =
        settings.Model(
          ..model.settings_page_model,
          domain_authority_input: saved_domain_authority,
          alert: option.Some(#("error", "Error: " <> error_message)),
        )

      #(
        Model(
          ..model,
          cache: cache_after_rollback,
          settings_page_model: new_settings_model,
        ),
        effect.none(),
      )
    }

    HandleQueryResponse(query_name, variables, Ok(response_body)) -> {
      // Store response in cache
      let cache_with_data =
        squall_cache.store_query(
          model.cache,
          query_name,
          variables,
          response_body,
          0,
        )

      // Process any new pending fetches
      let #(final_cache, effects) =
        squall_cache.process_pending(
          cache_with_data,
          model.registry,
          HandleQueryResponse,
          fn() { 0 },
        )

      // Reset is_backfilling flag for TriggerBackfill mutation
      let updated_is_backfilling = case query_name {
        "TriggerBackfill" -> False
        _ -> model.is_backfilling
      }

      // Update auth state when GetCurrentSession response arrives
      let new_auth_state = case query_name {
        "GetCurrentSession" -> {
          case
            get_current_session.parse_get_current_session_response(
              response_body,
            )
          {
            Ok(data) -> {
              case data.current_session {
                option.Some(session) ->
                  Authenticated(
                    did: session.did,
                    handle: session.handle,
                    is_admin: session.is_admin,
                  )
                option.None -> NotAuthenticated
              }
            }
            Error(_) -> NotAuthenticated
          }
        }
        _ -> model.auth_state
      }

      // Show success message for mutations and populate settings data
      let new_settings_model = case query_name {
        "UpdateDomainAuthority" ->
          settings.set_alert(
            model.settings_page_model,
            "success",
            "Domain authority updated successfully",
          )
        "UploadLexicons" -> {
          // Clear the file input so the same file can be uploaded again
          file_upload.clear_file_input("lexicon-file-input")
          settings.set_alert(
            model.settings_page_model,
            "success",
            "Lexicons uploaded successfully",
          )
        }
        "ResetAll" -> {
          // Clear the domain authority input when reset completes
          let cleared_model =
            settings.Model(
              ..model.settings_page_model,
              domain_authority_input: "",
            )
          settings.set_alert(
            cleared_model,
            "success",
            "All data has been reset",
          )
        }
        "GetSettings" -> {
          // Populate the input field with the loaded domain authority
          case get_settings.parse_get_settings_response(response_body) {
            Ok(data) ->
              settings.Model(
                ..model.settings_page_model,
                domain_authority_input: data.settings.domain_authority,
              )
            Error(_) -> model.settings_page_model
          }
        }
        _ -> model.settings_page_model
      }

      // Invalidate queries after mutations that change data
      let #(cache_after_mutation, mutation_effects) = case query_name {
        "ResetAll" -> {
          // Invalidate home page queries so they refetch when navigating home
          let cache1 =
            squall_cache.invalidate(
              final_cache,
              "GetStatistics",
              json.object([]),
            )
          let cache2 =
            squall_cache.invalidate(
              cache1,
              "GetActivityBuckets",
              json.object([
                #(
                  "range",
                  json.string(get_activity_buckets.time_range_to_string(
                    model.time_range,
                  )),
                ),
              ]),
            )
          let cache3 =
            squall_cache.invalidate(
              cache2,
              "GetRecentActivity",
              json.object([#("hours", json.int(24))]),
            )

          // Refetch settings to keep the settings page working
          let #(cache_with_settings, _) =
            squall_cache.lookup(
              cache3,
              "GetSettings",
              json.object([]),
              get_settings.parse_get_settings_response,
            )

          let #(final_cache_reset, refetch_effects) =
            squall_cache.process_pending(
              cache_with_settings,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          #(final_cache_reset, refetch_effects)
        }
        "UploadLexicons" -> {
          // Invalidate statistics since lexicon count changed
          let cache_invalidated =
            squall_cache.invalidate(
              final_cache,
              "GetStatistics",
              json.object([]),
            )
          #(cache_invalidated, [])
        }
        _ -> #(final_cache, [])
      }

      // Check if we need to redirect after session loads
      let redirect_effect = case query_name {
        "GetCurrentSession" -> {
          // If we're on settings route but not admin, redirect to home
          case model.route, new_auth_state {
            Settings, NotAuthenticated -> [
              modem.push("/", option.None, option.None),
            ]
            Settings, Authenticated(_, _, False) -> [
              modem.push("/", option.None, option.None),
            ]
            _, _ -> []
          }
        }
        _ -> []
      }

      #(
        Model(
          ..model,
          cache: cache_after_mutation,
          settings_page_model: new_settings_model,
          is_backfilling: updated_is_backfilling,
          auth_state: new_auth_state,
        ),
        effect.batch(
          [effects, mutation_effects, redirect_effect] |> list.flatten,
        ),
      )
    }

    HandleQueryResponse(query_name, _variables, Error(err)) -> {
      // Reset is_backfilling flag for TriggerBackfill mutation
      let updated_is_backfilling = case query_name {
        "TriggerBackfill" -> False
        _ -> model.is_backfilling
      }

      // Show error message for mutations
      let new_settings_model = case query_name {
        "UpdateDomainAuthority"
        | "UploadLexicons"
        | "ResetAll"
        | "TriggerBackfill" ->
          settings.set_alert(
            model.settings_page_model,
            "error",
            "Error: " <> err,
          )
        _ -> model.settings_page_model
      }

      #(
        Model(
          ..model,
          settings_page_model: new_settings_model,
          is_backfilling: updated_is_backfilling,
        ),
        effect.none(),
      )
    }

    OnRouteChange(route) -> {
      // Clear any alerts when navigating away from settings
      let cleared_settings_model = case model.route {
        Settings -> settings.clear_alert(model.settings_page_model)
        _ -> model.settings_page_model
      }

      // When route changes, fetch data for that route
      case route {
        Home -> {
          // Fetch home page data
          // GetStatistics query
          let #(cache1, _) =
            squall_cache.lookup(
              model.cache,
              "GetStatistics",
              json.object([]),
              get_statistics.parse_get_statistics_response,
            )

          // GetSettings query (for configuration alerts)
          let #(cache2, _) =
            squall_cache.lookup(
              cache1,
              "GetSettings",
              json.object([]),
              get_settings.parse_get_settings_response,
            )

          // GetActivityBuckets query
          let #(cache3, _) =
            squall_cache.lookup(
              cache2,
              "GetActivityBuckets",
              json.object([
                #(
                  "range",
                  json.string(get_activity_buckets.time_range_to_string(
                    model.time_range,
                  )),
                ),
              ]),
              get_activity_buckets.parse_get_activity_buckets_response,
            )

          // GetRecentActivity query
          let #(cache4, _) =
            squall_cache.lookup(
              cache3,
              "GetRecentActivity",
              json.object([#("hours", json.int(24))]),
              get_recent_activity.parse_get_recent_activity_response,
            )

          // Process all pending fetches
          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache4,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          #(
            Model(
              ..model,
              route: route,
              cache: final_cache,
              settings_page_model: cleared_settings_model,
            ),
            effect.batch(effects),
          )
        }
        Settings -> {
          // Check if user is admin
          let is_admin = case model.auth_state {
            Authenticated(_, _, admin) -> admin
            NotAuthenticated -> False
          }

          case is_admin {
            False -> {
              // Non-admin trying to access settings - redirect to home
              #(model, modem.push("/", option.None, option.None))
            }
            True -> {
              // Fetch settings data
              let #(cache_with_lookup, _) =
                squall_cache.lookup(
                  model.cache,
                  "GetSettings",
                  json.object([]),
                  get_settings.parse_get_settings_response,
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
                  route: route,
                  cache: final_cache,
                  settings_page_model: cleared_settings_model,
                ),
                effect.batch(effects),
              )
            }
          }
        }
        Lexicons -> {
          // Fetch lexicons data
          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              model.cache,
              "GetLexicons",
              json.object([]),
              get_lexicons.parse_get_lexicons_response,
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
              route: route,
              cache: final_cache,
              settings_page_model: cleared_settings_model,
            ),
            effect.batch(effects),
          )
        }
        _ -> #(
          Model(
            ..model,
            route: route,
            settings_page_model: cleared_settings_model,
          ),
          effect.none(),
        )
      }
    }

    HomePageMsg(home_msg) -> {
      case home_msg {
        home.ChangeTimeRange(new_range) -> {
          // Update time range and fetch new activity data
          let variables =
            json.object([
              #(
                "range",
                json.string(get_activity_buckets.time_range_to_string(new_range)),
              ),
            ])

          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              model.cache,
              "GetActivityBuckets",
              variables,
              get_activity_buckets.parse_get_activity_buckets_response,
            )

          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache_with_lookup,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          #(
            Model(..model, cache: final_cache, time_range: new_range),
            effect.batch(effects),
          )
        }

        home.OpenGraphiQL -> {
          // Navigate to external GraphiQL page
          navigation.navigate_to_external("/graphiql")
          #(model, effect.none())
        }

        home.TriggerBackfill -> {
          // Trigger backfill mutation
          let variables = json.object([])

          // Invalidate any cached mutation result to ensure a fresh request
          let cache_invalidated =
            squall_cache.invalidate(model.cache, "TriggerBackfill", variables)

          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              cache_invalidated,
              "TriggerBackfill",
              variables,
              trigger_backfill.parse_trigger_backfill_response,
            )

          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache_with_lookup,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          // Set is_backfilling to True while request is pending
          #(
            Model(..model, cache: final_cache, is_backfilling: True),
            effect.batch(effects),
          )
        }
      }
    }

    SettingsPageMsg(settings_msg) -> {
      case settings_msg {
        settings.UpdateDomainAuthorityInput(value) -> {
          // Clear alert when user starts typing
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              domain_authority_input: value,
              alert: None,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.SubmitDomainAuthority -> {
          // Clear any existing alert
          let cleared_settings_model =
            settings.Model(..model.settings_page_model, alert: None)

          // Execute optimistic mutation
          let variables =
            json.object([
              #(
                "domainAuthority",
                json.string(model.settings_page_model.domain_authority_input),
              ),
            ])

          // Create optimistic entity - get current oauthClientId from cache
          let current_oauth_client_id = case
            squall_cache.lookup(
              model.cache,
              "GetSettings",
              json.object([]),
              get_settings.parse_get_settings_response,
            )
          {
            #(_, squall_cache.Data(data)) -> data.settings.oauth_client_id
            _ -> None
          }

          let optimistic_entity =
            json.object([
              #("id", json.string("Settings:singleton")),
              #(
                "domainAuthority",
                json.string(model.settings_page_model.domain_authority_input),
              ),
              #(
                "oauthClientId",
                json.nullable(current_oauth_client_id, json.string),
              ),
            ])

          let #(updated_cache, _mutation_id, mutation_effect) =
            squall_cache.execute_optimistic_mutation(
              model.cache,
              model.registry,
              "UpdateDomainAuthority",
              variables,
              "Settings:singleton",
              fn(_current) { optimistic_entity },
              update_domain_authority.parse_update_domain_authority_response,
              fn(mutation_id, result, response_body) {
                case result {
                  Ok(_) ->
                    HandleOptimisticMutationSuccess(mutation_id, response_body)
                  Error(err) ->
                    HandleOptimisticMutationFailure(mutation_id, err)
                }
              },
            )

          #(
            Model(
              ..model,
              cache: updated_cache,
              settings_page_model: cleared_settings_model,
            ),
            mutation_effect,
          )
        }

        settings.SelectLexiconFile -> {
          // File selection is handled by browser - we'll read the file on upload
          #(model, effect.none())
        }

        settings.UploadLexicons -> {
          // Read the file and convert to base64
          io.println("[UploadLexicons] Button clicked, creating file effect")
          let file_effect =
            effect.from(fn(dispatch) {
              io.println(
                "[UploadLexicons] Effect running, calling read_file_as_base64",
              )
              file_upload.read_file_as_base64("lexicon-file-input", fn(result) {
                io.println("[UploadLexicons] Callback received result")
                dispatch(FileRead(result))
              })
            })
          #(model, file_effect)
        }

        settings.UpdateResetConfirmation(value) -> {
          // Clear alert when user starts typing
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              reset_confirmation: value,
              alert: None,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.SubmitReset -> {
          // Execute ResetAll mutation
          let variables =
            json.object([
              #(
                "confirm",
                json.string(model.settings_page_model.reset_confirmation),
              ),
            ])

          // Invalidate any cached mutation result to ensure a fresh request
          let cache_invalidated =
            squall_cache.invalidate(model.cache, "ResetAll", variables)

          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              cache_invalidated,
              "ResetAll",
              variables,
              reset_all.parse_reset_all_response,
            )

          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache_with_lookup,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          // Clear the confirmation field and alert after submission
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              reset_confirmation: "",
              alert: None,
            )

          #(
            Model(
              ..model,
              cache: final_cache,
              settings_page_model: new_settings_model,
            ),
            effect.batch(effects),
          )
        }
      }
    }

    FileRead(Ok(base64_content)) -> {
      // File was successfully read, now upload it
      io.println("[FileRead] Successfully read file, uploading...")
      let variables = json.object([#("zipBase64", json.string(base64_content))])

      // Invalidate any cached mutation result to ensure a fresh request
      let cache_invalidated =
        squall_cache.invalidate(model.cache, "UploadLexicons", variables)

      let #(cache_with_lookup, _) =
        squall_cache.lookup(
          cache_invalidated,
          "UploadLexicons",
          variables,
          upload_lexicons.parse_upload_lexicons_response,
        )

      let #(final_cache, effects) =
        squall_cache.process_pending(
          cache_with_lookup,
          model.registry,
          HandleQueryResponse,
          fn() { 0 },
        )

      // Clear the selected file
      let new_settings_model =
        settings.Model(..model.settings_page_model, selected_file: None)

      #(
        Model(
          ..model,
          cache: final_cache,
          settings_page_model: new_settings_model,
        ),
        effect.batch(effects),
      )
    }

    LexiconsPageMsg(msg) -> {
      let eff = lexicons.update(msg)
      #(model, effect.map(eff, LexiconsPageMsg))
    }

    FileRead(Error(err)) -> {
      // Handle file read error
      io.println("[FileRead] Error reading file: " <> err)
      let new_settings_model =
        settings.set_alert(model.settings_page_model, "error", err)
      #(Model(..model, settings_page_model: new_settings_model), effect.none())
    }
  }
}

// VIEW

fn view(model: Model) -> Element(Msg) {
  // Convert AuthState to Option for layout
  let auth_info = case model.auth_state {
    NotAuthenticated -> None
    Authenticated(_did, handle, is_admin) -> option.Some(#(handle, is_admin))
  }

  html.div(
    [attribute.class("bg-zinc-950 text-zinc-300 font-mono min-h-screen")],
    [
      html.div([attribute.class("max-w-4xl mx-auto px-6 py-12")], [
        layout.header(auth_info),
        case model.route {
          Home -> view_home(model)
          Settings -> view_settings(model)
          Lexicons -> view_lexicons(model)
          Upload -> view_upload(model)
        },
      ]),
    ],
  )
}

fn view_home(model: Model) -> Element(Msg) {
  let is_admin = case model.auth_state {
    Authenticated(_, _, is_admin) -> is_admin
    NotAuthenticated -> False
  }

  element.map(
    home.view(model.cache, model.time_range, model.is_backfilling, is_admin),
    HomePageMsg,
  )
}

fn view_settings(model: Model) -> Element(Msg) {
  let is_admin = case model.auth_state {
    Authenticated(_, _, is_admin) -> is_admin
    NotAuthenticated -> False
  }

  element.map(
    settings.view(model.cache, model.settings_page_model, is_admin),
    SettingsPageMsg,
  )
}

fn view_lexicons(model: Model) -> Element(Msg) {
  element.map(lexicons.view(model.cache), LexiconsPageMsg)
}

fn view_upload(_model: Model) -> Element(Msg) {
  html.div([], [
    html.h1([attribute.class("text-xl font-bold text-zinc-100 mb-4")], [
      html.text("Upload"),
    ]),
    html.p([attribute.class("text-zinc-400")], [
      html.text("Upload and manage data"),
    ]),
  ])
}

// ROUTING

fn on_url_change(uri: uri.Uri) -> Msg {
  OnRouteChange(parse_route(uri))
}

fn parse_route(uri: uri.Uri) -> Route {
  case uri.path {
    "/" -> Home
    "/settings" -> Settings
    "/lexicons" -> Lexicons
    "/upload" -> Upload
    _ -> Home
  }
}
