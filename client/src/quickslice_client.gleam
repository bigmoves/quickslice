/// ```graphql
/// mutation TriggerBackfill {
///   triggerBackfill
/// }
/// ```
/// ```graphql
/// query IsBackfilling {
///   isBackfilling
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
/// ```graphql
/// mutation CreateOAuthClient($clientName: String!, $clientType: String!, $redirectUris: [String!]!) {
///   createOAuthClient(clientName: $clientName, clientType: $clientType, redirectUris: $redirectUris) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     createdAt
///   }
/// }
/// ```
/// ```graphql
/// mutation UpdateOAuthClient($clientId: String!, $clientName: String!, $redirectUris: [String!]!) {
///   updateOAuthClient(clientId: $clientId, clientName: $clientName, redirectUris: $redirectUris) {
///     clientId
///     clientSecret
///     clientName
///     clientType
///     redirectUris
///     createdAt
///   }
/// }
/// ```
import backfill_polling
import components/layout
import file_upload
import generated/queries
import generated/queries/create_o_auth_client
import generated/queries/delete_o_auth_client
import generated/queries/get_activity_buckets.{ONEDAY}
import generated/queries/get_current_session
import generated/queries/get_lexicons
import generated/queries/get_o_auth_clients
import generated/queries/get_recent_activity
import generated/queries/get_settings
import generated/queries/get_statistics
import generated/queries/is_backfilling
import generated/queries/reset_all
import generated/queries/trigger_backfill
import generated/queries/update_domain_authority
import generated/queries/update_o_auth_client
import generated/queries/upload_lexicons
import gleam/dynamic/decode
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/set
import gleam/string
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

@external(javascript, "./quickslice_client.ffi.mjs", "setTimeout")
fn set_timeout(ms: Int, callback: fn() -> Nil) -> Nil

/// Extract the first error message from a GraphQL response body
/// Returns Some(message) if errors exist, None otherwise
fn extract_graphql_error(response_body: String) -> option.Option(String) {
  case json.parse(response_body, decode.dynamic) {
    Ok(parsed) -> {
      let error_decoder = {
        use errors <- decode.field(
          "errors",
          decode.list({
            use message <- decode.field("message", decode.string)
            decode.success(message)
          }),
        )
        decode.success(errors)
      }
      case decode.run(parsed, error_decoder) {
        Ok([first_error, ..]) -> option.Some(first_error)
        _ -> option.None
      }
    }
    Error(_) -> option.None
  }
}

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
    backfill_status: backfill_polling.BackfillStatus,
    auth_state: AuthState,
    mobile_menu_open: Bool,
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

  // Check if a backfill is already in progress (persists across page refresh)
  let #(cache_with_backfill_check, _) =
    squall_cache.lookup(
      cache_with_session,
      "IsBackfilling",
      json.object([]),
      is_backfilling.parse_is_backfilling_response,
    )

  // Trigger initial data fetches for the route
  let #(initial_cache, data_effects) = case initial_route {
    Home -> {
      // GetStatistics query
      let #(cache1, _) =
        squall_cache.lookup(
          cache_with_backfill_check,
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
          cache_with_backfill_check,
          "GetSettings",
          json.object([]),
          get_settings.parse_get_settings_response,
        )

      // GetOAuthClients query
      let #(cache2, _) =
        squall_cache.lookup(
          cache1,
          "GetOAuthClients",
          json.object([]),
          get_o_auth_clients.parse_get_o_auth_clients_response,
        )

      // Process pending fetches
      let #(final_cache, fx) =
        squall_cache.process_pending(cache2, reg, HandleQueryResponse, fn() {
          0
        })
      #(final_cache, fx)
    }
    Lexicons -> {
      // GetLexicons query
      let #(cache1, _) =
        squall_cache.lookup(
          cache_with_backfill_check,
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
    _ -> #(cache_with_backfill_check, [])
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
      backfill_status: backfill_polling.Idle,
      auth_state: NotAuthenticated,
      mobile_menu_open: False,
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
  BackfillPollTick
  ToggleMobileMenu
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

      // Update backfill status when IsBackfilling response arrives
      let new_backfill_status = case query_name {
        "IsBackfilling" -> {
          case is_backfilling.parse_is_backfilling_response(response_body) {
            Ok(data) -> {
              case model.backfill_status, data.is_backfilling {
                // On init (Idle), if server says backfilling, go to InProgress
                backfill_polling.Idle, True -> backfill_polling.InProgress
                // Otherwise use the normal state machine
                _, _ ->
                  backfill_polling.update_status(
                    model.backfill_status,
                    data.is_backfilling,
                  )
              }
            }
            Error(_) -> model.backfill_status
          }
        }
        _ -> model.backfill_status
      }

      // Schedule next poll if:
      // 1. This is an IsBackfilling response
      // 2. We were already polling (not from init where we were Idle)
      // 3. We should continue polling
      let poll_effect = case
        query_name,
        backfill_polling.should_poll(model.backfill_status),
        backfill_polling.should_poll(new_backfill_status)
      {
        "IsBackfilling", True, True -> [
          effect.from(fn(dispatch) {
            set_timeout(10_000, fn() { dispatch(BackfillPollTick) })
          }),
        ]
        // Coming from Idle (init) and now InProgress - start the first poll
        "IsBackfilling", False, True -> [
          effect.from(fn(dispatch) {
            set_timeout(10_000, fn() { dispatch(BackfillPollTick) })
          }),
        ]
        _, _, _ -> []
      }

      // When backfill completes, invalidate home page queries to refresh data
      let backfill_just_completed = case
        model.backfill_status,
        new_backfill_status
      {
        backfill_polling.InProgress, backfill_polling.Completed -> True
        backfill_polling.Triggered, backfill_polling.Completed -> True
        _, _ -> False
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
        "CreateOAuthClient" | "UpdateOAuthClient" | "DeleteOAuthClient" ->
          case extract_graphql_error(response_body) {
            option.Some(err) ->
              settings.set_oauth_alert(model.settings_page_model, "error", err)
            option.None -> {
              let message = case query_name {
                "CreateOAuthClient" -> "OAuth client created successfully"
                "UpdateOAuthClient" -> "OAuth client updated successfully"
                "DeleteOAuthClient" -> "OAuth client deleted successfully"
                _ -> "Operation completed"
              }
              settings.set_oauth_alert(
                model.settings_page_model,
                "success",
                message,
              )
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
        "CreateOAuthClient" | "UpdateOAuthClient" | "DeleteOAuthClient" -> {
          // Invalidate and refetch OAuth clients list
          let cache_invalidated =
            squall_cache.invalidate(
              final_cache,
              "GetOAuthClients",
              json.object([]),
            )
          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              cache_invalidated,
              "GetOAuthClients",
              json.object([]),
              get_o_auth_clients.parse_get_o_auth_clients_response,
            )
          let #(refetched_cache, refetch_effects) =
            squall_cache.process_pending(
              cache_with_lookup,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )
          #(refetched_cache, refetch_effects)
        }
        _ -> #(final_cache, [])
      }

      // When backfill completes, invalidate and refetch home page queries
      let #(cache_after_backfill, backfill_effects) = case
        backfill_just_completed
      {
        True -> {
          // Invalidate home page queries
          let cache1 =
            squall_cache.invalidate(
              cache_after_mutation,
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

          // Refetch the queries
          let #(cache4, _) =
            squall_cache.lookup(
              cache3,
              "GetStatistics",
              json.object([]),
              get_statistics.parse_get_statistics_response,
            )
          let #(cache5, _) =
            squall_cache.lookup(
              cache4,
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
          let #(cache6, _) =
            squall_cache.lookup(
              cache5,
              "GetRecentActivity",
              json.object([#("hours", json.int(24))]),
              get_recent_activity.parse_get_recent_activity_response,
            )

          let #(final_cache_backfill, refetch_effects) =
            squall_cache.process_pending(
              cache6,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          #(final_cache_backfill, refetch_effects)
        }
        False -> #(cache_after_mutation, [])
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
          cache: cache_after_backfill,
          settings_page_model: new_settings_model,
          backfill_status: new_backfill_status,
          auth_state: new_auth_state,
        ),
        effect.batch(
          [
            effects,
            mutation_effects,
            backfill_effects,
            redirect_effect,
            poll_effect,
          ]
          |> list.flatten,
        ),
      )
    }

    HandleQueryResponse(query_name, _variables, Error(err)) -> {
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
        "CreateOAuthClient" | "UpdateOAuthClient" | "DeleteOAuthClient" ->
          settings.set_oauth_alert(
            model.settings_page_model,
            "error",
            "Error: " <> err,
          )
        _ -> model.settings_page_model
      }

      #(Model(..model, settings_page_model: new_settings_model), effect.none())
    }

    BackfillPollTick -> {
      case backfill_polling.should_poll(model.backfill_status) {
        True -> {
          let #(updated_cache, effects) =
            backfill_polling.poll(
              model.cache,
              model.registry,
              HandleQueryResponse,
            )
          #(Model(..model, cache: updated_cache), effect.batch(effects))
        }
        False -> #(model, effect.none())
      }
    }

    ToggleMobileMenu -> {
      #(
        Model(..model, mobile_menu_open: !model.mobile_menu_open),
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
              mobile_menu_open: False,
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
              let #(cache_with_settings, _) =
                squall_cache.lookup(
                  model.cache,
                  "GetSettings",
                  json.object([]),
                  get_settings.parse_get_settings_response,
                )

              // Fetch OAuth clients data
              let #(cache_with_lookup, _) =
                squall_cache.lookup(
                  cache_with_settings,
                  "GetOAuthClients",
                  json.object([]),
                  get_o_auth_clients.parse_get_o_auth_clients_response,
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
                  mobile_menu_open: False,
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
              mobile_menu_open: False,
            ),
            effect.batch(effects),
          )
        }
        _ -> #(
          Model(
            ..model,
            route: route,
            settings_page_model: cleared_settings_model,
            mobile_menu_open: False,
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

          // Start polling for backfill status after 10 seconds
          let poll_effect =
            effect.from(fn(dispatch) {
              set_timeout(10_000, fn() { dispatch(BackfillPollTick) })
            })

          // Set backfill_status to Triggered (waiting for server confirmation)
          #(
            Model(
              ..model,
              cache: final_cache,
              backfill_status: backfill_polling.Triggered,
            ),
            effect.batch([effect.batch(effects), poll_effect]),
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

          let optimistic_entity =
            json.object([
              #("id", json.string("Settings:singleton")),
              #(
                "domainAuthority",
                json.string(model.settings_page_model.domain_authority_input),
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

        // OAuth Client Message Handlers
        settings.ToggleNewClientForm -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              show_new_client_form: !model.settings_page_model.show_new_client_form,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateNewClientName(value) -> {
          let new_settings_model =
            settings.Model(..model.settings_page_model, new_client_name: value)
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateNewClientType(value) -> {
          let new_settings_model =
            settings.Model(..model.settings_page_model, new_client_type: value)
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateNewClientRedirectUris(value) -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              new_client_redirect_uris: value,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateNewClientScope(value) -> {
          let new_settings_model =
            settings.Model(..model.settings_page_model, new_client_scope: value)
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.SubmitNewClient -> {
          // Parse redirect URIs from newline-separated text
          let uris =
            string.split(
              model.settings_page_model.new_client_redirect_uris,
              "\n",
            )
            |> list.filter(fn(s) { string.length(string.trim(s)) > 0 })
            |> list.map(string.trim)

          let variables =
            json.object([
              #(
                "clientName",
                json.string(model.settings_page_model.new_client_name),
              ),
              #("clientType", json.string("CONFIDENTIAL")),
              #("redirectUris", json.array(uris, json.string)),
              #(
                "scope",
                json.string(model.settings_page_model.new_client_scope),
              ),
            ])

          // Invalidate cached mutation to ensure fresh request
          let cache_invalidated =
            squall_cache.invalidate(model.cache, "CreateOAuthClient", variables)

          let #(cache_with_lookup, _) =
            squall_cache.lookup(
              cache_invalidated,
              "CreateOAuthClient",
              variables,
              create_o_auth_client.parse_create_o_auth_client_response,
            )

          // Invalidate GetOAuthClients cache to trigger refetch
          let cache_with_invalidated_query =
            squall_cache.invalidate(
              cache_with_lookup,
              "GetOAuthClients",
              json.object([]),
            )

          let #(final_cache, effects) =
            squall_cache.process_pending(
              cache_with_invalidated_query,
              model.registry,
              HandleQueryResponse,
              fn() { 0 },
            )

          // Reset form state
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              show_new_client_form: False,
              new_client_name: "",
              new_client_type: "PUBLIC",
              new_client_redirect_uris: "",
              new_client_scope: "atproto transition:generic",
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

        settings.StartEditClient(client_id) -> {
          // Look up client from cache to populate edit fields
          let #(_cache, result) =
            squall_cache.lookup(
              model.cache,
              "GetOAuthClients",
              json.object([]),
              get_o_auth_clients.parse_get_o_auth_clients_response,
            )

          let new_settings_model = case result {
            squall_cache.Data(data) -> {
              case
                list.find(data.oauth_clients, fn(c) { c.client_id == client_id })
              {
                Ok(client) -> {
                  settings.Model(
                    ..model.settings_page_model,
                    editing_client_id: option.Some(client_id),
                    edit_client_name: client.client_name,
                    edit_client_redirect_uris: string.join(
                      client.redirect_uris,
                      "\n",
                    ),
                    edit_client_scope: case client.scope {
                      option.Some(s) -> s
                      option.None -> ""
                    },
                  )
                }
                Error(_) -> model.settings_page_model
              }
            }
            _ -> model.settings_page_model
          }

          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.CancelEditClient -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              editing_client_id: None,
              edit_client_name: "",
              edit_client_redirect_uris: "",
              edit_client_scope: "",
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateEditClientName(value) -> {
          let new_settings_model =
            settings.Model(..model.settings_page_model, edit_client_name: value)
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateEditClientRedirectUris(value) -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              edit_client_redirect_uris: value,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.UpdateEditClientScope(value) -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              edit_client_scope: value,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.SubmitEditClient -> {
          case model.settings_page_model.editing_client_id {
            option.Some(client_id) -> {
              // Parse redirect URIs from newline-separated text
              let uris =
                string.split(
                  model.settings_page_model.edit_client_redirect_uris,
                  "\n",
                )
                |> list.filter(fn(s) { string.length(string.trim(s)) > 0 })
                |> list.map(string.trim)

              let variables =
                json.object([
                  #("clientId", json.string(client_id)),
                  #(
                    "clientName",
                    json.string(model.settings_page_model.edit_client_name),
                  ),
                  #("redirectUris", json.array(uris, json.string)),
                  #(
                    "scope",
                    json.string(model.settings_page_model.edit_client_scope),
                  ),
                ])

              // Invalidate cached mutation to ensure fresh request
              let cache_invalidated =
                squall_cache.invalidate(
                  model.cache,
                  "UpdateOAuthClient",
                  variables,
                )

              let #(cache_with_lookup, _) =
                squall_cache.lookup(
                  cache_invalidated,
                  "UpdateOAuthClient",
                  variables,
                  update_o_auth_client.parse_update_o_auth_client_response,
                )

              // Invalidate GetOAuthClients cache to trigger refetch
              let cache_with_invalidated_query =
                squall_cache.invalidate(
                  cache_with_lookup,
                  "GetOAuthClients",
                  json.object([]),
                )

              let #(final_cache, effects) =
                squall_cache.process_pending(
                  cache_with_invalidated_query,
                  model.registry,
                  HandleQueryResponse,
                  fn() { 0 },
                )

              // Clear edit state
              let new_settings_model =
                settings.Model(
                  ..model.settings_page_model,
                  editing_client_id: None,
                  edit_client_name: "",
                  edit_client_redirect_uris: "",
                  edit_client_scope: "",
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
            None -> #(model, effect.none())
          }
        }

        settings.ToggleSecretVisibility(client_id) -> {
          let new_visible_secrets = case
            set.contains(model.settings_page_model.visible_secrets, client_id)
          {
            True ->
              set.delete(model.settings_page_model.visible_secrets, client_id)
            False ->
              set.insert(model.settings_page_model.visible_secrets, client_id)
          }

          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              visible_secrets: new_visible_secrets,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.ConfirmDeleteClient(client_id) -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              delete_confirm_client_id: option.Some(client_id),
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.CancelDeleteClient -> {
          let new_settings_model =
            settings.Model(
              ..model.settings_page_model,
              delete_confirm_client_id: None,
            )
          #(
            Model(..model, settings_page_model: new_settings_model),
            effect.none(),
          )
        }

        settings.SubmitDeleteClient -> {
          case model.settings_page_model.delete_confirm_client_id {
            option.Some(client_id) -> {
              let variables =
                json.object([#("clientId", json.string(client_id))])

              // Invalidate cached mutation to ensure fresh request
              let cache_invalidated =
                squall_cache.invalidate(
                  model.cache,
                  "DeleteOAuthClient",
                  variables,
                )

              let #(cache_with_lookup, _) =
                squall_cache.lookup(
                  cache_invalidated,
                  "DeleteOAuthClient",
                  variables,
                  delete_o_auth_client.parse_delete_o_auth_client_response,
                )

              // Invalidate GetOAuthClients cache to trigger refetch
              let cache_with_invalidated_query =
                squall_cache.invalidate(
                  cache_with_lookup,
                  "GetOAuthClients",
                  json.object([]),
                )

              let #(final_cache, effects) =
                squall_cache.process_pending(
                  cache_with_invalidated_query,
                  model.registry,
                  HandleQueryResponse,
                  fn() { 0 },
                )

              // Clear delete confirmation state
              let new_settings_model =
                settings.Model(
                  ..model.settings_page_model,
                  delete_confirm_client_id: None,
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
            None -> #(model, effect.none())
          }
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
      html.div(
        [attribute.class("max-w-4xl mx-auto px-4 py-6 sm:px-6 sm:py-12")],
        [
          layout.header(
            auth_info,
            backfill_polling.should_poll(model.backfill_status),
            model.mobile_menu_open,
            ToggleMobileMenu,
          ),
          case model.route {
            Home -> view_home(model)
            Settings -> view_settings(model)
            Lexicons -> view_lexicons(model)
            Upload -> view_upload(model)
          },
        ],
      ),
    ],
  )
}

fn view_home(model: Model) -> Element(Msg) {
  let #(is_admin, is_authenticated) = case model.auth_state {
    Authenticated(_, _, is_admin) -> #(is_admin, True)
    NotAuthenticated -> #(False, False)
  }

  element.map(
    home.view(
      model.cache,
      model.time_range,
      model.backfill_status,
      is_admin,
      is_authenticated,
    ),
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
