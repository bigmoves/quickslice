import car/car
import car/cid
import database/repositories/actors
import database/repositories/config as config_repo
import database/repositories/lexicons
import database/repositories/records
import database/types.{type Record, Record}
import envoy
import gleam/bit_array
import gleam/bytes_tree
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/hackney
import gleam/http/request
import gleam/http/response as gleam_http_response
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import honk
import honk/errors
import lib/oauth/did_cache
import logging
import sqlight

/// Opaque type for monotonic time
pub type MonotonicTime

/// Get current monotonic time for timing measurements
@external(erlang, "backfill_ffi", "monotonic_now")
fn monotonic_now() -> MonotonicTime

/// Get elapsed milliseconds since a start time
@external(erlang, "backfill_ffi", "elapsed_ms")
fn elapsed_ms(start: MonotonicTime) -> Int

/// ATP data resolved from DID
pub type AtprotoData {
  AtprotoData(did: String, handle: String, pds: String)
}

/// Configuration for backfill operations
pub type BackfillConfig {
  BackfillConfig(
    plc_directory_url: String,
    index_actors: Bool,
    max_concurrent_per_pds: Int,
    max_pds_workers: Int,
    max_http_concurrent: Int,
    repo_fetch_timeout_ms: Int,
    did_cache: Option(Subject(did_cache.Message)),
  )
}

/// Creates a default backfill configuration
pub fn default_config(db: sqlight.Connection) -> BackfillConfig {
  // Get PLC directory URL from database config
  let plc_url = config_repo.get_plc_directory_url(db)

  // Get max concurrent per PDS from environment or use default of 4
  let max_pds_concurrent = case envoy.get("BACKFILL_PDS_CONCURRENCY") {
    Ok(val) -> {
      case int.parse(val) {
        Ok(n) -> n
        Error(_) -> 4
      }
    }
    Error(_) -> 4
  }

  // Get max PDS workers from environment or use default of 10
  let max_pds_workers = case envoy.get("BACKFILL_MAX_PDS_WORKERS") {
    Ok(val) -> {
      case int.parse(val) {
        Ok(n) -> n
        Error(_) -> 10
      }
    }
    Error(_) -> 10
  }

  // Get max HTTP concurrent from environment or use default of 50
  let max_http = case envoy.get("BACKFILL_MAX_HTTP_CONCURRENT") {
    Ok(val) -> {
      case int.parse(val) {
        Ok(n) -> n
        Error(_) -> 50
      }
    }
    Error(_) -> 50
  }

  // Get repo fetch timeout from environment or use default of 60s
  let repo_timeout = case envoy.get("BACKFILL_REPO_TIMEOUT") {
    Ok(val) -> {
      case int.parse(val) {
        Ok(n) -> n * 1000
        Error(_) -> 60_000
      }
    }
    Error(_) -> 60_000
  }

  // Configure hackney pool with the configured HTTP limit
  configure_hackney_pool(max_http)

  BackfillConfig(
    plc_directory_url: plc_url,
    index_actors: True,
    max_concurrent_per_pds: max_pds_concurrent,
    max_pds_workers: max_pds_workers,
    max_http_concurrent: max_http,
    repo_fetch_timeout_ms: repo_timeout,
    did_cache: None,
  )
}

/// Creates a backfill configuration with a DID cache
pub fn config_with_cache(
  cache: Subject(did_cache.Message),
  db: sqlight.Connection,
) -> BackfillConfig {
  let config = default_config(db)
  BackfillConfig(..config, did_cache: Some(cache))
}

/// Configure hackney connection pool with specified limits
/// This initializes the HTTP semaphore for rate limiting
@external(erlang, "backfill_ffi", "configure_pool")
pub fn configure_hackney_pool(max_concurrent: Int) -> Nil

/// Acquire a permit from the global HTTP semaphore
/// Blocks if at the concurrent request limit (150)
@external(erlang, "backfill_ffi", "acquire_permit")
fn acquire_permit() -> Nil

/// Release a permit back to the global HTTP semaphore
@external(erlang, "backfill_ffi", "release_permit")
fn release_permit() -> Nil

/// Execute an HTTP request with semaphore-based rate limiting
/// Acquires a permit before sending, releases after completion
fn send_with_permit(
  req: request.Request(String),
) -> Result(gleam_http_response.Response(String), hackney.Error) {
  acquire_permit()
  let result = hackney.send(req)
  release_permit()
  result
}

/// Execute an HTTP request with permit, returning raw binary response
/// Used for binary data like CAR files
fn send_bits_with_permit(
  req: request.Request(bytes_tree.BytesTree),
) -> Result(gleam_http_response.Response(BitArray), hackney.Error) {
  acquire_permit()
  let result = hackney.send_bits(req)
  release_permit()
  result
}

/// Fetch a repo as a CAR file using com.atproto.sync.getRepo
/// Returns raw CAR bytes
fn fetch_repo_car(did: String, pds_url: String) -> Result(BitArray, String) {
  let url = pds_url <> "/xrpc/com.atproto.sync.getRepo?did=" <> did

  case request.to(url) {
    Error(_) -> Error("Failed to create request for getRepo: " <> did)
    Ok(req) -> {
      // Convert to BytesTree body for send_bits (empty for GET request)
      let bits_req = request.set_body(req, bytes_tree.new())

      case send_bits_with_permit(bits_req) {
        Error(err) -> {
          Error(
            "Failed to fetch repo CAR for "
            <> did
            <> ": "
            <> string.inspect(err),
          )
        }
        Ok(resp) -> {
          case resp.status {
            200 -> {
              // Response body is already BitArray
              Ok(resp.body)
            }
            status -> {
              Error(
                "getRepo failed for "
                <> did
                <> " (status: "
                <> string.inspect(status)
                <> ")",
              )
            }
          }
        }
      }
    }
  }
}

/// Convert a CAR record (with proper MST path) to a database Record type
fn car_record_with_path_to_db_record(
  car_record: car.RecordWithPath,
  did: String,
) -> Record {
  let now =
    timestamp.system_time()
    |> timestamp.to_rfc3339(duration.seconds(0))

  Record(
    uri: "at://"
      <> did
      <> "/"
      <> car_record.collection
      <> "/"
      <> car_record.rkey,
    cid: cid.to_string(car_record.cid),
    did: did,
    collection: car_record.collection,
    json: car.record_to_json(car_record),
    indexed_at: now,
  )
}

/// Result of record validation
type ValidationResult {
  /// Record passed validation
  Valid
  /// Record failed schema validation - should be skipped
  Invalid(String)
  /// Could not parse JSON for validation - insert anyway (CBOR edge cases)
  ParseError(String)
}

/// Validate a record against lexicon schemas using pre-built context
fn validate_record(
  ctx: honk.ValidationContext,
  collection: String,
  json_string: String,
) -> ValidationResult {
  case honk.parse_json_string(json_string) {
    Error(e) ->
      // JSON parse failure - likely CBOR edge case, allow record through
      ParseError("Failed to parse record JSON: " <> errors.to_string(e))
    Ok(record_json) -> {
      case honk.validate_record_with_context(ctx, collection, record_json) {
        Ok(_) -> Valid
        Error(e) -> Invalid(errors.to_string(e))
      }
    }
  }
}

/// Load lexicons and build validation context
/// Returns None on error (validation will be skipped)
fn load_validation_context(
  conn: sqlight.Connection,
) -> Option(honk.ValidationContext) {
  case lexicons.get_all(conn) {
    Error(_) -> {
      logging.log(
        logging.Warning,
        "[backfill] Failed to load lexicons, skipping validation",
      )
      None
    }
    Ok(lexs) -> {
      let lexicon_jsons =
        lexs
        |> list.filter_map(fn(lex) {
          case honk.parse_json_string(lex.json) {
            Ok(json_val) -> Ok(json_val)
            Error(_) -> Error(Nil)
          }
        })
      // Build context once from all lexicons
      case honk.build_validation_context(lexicon_jsons) {
        Ok(ctx) -> Some(ctx)
        Error(_) -> {
          logging.log(
            logging.Warning,
            "[backfill] Failed to build validation context, skipping validation",
          )
          None
        }
      }
    }
  }
}

/// Backfill a single repo using CAR file approach
/// Fetches entire repo, parses CAR with MST walking, filters by collections, indexes records
fn backfill_repo_car(
  did: String,
  pds_url: String,
  collections: List(String),
  conn: sqlight.Connection,
) -> Int {
  // Load validation context - this path is used for single-repo backfills (e.g., new actor sync)
  let validation_ctx = load_validation_context(conn)
  backfill_repo_car_with_context(
    did,
    pds_url,
    collections,
    conn,
    validation_ctx,
  )
}

/// Backfill a single repo with pre-built validation context
fn backfill_repo_car_with_context(
  did: String,
  pds_url: String,
  collections: List(String),
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
) -> Int {
  let total_start = monotonic_now()

  // Phase 1: Fetch
  let fetch_start = monotonic_now()
  case fetch_repo_car(did, pds_url) {
    Ok(car_bytes) -> {
      let fetch_ms = elapsed_ms(fetch_start)
      let car_size = bit_array.byte_size(car_bytes)

      // Phase 2: Parse CAR and walk MST
      let parse_start = monotonic_now()
      case car.extract_records_with_paths(car_bytes, collections) {
        Ok(car_records) -> {
          let parse_ms = elapsed_ms(parse_start)

          // Phase 3: Convert and validate
          let validate_start = monotonic_now()
          let #(db_records, invalid_count) =
            car_records
            |> list.fold(#([], 0), fn(acc, r) {
              let #(records, invalids) = acc
              let db_record = car_record_with_path_to_db_record(r, did)
              case validation_ctx {
                None -> #([db_record, ..records], invalids)
                Some(ctx) -> {
                  case validate_record(ctx, r.collection, db_record.json) {
                    Valid -> #([db_record, ..records], invalids)
                    ParseError(_) -> #([db_record, ..records], invalids)
                    Invalid(_) -> #(records, invalids + 1)
                  }
                }
              }
            })
          let validate_ms = elapsed_ms(validate_start)

          // Phase 4: Insert into database
          let insert_start = monotonic_now()
          index_records(db_records, conn)
          let insert_ms = elapsed_ms(insert_start)

          let count = list.length(db_records)
          let total_ms = elapsed_ms(total_start)

          // Log summary at debug level (detailed per-repo timing)
          logging.log(
            logging.Debug,
            "[backfill] "
              <> did
              <> " fetch="
              <> int.to_string(fetch_ms)
              <> "ms parse="
              <> int.to_string(parse_ms)
              <> "ms validate="
              <> int.to_string(validate_ms)
              <> "ms insert="
              <> int.to_string(insert_ms)
              <> "ms total="
              <> int.to_string(total_ms)
              <> "ms records="
              <> int.to_string(count)
              <> " invalid="
              <> int.to_string(invalid_count)
              <> " size="
              <> int.to_string(car_size),
          )
          count
        }
        Error(err) -> {
          logging.log(
            logging.Warning,
            "[backfill] CAR parse error for "
              <> did
              <> ": "
              <> string.inspect(err),
          )
          0
        }
      }
    }
    Error(err) -> {
      logging.log(logging.Warning, "[backfill] " <> err)
      0
    }
  }
}

/// Check if an NSID matches the configured domain authority
/// NSID format is like "com.example.post" where "com.example" is the authority
pub fn nsid_matches_domain_authority(
  nsid: String,
  domain_authority: String,
) -> Bool {
  // NSID format: authority.name (e.g., "com.example.post")
  // We need to check if the NSID starts with the domain authority
  string.starts_with(nsid, domain_authority <> ".")
}

/// Get local and external collection IDs from configured lexicons
/// Returns #(local_collection_ids, external_collection_ids)
pub fn get_collection_ids(
  conn: sqlight.Connection,
) -> #(List(String), List(String)) {
  let domain_authority = case config_repo.get(conn, "domain_authority") {
    Ok(authority) -> authority
    Error(_) -> ""
  }

  case lexicons.get_record_types(conn) {
    Ok(lexicon_list) -> {
      let #(local, external) =
        list.partition(lexicon_list, fn(lex) {
          nsid_matches_domain_authority(lex.id, domain_authority)
        })
      #(
        list.map(local, fn(lex) { lex.id }),
        list.map(external, fn(lex) { lex.id }),
      )
    }
    Error(_) -> #([], [])
  }
}

/// Resolve a DID to get ATP data (PDS endpoint and handle)
pub fn resolve_did(did: String, plc_url: String) -> Result(AtprotoData, String) {
  // Check if this is a did:web DID
  case string.starts_with(did, "did:web:") {
    True -> resolve_did_web(did)
    False -> resolve_did_plc(did, plc_url)
  }
}

/// Resolve a DID with caching support
/// Uses the did_cache if provided, otherwise falls back to direct resolution
pub fn resolve_did_cached(
  did: String,
  plc_url: String,
  cache: Option(Subject(did_cache.Message)),
) -> Result(AtprotoData, String) {
  case cache {
    Some(c) -> {
      // Try to get from cache first
      case did_cache.get(c, did) {
        Ok(cached_json) -> {
          // Parse cached ATP data
          case parse_cached_atp_data(cached_json) {
            Ok(data) -> Ok(data)
            Error(_) -> {
              // Cache had invalid data, resolve fresh
              resolve_and_cache(did, plc_url, c)
            }
          }
        }
        Error(_) -> {
          // Not in cache, resolve and cache
          resolve_and_cache(did, plc_url, c)
        }
      }
    }
    None -> resolve_did(did, plc_url)
  }
}

/// Resolve DID and store in cache
fn resolve_and_cache(
  did: String,
  plc_url: String,
  cache: Subject(did_cache.Message),
) -> Result(AtprotoData, String) {
  case resolve_did(did, plc_url) {
    Ok(data) -> {
      // Cache for 1 hour (3600 seconds)
      let cached_json = encode_atp_data(data)
      did_cache.put(cache, did, cached_json, 3600)
      Ok(data)
    }
    err -> err
  }
}

/// Encode AtprotoData to JSON for caching
fn encode_atp_data(data: AtprotoData) -> String {
  json.object([
    #("did", json.string(data.did)),
    #("handle", json.string(data.handle)),
    #("pds", json.string(data.pds)),
  ])
  |> json.to_string
}

/// Parse AtprotoData from cached JSON
fn parse_cached_atp_data(json_str: String) -> Result(AtprotoData, String) {
  let decoder = {
    use did <- decode.field("did", decode.string)
    use handle <- decode.field("handle", decode.string)
    use pds <- decode.field("pds", decode.string)
    decode.success(AtprotoData(did: did, handle: handle, pds: pds))
  }

  json.parse(json_str, decoder)
  |> result.map_error(fn(_) { "Failed to parse cached ATP data" })
}

/// Resolve a did:web DID by fetching the DID document from HTTPS
fn resolve_did_web(did: String) -> Result(AtprotoData, String) {
  // Extract the domain from did:web:example.com
  // did:web format: did:web:<domain>[:<port>][:<path>]
  let parts = string.split(did, ":")
  case parts {
    ["did", "web", domain, ..rest] -> {
      // Build the URL: https://<domain>/.well-known/did.json
      // If there are additional path components, they go before /.well-known/did.json
      let base_domain = case rest {
        [] -> domain
        path_parts -> domain <> "/" <> string.join(path_parts, "/")
      }
      let url = "https://" <> base_domain <> "/.well-known/did.json"

      case request.to(url) {
        Error(_) -> Error("Failed to create request for did:web DID: " <> did)
        Ok(req) -> {
          case send_with_permit(req) {
            Error(_) -> Error("Failed to fetch did:web DID data for: " <> did)
            Ok(resp) -> {
              case resp.status {
                200 -> parse_atproto_data(resp.body, did)
                _ ->
                  Error(
                    "Failed to resolve DID "
                    <> did
                    <> " (status: "
                    <> string.inspect(resp.status)
                    <> ")",
                  )
              }
            }
          }
        }
      }
    }
    _ -> Error("Invalid did:web format: " <> did)
  }
}

/// Resolve a did:plc DID through the PLC directory
fn resolve_did_plc(did: String, plc_url: String) -> Result(AtprotoData, String) {
  let url = plc_url <> "/" <> did

  case request.to(url) {
    Error(_) -> Error("Failed to create request for DID: " <> did)
    Ok(req) -> {
      case send_with_permit(req) {
        Error(_) -> Error("Failed to fetch DID data for: " <> did)
        Ok(resp) -> {
          case resp.status {
            200 -> parse_atproto_data(resp.body, did)
            _ ->
              Error(
                "Failed to resolve DID "
                <> did
                <> " (status: "
                <> string.inspect(resp.status)
                <> ")",
              )
          }
        }
      }
    }
  }
}

/// Parse ATP data from PLC directory response
fn parse_atproto_data(body: String, did: String) -> Result(AtprotoData, String) {
  // Simple decoder that extracts service and alsoKnownAs arrays
  let decoder = {
    use service_list <- decode.field(
      "service",
      decode.optional(decode.list(decode.dynamic)),
    )
    use handle_list <- decode.field(
      "alsoKnownAs",
      decode.optional(decode.list(decode.string)),
    )
    decode.success(#(service_list, handle_list))
  }

  case json.parse(body, decoder) {
    Error(_) -> Error("Failed to parse ATP data for DID: " <> did)
    Ok(#(service_list_opt, handle_list_opt)) -> {
      // Extract PDS endpoint from service list
      let pds = case service_list_opt {
        Some(service_list) ->
          service_list
          |> list.find_map(fn(service_dyn) {
            // Try to extract the service endpoint
            let service_decoder = {
              use service_type <- decode.field("type", decode.string)
              use endpoint <- decode.field("serviceEndpoint", decode.string)
              decode.success(#(service_type, endpoint))
            }

            case decode.run(service_dyn, service_decoder) {
              Ok(#("AtprotoPersonalDataServer", endpoint)) -> Ok(endpoint)
              _ -> Error(Nil)
            }
          })
          |> result.unwrap("https://bsky.social")
        None -> "https://bsky.social"
      }

      // Extract handle from alsoKnownAs
      let handle = case handle_list_opt {
        Some(handle_list) ->
          handle_list
          |> list.find(fn(h) { string.starts_with(h, "at://") })
          |> result.map(fn(h) { string.replace(h, "at://", "") })
          |> result.unwrap(did)
        None -> did
      }

      Ok(AtprotoData(did: did, handle: handle, pds: pds))
    }
  }
}

/// Worker function that resolves a DID and sends result back
fn resolve_did_worker(
  did: String,
  plc_url: String,
  cache: Option(Subject(did_cache.Message)),
  reply_to: Subject(Result(AtprotoData, Nil)),
) -> Nil {
  let result = case resolve_did_cached(did, plc_url, cache) {
    Ok(atp_data) -> Ok(atp_data)
    Error(err) -> {
      logging.log(
        logging.Error,
        "[backfill] Error resolving DID " <> did <> ": " <> err,
      )
      Error(Nil)
    }
  }
  process.send(reply_to, result)
}

/// Get ATP data for a list of repos (DIDs) - fully concurrent version
pub fn get_atp_data_for_repos(
  repos: List(String),
  config: BackfillConfig,
) -> List(AtprotoData) {
  // Spawn all workers at once - Erlang VM can handle it
  let subject = process.new_subject()
  let repo_count = list.length(repos)

  // Spawn all workers concurrently
  let _workers =
    repos
    |> list.map(fn(repo) {
      process.spawn_unlinked(fn() {
        resolve_did_worker(
          repo,
          config.plc_directory_url,
          config.did_cache,
          subject,
        )
      })
    })

  // Collect results from all workers
  list.range(1, repo_count)
  |> list.filter_map(fn(_) {
    case process.receive(subject, 30_000) {
      Ok(result) -> result
      Error(_) -> Error(Nil)
    }
  })
}

/// CAR-based worker that fetches entire repo and filters by collections
fn fetch_repo_car_worker(
  repo: String,
  pds: String,
  collections: List(String),
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
  reply_to: Subject(Int),
) -> Nil {
  // Wrap in rescue to catch any crashes
  let count = rescue_car_backfill(repo, pds, collections, conn, validation_ctx)
  process.send(reply_to, count)
}

/// Wrapper to catch crashes in CAR backfill
fn rescue_car_backfill(
  repo: String,
  pds: String,
  collections: List(String),
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
) -> Int {
  case
    rescue(fn() {
      backfill_repo_car_with_context(
        repo,
        pds,
        collections,
        conn,
        validation_ctx,
      )
    })
  {
    Ok(count) -> count
    Error(err) -> {
      logging.log(
        logging.Error,
        "[backfill] CAR worker crashed for "
          <> repo
          <> ": "
          <> string.inspect(err),
      )
      0
    }
  }
}

/// Rescue wrapper - catches exceptions
@external(erlang, "backfill_ffi", "rescue")
pub fn rescue(f: fn() -> a) -> Result(a, Dynamic)

/// CAR-based PDS worker - fetches each repo as CAR and filters locally
fn pds_worker_car(
  pds_url: String,
  repos: List(String),
  collections: List(String),
  max_concurrent: Int,
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
  timeout_ms: Int,
  reply_to: Subject(Int),
) -> Nil {
  logging.log(
    logging.Debug,
    "[backfill] PDS worker starting for "
      <> pds_url
      <> " with "
      <> int.to_string(list.length(repos))
      <> " repos",
  )
  let subject = process.new_subject()

  // Start initial batch of workers
  let #(initial_repos, remaining_repos) = list.split(repos, max_concurrent)
  let initial_count = list.length(initial_repos)

  // Spawn initial workers
  list.each(initial_repos, fn(repo) {
    process.spawn_unlinked(fn() {
      fetch_repo_car_worker(
        repo,
        pds_url,
        collections,
        conn,
        validation_ctx,
        subject,
      )
    })
  })

  // Process with sliding window
  let total_count =
    sliding_window_car(
      remaining_repos,
      subject,
      initial_count,
      pds_url,
      collections,
      conn,
      validation_ctx,
      timeout_ms,
      0,
    )

  logging.log(
    logging.Debug,
    "[backfill] PDS worker finished for "
      <> pds_url
      <> " with "
      <> int.to_string(total_count)
      <> " total records",
  )
  process.send(reply_to, total_count)
}

/// Sliding window for CAR-based processing
fn sliding_window_car(
  remaining: List(String),
  subject: Subject(Int),
  in_flight: Int,
  pds_url: String,
  collections: List(String),
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
  timeout_ms: Int,
  total: Int,
) -> Int {
  case in_flight {
    0 -> total
    _ -> {
      case process.receive(subject, timeout_ms) {
        Ok(count) -> {
          let new_total = total + count
          case remaining {
            [next, ..rest] -> {
              process.spawn_unlinked(fn() {
                fetch_repo_car_worker(
                  next,
                  pds_url,
                  collections,
                  conn,
                  validation_ctx,
                  subject,
                )
              })
              sliding_window_car(
                rest,
                subject,
                in_flight,
                pds_url,
                collections,
                conn,
                validation_ctx,
                timeout_ms,
                new_total,
              )
            }
            [] ->
              sliding_window_car(
                [],
                subject,
                in_flight - 1,
                pds_url,
                collections,
                conn,
                validation_ctx,
                timeout_ms,
                new_total,
              )
          }
        }
        Error(_) -> {
          logging.log(
            logging.Warning,
            "[backfill] Timeout waiting for CAR worker on "
              <> pds_url
              <> " (in_flight: "
              <> int.to_string(in_flight)
              <> ", remaining: "
              <> int.to_string(list.length(remaining))
              <> ")",
          )
          sliding_window_car(
            remaining,
            subject,
            in_flight - 1,
            pds_url,
            collections,
            conn,
            validation_ctx,
            timeout_ms,
            total,
          )
        }
      }
    }
  }
}

/// Sliding window for PDS worker processing
/// Limits how many PDS endpoints are processed concurrently
fn sliding_window_pds(
  remaining: List(#(String, List(#(String, String)))),
  subject: Subject(Int),
  in_flight: Int,
  collections: List(String),
  max_concurrent_per_pds: Int,
  conn: sqlight.Connection,
  validation_ctx: Option(honk.ValidationContext),
  timeout_ms: Int,
  total: Int,
  pds_count: Int,
  completed: Int,
) -> Int {
  case in_flight {
    0 -> total
    _ -> {
      // 5 minute timeout per PDS worker
      case process.receive(subject, 300_000) {
        Ok(count) -> {
          let new_total = total + count
          let new_completed = completed + 1
          logging.log(
            logging.Info,
            "[backfill] PDS worker "
              <> int.to_string(new_completed)
              <> "/"
              <> int.to_string(pds_count)
              <> " done ("
              <> int.to_string(count)
              <> " records)",
          )
          case remaining {
            [#(pds_url, repo_pairs), ..rest] -> {
              let pds_repos =
                repo_pairs
                |> list.map(fn(pair) {
                  let #(_pds, repo) = pair
                  repo
                })
              process.spawn_unlinked(fn() {
                pds_worker_car(
                  pds_url,
                  pds_repos,
                  collections,
                  max_concurrent_per_pds,
                  conn,
                  validation_ctx,
                  timeout_ms,
                  subject,
                )
              })
              sliding_window_pds(
                rest,
                subject,
                in_flight,
                collections,
                max_concurrent_per_pds,
                conn,
                validation_ctx,
                timeout_ms,
                new_total,
                pds_count,
                new_completed,
              )
            }
            [] ->
              sliding_window_pds(
                [],
                subject,
                in_flight - 1,
                collections,
                max_concurrent_per_pds,
                conn,
                validation_ctx,
                timeout_ms,
                new_total,
                pds_count,
                new_completed,
              )
          }
        }
        Error(_) -> {
          logging.log(
            logging.Warning,
            "[backfill] PDS worker timed out (in_flight: "
              <> int.to_string(in_flight)
              <> ", remaining: "
              <> int.to_string(list.length(remaining))
              <> ")",
          )
          sliding_window_pds(
            remaining,
            subject,
            in_flight - 1,
            collections,
            max_concurrent_per_pds,
            conn,
            validation_ctx,
            timeout_ms,
            total,
            pds_count,
            completed,
          )
        }
      }
    }
  }
}

/// CAR-based streaming: fetch repos as CAR files and filter locally
/// One request per repo instead of one per (repo, collection)
pub fn get_records_for_repos_car(
  repos: List(String),
  collections: List(String),
  atp_data: List(AtprotoData),
  config: BackfillConfig,
  conn: sqlight.Connection,
) -> Int {
  // Build validation context ONCE for all repos
  let validation_ctx = load_validation_context(conn)

  // Group repos by PDS
  let repos_by_pds =
    repos
    |> list.filter_map(fn(repo) {
      case list.find(atp_data, fn(data) { data.did == repo }) {
        Error(_) -> {
          logging.log(
            logging.Error,
            "[backfill] No ATP data found for repo: " <> repo,
          )
          Error(Nil)
        }
        Ok(data) -> Ok(#(data.pds, repo))
      }
    })
    |> list.group(fn(pair) {
      let #(pds, _repo) = pair
      pds
    })

  let pds_entries = dict.to_list(repos_by_pds)
  let pds_count = list.length(pds_entries)

  logging.log(
    logging.Info,
    "[backfill] Processing "
      <> int.to_string(pds_count)
      <> " PDS endpoints (max "
      <> int.to_string(config.max_pds_workers)
      <> " concurrent)...",
  )

  // Use sliding window to limit concurrent PDS workers
  let subject = process.new_subject()
  let #(initial_pds, remaining_pds) =
    list.split(pds_entries, config.max_pds_workers)
  let initial_count = list.length(initial_pds)

  // Spawn initial batch of PDS workers
  list.each(initial_pds, fn(pds_entry) {
    let #(pds_url, repo_pairs) = pds_entry
    let pds_repos =
      repo_pairs
      |> list.map(fn(pair) {
        let #(_pds, repo) = pair
        repo
      })

    process.spawn_unlinked(fn() {
      pds_worker_car(
        pds_url,
        pds_repos,
        collections,
        config.max_concurrent_per_pds,
        conn,
        validation_ctx,
        config.repo_fetch_timeout_ms,
        subject,
      )
    })
  })

  // Process remaining with sliding window
  let result =
    sliding_window_pds(
      remaining_pds,
      subject,
      initial_count,
      collections,
      config.max_concurrent_per_pds,
      conn,
      validation_ctx,
      config.repo_fetch_timeout_ms,
      0,
      pds_count,
      0,
    )

  logging.log(
    logging.Info,
    "[backfill] All PDS workers complete, total: "
      <> int.to_string(result)
      <> " records",
  )
  result
}

/// Index records into the database using batch inserts
pub fn index_records(records: List(Record), conn: sqlight.Connection) -> Nil {
  case records.batch_insert(conn, records) {
    Ok(_) -> Nil
    Error(err) -> {
      logging.log(
        logging.Error,
        "[backfill] Failed to batch insert records: " <> string.inspect(err),
      )
    }
  }
}

/// Index actors into the database using batch upsert for efficiency
pub fn index_actors(
  atp_data: List(AtprotoData),
  conn: sqlight.Connection,
) -> Nil {
  // Convert AtprotoData to tuples for batch upsert
  let actor_tuples =
    atp_data
    |> list.map(fn(data) { #(data.did, data.handle) })

  case actors.batch_upsert(conn, actor_tuples) {
    Ok(_) -> Nil
    Error(err) -> {
      logging.log(
        logging.Error,
        "[backfill] Failed to batch upsert actors: " <> string.inspect(err),
      )
    }
  }
}

/// Backfill all collections (primary and external) for a newly discovered actor
/// This is called when a new actor is created via Jetstream or GraphQL mutations
pub fn backfill_collections_for_actor(
  db: sqlight.Connection,
  did: String,
  collection_ids: List(String),
  external_collection_ids: List(String),
  plc_url: String,
) -> Nil {
  // Ensure HTTP semaphore is initialized (may not be if called outside normal backfill flow)
  configure_hackney_pool(150)

  let all_collections = list.append(collection_ids, external_collection_ids)
  let total_count = list.length(all_collections)

  logging.log(
    logging.Info,
    "[backfill] Starting background sync for new actor: "
      <> did
      <> " ("
      <> string.inspect(total_count)
      <> " collections: "
      <> string.inspect(list.length(collection_ids))
      <> " primary + "
      <> string.inspect(list.length(external_collection_ids))
      <> " external)",
  )

  // Resolve DID to get PDS endpoint
  case resolve_did(did, plc_url) {
    Ok(atp_data) -> {
      // Use CAR-based approach - single request gets all collections
      let total_records =
        backfill_repo_car(did, atp_data.pds, all_collections, db)

      logging.log(
        logging.Info,
        "[backfill] Completed sync for "
          <> did
          <> " ("
          <> string.inspect(total_records)
          <> " total records)",
      )
    }
    Error(err) -> {
      logging.log(
        logging.Error,
        "[backfill] Failed to resolve DID for backfill: " <> did <> " - " <> err,
      )
    }
  }
}

/// Fetch repos that have records for a specific collection from the relay with pagination
fn fetch_repos_for_collection(
  collection: String,
  db: sqlight.Connection,
) -> Result(List(String), String) {
  fetch_repos_paginated(collection, None, [], db)
}

/// Helper function for paginated repo fetching
fn fetch_repos_paginated(
  collection: String,
  cursor: Option(String),
  acc: List(String),
  db: sqlight.Connection,
) -> Result(List(String), String) {
  // Get relay URL from database config
  let relay_url = config_repo.get_relay_url(db)

  // Build URL with large limit and cursor
  let base_url =
    relay_url
    <> "/xrpc/com.atproto.sync.listReposByCollection?collection="
    <> collection
    <> "&limit=1000"

  let url = case cursor {
    Some(c) -> base_url <> "&cursor=" <> c
    None -> base_url
  }

  case request.to(url) {
    Error(_) -> Error("Failed to create request for collection: " <> collection)
    Ok(req) -> {
      case send_with_permit(req) {
        Error(_) ->
          Error("Failed to fetch repos for collection: " <> collection)
        Ok(resp) -> {
          case resp.status {
            200 -> {
              case parse_repos_response(resp.body) {
                Ok(#(repos, next_cursor)) -> {
                  let new_acc = list.append(acc, repos)
                  case next_cursor {
                    Some(c) ->
                      fetch_repos_paginated(collection, Some(c), new_acc, db)
                    None -> {
                      logging.log(
                        logging.Info,
                        "[backfill] Found "
                          <> string.inspect(list.length(new_acc))
                          <> " total repositories for collection \""
                          <> collection
                          <> "\"",
                      )
                      Ok(new_acc)
                    }
                  }
                }
                Error(err) -> Error(err)
              }
            }
            _ ->
              Error(
                "Failed to fetch repos for collection "
                <> collection
                <> " (status: "
                <> string.inspect(resp.status)
                <> ")",
              )
          }
        }
      }
    }
  }
}

/// Parse the response from com.atproto.sync.listReposByCollection
fn parse_repos_response(
  body: String,
) -> Result(#(List(String), Option(String)), String) {
  let decoder = {
    use repos <- decode.field(
      "repos",
      decode.list({
        use did <- decode.field("did", decode.string)
        decode.success(did)
      }),
    )
    decode.success(repos)
  }

  // Parse repos first
  case json.parse(body, decoder) {
    Error(_) -> Error("Failed to parse repos response")
    Ok(repos) -> {
      // Try to extract cursor separately
      let cursor_decoder = {
        use cursor <- decode.field("cursor", decode.optional(decode.string))
        decode.success(cursor)
      }

      let cursor = case json.parse(body, cursor_decoder) {
        Ok(c) -> c
        Error(_) -> None
      }

      Ok(#(repos, cursor))
    }
  }
}

/// Main backfill function - backfill collections for specified repos
pub fn backfill_collections(
  repos: List(String),
  collections: List(String),
  external_collections: List(String),
  config: BackfillConfig,
  conn: sqlight.Connection,
) -> Nil {
  logging.log(logging.Info, "")
  logging.log(logging.Info, "[backfill] Starting backfill operation")

  case collections {
    [] -> {
      logging.log(
        logging.Error,
        "[backfill] No collections specified for backfill",
      )
      Nil
    }
    _ -> {
      logging.log(
        logging.Info,
        "[backfill] Processing "
          <> string.inspect(list.length(collections))
          <> " collections: "
          <> string.join(collections, ", "),
      )

      run_backfill(repos, collections, external_collections, config, conn)
    }
  }
}

fn run_backfill(
  repos: List(String),
  collections: List(String),
  external_collections: List(String),
  config: BackfillConfig,
  conn: sqlight.Connection,
) -> Nil {
  case external_collections {
    [] -> Nil
    _ ->
      logging.log(
        logging.Info,
        "[backfill] Including "
          <> string.inspect(list.length(external_collections))
          <> " external collections: "
          <> string.join(external_collections, ", "),
      )
  }

  // Determine which repos to use
  let all_repos = case repos {
    [] -> {
      // Fetch repos for all collections from the relay
      logging.log(
        logging.Info,
        "[backfill] Fetching repositories for collections...",
      )
      let fetched_repos =
        collections
        |> list.filter_map(fn(collection) {
          case fetch_repos_for_collection(collection, conn) {
            Ok(repos) -> Ok(repos)
            Error(err) -> {
              logging.log(logging.Error, "[backfill] " <> err)
              Error(Nil)
            }
          }
        })
        |> list.flatten
        |> list.unique

      logging.log(
        logging.Info,
        "[backfill] Processing "
          <> string.inspect(list.length(fetched_repos))
          <> " unique repositories",
      )
      fetched_repos
    }
    provided_repos -> {
      logging.log(
        logging.Info,
        "[backfill] Using "
          <> string.inspect(list.length(provided_repos))
          <> " provided repositories",
      )
      provided_repos
    }
  }

  // Get ATP data for all repos
  logging.log(logging.Info, "[backfill] Resolving ATP data for repositories...")
  let atp_data = get_atp_data_for_repos(all_repos, config)
  logging.log(
    logging.Info,
    "[backfill] Resolved ATP data for "
      <> string.inspect(list.length(atp_data))
      <> "/"
      <> string.inspect(list.length(all_repos))
      <> " repositories",
  )

  // Index actors first (if enabled in config)
  case config.index_actors {
    True -> {
      logging.log(logging.Info, "[backfill] Indexing actors...")
      index_actors(atp_data, conn)
      logging.log(
        logging.Info,
        "[backfill] Indexed "
          <> string.inspect(list.length(atp_data))
          <> " actors",
      )
    }
    False ->
      logging.log(
        logging.Info,
        "[backfill] Skipping actor indexing (disabled in config)",
      )
  }

  // Combine main and external collections for concurrent processing
  let all_collections = list.append(collections, external_collections)

  // Use CAR-based approach: one getRepo request per repo, filter locally
  logging.log(
    logging.Info,
    "[backfill] Fetching repos as CAR files and filtering locally...",
  )
  let total_count =
    get_records_for_repos_car(
      all_repos,
      all_collections,
      atp_data,
      config,
      conn,
    )
  logging.log(
    logging.Info,
    "[backfill] Backfill complete! Indexed "
      <> string.inspect(total_count)
      <> " total records via CAR",
  )
}
