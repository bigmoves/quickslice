import database
import envoy
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http/request
import gleam/httpc
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/duration
import gleam/time/timestamp
import sqlight

/// Convert a Dynamic value (Erlang term) to JSON string
fn dynamic_to_json(value: Dynamic) -> String {
  // Erlang's json:encode returns an iolist, we need to convert it to a string
  let iolist = do_json_encode(value)
  iolist_to_string(iolist)
}

/// Encode a dynamic value to JSON (returns iolist)
@external(erlang, "json", "encode")
fn do_json_encode(value: Dynamic) -> Dynamic

/// Convert an iolist to a string
@external(erlang, "erlang", "iolist_to_binary")
fn iolist_to_binary(iolist: Dynamic) -> Dynamic

/// Wrapper to convert iolist to string
fn iolist_to_string(iolist: Dynamic) -> String {
  let binary = iolist_to_binary(iolist)
  // The binary is already a string in Gleam's representation
  case decode.run(binary, decode.string) {
    Ok(str) -> str
    Error(_) -> {
      io.println_error("⚠️  Failed to convert iolist to string")
      string.inspect(iolist)
    }
  }
}

/// ATP data resolved from DID
pub type AtprotoData {
  AtprotoData(did: String, handle: String, pds: String)
}

/// Configuration for backfill operations
pub type BackfillConfig {
  BackfillConfig(
    plc_directory_url: String,
    index_actors: Bool,
    max_workers: Int,
  )
}

/// Creates a default backfill configuration
pub fn default_config() -> BackfillConfig {
  // Get PLC directory URL from environment variable or use default
  let plc_url = case envoy.get("PLC_DIRECTORY_URL") {
    Ok(url) -> url
    Error(_) -> "https://plc.directory"
  }

  BackfillConfig(
    plc_directory_url: plc_url,
    index_actors: True,
    max_workers: 10,
  )
}

/// Resolve a DID to get ATP data (PDS endpoint and handle)
pub fn resolve_did(did: String, plc_url: String) -> Result(AtprotoData, String) {
  let url = plc_url <> "/" <> did

  case request.to(url) {
    Error(_) -> Error("Failed to create request for DID: " <> did)
    Ok(req) -> {
      case httpc.send(req) {
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
  reply_to: Subject(Result(AtprotoData, Nil)),
) -> Nil {
  let result = case resolve_did(did, plc_url) {
    Ok(atp_data) -> Ok(atp_data)
    Error(err) -> {
      io.println_error("Error resolving DID " <> did <> ": " <> err)
      Error(Nil)
    }
  }
  process.send(reply_to, result)
}

/// Get ATP data for a list of repos (DIDs) - concurrent version
pub fn get_atp_data_for_repos(
  repos: List(String),
  config: BackfillConfig,
) -> List(AtprotoData) {
  // Split repos into chunks based on max_workers to limit concurrency
  let chunk_size = case list.length(repos) {
    n if n <= config.max_workers -> 1
    n -> { n + config.max_workers - 1 } / config.max_workers
  }

  repos
  |> list.sized_chunk(chunk_size)
  |> list.flat_map(fn(chunk) {
    // Process each chunk concurrently
    let subject = process.new_subject()

    // Spawn workers for this chunk
    let _workers =
      chunk
      |> list.map(fn(repo) {
        process.spawn_unlinked(fn() {
          resolve_did_worker(repo, config.plc_directory_url, subject)
        })
      })

    // Collect results from all workers in this chunk
    list.range(1, list.length(chunk))
    |> list.filter_map(fn(_) {
      case process.receive(subject, 30_000) {
        Ok(result) -> result
        Error(_) -> Error(Nil)
      }
    })
  })
}

/// Fetch records for a single repo and collection with pagination
pub fn fetch_records_for_repo_collection(
  repo: String,
  collection: String,
  pds_url: String,
) -> List(database.Record) {
  fetch_records_paginated(repo, collection, pds_url, None, [])
}

/// Helper function for paginated record fetching
fn fetch_records_paginated(
  repo: String,
  collection: String,
  pds_url: String,
  cursor: Option(String),
  acc: List(database.Record),
) -> List(database.Record) {
  // Build URL with query parameters
  let base_url =
    pds_url
    <> "/xrpc/com.atproto.repo.listRecords?repo="
    <> repo
    <> "&collection="
    <> collection
    <> "&limit=100"

  let url = case cursor {
    Some(c) -> base_url <> "&cursor=" <> c
    None -> base_url
  }

  case request.to(url) {
    Error(_) -> {
      io.println_error("Failed to create request for: " <> url)
      acc
    }
    Ok(req) -> {
      case httpc.send(req) {
        Error(_) -> {
          io.println_error(
            "Failed to fetch records for " <> repo <> "/" <> collection,
          )
          acc
        }
        Ok(resp) -> {
          case resp.status {
            200 -> {
              case parse_list_records_response(resp.body, repo, collection) {
                Ok(#(records, next_cursor)) -> {
                  let new_acc = list.append(acc, records)
                  case next_cursor {
                    Some(c) ->
                      fetch_records_paginated(
                        repo,
                        collection,
                        pds_url,
                        Some(c),
                        new_acc,
                      )
                    None -> new_acc
                  }
                }
                Error(err) -> {
                  io.println_error(
                    "Failed to parse records for "
                    <> repo
                    <> "/"
                    <> collection
                    <> ": "
                    <> err,
                  )
                  acc
                }
              }
            }
            _ -> {
              io.println_error(
                "Failed to fetch records for "
                <> repo
                <> "/"
                <> collection
                <> " (status: "
                <> string.inspect(resp.status)
                <> ")",
              )
              acc
            }
          }
        }
      }
    }
  }
}

/// Parse the response from com.atproto.repo.listRecords
fn parse_list_records_response(
  body: String,
  repo: String,
  collection: String,
) -> Result(#(List(database.Record), Option(String)), String) {
  let decoder = {
    use records <- decode.field(
      "records",
      decode.list({
        use uri <- decode.field("uri", decode.string)
        use cid <- decode.field("cid", decode.string)
        use value <- decode.field("value", decode.dynamic)
        decode.success(#(uri, cid, value))
      }),
    )

    decode.success(records)
  }

  // Parse the records first
  case json.parse(body, decoder) {
    Error(err) -> {
      io.println_error("Failed to parse records: " <> string.inspect(err))
      io.println_error("Response body snippet: " <> string.slice(body, 0, 200))
      Error("Failed to parse listRecords response")
    }
    Ok(record_tuples) -> {
      // Try to extract cursor separately (it might not exist in the JSON)
      let cursor_decoder = {
        use cursor <- decode.field("cursor", decode.optional(decode.string))
        decode.success(cursor)
      }

      let cursor = case json.parse(body, cursor_decoder) {
        Ok(c) -> c
        Error(_) -> None
      }

      let now =
        timestamp.system_time()
        |> timestamp.to_rfc3339(duration.seconds(0))
      let records =
        record_tuples
        |> list.map(fn(tuple) {
          let #(uri, cid, value) = tuple
          database.Record(
            uri: uri,
            cid: cid,
            did: repo,
            collection: collection,
            json: dynamic_to_json(value),
            indexed_at: now,
          )
        })

      Ok(#(records, cursor))
    }
  }
}

/// Worker function that fetches records for a repo/collection pair
fn fetch_records_worker(
  repo: String,
  collection: String,
  pds: String,
  reply_to: Subject(List(database.Record)),
) -> Nil {
  let records = fetch_records_for_repo_collection(repo, collection, pds)
  process.send(reply_to, records)
}

/// Get all records for multiple repos and collections - concurrent version
pub fn get_records_for_repos(
  repos: List(String),
  collections: List(String),
  atp_data: List(AtprotoData),
  config: BackfillConfig,
) -> List(database.Record) {
  // Create all repo/collection job pairs
  let jobs =
    repos
    |> list.flat_map(fn(repo) {
      case list.find(atp_data, fn(data) { data.did == repo }) {
        Error(_) -> {
          io.println_error("No ATP data found for repo: " <> repo)
          []
        }
        Ok(data) -> {
          collections
          |> list.map(fn(collection) { #(repo, collection, data.pds) })
        }
      }
    })

  // Split jobs into chunks based on max_workers
  let chunk_size = case list.length(jobs) {
    0 -> 1
    n if n <= config.max_workers -> 1
    n -> { n + config.max_workers - 1 } / config.max_workers
  }

  jobs
  |> list.sized_chunk(chunk_size)
  |> list.flat_map(fn(chunk) {
    // Process each chunk concurrently
    let subject = process.new_subject()

    // Spawn workers for this chunk
    let _workers =
      chunk
      |> list.map(fn(job) {
        let #(repo, collection, pds) = job
        process.spawn_unlinked(fn() {
          fetch_records_worker(repo, collection, pds, subject)
        })
      })

    // Collect results from all workers in this chunk
    list.range(1, list.length(chunk))
    |> list.flat_map(fn(_) {
      case process.receive(subject, 60_000) {
        Ok(records) -> records
        Error(_) -> []
      }
    })
  })
}

/// Index records into the database
pub fn index_records(
  records: List(database.Record),
  conn: sqlight.Connection,
) -> Nil {
  records
  |> list.each(fn(record) {
    case
      database.insert_record(
        conn,
        record.uri,
        record.cid,
        record.did,
        record.collection,
        record.json,
      )
    {
      Ok(_) -> Nil
      Error(err) -> {
        io.println_error(
          "Failed to insert record "
          <> record.uri
          <> ": "
          <> string.inspect(err),
        )
      }
    }
  })
}

/// Index actors into the database
pub fn index_actors(
  atp_data: List(AtprotoData),
  conn: sqlight.Connection,
) -> Nil {
  atp_data
  |> list.each(fn(data) {
    case database.upsert_actor(conn, data.did, data.handle) {
      Ok(_) -> Nil
      Error(err) -> {
        io.println_error(
          "Failed to upsert actor " <> data.did <> ": " <> string.inspect(err),
        )
      }
    }
  })
}

/// Fetch repos that have records for a specific collection from the relay with pagination
fn fetch_repos_for_collection(
  collection: String,
) -> Result(List(String), String) {
  fetch_repos_paginated(collection, None, [])
}

/// Helper function for paginated repo fetching
fn fetch_repos_paginated(
  collection: String,
  cursor: Option(String),
  acc: List(String),
) -> Result(List(String), String) {
  // Get relay URL from environment variable or use default
  let relay_url = case envoy.get("RELAY_URL") {
    Ok(url) -> url
    Error(_) -> "https://relay1.us-west.bsky.network"
  }

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
      case httpc.send(req) {
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
                      fetch_repos_paginated(collection, Some(c), new_acc)
                    None -> {
                      io.println(
                        "✓ Found "
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
  io.println("")
  io.println("🔄 Starting backfill operation")

  case collections {
    [] -> io.println("⚠️ No collections specified for backfill")
    _ ->
      io.println(
        "📚 Processing "
        <> string.inspect(list.length(collections))
        <> " collections: "
        <> string.join(collections, ", "),
      )
  }

  case external_collections {
    [] -> Nil
    _ ->
      io.println(
        "🌐 Including "
        <> string.inspect(list.length(external_collections))
        <> " external collections: "
        <> string.join(external_collections, ", "),
      )
  }

  // Determine which repos to use
  let all_repos = case repos {
    [] -> {
      // Fetch repos for all collections from the relay
      io.println("📊 Fetching repositories for collections...")
      let fetched_repos =
        collections
        |> list.filter_map(fn(collection) {
          case fetch_repos_for_collection(collection) {
            Ok(repos) -> Ok(repos)
            Error(err) -> {
              io.println_error(err)
              Error(Nil)
            }
          }
        })
        |> list.flatten
        |> list.unique

      io.println(
        "📋 Processing "
        <> string.inspect(list.length(fetched_repos))
        <> " unique repositories",
      )
      fetched_repos
    }
    provided_repos -> {
      io.println(
        "📋 Using "
        <> string.inspect(list.length(provided_repos))
        <> " provided repositories",
      )
      provided_repos
    }
  }

  // Get ATP data for all repos
  io.println("🔍 Resolving ATP data for repositories...")
  let atp_data = get_atp_data_for_repos(all_repos, config)
  io.println(
    "✓ Resolved ATP data for "
    <> string.inspect(list.length(atp_data))
    <> "/"
    <> string.inspect(list.length(all_repos))
    <> " repositories",
  )

  // Get all records for all repos and collections (main collections only)
  io.println("📥 Fetching records for repositories and collections...")
  let main_records =
    get_records_for_repos(all_repos, collections, atp_data, config)

  // Get external collections for the same repos
  let external_records = case external_collections {
    [] -> []
    _ ->
      get_records_for_repos(all_repos, external_collections, atp_data, config)
  }

  let all_records = list.append(main_records, external_records)
  io.println(
    "✓ Fetched " <> string.inspect(list.length(all_records)) <> " total records",
  )

  // Index actors (if enabled in config)
  case config.index_actors {
    True -> {
      io.println("📝 Indexing actors...")
      index_actors(atp_data, conn)
      io.println(
        "✓ Indexed " <> string.inspect(list.length(atp_data)) <> " actors",
      )
    }
    False -> io.println("⏭️  Skipping actor indexing (disabled in config)")
  }

  // Index records
  io.println(
    "📝 Indexing " <> string.inspect(list.length(all_records)) <> " records...",
  )
  index_records(all_records, conn)
  io.println("✅ Backfill complete!")
}
