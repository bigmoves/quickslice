import atproto_auth
import database
import dpop
import gleam/bit_array
import gleam/http
import gleam/http/request
import gleam/list
import gleam/string
import lexicon
import sqlight
import wisp

/// Handle createRecord XRPC method with authentication
pub fn handle_create_record(
  req: wisp.Request,
  db: sqlight.Connection,
  nsid: String,
  auth_base_url: String,
) -> wisp.Response {
  // Only accept POST requests
  case req.method {
    http.Post -> {
      // Step 1: Extract bearer token from Authorization header
      case request.get_header(req, "authorization") {
        Error(_) -> unauthorized_response()
        Ok(auth_header) -> {
          // Parse "Bearer {token}" format
          case string.starts_with(auth_header, "Bearer ") {
            False -> unauthorized_response()
            True -> {
              let token = string.drop_start(auth_header, 7)
              handle_create_with_auth(req, db, nsid, token, auth_base_url)
            }
          }
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"MethodNotAllowed\", \"message\": \"Only POST is allowed\"}",
      ))
    }
  }
}

fn handle_create_with_auth(
  req: wisp.Request,
  _db: sqlight.Connection,
  _nsid: String,
  token: String,
  auth_base_url: String,
) -> wisp.Response {
  // Step 2: Get ATProto session (includes PDS URL, access token, and DPoP JWK)
  case atproto_auth.get_atproto_session(token, auth_base_url) {
    Error(atproto_auth.UnauthorizedToken) -> unauthorized_response()
    Error(_) -> internal_error_response("Failed to get ATProto session")
    Ok(session) -> {
      // Read the request body
      case wisp.read_body_bits(req) {
        Ok(body) -> {
          case bit_array.to_string(body) {
            Ok(body_string) -> {
              // Proxy to PDS with DPoP authentication
              let pds_url =
                session.pds_endpoint <> "/xrpc/com.atproto.repo.createRecord"

              case
                dpop.make_dpop_request("POST", pds_url, session, body_string)
              {
                Ok(pds_response) -> {
                  wisp.response(pds_response.status)
                  |> wisp.set_header("content-type", "application/json")
                  |> wisp.set_body(wisp.Text(pds_response.body))
                }
                Error(_) -> {
                  internal_error_response("Failed to create record on PDS")
                }
              }
            }
            Error(_) -> {
              bad_request_response("Request body must be valid UTF-8")
            }
          }
        }
        Error(_) -> {
          bad_request_response("Failed to read request body")
        }
      }
    }
  }
}

/// Handle getRecord XRPC method
pub fn handle_get_record(
  req: wisp.Request,
  db: sqlight.Connection,
  _nsid: String,
) -> wisp.Response {
  // Only accept GET requests
  case req.method {
    http.Get -> {
      // Extract URI from query parameters
      let query_params = wisp.get_query(req)
      case list.key_find(query_params, "uri") {
        Ok(uri) -> {
          // Fetch the record from database
          case database.get_record(db, uri) {
            Ok([record]) -> {
              // Return the record
              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"uri\": \""
                <> record.uri
                <> "\", \"cid\": \""
                <> record.cid
                <> "\", \"value\": "
                <> record.json
                <> "}",
              ))
            }
            Ok([]) -> {
              wisp.response(404)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"RecordNotFound\", \"message\": \"Record not found: "
                <> uri
                <> "\"}",
              ))
            }
            Ok(_) -> {
              // Multiple records (unexpected, URI should be unique)
              wisp.response(500)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"InternalError\", \"message\": \"Multiple records found\"}",
              ))
            }
            Error(_db_error) -> {
              wisp.response(500)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"InternalError\", \"message\": \"Database error\"}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"InvalidRequest\", \"message\": \"Missing required query parameter: uri\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"MethodNotAllowed\", \"message\": \"Only GET is allowed\"}",
      ))
    }
  }
}

/// Handle updateRecord XRPC method
pub fn handle_update_record(
  req: wisp.Request,
  db: sqlight.Connection,
  nsid: String,
) -> wisp.Response {
  // Only accept POST requests (AT Protocol uses POST for updates)
  case req.method {
    http.Post -> {
      // Extract URI from query parameters
      let query_params = wisp.get_query(req)
      case list.key_find(query_params, "uri") {
        Ok(uri) -> {
          // Read the new record data
          case wisp.read_body_bits(req) {
            Ok(body) -> {
              case bit_array.to_string(body) {
                Ok(body_string) -> {
                  // Get the lexicon for validation
                  case database.get_lexicon(db, nsid) {
                    Ok([lexicon_record]) -> {
                      // Validate the new record against the lexicon
                      case
                        lexicon.validate_record(
                          [lexicon_record.json],
                          nsid,
                          body_string,
                        )
                      {
                        Ok(_) -> {
                          // TODO: Update the record in the database
                          // This would require extracting the updated values
                          // and calling database.insert_record (which upserts)

                          wisp.response(200)
                          |> wisp.set_header("content-type", "application/json")
                          |> wisp.set_body(wisp.Text(
                            "{\"uri\": \""
                            <> uri
                            <> "\", \"cid\": \"bafyupdated\"}",
                          ))
                        }
                        Error(validation_error) -> {
                          wisp.response(400)
                          |> wisp.set_header("content-type", "application/json")
                          |> wisp.set_body(wisp.Text(
                            "{\"error\": \"InvalidRecord\", \"message\": \""
                            <> lexicon.describe_error(validation_error)
                            <> "\"}",
                          ))
                        }
                      }
                    }
                    Ok([]) -> {
                      wisp.response(404)
                      |> wisp.set_header("content-type", "application/json")
                      |> wisp.set_body(wisp.Text(
                        "{\"error\": \"LexiconNotFound\", \"message\": \"No lexicon found for collection\"}",
                      ))
                    }
                    Ok(_) -> {
                      wisp.response(500)
                      |> wisp.set_header("content-type", "application/json")
                      |> wisp.set_body(wisp.Text(
                        "{\"error\": \"InternalError\", \"message\": \"Multiple lexicons found\"}",
                      ))
                    }
                    Error(_db_error) -> {
                      wisp.response(500)
                      |> wisp.set_header("content-type", "application/json")
                      |> wisp.set_body(wisp.Text(
                        "{\"error\": \"InternalError\", \"message\": \"Failed to fetch lexicon\"}",
                      ))
                    }
                  }
                }
                Error(_) -> {
                  wisp.response(400)
                  |> wisp.set_header("content-type", "application/json")
                  |> wisp.set_body(wisp.Text(
                    "{\"error\": \"InvalidRequest\", \"message\": \"Request body must be valid UTF-8\"}",
                  ))
                }
              }
            }
            Error(_) -> {
              wisp.response(400)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"InvalidRequest\", \"message\": \"Failed to read request body\"}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"InvalidRequest\", \"message\": \"Missing required query parameter: uri\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"MethodNotAllowed\", \"message\": \"Only POST is allowed\"}",
      ))
    }
  }
}

/// Handle deleteRecord XRPC method
pub fn handle_delete_record(
  req: wisp.Request,
  db: sqlight.Connection,
  _nsid: String,
) -> wisp.Response {
  // Only accept POST requests (AT Protocol uses POST for deletes)
  case req.method {
    http.Post -> {
      // Extract URI from query parameters
      let query_params = wisp.get_query(req)
      case list.key_find(query_params, "uri") {
        Ok(uri) -> {
          // Delete the record from the database
          case database.delete_record(db, uri) {
            Ok(_) -> {
              wisp.response(200)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text("{\"success\": true}"))
            }
            Error(_db_error) -> {
              wisp.response(500)
              |> wisp.set_header("content-type", "application/json")
              |> wisp.set_body(wisp.Text(
                "{\"error\": \"InternalError\", \"message\": \"Failed to delete record\"}",
              ))
            }
          }
        }
        Error(_) -> {
          wisp.response(400)
          |> wisp.set_header("content-type", "application/json")
          |> wisp.set_body(wisp.Text(
            "{\"error\": \"InvalidRequest\", \"message\": \"Missing required query parameter: uri\"}",
          ))
        }
      }
    }
    _ -> {
      wisp.response(405)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"error\": \"MethodNotAllowed\", \"message\": \"Only POST is allowed\"}",
      ))
    }
  }
}

// Helper response functions

fn unauthorized_response() -> wisp.Response {
  wisp.response(401)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"AuthRequired\", \"message\": \"Valid authorization required\"}",
  ))
}

fn bad_request_response(message: String) -> wisp.Response {
  wisp.response(400)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"BadRequest\", \"message\": \"" <> message <> "\"}",
  ))
}

fn internal_error_response(message: String) -> wisp.Response {
  wisp.response(500)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"InternalError\", \"message\": \"" <> message <> "\"}",
  ))
}
