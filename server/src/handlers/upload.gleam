/// Upload interface handler
///
/// Serves a simple form to upload blobs and test the uploadBlob mutation
import gleam/erlang/process.{type Subject}
import gleam/http
import lib/oauth/did_cache
import admin_session as session
import sqlight
import wisp

pub fn handle_upload_request(
  req: wisp.Request,
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
) -> wisp.Response {
  case req.method {
    http.Get -> handle_upload_form(req, db, did_cache)
    http.Post -> handle_upload_submit(req)
    _ -> method_not_allowed_response()
  }
}

fn handle_upload_form(
  req: wisp.Request,
  db: sqlight.Connection,
  did_cache: Subject(did_cache.Message),
) -> wisp.Response {
  // Require authentication - get token from session
  case session.get_current_user(req, db, did_cache) {
    Error(_) -> {
      // User is not logged in - redirect to home with error
      wisp.redirect("/?error=Please+log+in+to+upload+blobs")
    }
    Ok(#(_did, handle, _access_token)) -> {
      // TODO: Migrate upload page to client SPA
      wisp.html_response(
        "<h1>Upload</h1><p>Upload page will be migrated to the client SPA. Logged in as @"
          <> handle
          <> "</p>",
        200,
      )
    }
  }
}

fn handle_upload_submit(_req: wisp.Request) -> wisp.Response {
  // For now, we'll handle uploads via JavaScript on the client side
  // If we need server-side processing, we can implement it here
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"Not implemented\", \"message\": \"Use client-side upload\"}",
  ))
}

fn method_not_allowed_response() -> wisp.Response {
  wisp.response(405)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.set_body(wisp.Text(
    "{\"error\": \"MethodNotAllowed\", \"message\": \"Only GET is allowed\"}",
  ))
}
