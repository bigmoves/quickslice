/// Upload interface handler
///
/// Serves a simple form to upload blobs and test the uploadBlob mutation
import gleam/http
import lustre/element
import oauth/handlers
import oauth/session
import pages/upload
import sqlight
import wisp

pub fn handle_upload_request(
  req: wisp.Request,
  db: sqlight.Connection,
  oauth_config: handlers.OAuthConfig,
) -> wisp.Response {
  case req.method {
    http.Get -> handle_upload_form(req, db, oauth_config)
    http.Post -> handle_upload_submit(req)
    _ -> method_not_allowed_response()
  }
}

fn handle_upload_form(
  req: wisp.Request,
  db: sqlight.Connection,
  oauth_config: handlers.OAuthConfig,
) -> wisp.Response {
  // Require authentication - get token from session (with automatic refresh)
  let refresh_fn = fn(refresh_token) {
    handlers.refresh_access_token(oauth_config, refresh_token)
  }

  case session.get_current_user(req, db, refresh_fn) {
    Error(_) -> {
      // User is not logged in - redirect to home with error
      wisp.redirect("/?error=Please+log+in+to+upload+blobs")
    }
    Ok(#(_did, handle, access_token)) -> {
      upload.view(handle, access_token)
      |> element.to_document_string
      |> wisp.html_response(200)
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
