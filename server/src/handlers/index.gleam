/// Index/SPA handler
///
/// Serves the client SPA's index.html for root and fallback routes
import simplifile
import wisp

/// Handle index route and SPA fallback
/// Serves the bundled client application's index.html
pub fn handle() -> wisp.Response {
  // Serve the client SPA's index.html (bundled by lustre dev tools)
  let assert Ok(priv_dir) = wisp.priv_directory("server")
  let index_path = priv_dir <> "/static/index.html"

  case simplifile.read(index_path) {
    Ok(contents) -> wisp.html_response(contents, 200)
    Error(_) ->
      wisp.html_response(
        "<h1>Error</h1><p>Client application not found. Run 'gleam run -m lustre/dev build' in the client directory.</p>",
        500,
      )
  }
}
