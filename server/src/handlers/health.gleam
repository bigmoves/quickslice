/// Health check endpoint handler
///
/// Handles /health endpoint with database connectivity verification
import database/executor.{type Executor}
import database/repositories/lexicons
import wisp

/// Handle health check request
/// Returns 200 if database is accessible, 503 if not
pub fn handle(db: Executor) -> wisp.Response {
  // Try a simple database query to verify connectivity
  case lexicons.get_count(db) {
    Ok(_) -> {
      // Database is accessible
      wisp.response(200)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text("{\"status\": \"healthy\"}"))
    }
    Error(_) -> {
      // Database is not accessible
      wisp.response(503)
      |> wisp.set_header("content-type", "application/json")
      |> wisp.set_body(wisp.Text(
        "{\"status\": \"unhealthy\", \"message\": \"Database connection failed\"}",
      ))
    }
  }
}
