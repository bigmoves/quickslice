/// Logout handler
import admin_session as session
import database/executor.{type Executor}
import database/repositories/admin_session as admin_session_repo
import wisp.{type Request, type Response}

/// Handle POST /logout - Clear session and redirect
pub fn handle(req: Request, db: Executor) -> Response {
  // Get session ID and delete from admin_session table
  case session.get_session_id(req) {
    Ok(session_id) -> {
      let _ = admin_session_repo.delete(db, session_id)
      wisp.log_info("Admin user logged out")
    }
    Error(_) -> Nil
  }

  // Clear cookie and redirect
  wisp.redirect("/")
  |> session.clear_session_cookie(req)
}
