/// Shared helper functions for settings page components
import components/alert
import gleam/option.{type Option, None, Some}
import lustre/element.{type Element}
import pages/settings/types.{type Msg}

/// Render an alert message if present.
/// Used by all section modules to display success/error/info alerts.
pub fn render_alert(alert_state: Option(#(String, String))) -> Element(Msg) {
  case alert_state {
    Some(#(kind, message)) -> {
      let alert_kind = case kind {
        "success" -> alert.Success
        "error" -> alert.Error
        _ -> alert.Info
      }
      alert.alert(alert_kind, message)
    }
    None -> element.none()
  }
}
