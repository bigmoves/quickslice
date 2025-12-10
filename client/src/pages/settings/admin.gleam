/// Admin Management Section
///
/// Displays current admins list with add/remove functionality.
import components/button
import generated/queries/get_settings
import gleam/list
import gleam/option.{Some}
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import pages/settings/helpers.{render_alert}
import pages/settings/types.{
  type Model, type Msg, CancelRemoveAdmin, ConfirmRemoveAdmin, SubmitAddAdmin,
  SubmitRemoveAdmin, UpdateNewAdminDid,
}

pub fn view(settings: get_settings.Settings, model: Model) -> Element(Msg) {
  html.div([attribute.class("bg-zinc-800/50 rounded p-6")], [
    html.h2([attribute.class("text-xl font-semibold text-zinc-300 mb-4")], [
      element.text("Admin Management"),
    ]),
    render_alert(model.admin_alert),
    // Current admins list
    html.div([attribute.class("mb-6")], [
      html.h3([attribute.class("text-sm font-medium text-zinc-400 mb-2")], [
        element.text("Current Admins"),
      ]),
      html.ul([attribute.class("space-y-2")], {
        list.map(settings.admin_dids, fn(did) {
          html.li(
            [
              attribute.class(
                "flex items-center justify-between bg-zinc-900/50 px-3 py-2 rounded border border-zinc-800",
              ),
            ],
            [
              html.span(
                [attribute.class("font-mono text-sm text-zinc-300 truncate")],
                [element.text(did)],
              ),
              case model.remove_confirm_did {
                Some(confirm_did) if confirm_did == did ->
                  html.div([attribute.class("flex gap-2")], [
                    html.button(
                      [
                        attribute.class(
                          "text-xs px-2 py-1 text-zinc-400 hover:text-zinc-300 cursor-pointer",
                        ),
                        event.on_click(CancelRemoveAdmin),
                      ],
                      [element.text("Cancel")],
                    ),
                    html.button(
                      [
                        attribute.class(
                          "text-xs px-2 py-1 text-red-400 hover:bg-red-900/30 rounded cursor-pointer",
                        ),
                        event.on_click(SubmitRemoveAdmin),
                      ],
                      [element.text("Confirm Remove")],
                    ),
                  ])
                _ ->
                  html.button(
                    [
                      attribute.class(
                        "text-xs px-2 py-1 text-zinc-500 hover:text-red-400 transition-colors cursor-pointer",
                      ),
                      event.on_click(ConfirmRemoveAdmin(did)),
                    ],
                    [element.text("Remove")],
                  )
              },
            ],
          )
        })
      }),
    ]),
    // Add admin form
    html.div([attribute.class("border-t border-zinc-800 pt-4")], [
      html.h3([attribute.class("text-sm font-medium text-zinc-400 mb-2")], [
        element.text("Add Admin"),
      ]),
      html.div([attribute.class("flex gap-2")], [
        html.input([
          attribute.type_("text"),
          attribute.class(
            "flex-1 font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700",
          ),
          attribute.placeholder("did:plc:... or did:web:..."),
          attribute.value(model.new_admin_did),
          event.on_input(UpdateNewAdminDid),
        ]),
        button.button(
          disabled: model.new_admin_did == "",
          on_click: SubmitAddAdmin,
          text: "Add Admin",
        ),
      ]),
      html.p([attribute.class("text-xs text-zinc-500 mt-2")], [
        element.text("Enter the DID of the user you want to make an admin."),
      ]),
    ]),
  ])
}
