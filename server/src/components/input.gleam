import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

/// Standard input styling used throughout the app
const input_classes = "font-mono px-4 py-2 text-sm text-zinc-300 bg-zinc-900 border border-zinc-800 rounded focus:outline-none focus:border-zinc-700 w-full"

/// Standard label styling
const label_classes = "block text-sm text-zinc-400 mb-2"

/// Render a text input field with label
pub fn text_input(
  label label: String,
  name name: String,
  value value: String,
  placeholder placeholder: String,
  on_input on_input: fn(String) -> msg,
) -> Element(msg) {
  html.div([attribute.class("mb-4")], [
    html.label([attribute.class(label_classes), attribute.for(name)], [
      element.text(label),
    ]),
    html.input([
      attribute.type_("text"),
      attribute.name(name),
      attribute.id(name),
      attribute.value(value),
      attribute.placeholder(placeholder),
      attribute.class(input_classes),
      event.on_input(on_input),
    ]),
  ])
}

/// Render a text input field with label for forms (no event handler)
pub fn form_text_input(
  label label: String,
  name name: String,
  value value: String,
  placeholder placeholder: String,
  required required: Bool,
) -> Element(msg) {
  let required_attr = case required {
    True -> [attribute.attribute("required", "")]
    False -> []
  }

  html.div([attribute.class("mb-4")], [
    html.label([attribute.class(label_classes), attribute.for(name)], [
      element.text(label),
      case required {
        True ->
          html.span([attribute.class("text-red-500 ml-1")], [element.text("*")])
        False -> element.none()
      },
    ]),
    html.input(
      [
        attribute.type_("text"),
        attribute.name(name),
        attribute.id(name),
        attribute.value(value),
        attribute.placeholder(placeholder),
        attribute.class(input_classes),
        ..required_attr
      ],
    ),
  ])
}

/// Render a file input field with label for forms
pub fn form_file_input(
  label label: String,
  name name: String,
  accept accept: String,
  required required: Bool,
) -> Element(msg) {
  let required_attr = case required {
    True -> [attribute.attribute("required", "")]
    False -> []
  }

  html.div([attribute.class("mb-4")], [
    html.label([attribute.class(label_classes), attribute.for(name)], [
      element.text(label),
      case required {
        True ->
          html.span([attribute.class("text-red-500 ml-1")], [element.text("*")])
        False -> element.none()
      },
    ]),
    html.input(
      [
        attribute.type_("file"),
        attribute.name(name),
        attribute.id(name),
        attribute.attribute("accept", accept),
        attribute.class(input_classes),
        ..required_attr
      ],
    ),
  ])
}

