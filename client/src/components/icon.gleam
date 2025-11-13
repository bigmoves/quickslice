import lustre/attribute.{type Attribute}
import lustre/element.{type Element}

pub type Size {
  Xs
  Sm
  Md
  Lg
  Xl
}

/// Render a Lucide icon by name with a specific size
///
/// ## Examples
///
/// ```gleam
/// icon.icon("edit", icon.Sm, [])
/// icon.icon("user", icon.Md, [attribute.class("text-blue-500")])
/// ```
pub fn icon(
  name: String,
  size: Size,
  attrs: List(Attribute(msg)),
) -> Element(msg) {
  let #(width, height) = case size {
    Xs -> #("12", "12")
    Sm -> #("16", "16")
    Md -> #("20", "20")
    Lg -> #("24", "24")
    Xl -> #("32", "32")
  }

  lucide(name, [
    attribute.attribute("width", width),
    attribute.attribute("height", height),
    ..attrs
  ])
}

/// Render a Lucide icon by name with custom attributes
///
/// ## Examples
///
/// ```gleam
/// icon.lucide("edit", [attribute.class("size-4")])
/// icon.lucide("user", [attribute.class("size-6 text-blue-500")])
/// ```
@external(javascript, "./icon.ffi.mjs", "lucide")
pub fn lucide(name: String, attrs: List(Attribute(msg))) -> Element(msg)
