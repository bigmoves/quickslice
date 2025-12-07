/// OG image generation for doc pages
import lustre/attribute.{styles}
import lustre/element.{type Element}
import lustre/element/html.{div, text}
import og_image
import www/config.{type DocPage}
import www/layout

/// Base URL for OG image links
pub const base_url = "https://quickslice.slices.network"

/// Render an OG image for a doc page
pub fn render(page: DocPage) -> Result(BitArray, og_image.RenderError) {
  page
  |> build_element
  |> og_image.render(og_image.defaults())
}

/// Build the Lustre element for the OG image
fn build_element(page: DocPage) -> Element(Nil) {
  div(
    [
      styles([
        #("display", "flex"),
        #("flex-direction", "column"),
        #("justify-content", "center"),
        #("align-items", "flex-start"),
        #("width", "100%"),
        #("height", "100%"),
        #("padding", "80px"),
        #("background-color", "#0f0f17"),
        #("font-family", "Geist Sans"),
      ]),
    ],
    [
      layout.logo(),
      divider(),
      title(page.title),
      domain(),
    ],
  )
}

/// Gradient divider line
fn divider() -> Element(Nil) {
  div(
    [
      styles([
        #("width", "300px"),
        #("height", "4px"),
        #("margin-top", "32px"),
        #("margin-bottom", "32px"),
        #("background", "linear-gradient(90deg, #FF6347, #00CED1, #32CD32)"),
        #("border-radius", "2px"),
      ]),
    ],
    [],
  )
}

/// Page title
fn title(page_title: String) -> Element(Nil) {
  div(
    [
      styles([
        #("color", "#ffffff"),
        #("font-size", "72px"),
        #("font-weight", "700"),
        #("line-height", "1.1"),
      ]),
    ],
    [text(page_title)],
  )
}

/// Domain text at bottom
fn domain() -> Element(Nil) {
  div(
    [
      styles([
        #("color", "#888888"),
        #("font-size", "24px"),
        #("margin-top", "auto"),
      ]),
    ],
    [text("quickslice.slices.network")],
  )
}

/// Get the output path for a page's OG image
pub fn output_path(page: DocPage) -> String {
  "priv/og/" <> page.slug <> ".png"
}

/// Get the URL for a page's OG image
pub fn url(page: DocPage) -> String {
  base_url <> "/og/" <> page.slug <> ".png"
}
