/// OG image generation for doc pages
import lustre/attribute.{styles}
import lustre/element.{type Element}
import lustre/element/html.{div, text}
import og_image
import www/config.{type DocPage}
import www/logo

/// Render an OG image for a doc page
pub fn render(page: DocPage) -> Result(BitArray, og_image.RenderError) {
  let config = og_image.Config(..og_image.defaults(), format: og_image.WebP(90))
  page
  |> build_element
  |> og_image.render(config)
}

/// Build the Lustre element for the OG image
fn build_element(page: DocPage) -> Element(Nil) {
  div(
    [
      styles([
        #("display", "flex"),
        #("flex-direction", "column"),
        #("justify-content", "space-between"),
        #("align-items", "flex-start"),
        #("width", "100%"),
        #("height", "100%"),
        #("padding", "80px"),
        #("background-color", "#0f0f17"),
        #("font-family", "Geist Sans"),
        #("position", "relative"),
      ]),
    ],
    [
      title(page.title),
      brand(),
      bottom_border(),
    ],
  )
}

/// Logo and quickslice text at bottom
fn brand() -> Element(Nil) {
  div(
    [
      styles([
        #("display", "flex"),
        #("align-items", "center"),
        #("gap", "12px"),
      ]),
    ],
    [
      div(
        [
          styles([
            #("width", "48px"),
            #("height", "48px"),
          ]),
        ],
        [logo.logo()],
      ),
      div(
        [
          styles([
            #("color", "#888888"),
            #("font-size", "40px"),
            #("font-weight", "600"),
          ]),
        ],
        [text("quickslice")],
      ),
    ],
  )
}

/// Gradient border along bottom edge
fn bottom_border() -> Element(Nil) {
  div(
    [
      styles([
        #("position", "absolute"),
        #("bottom", "0"),
        #("left", "0"),
        #("width", "100%"),
        #("height", "4px"),
        #("background", "linear-gradient(90deg, #FF6347, #00CED1, #32CD32)"),
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

/// Get the output path for a page's OG image
pub fn output_path(page: DocPage) -> String {
  "priv/og/" <> page.slug <> ".webp"
}
