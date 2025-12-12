/// OG image generation for the site
import lustre/attribute.{styles}
import lustre/element.{type Element}
import lustre/element/html
import og_image
import www/logo

/// Render the single OG image for the site
pub fn render() -> Result(BitArray, og_image.RenderError) {
  let config = og_image.Config(..og_image.defaults(), format: og_image.WebP(90))
  build_element()
  |> og_image.render(config)
}

/// Build the Lustre element for the OG image
fn build_element() -> Element(Nil) {
  html.div(
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
      brand(),
      tagline(),
      bottom_border(),
    ],
  )
}

/// Logo and quickslice text at top
fn brand() -> Element(Nil) {
  html.div(
    [
      styles([
        #("display", "flex"),
        #("align-items", "center"),
        #("gap", "16px"),
      ]),
    ],
    [
      html.div(
        [
          styles([
            #("width", "64px"),
            #("height", "64px"),
          ]),
        ],
        [logo.logo()],
      ),
      html.div(
        [
          styles([
            #("color", "#ffffff"),
            #("font-size", "56px"),
            #("font-weight", "700"),
          ]),
        ],
        [html.text("quickslice")],
      ),
    ],
  )
}

/// Tagline text
fn tagline() -> Element(Nil) {
  html.div(
    [
      styles([
        #("color", "#888888"),
        #("font-size", "36px"),
        #("font-weight", "400"),
        #("line-height", "1.4"),
        #("max-width", "900px"),
      ]),
    ],
    [html.text("Auto-indexing service and GraphQL API for AT Protocol Records")],
  )
}

/// Gradient border along bottom edge
fn bottom_border() -> Element(Nil) {
  html.div(
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

/// Get the output path for the OG image
pub fn output_path() -> String {
  "priv/og/default.webp"
}
