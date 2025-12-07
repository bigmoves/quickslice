/// OG image generation for doc pages
import lustre/attribute.{attribute, styles}
import lustre/element.{type Element}
import lustre/element/html.{div, img, text}
import og_image
import www/config.{type DocPage}

/// The quickslice logo as raw SVG string (for og_image rendering)
const logo_svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 128 128\"><defs><linearGradient id=\"board1\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"100%\"><stop offset=\"0%\" stop-color=\"#FF6347\" stop-opacity=\"1\"/><stop offset=\"100%\" stop-color=\"#FF4500\" stop-opacity=\"1\"/></linearGradient><linearGradient id=\"board2\" x1=\"0%\" y1=\"0%\" x2=\"100%\" y2=\"100%\"><stop offset=\"0%\" stop-color=\"#00CED1\" stop-opacity=\"1\"/><stop offset=\"100%\" stop-color=\"#4682B4\" stop-opacity=\"1\"/></linearGradient></defs><g transform=\"translate(64, 64)\"><ellipse cx=\"0\" cy=\"-28\" rx=\"50\" ry=\"20\" fill=\"url(#board1)\"/><ellipse cx=\"0\" cy=\"0\" rx=\"60\" ry=\"20\" fill=\"url(#board2)\"/><ellipse cx=\"0\" cy=\"28\" rx=\"40\" ry=\"20\" fill=\"#32CD32\"/></g></svg>"

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
      logo(),
      divider(),
      title(page.title),
      domain(),
    ],
  )
}

/// Logo as img element with raw SVG source
fn logo() -> Element(Nil) {
  img([
    attribute("src", logo_svg),
    styles([
      #("width", "80px"),
      #("height", "80px"),
    ]),
  ])
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
