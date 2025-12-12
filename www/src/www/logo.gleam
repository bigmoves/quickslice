/// Quickslice logo SVG
import lustre/attribute.{attribute, class}
import lustre/element.{type Element}
import lustre/element/svg

/// Render the quickslice logo SVG
/// Uses solid colors to avoid gradient ID collisions when multiple logos are on the page
pub fn logo() -> Element(Nil) {
  svg.svg(
    [
      attribute("viewBox", "0 0 128 128"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
      class("sidebar-logo"),
    ],
    [
      // Surfboard/skateboard deck shapes stacked
      svg.g([attribute("transform", "translate(64, 64)")], [
        // Top board slice (red-orange)
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "-28"),
          attribute("rx", "50"),
          attribute("ry", "20"),
          attribute("fill", "#FF5722"),
        ]),
        // Middle board slice (cyan-blue)
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "0"),
          attribute("rx", "60"),
          attribute("ry", "20"),
          attribute("fill", "#00ACC1"),
        ]),
        // Bottom board slice (lime green)
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "28"),
          attribute("rx", "40"),
          attribute("ry", "20"),
          attribute("fill", "#32CD32"),
        ]),
      ]),
    ],
  )
}
