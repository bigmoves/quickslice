/// Quickslice logo SVG
import lustre/attribute.{attribute, class, id}
import lustre/element.{type Element}
import lustre/element/svg

/// Render the quickslice logo SVG
pub fn logo() -> Element(Nil) {
  svg.svg(
    [
      attribute("viewBox", "0 0 128 128"),
      attribute("xmlns", "http://www.w3.org/2000/svg"),
      class("sidebar-logo"),
    ],
    [
      // Define gradients
      svg.defs([], [
        svg.linear_gradient(
          [
            id("board1"),
            attribute("x1", "0%"),
            attribute("y1", "0%"),
            attribute("x2", "100%"),
            attribute("y2", "100%"),
          ],
          [
            svg.stop([
              attribute("offset", "0%"),
              attribute("stop-color", "#FF6347"),
              attribute("stop-opacity", "1"),
            ]),
            svg.stop([
              attribute("offset", "100%"),
              attribute("stop-color", "#FF4500"),
              attribute("stop-opacity", "1"),
            ]),
          ],
        ),
        svg.linear_gradient(
          [
            id("board2"),
            attribute("x1", "0%"),
            attribute("y1", "0%"),
            attribute("x2", "100%"),
            attribute("y2", "100%"),
          ],
          [
            svg.stop([
              attribute("offset", "0%"),
              attribute("stop-color", "#00CED1"),
              attribute("stop-opacity", "1"),
            ]),
            svg.stop([
              attribute("offset", "100%"),
              attribute("stop-color", "#4682B4"),
              attribute("stop-opacity", "1"),
            ]),
          ],
        ),
      ]),
      // Surfboard/skateboard deck shapes stacked
      svg.g([attribute("transform", "translate(64, 64)")], [
        // Top board slice
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "-28"),
          attribute("rx", "50"),
          attribute("ry", "20"),
          attribute("fill", "url(#board1)"),
        ]),
        // Middle board slice
        svg.ellipse([
          attribute("cx", "0"),
          attribute("cy", "0"),
          attribute("rx", "60"),
          attribute("ry", "20"),
          attribute("fill", "url(#board2)"),
        ]),
        // Bottom board slice
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
