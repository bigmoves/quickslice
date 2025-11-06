import lustre/attribute
import lustre/element.{type Element}
import lustre/element/svg

/// Render the Slices logo SVG
pub fn view(class: String) -> Element(msg) {
  svg.svg(
    [
      attribute.attribute("viewBox", "0 0 60 60"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.class(class),
    ],
    [
      // Define gradients
      svg.defs(
        [],
        [
          svg.linear_gradient(
            [
              attribute.id("board1"),
              attribute.attribute("x1", "0%"),
              attribute.attribute("y1", "0%"),
              attribute.attribute("x2", "100%"),
              attribute.attribute("y2", "100%"),
            ],
            [
              svg.stop([
                attribute.attribute("offset", "0%"),
                attribute.attribute("stop-color", "#FF6347"),
                attribute.attribute("stop-opacity", "1"),
              ]),
              svg.stop([
                attribute.attribute("offset", "100%"),
                attribute.attribute("stop-color", "#FF4500"),
                attribute.attribute("stop-opacity", "1"),
              ]),
            ],
          ),
          svg.linear_gradient(
            [
              attribute.id("board2"),
              attribute.attribute("x1", "0%"),
              attribute.attribute("y1", "0%"),
              attribute.attribute("x2", "100%"),
              attribute.attribute("y2", "100%"),
            ],
            [
              svg.stop([
                attribute.attribute("offset", "0%"),
                attribute.attribute("stop-color", "#00CED1"),
                attribute.attribute("stop-opacity", "1"),
              ]),
              svg.stop([
                attribute.attribute("offset", "100%"),
                attribute.attribute("stop-color", "#4682B4"),
                attribute.attribute("stop-opacity", "1"),
              ]),
            ],
          ),
        ],
      ),
      // Surfboard/skateboard deck shapes stacked
      svg.g([attribute.attribute("transform", "translate(30, 30)")], [
        // Top board slice
        svg.ellipse([
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "-8"),
          attribute.attribute("rx", "15"),
          attribute.attribute("ry", "6"),
          attribute.attribute("fill", "url(#board1)"),
        ]),
        // Middle board slice
        svg.ellipse([
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "0"),
          attribute.attribute("rx", "18"),
          attribute.attribute("ry", "6"),
          attribute.attribute("fill", "url(#board2)"),
        ]),
        // Bottom board slice
        svg.ellipse([
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "8"),
          attribute.attribute("rx", "12"),
          attribute.attribute("ry", "6"),
          attribute.attribute("fill", "#32CD32"),
        ]),
      ]),
    ],
  )
}
