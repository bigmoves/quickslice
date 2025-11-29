/// Backfill Animation Component
///
/// Animated SVG showing pulsing ellipses to indicate backfill in progress
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/svg

/// Render the backfill animation SVG
pub fn view(class: String) -> Element(msg) {
  svg.svg(
    [
      attribute.attribute("viewBox", "0 0 128 128"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("overflow", "visible"),
      attribute.class(class),
    ],
    [
      // Define gradients
      svg.defs([], [
        svg.linear_gradient(
          [
            attribute.id("backfill-board1"),
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
            attribute.id("backfill-board2"),
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
        // Inline style for animations
        element.element("style", [], [
          element.text(
            "
            .backfill-ellipse1 { animation: backfill-pulse 1.5s ease-in-out infinite; }
            .backfill-ellipse2 { animation: backfill-pulse 1.5s ease-in-out infinite 0.2s; }
            .backfill-ellipse3 { animation: backfill-pulse 1.5s ease-in-out infinite 0.4s; }
            @keyframes backfill-pulse {
              0%, 100% { transform: scale(1); opacity: 1; }
              50% { transform: scale(1.1); opacity: 0.8; }
            }
          ",
          ),
        ]),
      ]),
      // Pulsing ellipses
      svg.g([attribute.attribute("transform", "translate(64, 64)")], [
        // Top ellipse
        svg.ellipse([
          attribute.class("backfill-ellipse1"),
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "-28"),
          attribute.attribute("rx", "50"),
          attribute.attribute("ry", "20"),
          attribute.attribute("fill", "url(#backfill-board1)"),
          attribute.attribute("style", "transform-origin: 0 -28px;"),
        ]),
        // Middle ellipse
        svg.ellipse([
          attribute.class("backfill-ellipse2"),
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "0"),
          attribute.attribute("rx", "60"),
          attribute.attribute("ry", "20"),
          attribute.attribute("fill", "url(#backfill-board2)"),
          attribute.attribute("style", "transform-origin: 0 0;"),
        ]),
        // Bottom ellipse
        svg.ellipse([
          attribute.class("backfill-ellipse3"),
          attribute.attribute("cx", "0"),
          attribute.attribute("cy", "28"),
          attribute.attribute("rx", "40"),
          attribute.attribute("ry", "20"),
          attribute.attribute("fill", "#32CD32"),
          attribute.attribute("style", "transform-origin: 0 28px;"),
        ]),
      ]),
    ],
  )
}
