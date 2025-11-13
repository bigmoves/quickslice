import * as icons from "lucide";
import { namespaced } from "../../lustre/lustre/element.mjs";
import { attribute } from "../../lustre/lustre/attribute.mjs";
import { prepend, toList } from "../../lustre/gleam.mjs";

const SVG_NAMESPACE = "http://www.w3.org/2000/svg";

/**
 * Convert Lucide icon data to Lustre SVG children elements
 * @param {Array} iconData - Array of [tagName, attributes] tuples
 * @returns {List} Lustre list of path elements
 */
function buildSvgChildren(iconData) {
  let children = toList([]);

  // Build children list in reverse since prepend adds to front
  for (let i = iconData.length - 1; i >= 0; i--) {
    const [tag, attrs] = iconData[i];

    // Convert JS object attrs to Lustre attribute list
    let attrList = toList([]);
    for (const [key, value] of Object.entries(attrs)) {
      attrList = prepend(attribute(key, value), attrList);
    }

    // Create the SVG element (path, circle, etc.)
    const childElement = namespaced(SVG_NAMESPACE, tag, attrList, toList([]));
    children = prepend(childElement, children);
  }

  return children;
}

/**
 * Render a Lucide icon as a proper SVG element
 * @param {string} name - The icon name (e.g., "edit", "user", "settings", "chevron-right")
 * @param {Array} attrs - Lustre attributes to apply to the SVG element
 * @returns {Object} Lustre namespaced SVG element
 */
export function lucide(name, attrs) {
  // Convert kebab-case or lowercase to PascalCase for Lucide icon names
  // e.g., "edit" -> "Edit", "chevron-right" -> "ChevronRight"
  const iconName = name
    .split("-")
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join("");

  const iconData = icons[iconName];

  if (!iconData) {
    console.warn(`Icon "${name}" (${iconName}) not found in lucide package`);
    // Return empty SVG element
    return namespaced(SVG_NAMESPACE, "svg", attrs, toList([]));
  }

  // Build SVG children from icon data
  const children = buildSvgChildren(iconData);

  // Check if user provided width/height attributes
  let hasWidth = false;
  let hasHeight = false;
  let current = attrs;

  // Iterate through the Gleam list (head/tail structure)
  while (current && current.head !== undefined) {
    const attr = current.head;
    // Check if this is an Attribute (kind: 0, not Property or Event)
    if (attr && attr.kind === 0) {
      if (attr.name === "width") hasWidth = true;
      if (attr.name === "height") hasHeight = true;
    }
    current = current.tail;
  }

  // Build default attributes, excluding width/height if user provided them
  const defaultAttrsList = [
    attribute("xmlns", SVG_NAMESPACE),
    attribute("viewBox", "0 0 24 24"),
    attribute("fill", "none"),
    attribute("stroke", "currentColor"),
    attribute("stroke-width", "2"),
    attribute("stroke-linecap", "round"),
    attribute("stroke-linejoin", "round"),
  ];

  if (!hasWidth) defaultAttrsList.push(attribute("width", "24"));
  if (!hasHeight) defaultAttrsList.push(attribute("height", "24"));

  const defaultAttrs = toList(defaultAttrsList);

  // Merge: user attributes first (higher priority), then defaults
  let mergedAttrs = defaultAttrs;
  current = attrs;
  while (current && current.head !== undefined) {
    mergedAttrs = prepend(current.head, mergedAttrs);
    current = current.tail;
  }

  // Return proper namespaced SVG element
  return namespaced(SVG_NAMESPACE, "svg", mergedAttrs, children);
}
