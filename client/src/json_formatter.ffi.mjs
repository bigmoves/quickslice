/// JavaScript FFI for JSON formatting

export function prettyPrint(jsonString) {
  try {
    const obj = JSON.parse(jsonString);
    return JSON.stringify(obj, null, 2);
  } catch (_e) {
    // If parsing fails, return the original string
    return jsonString;
  }
}

// Recursively look for string properties that are JSON and parse them
function expandNestedJson(obj) {
  if (typeof obj === "string") {
    try {
      // Try to parse as JSON
      const parsed = JSON.parse(obj);
      return expandNestedJson(parsed);
    } catch (_e) {
      return obj;
    }
  } else if (Array.isArray(obj)) {
    return obj.map(expandNestedJson);
  } else if (obj !== null && typeof obj === "object") {
    const expanded = {};
    for (const [key, value] of Object.entries(obj)) {
      expanded[key] = expandNestedJson(value);
    }
    return expanded;
  }
  return obj;
}

// Pretty print with nested JSON string expansion
export function prettyPrintNested(jsonString) {
  try {
    const obj = JSON.parse(jsonString);
    const expanded = expandNestedJson(obj);
    return JSON.stringify(expanded, null, 2);
  } catch (_e) {
    // If parsing fails, return the original string
    return jsonString;
  }
}
