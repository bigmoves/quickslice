// FFI module for JSON formatting using JavaScript's JSON.stringify

export function format(jsonString) {
  try {
    const parsed = JSON.parse(jsonString);
    return JSON.stringify(parsed, null, 2);
  } catch (_e) {
    // If parsing fails, return original string
    return jsonString;
  }
}
