/**
 * Navigate to an external URL (outside the SPA)
 */
export function navigateToExternal(url) {
  globalThis.location.href = url;
}
