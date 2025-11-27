/// JavaScript FFI for quickslice_client

/**
 * Get the window origin (protocol + host + port)
 * @returns {string} The window origin (e.g., "http://localhost:8080")
 */
export function getWindowOrigin() {
  return window.location.origin;
}

/**
 * Set a timeout to call a callback after a delay
 * @param {function} callback - The function to call
 * @param {number} milliseconds - The delay in milliseconds
 * @returns {void}
 */
export function doSetTimeout(callback, milliseconds) {
  setTimeout(callback, milliseconds);
}
