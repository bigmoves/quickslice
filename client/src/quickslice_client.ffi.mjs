/// JavaScript FFI for quickslice_client

/**
 * Set a timeout to call a callback after a delay
 * @param {function} callback - The function to call
 * @param {number} milliseconds - The delay in milliseconds
 * @returns {void}
 */
export function doSetTimeout(callback, milliseconds) {
  setTimeout(callback, milliseconds);
}
