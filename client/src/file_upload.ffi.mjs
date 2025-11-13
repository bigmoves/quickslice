/// JavaScript FFI for file uploads

import { Error, Ok } from "./gleam.mjs";

/**
 * Read a file from an input element and encode it as base64
 * This is designed to work with Lustre's effect system by handling async via callbacks
 * @param {string} fileInputId - The ID of the file input element
 * @param {function} dispatch - The dispatch function to call with the result
 * @returns {void}
 */
export function readFileAsBase64(fileInputId, dispatch) {
  const input = document.getElementById(fileInputId);

  if (!input) {
    dispatch(new Error("File input not found"));
    return;
  }

  const file = input.files?.[0];

  if (!file) {
    console.log("[readFileAsBase64] No file selected");
    dispatch(new Error("No file selected"));
    return;
  }

  const reader = new FileReader();

  reader.onload = (e) => {
    try {
      const base64 = e.target.result.split(",")[1]; // Remove data:... prefix
      dispatch(new Ok(base64));
    } catch (err) {
      dispatch(new Error(`Failed to encode file: ${err.message}`));
    }
  };

  reader.onerror = () => {
    dispatch(new Error("Failed to read file"));
  };

  reader.readAsDataURL(file);
}

/**
 * Clear a file input element
 * @param {string} fileInputId - The ID of the file input element
 * @returns {void}
 */
export function clearFileInput(fileInputId) {
  const input = document.getElementById(fileInputId);
  if (input) {
    input.value = "";
  }
}
