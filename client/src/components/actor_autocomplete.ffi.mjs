/// JavaScript FFI for actor autocomplete
/// Provides debounced search against Bluesky's public API

import { Ok, Error, toList } from "../gleam.mjs";
import { Some, None } from "../../gleam_stdlib/gleam/option.mjs";
import { Actor } from "./actor_autocomplete.mjs";

let debounceTimer = null;

/**
 * Search for actors with debouncing
 * @param {string} query - The search query
 * @param {number} debounceMs - Debounce delay in milliseconds
 * @param {function} dispatch - Callback to dispatch results
 */
export function searchActors(query, debounceMs, dispatch) {
  // Clear any pending search
  if (debounceTimer) {
    clearTimeout(debounceTimer);
  }

  // Don't search empty queries
  if (!query || query.trim() === "") {
    dispatch(new Ok(toList([])));
    return;
  }

  debounceTimer = setTimeout(async () => {
    try {
      const url = new URL("https://public.api.bsky.app/xrpc/app.bsky.actor.searchActorsTypeahead");
      url.searchParams.set("q", query);
      url.searchParams.set("limit", "5");

      const response = await fetch(url.toString());

      if (!response.ok) {
        dispatch(new Error("Search failed"));
        return;
      }

      const data = await response.json();

      // Map to Gleam Actor records with proper Option types
      const actors = (data.actors || []).map(actor =>
        new Actor(
          actor.did || "",
          actor.handle || "",
          actor.displayName || "",
          actor.avatar ? new Some(actor.avatar) : new None()
        )
      );

      dispatch(new Ok(toList(actors)));
    } catch (err) {
      dispatch(new Error(err.message || "Search failed"));
    }
  }, debounceMs);
}

/**
 * Cancel any pending search
 */
export function cancelSearch() {
  if (debounceTimer) {
    clearTimeout(debounceTimer);
    debounceTimer = null;
  }
}
