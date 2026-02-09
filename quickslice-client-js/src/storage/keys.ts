/**
 * Storage key factory - generates namespaced keys
 *
 * With cookie-based auth, we only need to store:
 * - OAuth flow state (sessionStorage, ephemeral)
 * - Client ID (for OAuth flow)
 *
 * Tokens are stored server-side and accessed via HTTP-only cookies.
 */
export interface StorageKeys {
  clientId: string;
  codeVerifier: string;
  oauthState: string;
  redirectUri: string;
}

export function createStorageKeys(namespace: string): StorageKeys {
  return {
    clientId: `quickslice_${namespace}_client_id`,
    codeVerifier: `quickslice_${namespace}_code_verifier`,
    oauthState: `quickslice_${namespace}_oauth_state`,
    redirectUri: `quickslice_${namespace}_redirect_uri`,
  };
}

export type StorageKey = string;
