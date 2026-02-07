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
export declare function createStorageKeys(namespace: string): StorageKeys;
export type StorageKey = string;
