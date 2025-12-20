/**
 * Storage key factory - generates namespaced keys
 */
export interface StorageKeys {
    accessToken: string;
    refreshToken: string;
    tokenExpiresAt: string;
    clientId: string;
    userDid: string;
    codeVerifier: string;
    oauthState: string;
    redirectUri: string;
}
export declare function createStorageKeys(namespace: string): StorageKeys;
export type StorageKey = string;
