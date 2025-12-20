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

export function createStorageKeys(namespace: string): StorageKeys {
  return {
    accessToken: `quickslice_${namespace}_access_token`,
    refreshToken: `quickslice_${namespace}_refresh_token`,
    tokenExpiresAt: `quickslice_${namespace}_token_expires_at`,
    clientId: `quickslice_${namespace}_client_id`,
    userDid: `quickslice_${namespace}_user_did`,
    codeVerifier: `quickslice_${namespace}_code_verifier`,
    oauthState: `quickslice_${namespace}_oauth_state`,
    redirectUri: `quickslice_${namespace}_redirect_uri`,
  };
}

export type StorageKey = string;
