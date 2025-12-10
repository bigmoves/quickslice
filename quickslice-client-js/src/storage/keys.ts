/**
 * Storage key constants
 */
export const STORAGE_KEYS = {
  accessToken: 'quickslice_access_token',
  refreshToken: 'quickslice_refresh_token',
  tokenExpiresAt: 'quickslice_token_expires_at',
  clientId: 'quickslice_client_id',
  userDid: 'quickslice_user_did',
  codeVerifier: 'quickslice_code_verifier',
  oauthState: 'quickslice_oauth_state',
  redirectUri: 'quickslice_redirect_uri',
} as const;

export type StorageKey = (typeof STORAGE_KEYS)[keyof typeof STORAGE_KEYS];
