/**
 * Storage key constants
 */
export declare const STORAGE_KEYS: {
    readonly accessToken: "quickslice_access_token";
    readonly refreshToken: "quickslice_refresh_token";
    readonly tokenExpiresAt: "quickslice_token_expires_at";
    readonly clientId: "quickslice_client_id";
    readonly userDid: "quickslice_user_did";
    readonly codeVerifier: "quickslice_code_verifier";
    readonly oauthState: "quickslice_oauth_state";
    readonly redirectUri: "quickslice_redirect_uri";
};
export type StorageKey = (typeof STORAGE_KEYS)[keyof typeof STORAGE_KEYS];
