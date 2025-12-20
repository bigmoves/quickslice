import { Storage } from '../storage/storage';
/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export declare function getValidAccessToken(storage: Storage, namespace: string, tokenUrl: string): Promise<string>;
/**
 * Store tokens from OAuth response
 */
export declare function storeTokens(storage: Storage, tokens: {
    access_token: string;
    refresh_token?: string;
    expires_in: number;
    sub?: string;
}): void;
/**
 * Check if we have a valid session
 */
export declare function hasValidSession(storage: Storage): boolean;
