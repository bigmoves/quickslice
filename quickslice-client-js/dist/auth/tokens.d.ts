/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export declare function getValidAccessToken(tokenUrl: string): Promise<string>;
/**
 * Store tokens from OAuth response
 */
export declare function storeTokens(tokens: {
    access_token: string;
    refresh_token?: string;
    expires_in: number;
    sub?: string;
}): void;
/**
 * Check if we have a valid session
 */
export declare function hasValidSession(): boolean;
