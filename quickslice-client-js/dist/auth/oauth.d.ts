export interface LoginOptions {
    handle?: string;
    redirectUri?: string;
}
/**
 * Initiate OAuth login flow with PKCE
 */
export declare function initiateLogin(authorizeUrl: string, clientId: string, options?: LoginOptions): Promise<void>;
/**
 * Handle OAuth callback - exchange code for tokens
 * Returns true if callback was handled, false if not a callback
 */
export declare function handleOAuthCallback(tokenUrl: string): Promise<boolean>;
/**
 * Logout - clear all stored data
 */
export declare function logout(options?: {
    reload?: boolean;
}): Promise<void>;
