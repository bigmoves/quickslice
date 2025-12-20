import { Storage } from '../storage/storage';
export interface LoginOptions {
    handle?: string;
    redirectUri?: string;
    scope?: string;
}
/**
 * Initiate OAuth login flow with PKCE
 */
export declare function initiateLogin(storage: Storage, authorizeUrl: string, clientId: string, options?: LoginOptions): Promise<void>;
/**
 * Handle OAuth callback - exchange code for tokens
 * Returns true if callback was handled, false if not a callback
 */
export declare function handleOAuthCallback(storage: Storage, namespace: string, tokenUrl: string): Promise<boolean>;
/**
 * Logout - clear all stored data
 */
export declare function logout(storage: Storage, namespace: string, options?: {
    reload?: boolean;
}): Promise<void>;
