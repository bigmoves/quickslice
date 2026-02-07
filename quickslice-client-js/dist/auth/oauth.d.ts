import { Storage } from '../storage/storage';
import { SessionInfo } from './session';
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
 * Handle OAuth callback - exchange code for tokens and create session
 * Returns session info if callback was handled, null if not a callback
 */
export declare function handleOAuthCallback(storage: Storage, namespace: string, tokenUrl: string, serverUrl: string): Promise<SessionInfo | null>;
/**
 * Logout - destroy session and clear local data
 */
export declare function logout(storage: Storage, namespace: string, serverUrl: string, options?: {
    reload?: boolean;
}): Promise<void>;
