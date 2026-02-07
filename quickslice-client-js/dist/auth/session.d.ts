/**
 * Session management for cookie-based authentication
 *
 * Handles server-side session creation and management via HTTP-only cookies.
 * DPoP keys remain client-side for ATProto compatibility.
 */
export interface SessionInfo {
    authenticated: boolean;
    did: string | null;
    handle: string | null;
}
export interface CreateSessionOptions {
    clientId: string;
    userDid?: string;
    atpSessionId?: string;
}
/**
 * Create a new session on the server after OAuth callback
 *
 * This is called after the OAuth token exchange to establish a cookie-based session.
 * The server stores the tokens and returns a session cookie.
 */
export declare function createSession(serverUrl: string, namespace: string, options: CreateSessionOptions): Promise<SessionInfo>;
/**
 * Get current session status from the server
 *
 * Returns session info if a valid session cookie exists, otherwise returns
 * an unauthenticated session info.
 */
export declare function getSession(serverUrl: string): Promise<SessionInfo>;
/**
 * Destroy the current session (logout)
 *
 * Clears the session cookie and server-side session data.
 */
export declare function destroySession(serverUrl: string): Promise<void>;
