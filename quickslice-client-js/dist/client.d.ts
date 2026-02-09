import { LoginOptions } from './auth/oauth';
import { SessionInfo } from './auth/session';
export interface QuicksliceClientOptions {
    server: string;
    clientId: string;
    redirectUri?: string;
    scope?: string;
}
export interface User {
    did: string;
    handle?: string;
}
export interface QueryOptions {
    signal?: AbortSignal;
}
export declare class QuicksliceClient {
    private server;
    private clientId;
    private redirectUri?;
    private scope?;
    private graphqlUrl;
    private authorizeUrl;
    private tokenUrl;
    private initialized;
    private namespace;
    private storage;
    private cachedSession;
    constructor(options: QuicksliceClientOptions);
    /**
     * Initialize the client - must be called before other methods
     */
    init(): Promise<void>;
    private getStorage;
    /**
     * Start OAuth login flow
     */
    loginWithRedirect(options?: LoginOptions): Promise<void>;
    /**
     * Handle OAuth callback after redirect
     * Returns the session info if callback was handled, null otherwise
     */
    handleRedirectCallback(): Promise<SessionInfo | null>;
    /**
     * Logout and clear all stored data
     */
    logout(options?: {
        reload?: boolean;
    }): Promise<void>;
    /**
     * Check if user is authenticated
     * Queries the server to verify session is valid
     */
    isAuthenticated(): Promise<boolean>;
    /**
     * Get current user info from session
     * For richer profile info, use client.query() with your own schema
     */
    getUser(): Promise<User | null>;
    /**
     * Execute a GraphQL query (authenticated)
     * Uses session cookie for auth - no client-side token management
     */
    query<T = unknown>(query: string, variables?: Record<string, unknown>, options?: QueryOptions): Promise<T>;
    /**
     * Execute a GraphQL mutation (authenticated)
     */
    mutate<T = unknown>(mutation: string, variables?: Record<string, unknown>, options?: QueryOptions): Promise<T>;
    /**
     * Execute a public GraphQL query (no auth)
     */
    publicQuery<T = unknown>(query: string, variables?: Record<string, unknown>, options?: QueryOptions): Promise<T>;
}
