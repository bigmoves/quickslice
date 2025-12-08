import { LoginOptions } from './auth/oauth';
export interface QuicksliceClientOptions {
    server: string;
    clientId: string;
}
export interface User {
    did: string;
}
export declare class QuicksliceClient {
    private server;
    private clientId;
    private graphqlUrl;
    private authorizeUrl;
    private tokenUrl;
    private initialized;
    constructor(options: QuicksliceClientOptions);
    /**
     * Initialize the client - must be called before other methods
     */
    init(): Promise<void>;
    /**
     * Start OAuth login flow
     */
    loginWithRedirect(options?: LoginOptions): Promise<void>;
    /**
     * Handle OAuth callback after redirect
     * Returns true if callback was handled
     */
    handleRedirectCallback(): Promise<boolean>;
    /**
     * Logout and clear all stored data
     */
    logout(options?: {
        reload?: boolean;
    }): Promise<void>;
    /**
     * Check if user is authenticated
     */
    isAuthenticated(): Promise<boolean>;
    /**
     * Get current user's DID (from stored token data)
     * For richer profile info, use client.query() with your own schema
     */
    getUser(): User | null;
    /**
     * Get access token (auto-refreshes if needed)
     */
    getAccessToken(): Promise<string>;
    /**
     * Execute a GraphQL query (authenticated)
     */
    query<T = unknown>(query: string, variables?: Record<string, unknown>): Promise<T>;
    /**
     * Execute a GraphQL mutation (authenticated)
     */
    mutate<T = unknown>(mutation: string, variables?: Record<string, unknown>): Promise<T>;
    /**
     * Execute a public GraphQL query (no auth)
     */
    publicQuery<T = unknown>(query: string, variables?: Record<string, unknown>): Promise<T>;
}
