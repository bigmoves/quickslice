import { storage } from './storage/storage';
import { STORAGE_KEYS } from './storage/keys';
import { getOrCreateDPoPKey } from './auth/dpop';
import { initiateLogin, handleOAuthCallback, logout as doLogout, LoginOptions } from './auth/oauth';
import { getValidAccessToken, hasValidSession } from './auth/tokens';
import { graphqlRequest } from './graphql';

export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
}

export interface User {
  did: string;
}

export class QuicksliceClient {
  private server: string;
  private clientId: string;
  private graphqlUrl: string;
  private authorizeUrl: string;
  private tokenUrl: string;
  private initialized = false;

  constructor(options: QuicksliceClientOptions) {
    this.server = options.server.replace(/\/$/, ''); // Remove trailing slash
    this.clientId = options.clientId;

    this.graphqlUrl = `${this.server}/graphql`;
    this.authorizeUrl = `${this.server}/oauth/authorize`;
    this.tokenUrl = `${this.server}/oauth/token`;
  }

  /**
   * Initialize the client - must be called before other methods
   */
  async init(): Promise<void> {
    if (this.initialized) return;

    // Ensure DPoP key exists
    await getOrCreateDPoPKey();

    this.initialized = true;
  }

  /**
   * Start OAuth login flow
   */
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.authorizeUrl, this.clientId, options);
  }

  /**
   * Handle OAuth callback after redirect
   * Returns true if callback was handled
   */
  async handleRedirectCallback(): Promise<boolean> {
    await this.init();
    return await handleOAuthCallback(this.tokenUrl);
  }

  /**
   * Logout and clear all stored data
   */
  async logout(options: { reload?: boolean } = {}): Promise<void> {
    await doLogout(options);
  }

  /**
   * Check if user is authenticated
   */
  async isAuthenticated(): Promise<boolean> {
    return hasValidSession();
  }

  /**
   * Get current user's DID (from stored token data)
   * For richer profile info, use client.query() with your own schema
   */
  getUser(): User | null {
    if (!hasValidSession()) {
      return null;
    }

    const did = storage.get(STORAGE_KEYS.userDid);
    if (!did) {
      return null;
    }

    return { did };
  }

  /**
   * Get access token (auto-refreshes if needed)
   */
  async getAccessToken(): Promise<string> {
    await this.init();
    return await getValidAccessToken(this.tokenUrl);
  }

  /**
   * Execute a GraphQL query (authenticated)
   */
  async query<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      true
    );
  }

  /**
   * Execute a GraphQL mutation (authenticated)
   */
  async mutate<T = unknown>(
    mutation: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    return this.query<T>(mutation, variables);
  }

  /**
   * Execute a public GraphQL query (no auth)
   */
  async publicQuery<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      false
    );
  }
}
