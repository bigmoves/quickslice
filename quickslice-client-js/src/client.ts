import { createStorageKeys } from './storage/keys';
import { createStorage, Storage } from './storage/storage';
import { getOrCreateDPoPKey } from './auth/dpop';
import { initiateLogin, handleOAuthCallback, logout as doLogout, LoginOptions } from './auth/oauth';
import { getValidAccessToken, hasValidSession } from './auth/tokens';
import { graphqlRequest } from './graphql';
import { generateNamespaceHash } from './utils/crypto';

export interface QuicksliceClientOptions {
  server: string;
  clientId: string;
  redirectUri?: string;
  scope?: string;
}

export interface User {
  did: string;
}

export class QuicksliceClient {
  private server: string;
  private clientId: string;
  private redirectUri?: string;
  private scope?: string;
  private graphqlUrl: string;
  private authorizeUrl: string;
  private tokenUrl: string;
  private initialized = false;
  private namespace: string = '';
  private storage: Storage | null = null;

  constructor(options: QuicksliceClientOptions) {
    this.server = options.server.replace(/\/$/, ''); // Remove trailing slash
    this.clientId = options.clientId;
    this.redirectUri = options.redirectUri;
    this.scope = options.scope;

    this.graphqlUrl = `${this.server}/graphql`;
    this.authorizeUrl = `${this.server}/oauth/authorize`;
    this.tokenUrl = `${this.server}/oauth/token`;
  }

  /**
   * Initialize the client - must be called before other methods
   */
  async init(): Promise<void> {
    if (this.initialized) return;

    // Generate namespace from clientId
    this.namespace = await generateNamespaceHash(this.clientId);

    // Create namespaced storage
    const keys = createStorageKeys(this.namespace);
    this.storage = createStorage(keys);

    // Ensure DPoP key exists
    await getOrCreateDPoPKey(this.namespace);

    this.initialized = true;
  }

  private getStorage(): Storage {
    if (!this.storage) {
      throw new Error('Client not initialized. Call init() first.');
    }
    return this.storage;
  }

  /**
   * Start OAuth login flow
   */
  async loginWithRedirect(options: LoginOptions = {}): Promise<void> {
    await this.init();
    await initiateLogin(this.getStorage(), this.authorizeUrl, this.clientId, {
      ...options,
      redirectUri: options.redirectUri || this.redirectUri,
      scope: options.scope || this.scope,
    });
  }

  /**
   * Handle OAuth callback after redirect
   * Returns true if callback was handled
   */
  async handleRedirectCallback(): Promise<boolean> {
    await this.init();
    return await handleOAuthCallback(this.getStorage(), this.namespace, this.tokenUrl);
  }

  /**
   * Logout and clear all stored data
   */
  async logout(options: { reload?: boolean } = {}): Promise<void> {
    await this.init();
    await doLogout(this.getStorage(), this.namespace, options);
  }

  /**
   * Check if user is authenticated
   */
  async isAuthenticated(): Promise<boolean> {
    await this.init();
    return hasValidSession(this.getStorage());
  }

  /**
   * Get current user's DID (from stored token data)
   * For richer profile info, use client.query() with your own schema
   */
  async getUser(): Promise<User | null> {
    await this.init();
    if (!hasValidSession(this.getStorage())) {
      return null;
    }

    const did = this.getStorage().get('userDid');
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
    return await getValidAccessToken(this.getStorage(), this.namespace, this.tokenUrl);
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
      this.getStorage(),
      this.namespace,
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
      this.getStorage(),
      this.namespace,
      this.graphqlUrl,
      this.tokenUrl,
      query,
      variables,
      false
    );
  }
}
