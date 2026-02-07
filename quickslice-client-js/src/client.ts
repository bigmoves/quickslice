import { createStorageKeys } from './storage/keys';
import { createStorage, Storage } from './storage/storage';
import { getOrCreateDPoPKey } from './auth/dpop';
import { initiateLogin, handleOAuthCallback, logout as doLogout, LoginOptions } from './auth/oauth';
import { getSession, SessionInfo } from './auth/session';
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
  handle?: string;
}

export interface QueryOptions {
  signal?: AbortSignal;
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
  private cachedSession: SessionInfo | null = null;

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
   * Returns the session info if callback was handled, null otherwise
   */
  async handleRedirectCallback(): Promise<SessionInfo | null> {
    await this.init();
    const session = await handleOAuthCallback(
      this.getStorage(),
      this.namespace,
      this.tokenUrl,
      this.server
    );
    if (session) {
      this.cachedSession = session;
    }
    return session;
  }

  /**
   * Logout and clear all stored data
   */
  async logout(options: { reload?: boolean } = {}): Promise<void> {
    await this.init();
    this.cachedSession = null;
    await doLogout(this.getStorage(), this.namespace, this.server, options);
  }

  /**
   * Check if user is authenticated
   * Queries the server to verify session is valid
   */
  async isAuthenticated(): Promise<boolean> {
    await this.init();
    const session = await getSession(this.server);
    this.cachedSession = session;
    return session.authenticated;
  }

  /**
   * Get current user info from session
   * For richer profile info, use client.query() with your own schema
   */
  async getUser(): Promise<User | null> {
    await this.init();

    // Use cached session if available, otherwise fetch
    let session = this.cachedSession;
    if (!session) {
      session = await getSession(this.server);
      this.cachedSession = session;
    }

    if (!session.authenticated || !session.did) {
      return null;
    }

    return {
      did: session.did,
      handle: session.handle ?? undefined,
    };
  }

  /**
   * Execute a GraphQL query (authenticated)
   * Uses session cookie for auth - no client-side token management
   */
  async query<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {},
    options: QueryOptions = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.namespace,
      this.graphqlUrl,
      query,
      variables,
      true,
      options.signal
    );
  }

  /**
   * Execute a GraphQL mutation (authenticated)
   */
  async mutate<T = unknown>(
    mutation: string,
    variables: Record<string, unknown> = {},
    options: QueryOptions = {}
  ): Promise<T> {
    return this.query<T>(mutation, variables, options);
  }

  /**
   * Execute a public GraphQL query (no auth)
   */
  async publicQuery<T = unknown>(
    query: string,
    variables: Record<string, unknown> = {},
    options: QueryOptions = {}
  ): Promise<T> {
    await this.init();
    return await graphqlRequest<T>(
      this.namespace,
      this.graphqlUrl,
      query,
      variables,
      false,
      options.signal
    );
  }
}
