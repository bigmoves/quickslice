import { Storage } from '../storage/storage';
import { acquireLock, releaseLock } from '../storage/lock';
import { createDPoPProof } from './dpop';

const TOKEN_REFRESH_BUFFER_MS = 60000; // 60 seconds before expiry

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Refresh tokens using the refresh token
 */
async function refreshTokens(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<string> {
  const refreshToken = storage.get('refreshToken');
  const clientId = storage.get('clientId');

  if (!refreshToken || !clientId) {
    throw new Error('No refresh token available');
  }

  const dpopProof = await createDPoPProof(namespace, 'POST', tokenUrl);

  const response = await fetch(tokenUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      DPoP: dpopProof,
    },
    body: new URLSearchParams({
      grant_type: 'refresh_token',
      refresh_token: refreshToken,
      client_id: clientId,
    }),
  });

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(
      `Token refresh failed: ${errorData.error_description || response.statusText}`
    );
  }

  const tokens = await response.json();

  // Store new tokens (rotation - new refresh token each time)
  storage.set('accessToken', tokens.access_token);
  if (tokens.refresh_token) {
    storage.set('refreshToken', tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set('tokenExpiresAt', expiresAt.toString());

  return tokens.access_token;
}

/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export async function getValidAccessToken(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<string> {
  const accessToken = storage.get('accessToken');
  const expiresAt = parseInt(storage.get('tokenExpiresAt') || '0');

  // Check if token is still valid (with buffer)
  if (accessToken && Date.now() < expiresAt - TOKEN_REFRESH_BUFFER_MS) {
    return accessToken;
  }

  // Need to refresh - acquire lock first
  const lockKey = 'token_refresh';
  const lockValue = await acquireLock(namespace, lockKey);

  if (!lockValue) {
    // Failed to acquire lock, another tab is refreshing
    // Wait a bit and check cache again
    await sleep(100);
    const freshToken = storage.get('accessToken');
    const freshExpiry = parseInt(storage.get('tokenExpiresAt') || '0');
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }
    throw new Error('Failed to refresh token');
  }

  try {
    // Double-check after acquiring lock
    const freshToken = storage.get('accessToken');
    const freshExpiry = parseInt(storage.get('tokenExpiresAt') || '0');
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }

    // Actually refresh
    return await refreshTokens(storage, namespace, tokenUrl);
  } finally {
    releaseLock(namespace, lockKey, lockValue);
  }
}

/**
 * Store tokens from OAuth response
 */
export function storeTokens(
  storage: Storage,
  tokens: {
    access_token: string;
    refresh_token?: string;
    expires_in: number;
    sub?: string;
  }
): void {
  storage.set('accessToken', tokens.access_token);
  if (tokens.refresh_token) {
    storage.set('refreshToken', tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set('tokenExpiresAt', expiresAt.toString());

  if (tokens.sub) {
    storage.set('userDid', tokens.sub);
  }
}

/**
 * Check if we have a valid session
 */
export function hasValidSession(storage: Storage): boolean {
  const accessToken = storage.get('accessToken');
  const refreshToken = storage.get('refreshToken');
  return !!(accessToken || refreshToken);
}
