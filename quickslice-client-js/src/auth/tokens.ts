import { storage } from '../storage/storage';
import { STORAGE_KEYS } from '../storage/keys';
import { acquireLock, releaseLock } from '../storage/lock';
import { createDPoPProof } from './dpop';

const TOKEN_REFRESH_BUFFER_MS = 60000; // 60 seconds before expiry

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Refresh tokens using the refresh token
 */
async function refreshTokens(tokenUrl: string): Promise<string> {
  const refreshToken = storage.get(STORAGE_KEYS.refreshToken);
  const clientId = storage.get(STORAGE_KEYS.clientId);

  if (!refreshToken || !clientId) {
    throw new Error('No refresh token available');
  }

  const dpopProof = await createDPoPProof('POST', tokenUrl);

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
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());

  return tokens.access_token;
}

/**
 * Get a valid access token, refreshing if necessary.
 * Uses multi-tab locking to prevent duplicate refresh requests.
 */
export async function getValidAccessToken(tokenUrl: string): Promise<string> {
  const accessToken = storage.get(STORAGE_KEYS.accessToken);
  const expiresAt = parseInt(storage.get(STORAGE_KEYS.tokenExpiresAt) || '0');

  // Check if token is still valid (with buffer)
  if (accessToken && Date.now() < expiresAt - TOKEN_REFRESH_BUFFER_MS) {
    return accessToken;
  }

  // Need to refresh - acquire lock first
  const clientId = storage.get(STORAGE_KEYS.clientId);
  const lockKey = `token_refresh_${clientId}`;
  const lockValue = await acquireLock(lockKey);

  if (!lockValue) {
    // Failed to acquire lock, another tab is refreshing
    // Wait a bit and check cache again
    await sleep(100);
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(
      storage.get(STORAGE_KEYS.tokenExpiresAt) || '0'
    );
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }
    throw new Error('Failed to refresh token');
  }

  try {
    // Double-check after acquiring lock
    const freshToken = storage.get(STORAGE_KEYS.accessToken);
    const freshExpiry = parseInt(
      storage.get(STORAGE_KEYS.tokenExpiresAt) || '0'
    );
    if (freshToken && Date.now() < freshExpiry - TOKEN_REFRESH_BUFFER_MS) {
      return freshToken;
    }

    // Actually refresh
    return await refreshTokens(tokenUrl);
  } finally {
    releaseLock(lockKey, lockValue);
  }
}

/**
 * Store tokens from OAuth response
 */
export function storeTokens(tokens: {
  access_token: string;
  refresh_token?: string;
  expires_in: number;
  sub?: string;
}): void {
  storage.set(STORAGE_KEYS.accessToken, tokens.access_token);
  if (tokens.refresh_token) {
    storage.set(STORAGE_KEYS.refreshToken, tokens.refresh_token);
  }

  const expiresAt = Date.now() + tokens.expires_in * 1000;
  storage.set(STORAGE_KEYS.tokenExpiresAt, expiresAt.toString());

  if (tokens.sub) {
    storage.set(STORAGE_KEYS.userDid, tokens.sub);
  }
}

/**
 * Check if we have a valid session
 */
export function hasValidSession(): boolean {
  const accessToken = storage.get(STORAGE_KEYS.accessToken);
  const refreshToken = storage.get(STORAGE_KEYS.refreshToken);
  return !!(accessToken || refreshToken);
}
