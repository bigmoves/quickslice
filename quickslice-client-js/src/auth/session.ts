/**
 * Session management for cookie-based authentication
 *
 * Handles server-side session creation and management via HTTP-only cookies.
 * DPoP keys remain client-side for ATProto compatibility.
 */

import { getOrCreateDPoPKey } from './dpop';
import { sha256Base64Url } from '../utils/crypto';

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
export async function createSession(
  serverUrl: string,
  namespace: string,
  options: CreateSessionOptions
): Promise<SessionInfo> {
  // Get DPoP key to compute JKT for session binding
  const keyData = await getOrCreateDPoPKey(namespace);
  const dpopJkt = await computeJkt(keyData.publicJwk);

  const response = await fetch(`${serverUrl}/api/client/session`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    credentials: 'include', // Include cookies in the response
    body: JSON.stringify({
      clientId: options.clientId,
      dpopJkt,
      userDid: options.userDid,
      atpSessionId: options.atpSessionId,
    }),
  });

  if (!response.ok) {
    const errorData = await response.json().catch(() => ({}));
    throw new Error(
      `Failed to create session: ${errorData.message || response.statusText}`
    );
  }

  return await response.json();
}

/**
 * Get current session status from the server
 *
 * Returns session info if a valid session cookie exists, otherwise returns
 * an unauthenticated session info.
 */
export async function getSession(serverUrl: string): Promise<SessionInfo> {
  const response = await fetch(`${serverUrl}/api/client/session`, {
    method: 'GET',
    credentials: 'include', // Include session cookie
  });

  if (!response.ok) {
    // Return unauthenticated on error
    return {
      authenticated: false,
      did: null,
      handle: null,
    };
  }

  return await response.json();
}

/**
 * Destroy the current session (logout)
 *
 * Clears the session cookie and server-side session data.
 */
export async function destroySession(serverUrl: string): Promise<void> {
  await fetch(`${serverUrl}/api/client/session`, {
    method: 'DELETE',
    credentials: 'include', // Include session cookie
  });
  // Ignore response - session is destroyed regardless
}

/**
 * Compute JWK Thumbprint (JKT) for a public key
 * Per RFC 7638: SHA-256 hash of the canonical JWK representation
 */
async function computeJkt(jwk: JsonWebKey): Promise<string> {
  // Build canonical JWK representation (sorted keys, required members only)
  // For EC keys: crv, kty, x, y
  const canonical = JSON.stringify({
    crv: jwk.crv,
    kty: jwk.kty,
    x: jwk.x,
    y: jwk.y,
  });

  return await sha256Base64Url(canonical);
}
