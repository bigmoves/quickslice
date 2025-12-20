import { Storage } from '../storage/storage';
import { createDPoPProof, clearDPoPKeys } from './dpop';
import { generateCodeVerifier, generateCodeChallenge, generateState } from './pkce';
import { storeTokens } from './tokens';

export interface LoginOptions {
  handle?: string;
  redirectUri?: string;
  scope?: string;
}

/**
 * Initiate OAuth login flow with PKCE
 */
export async function initiateLogin(
  storage: Storage,
  authorizeUrl: string,
  clientId: string,
  options: LoginOptions = {}
): Promise<void> {
  const codeVerifier = generateCodeVerifier();
  const codeChallenge = await generateCodeChallenge(codeVerifier);
  const state = generateState();

  // Build redirect URI (use provided or derive from current page)
  const redirectUri = options.redirectUri || (window.location.origin + window.location.pathname);

  // Store for callback
  storage.set('codeVerifier', codeVerifier);
  storage.set('oauthState', state);
  storage.set('clientId', clientId);
  storage.set('redirectUri', redirectUri);

  // Build authorization URL
  const params = new URLSearchParams({
    client_id: clientId,
    redirect_uri: redirectUri,
    response_type: 'code',
    code_challenge: codeChallenge,
    code_challenge_method: 'S256',
    state: state,
  });

  if (options.handle) {
    params.set('login_hint', options.handle);
  }

  if (options.scope) {
    params.set('scope', options.scope);
  }

  window.location.href = `${authorizeUrl}?${params.toString()}`;
}

/**
 * Handle OAuth callback - exchange code for tokens
 * Returns true if callback was handled, false if not a callback
 */
export async function handleOAuthCallback(
  storage: Storage,
  namespace: string,
  tokenUrl: string
): Promise<boolean> {
  const params = new URLSearchParams(window.location.search);
  const code = params.get('code');
  const state = params.get('state');
  const error = params.get('error');

  if (error) {
    throw new Error(
      `OAuth error: ${error} - ${params.get('error_description') || ''}`
    );
  }

  if (!code || !state) {
    return false; // Not a callback
  }

  // Verify state
  const storedState = storage.get('oauthState');
  if (state !== storedState) {
    throw new Error('OAuth state mismatch - possible CSRF attack');
  }

  // Get stored values
  const codeVerifier = storage.get('codeVerifier');
  const clientId = storage.get('clientId');
  const redirectUri = storage.get('redirectUri');

  if (!codeVerifier || !clientId || !redirectUri) {
    throw new Error('Missing OAuth session data');
  }

  // Exchange code for tokens with DPoP
  const dpopProof = await createDPoPProof(namespace, 'POST', tokenUrl);

  const tokenResponse = await fetch(tokenUrl, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      DPoP: dpopProof,
    },
    body: new URLSearchParams({
      grant_type: 'authorization_code',
      code: code,
      redirect_uri: redirectUri,
      client_id: clientId,
      code_verifier: codeVerifier,
    }),
  });

  if (!tokenResponse.ok) {
    const errorData = await tokenResponse.json().catch(() => ({}));
    throw new Error(
      `Token exchange failed: ${errorData.error_description || tokenResponse.statusText}`
    );
  }

  const tokens = await tokenResponse.json();

  // Store tokens
  storeTokens(storage, tokens);

  // Clean up OAuth state
  storage.remove('codeVerifier');
  storage.remove('oauthState');
  storage.remove('redirectUri');

  // Clear URL params
  window.history.replaceState({}, document.title, window.location.pathname);

  return true;
}

/**
 * Logout - clear all stored data
 */
export async function logout(
  storage: Storage,
  namespace: string,
  options: { reload?: boolean } = {}
): Promise<void> {
  storage.clear();
  await clearDPoPKeys(namespace);

  if (options.reload !== false) {
    window.location.reload();
  }
}
