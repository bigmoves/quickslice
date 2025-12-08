import { base64UrlEncode, generateRandomString } from '../utils/base64url';

/**
 * Generate a PKCE code verifier (32 random bytes, base64url encoded)
 */
export function generateCodeVerifier(): string {
  return generateRandomString(32);
}

/**
 * Generate a PKCE code challenge from a verifier (SHA-256, base64url encoded)
 */
export async function generateCodeChallenge(verifier: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(verifier);
  const hash = await crypto.subtle.digest('SHA-256', data);
  return base64UrlEncode(hash);
}

/**
 * Generate a random state parameter for CSRF protection
 */
export function generateState(): string {
  return generateRandomString(16);
}
