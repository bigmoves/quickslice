import { base64UrlEncode } from './base64url';

/**
 * SHA-256 hash, returned as base64url string
 */
export async function sha256Base64Url(data: string): Promise<string> {
  const encoder = new TextEncoder();
  const hash = await crypto.subtle.digest('SHA-256', encoder.encode(data));
  return base64UrlEncode(hash);
}

/**
 * Sign a JWT with an ECDSA P-256 private key
 */
export async function signJwt(
  header: Record<string, unknown>,
  payload: Record<string, unknown>,
  privateKey: CryptoKey
): Promise<string> {
  const encoder = new TextEncoder();

  const headerB64 = base64UrlEncode(encoder.encode(JSON.stringify(header)));
  const payloadB64 = base64UrlEncode(encoder.encode(JSON.stringify(payload)));

  const signingInput = `${headerB64}.${payloadB64}`;

  const signature = await crypto.subtle.sign(
    { name: 'ECDSA', hash: 'SHA-256' },
    privateKey,
    encoder.encode(signingInput)
  );

  const signatureB64 = base64UrlEncode(signature);

  return `${signingInput}.${signatureB64}`;
}
