/**
 * Generate a PKCE code verifier (32 random bytes, base64url encoded)
 */
export declare function generateCodeVerifier(): string;
/**
 * Generate a PKCE code challenge from a verifier (SHA-256, base64url encoded)
 */
export declare function generateCodeChallenge(verifier: string): Promise<string>;
/**
 * Generate a random state parameter for CSRF protection
 */
export declare function generateState(): string;
