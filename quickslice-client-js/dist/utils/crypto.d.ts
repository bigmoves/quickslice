/**
 * SHA-256 hash, returned as base64url string
 */
export declare function sha256Base64Url(data: string): Promise<string>;
/**
 * Sign a JWT with an ECDSA P-256 private key
 */
export declare function signJwt(header: Record<string, unknown>, payload: Record<string, unknown>, privateKey: CryptoKey): Promise<string>;
