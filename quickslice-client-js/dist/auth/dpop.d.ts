interface DPoPKeyData {
    id: string;
    privateKey: CryptoKey;
    publicJwk: JsonWebKey;
    createdAt: number;
}
export declare function getOrCreateDPoPKey(namespace: string): Promise<DPoPKeyData>;
/**
 * Create a DPoP proof JWT
 */
export declare function createDPoPProof(namespace: string, method: string, url: string, accessToken?: string | null): Promise<string>;
/**
 * Clear DPoP keys from IndexedDB
 */
export declare function clearDPoPKeys(namespace: string): Promise<void>;
export {};
