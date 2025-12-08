interface DPoPKeyData {
    id: string;
    privateKey: CryptoKey;
    publicJwk: JsonWebKey;
    createdAt: number;
}
export declare function getOrCreateDPoPKey(): Promise<DPoPKeyData>;
/**
 * Create a DPoP proof JWT
 */
export declare function createDPoPProof(method: string, url: string, accessToken?: string | null): Promise<string>;
/**
 * Clear DPoP keys from IndexedDB
 */
export declare function clearDPoPKeys(): Promise<void>;
export {};
