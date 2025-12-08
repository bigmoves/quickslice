import { StorageKey } from './keys';
/**
 * Hybrid storage utility - sessionStorage for OAuth flow state,
 * localStorage for tokens (shared across tabs)
 */
export declare const storage: {
    get(key: StorageKey): string | null;
    set(key: StorageKey, value: string): void;
    remove(key: StorageKey): void;
    clear(): void;
};
