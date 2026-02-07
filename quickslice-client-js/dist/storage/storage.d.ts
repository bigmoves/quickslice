import { StorageKeys } from './keys';
/**
 * Create a namespaced storage interface
 *
 * With cookie-based auth, we only store OAuth flow state.
 * Tokens are managed server-side via HTTP-only cookies.
 */
export declare function createStorage(keys: StorageKeys): {
    get(key: keyof StorageKeys): string | null;
    set(key: keyof StorageKeys, value: string): void;
    remove(key: keyof StorageKeys): void;
    clear(): void;
};
export type Storage = ReturnType<typeof createStorage>;
