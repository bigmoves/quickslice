import { StorageKeys } from './keys';

/**
 * Create a namespaced storage interface
 */
export function createStorage(keys: StorageKeys) {
  return {
    get(key: keyof StorageKeys): string | null {
      const storageKey = keys[key];
      // OAuth flow state stays in sessionStorage (per-tab)
      if (key === 'codeVerifier' || key === 'oauthState') {
        return sessionStorage.getItem(storageKey);
      }
      // Tokens go in localStorage (shared across tabs)
      return localStorage.getItem(storageKey);
    },

    set(key: keyof StorageKeys, value: string): void {
      const storageKey = keys[key];
      if (key === 'codeVerifier' || key === 'oauthState') {
        sessionStorage.setItem(storageKey, value);
      } else {
        localStorage.setItem(storageKey, value);
      }
    },

    remove(key: keyof StorageKeys): void {
      const storageKey = keys[key];
      sessionStorage.removeItem(storageKey);
      localStorage.removeItem(storageKey);
    },

    clear(): void {
      (Object.keys(keys) as Array<keyof StorageKeys>).forEach((key) => {
        const storageKey = keys[key];
        sessionStorage.removeItem(storageKey);
        localStorage.removeItem(storageKey);
      });
    },
  };
}

export type Storage = ReturnType<typeof createStorage>;
