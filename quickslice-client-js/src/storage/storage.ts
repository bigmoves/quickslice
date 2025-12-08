import { STORAGE_KEYS, StorageKey } from './keys';

/**
 * Hybrid storage utility - sessionStorage for OAuth flow state,
 * localStorage for tokens (shared across tabs)
 */
export const storage = {
  get(key: StorageKey): string | null {
    // OAuth flow state stays in sessionStorage (per-tab)
    if (key === STORAGE_KEYS.codeVerifier || key === STORAGE_KEYS.oauthState) {
      return sessionStorage.getItem(key);
    }
    // Tokens go in localStorage (shared across tabs)
    return localStorage.getItem(key);
  },

  set(key: StorageKey, value: string): void {
    if (key === STORAGE_KEYS.codeVerifier || key === STORAGE_KEYS.oauthState) {
      sessionStorage.setItem(key, value);
    } else {
      localStorage.setItem(key, value);
    }
  },

  remove(key: StorageKey): void {
    sessionStorage.removeItem(key);
    localStorage.removeItem(key);
  },

  clear(): void {
    Object.values(STORAGE_KEYS).forEach((key) => {
      sessionStorage.removeItem(key);
      localStorage.removeItem(key);
    });
  },
};
