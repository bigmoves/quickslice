/**
 * Acquire a lock using localStorage for multi-tab coordination
 */
export declare function acquireLock(namespace: string, key: string, timeout?: number): Promise<string | null>;
/**
 * Release a lock
 */
export declare function releaseLock(namespace: string, key: string, lockValue: string): void;
