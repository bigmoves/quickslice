/**
 * Acquire a lock using localStorage for multi-tab coordination
 */
export declare function acquireLock(key: string, timeout?: number): Promise<string | null>;
/**
 * Release a lock
 */
export declare function releaseLock(key: string, lockValue: string): void;
