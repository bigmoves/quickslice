const LOCK_TIMEOUT = 5000; // 5 seconds
const LOCK_PREFIX = 'quickslice_lock_';

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Acquire a lock using localStorage for multi-tab coordination
 */
export async function acquireLock(
  key: string,
  timeout = LOCK_TIMEOUT
): Promise<string | null> {
  const lockKey = LOCK_PREFIX + key;
  const lockValue = `${Date.now()}_${Math.random()}`;
  const deadline = Date.now() + timeout;

  while (Date.now() < deadline) {
    const existing = localStorage.getItem(lockKey);

    if (existing) {
      // Check if lock is stale (older than timeout)
      const [timestamp] = existing.split('_');
      if (Date.now() - parseInt(timestamp) > LOCK_TIMEOUT) {
        // Lock is stale, remove it
        localStorage.removeItem(lockKey);
      } else {
        // Lock is held, wait and retry
        await sleep(50);
        continue;
      }
    }

    // Try to acquire
    localStorage.setItem(lockKey, lockValue);

    // Verify we got it (handle race condition)
    await sleep(10);
    if (localStorage.getItem(lockKey) === lockValue) {
      return lockValue; // Lock acquired
    }
  }

  return null; // Failed to acquire
}

/**
 * Release a lock
 */
export function releaseLock(key: string, lockValue: string): void {
  const lockKey = LOCK_PREFIX + key;
  // Only release if we still hold it
  if (localStorage.getItem(lockKey) === lockValue) {
    localStorage.removeItem(lockKey);
  }
}
