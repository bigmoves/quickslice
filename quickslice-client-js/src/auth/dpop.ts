import { generateRandomString } from '../utils/base64url';
import { sha256Base64Url, signJwt } from '../utils/crypto';

const DB_VERSION = 1;
const KEY_STORE = 'dpop-keys';
const KEY_ID = 'dpop-key';

interface DPoPKeyData {
  id: string;
  privateKey: CryptoKey;
  publicJwk: JsonWebKey;
  createdAt: number;
}

// Cache database connections per namespace
const dbPromises = new Map<string, Promise<IDBDatabase>>();

function getDbName(namespace: string): string {
  return `quickslice-oauth-${namespace}`;
}

function openDatabase(namespace: string): Promise<IDBDatabase> {
  const existing = dbPromises.get(namespace);
  if (existing) return existing;

  const promise = new Promise<IDBDatabase>((resolve, reject) => {
    const request = indexedDB.open(getDbName(namespace), DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;
      if (!db.objectStoreNames.contains(KEY_STORE)) {
        db.createObjectStore(KEY_STORE, { keyPath: 'id' });
      }
    };
  });

  dbPromises.set(namespace, promise);
  return promise;
}

async function getDPoPKey(namespace: string): Promise<DPoPKeyData | null> {
  const db = await openDatabase(namespace);
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readonly');
    const store = tx.objectStore(KEY_STORE);
    const request = store.get(KEY_ID);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result || null);
  });
}

async function storeDPoPKey(
  namespace: string,
  privateKey: CryptoKey,
  publicJwk: JsonWebKey
): Promise<void> {
  const db = await openDatabase(namespace);
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.put({
      id: KEY_ID,
      privateKey,
      publicJwk,
      createdAt: Date.now(),
    });

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}

export async function getOrCreateDPoPKey(namespace: string): Promise<DPoPKeyData> {
  const keyData = await getDPoPKey(namespace);

  if (keyData) {
    return keyData;
  }

  // Generate new P-256 key pair
  const keyPair = await crypto.subtle.generateKey(
    { name: 'ECDSA', namedCurve: 'P-256' },
    false, // NOT extractable - critical for security
    ['sign']
  );

  // Export public key as JWK
  const publicJwk = await crypto.subtle.exportKey('jwk', keyPair.publicKey);

  // Store in IndexedDB
  await storeDPoPKey(namespace, keyPair.privateKey, publicJwk);

  return {
    id: KEY_ID,
    privateKey: keyPair.privateKey,
    publicJwk,
    createdAt: Date.now(),
  };
}

/**
 * Create a DPoP proof JWT
 */
export async function createDPoPProof(
  namespace: string,
  method: string,
  url: string,
  accessToken: string | null = null
): Promise<string> {
  const keyData = await getOrCreateDPoPKey(namespace);

  // Strip WebCrypto-specific fields from JWK for interoperability
  const { kty, crv, x, y } = keyData.publicJwk;
  const minimalJwk = { kty, crv, x, y };

  const header = {
    alg: 'ES256',
    typ: 'dpop+jwt',
    jwk: minimalJwk,
  };

  const payload: Record<string, unknown> = {
    jti: generateRandomString(16),
    htm: method,
    htu: url,
    iat: Math.floor(Date.now() / 1000),
  };

  // Add access token hash if provided (for resource requests)
  if (accessToken) {
    payload.ath = await sha256Base64Url(accessToken);
  }

  return await signJwt(header, payload, keyData.privateKey);
}

/**
 * Clear DPoP keys from IndexedDB
 */
export async function clearDPoPKeys(namespace: string): Promise<void> {
  const db = await openDatabase(namespace);
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.clear();

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}
