import { generateRandomString } from '../utils/base64url';
import { sha256Base64Url, signJwt } from '../utils/crypto';

const DB_NAME = 'quickslice-oauth';
const DB_VERSION = 1;
const KEY_STORE = 'dpop-keys';
const KEY_ID = 'dpop-key';

interface DPoPKeyData {
  id: string;
  privateKey: CryptoKey;
  publicJwk: JsonWebKey;
  createdAt: number;
}

let dbPromise: Promise<IDBDatabase> | null = null;

function openDatabase(): Promise<IDBDatabase> {
  if (dbPromise) return dbPromise;

  dbPromise = new Promise((resolve, reject) => {
    const request = indexedDB.open(DB_NAME, DB_VERSION);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result);

    request.onupgradeneeded = (event) => {
      const db = (event.target as IDBOpenDBRequest).result;
      if (!db.objectStoreNames.contains(KEY_STORE)) {
        db.createObjectStore(KEY_STORE, { keyPath: 'id' });
      }
    };
  });

  return dbPromise;
}

async function getDPoPKey(): Promise<DPoPKeyData | null> {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readonly');
    const store = tx.objectStore(KEY_STORE);
    const request = store.get(KEY_ID);

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve(request.result || null);
  });
}

async function storeDPoPKey(
  privateKey: CryptoKey,
  publicJwk: JsonWebKey
): Promise<void> {
  const db = await openDatabase();
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

export async function getOrCreateDPoPKey(): Promise<DPoPKeyData> {
  const keyData = await getDPoPKey();

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
  await storeDPoPKey(keyPair.privateKey, publicJwk);

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
  method: string,
  url: string,
  accessToken: string | null = null
): Promise<string> {
  const keyData = await getOrCreateDPoPKey();

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
export async function clearDPoPKeys(): Promise<void> {
  const db = await openDatabase();
  return new Promise((resolve, reject) => {
    const tx = db.transaction(KEY_STORE, 'readwrite');
    const store = tx.objectStore(KEY_STORE);
    const request = store.clear();

    request.onerror = () => reject(request.error);
    request.onsuccess = () => resolve();
  });
}
