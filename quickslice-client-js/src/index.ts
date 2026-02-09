export { QuicksliceClient, QuicksliceClientOptions, QueryOptions, User } from './client';
export { SessionInfo } from './auth/session';
export {
  QuicksliceError,
  LoginRequiredError,
  NetworkError,
  OAuthError,
} from './errors';

import { QuicksliceClient, QuicksliceClientOptions } from './client';

/**
 * Create and initialize a Quickslice client
 */
export async function createQuicksliceClient(
  options: QuicksliceClientOptions
): Promise<QuicksliceClient> {
  const client = new QuicksliceClient(options);
  await client.init();
  return client;
}
