export { QuicksliceClient, QuicksliceClientOptions, User } from './client';
export { QuicksliceError, LoginRequiredError, NetworkError, OAuthError, } from './errors';
import { QuicksliceClient, QuicksliceClientOptions } from './client';
/**
 * Create and initialize a Quickslice client
 */
export declare function createQuicksliceClient(options: QuicksliceClientOptions): Promise<QuicksliceClient>;
