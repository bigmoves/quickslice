export interface GraphQLResponse<T = unknown> {
    data?: T;
    errors?: Array<{
        message: string;
        path?: string[];
    }>;
}
/**
 * Execute a GraphQL query or mutation
 *
 * With cookie-based auth, the session cookie is automatically included
 * via credentials: 'include'. DPoP proof is still added for request binding.
 */
export declare function graphqlRequest<T = unknown>(namespace: string, graphqlUrl: string, query: string, variables?: Record<string, unknown>, requireAuth?: boolean, signal?: AbortSignal): Promise<T>;
