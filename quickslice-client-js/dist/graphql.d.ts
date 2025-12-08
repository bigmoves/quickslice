export interface GraphQLResponse<T = unknown> {
    data?: T;
    errors?: Array<{
        message: string;
        path?: string[];
    }>;
}
/**
 * Execute a GraphQL query or mutation
 */
export declare function graphqlRequest<T = unknown>(graphqlUrl: string, tokenUrl: string, query: string, variables?: Record<string, unknown>, requireAuth?: boolean): Promise<T>;
