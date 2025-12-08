import { createDPoPProof } from './auth/dpop';
import { getValidAccessToken } from './auth/tokens';

export interface GraphQLResponse<T = unknown> {
  data?: T;
  errors?: Array<{ message: string; path?: string[] }>;
}

/**
 * Execute a GraphQL query or mutation
 */
export async function graphqlRequest<T = unknown>(
  graphqlUrl: string,
  tokenUrl: string,
  query: string,
  variables: Record<string, unknown> = {},
  requireAuth = false
): Promise<T> {
  const headers: Record<string, string> = {
    'Content-Type': 'application/json',
  };

  if (requireAuth) {
    const token = await getValidAccessToken(tokenUrl);
    if (!token) {
      throw new Error('Not authenticated');
    }

    // Create DPoP proof bound to this request
    const dpopProof = await createDPoPProof('POST', graphqlUrl, token);

    headers['Authorization'] = `DPoP ${token}`;
    headers['DPoP'] = dpopProof;
  }

  const response = await fetch(graphqlUrl, {
    method: 'POST',
    headers,
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    throw new Error(`GraphQL request failed: ${response.statusText}`);
  }

  const result: GraphQLResponse<T> = await response.json();

  if (result.errors && result.errors.length > 0) {
    throw new Error(`GraphQL error: ${result.errors[0].message}`);
  }

  return result.data as T;
}
