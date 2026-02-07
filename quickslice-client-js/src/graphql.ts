import { createDPoPProof } from './auth/dpop';

export interface GraphQLResponse<T = unknown> {
  data?: T;
  errors?: Array<{ message: string; path?: string[] }>;
}

/**
 * Execute a GraphQL query or mutation
 *
 * With cookie-based auth, the session cookie is automatically included
 * via credentials: 'include'. DPoP proof is still added for request binding.
 */
export async function graphqlRequest<T = unknown>(
  namespace: string,
  graphqlUrl: string,
  query: string,
  variables: Record<string, unknown> = {},
  requireAuth = false,
  signal?: AbortSignal
): Promise<T> {
  const headers: Record<string, string> = {
    'Content-Type': 'application/json',
  };

  // Add DPoP proof for authenticated requests
  // The session cookie is sent automatically with credentials: 'include'
  if (requireAuth) {
    // Create DPoP proof bound to this request (no access token hash since tokens are server-side)
    const dpopProof = await createDPoPProof(namespace, 'POST', graphqlUrl);
    headers['DPoP'] = dpopProof;
  }

  const response = await fetch(graphqlUrl, {
    method: 'POST',
    headers,
    body: JSON.stringify({ query, variables }),
    credentials: 'include', // Include session cookie
    signal,
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
