# Troubleshooting

Common issues and how to resolve them.

## OAuth Errors

### "Invalid redirect URI"

The redirect URI in your OAuth request doesn't match any registered URI for your client.

**Fix:** Check that the redirect URI in your app exactly matches one registered in Settings > OAuth Clients. URIs must match including protocol, host, port, and path.

### "Invalid client ID"

The client ID doesn't exist or was deleted.

**Fix:** Verify the client ID in Settings > OAuth Clients. If it was deleted, register a new client.

### "PKCE code verifier mismatch"

The code verifier sent during token exchange doesn't match the code challenge from authorization.

**Fix:** This usually means the code verifier wasn't stored correctly between the authorization redirect and callback. If using the SDK, ensure you're calling `handleRedirectCallback()` in the same browser session that initiated login.

### "DPoP proof invalid"

The DPoP proof header is missing, malformed, or signed with the wrong key.

**Fix:** If using the SDK, this is handled automatically. If implementing manually, ensure:
- The DPoP header contains a valid JWT
- The JWT is signed with the same key used during token exchange
- The `htm` and `htu` claims match the request method and URL

## GraphQL Errors

### "Cannot query field X on type Y"

The field doesn't exist on the type you're querying.

**Fix:** Check your query against the schema in GraphiQL. Common causes:
- Typo in field name
- Field exists on a different type (use inline fragments for unions)
- Lexicon wasn't imported yet

### "Variable $X of type Y used in position expecting Z"

Type mismatch between your variable declaration and how it's used.

**Fix:** Check variable types in your query definition. Common issues:
- Using `String` instead of `DateTime` for date fields
- Missing `!` for required variables
- Using wrong scalar type

### "Record not found"

The record you're trying to update or delete doesn't exist.

**Fix:** Verify the record key (rkey) is correct. You can query for the record first to confirm it exists.

## Jetstream Issues

### Records not appearing after creation

Records created via mutation should appear immediately due to optimistic indexing. If they don't:

**Check:**
1. Was the mutation successful? Check the response for errors.
2. Is the record in the user's PDS? Use `goat repo get` to verify.
3. Is Jetstream connected? Check the logs for connection errors.

### Old records missing

Records created before you deployed Quickslice won't appear until backfilled.

**Fix:** Trigger a backfill from the admin UI or wait for the scheduled backfill to complete.

### Backfill stuck or slow

**Check:**
1. Memory usage - backfill is memory-intensive. See [Deployment Guide](deployment.md) for tuning.
2. Network connectivity to PDS endpoints
3. Logs for specific PDS errors

## Database Issues

### "Database is locked"

SQLite can't acquire a write lock. Usually caused by long-running queries or concurrent access issues.

**Fix:**
- Ensure only one Quickslice instance writes to the database
- Check for stuck queries in logs
- Restart the service if needed

### Disk space full

SQLite needs space for WAL files and vacuuming.

**Fix:** Expand your volume. See your hosting platform's documentation.

## Debugging Tips

### Check if records are being indexed

Query for recent records:

```graphql
query {
  xyzStatusphereStatus(first: 5, sortBy: [{ field: indexedAt, direction: DESC }]) {
    edges {
      node {
        uri
        indexedAt
      }
    }
  }
}
```

### Verify OAuth is working

Query the viewer:

```graphql
query {
  viewer {
    did
    handle
  }
}
```

Returns `null` if not authenticated. Returns user info if authenticated.

### Inspect the GraphQL schema

Use GraphiQL at `/graphiql` to explore available types, queries, and mutations. The Docs panel shows all fields and their types.

### Check Lexicon registration

Use the MCP endpoint or admin UI to list registered Lexicons:

```graphql
query {
  __schema {
    types {
      name
    }
  }
}
```

Look for types matching your Lexicon NSIDs (e.g., `XyzStatusphereStatus`).

## FAQ

### "Why aren't my records showing up?"

1. **Just created?** Should appear immediately. Check mutation response for errors.
2. **Created before deployment?** Needs backfill. Trigger from admin UI.
3. **Different Lexicon?** Ensure the Lexicon is registered in your instance.

### "Why is my mutation failing?"

1. **401 Unauthorized?** Token expired or invalid. Re-authenticate.
2. **403 Forbidden?** Trying to modify another user's record.
3. **400 Bad Request?** Check input against Lexicon schema. Required fields missing?

### "How do I check what Lexicons are loaded?"

Go to Settings in the admin UI, or query via MCP:

```bash
claude mcp add --transport http quickslice https://yourapp.slices.network/mcp
# Then ask: "What lexicons are registered?"
```
