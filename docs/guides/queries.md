# Queries

Quickslice generates a GraphQL query for each Lexicon record type. Queries are public; no authentication required.

## Relay Connections

Queries return data in the [Relay Connection](https://relay.dev/graphql/connections.htm) format:

```graphql
query {
  xyzStatusphereStatus {
    edges {
      node {
        uri
        status
        createdAt
      }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
    totalCount
  }
}
```

- `edges`: Array of results, each containing a `node` (the record) and `cursor` (for pagination)
- `pageInfo`: Pagination metadata
- `totalCount`: Total number of matching records

## Built-in Fields

Every record includes these fields automatically:

| Field | Description |
|-------|-------------|
| `uri` | The AT-URI of the record |
| `cid` | Content identifier (hash) |
| `did` | Author's decentralized identifier |
| `collection` | The Lexicon collection (e.g., `app.bsky.feed.post`) |
| `actorHandle` | Author's handle (e.g., `alice.bsky.social`) |
| `indexedAt` | When Quickslice indexed the record |

The `actorHandle` field resolves the author's DID to their current handle, useful for display without a separate join:

```graphql
query {
  xyzStatusphereStatus(first: 10) {
    edges {
      node {
        status
        actorHandle
      }
    }
  }
}
```

## Filtering

Use the `where` argument to filter records:

```graphql
query {
  xyzStatusphereStatus(where: { status: { eq: "🎉" } }) {
    edges {
      node {
        status
        createdAt
      }
    }
  }
}
```

### Filter Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `eq` | Equal to | `{ status: { eq: "👍" } }` |
| `ne` | Not equal to | `{ status: { ne: "👎" } }` |
| `in` | In array | `{ status: { in: ["👍", "🎉"] } }` |
| `contains` | String contains (case-insensitive) | `{ displayName: { contains: "alice" } }` |
| `gt` | Greater than | `{ createdAt: { gt: "2025-01-01T00:00:00Z" } }` |
| `lt` | Less than | `{ createdAt: { lt: "2025-06-01T00:00:00Z" } }` |
| `gte` | Greater than or equal | `{ position: { gte: 1 } }` |
| `lte` | Less than or equal | `{ position: { lte: 10 } }` |
| `isNull` | Null check | `{ replyParent: { isNull: true } }` |

### Filtering Ref Fields

Reference fields (AT-URIs or strong refs pointing to other records) only support `isNull`. Use it to find records with or without a reference:

```graphql
query {
  # Find root posts (no reply parent)
  appBskyFeedPost(where: { replyParent: { isNull: true } }) {
    edges {
      node {
        text
      }
    }
  }
}
```

```graphql
query {
  # Find replies only
  appBskyFeedPost(where: { replyParent: { isNull: false } }) {
    edges {
      node {
        text
      }
    }
  }
}
```

### Multiple Conditions

Combine multiple conditions (they're ANDed together):

```graphql
query {
  appBskyActorProfile(where: {
    displayName: { contains: "alice" }
    createdAt: { gt: "2025-01-01T00:00:00Z" }
  }) {
    edges {
      node {
        displayName
        description
      }
    }
  }
}
```

## Sorting

Use `sortBy` to order results:

```graphql
query {
  xyzStatusphereStatus(sortBy: [{ field: createdAt, direction: DESC }]) {
    edges {
      node {
        status
        createdAt
      }
    }
  }
}
```

### Multi-Field Sorting

Sort by multiple fields (applied in order):

```graphql
query {
  appBskyActorProfile(sortBy: [
    { field: displayName, direction: ASC }
    { field: createdAt, direction: DESC }
  ]) {
    edges {
      node {
        displayName
        createdAt
      }
    }
  }
}
```

## Pagination

### Forward Pagination

Use `first` to limit results and `after` to get the next page:

```graphql
# First page
query {
  xyzStatusphereStatus(first: 10) {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}

# Next page (use endCursor from previous response)
query {
  xyzStatusphereStatus(first: 10, after: "cursor_from_previous_page") {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

### Backward Pagination

Use `last` and `before` to paginate backward:

```graphql
query {
  xyzStatusphereStatus(last: 10, before: "some_cursor") {
    edges {
      node { status }
      cursor
    }
    pageInfo {
      hasPreviousPage
      startCursor
    }
  }
}
```

### PageInfo Fields

| Field | Description |
|-------|-------------|
| `hasNextPage` | More items exist after this page |
| `hasPreviousPage` | More items exist before this page |
| `startCursor` | Cursor of the first item |
| `endCursor` | Cursor of the last item |

## Complete Example

Combining filtering, sorting, and pagination:

```graphql
query GetRecentStatuses($pageSize: Int!, $cursor: String) {
  xyzStatusphereStatus(
    where: { status: { in: ["👍", "🎉", "💙"] } }
    sortBy: [{ field: createdAt, direction: DESC }]
    first: $pageSize
    after: $cursor
  ) {
    edges {
      node {
        uri
        status
        createdAt
      }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
    totalCount
  }
}
```

Variables:

```json
{
  "pageSize": 20,
  "cursor": null
}
```
