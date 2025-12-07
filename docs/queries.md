# Queries

> **Note:** Queries are public and do not require authentication.

## Basic Query

Fetch records using Relay-style connections:

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
      hasPreviousPage
      startCursor
      endCursor
    }
    totalCount
  }
}
```

## Filtering

Use the `where` argument to filter records:

```graphql
query {
  xyzStatusphereStatus(where: {
    status: { eq: "🎉" }
  }) {
    edges {
      node {
        uri
        status
        createdAt
      }
    }
    totalCount
  }
}
```

### Filter Operators

- `eq`: Equal to
- `ne`: Not equal to
- `in`: In array
- `contains`: String contains (case-insensitive)

### Example Filters

```graphql
# String contains
query {
  appBskyActorProfile(where: {
    displayName: { contains: "alice" }
  }) {
    edges {
      node {
        uri
        displayName
        description
      }
    }
  }
}
```

## Sorting

Sort records using the `sortBy` argument:

```graphql
query {
  xyzStatusphereStatus(sortBy: [
    { field: createdAt, direction: DESC }
  ]) {
    edges {
      node {
        uri
        status
        createdAt
      }
    }
  }
}
```

### Sort Multiple Fields

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

## Pagination (Relay Cursor Connections)

### Forward Pagination

Use `first` and `after` to paginate forward:

```graphql
query {
  xyzStatusphereStatus(
    first: 10
    sortBy: [{ field: createdAt, direction: DESC }]
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

**Next page:**

```graphql
query {
  xyzStatusphereStatus(
    first: 10
    after: "cursor_from_previous_page"
    sortBy: [{ field: createdAt, direction: DESC }]
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
  }
}
```

### Backward Pagination

Use `last` and `before` to paginate backward:

```graphql
query {
  xyzStatusphereStatus(
    last: 10
    before: "some_cursor"
    sortBy: [{ field: createdAt, direction: DESC }]
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
      hasPreviousPage
      startCursor
    }
  }
}
```

## PageInfo Fields

The `pageInfo` object contains:

- `hasNextPage`: Boolean indicating if more items exist after this page
- `hasPreviousPage`: Boolean indicating if more items exist before this page
- `startCursor`: Cursor of the first item in the current page
- `endCursor`: Cursor of the last item in the current page

## Complete Example

Combining filters, sorting, and pagination:

```graphql
query GetRecentStatuses($pageSize: Int!, $cursor: String) {
  xyzStatusphereStatus(
    where: {
      status: { ne: "" }
    }
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
      hasPreviousPage
      startCursor
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

## Pagination Pattern

Here's a typical pagination flow:

**1. First page:**
```graphql
query {
  xyzStatusphereStatus(first: 10) {
    edges {
      node { uri status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

**2. Check if there's a next page:**
```json
{
  "pageInfo": {
    "hasNextPage": true,
    "endCursor": "cursor_xyz_123"
  }
}
```

**3. Fetch next page:**
```graphql
query {
  xyzStatusphereStatus(
    first: 10
    after: "cursor_xyz_123"
  ) {
    edges {
      node { uri status }
      cursor
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

**4. Continue until `hasNextPage` is `false`**
