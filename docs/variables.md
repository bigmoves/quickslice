# Variables

GraphQL variables allow you to parameterize your queries and mutations for reusability and security.

## Basic Variables

### Query with Variables

```graphql
query GetStatusByEmoji($emoji: String!) {
  xyzStatusphereStatus(where: {
    status: { eq: $emoji }
  }) {
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

Variables:

```json
{
  "emoji": "🎉"
}
```

### Mutation with Variables

```graphql
mutation CreateStatus($statusEmoji: String!, $timestamp: String!) {
  createXyzStatusphereStatus(
    input: {
      status: $statusEmoji
      createdAt: $timestamp
    }
  ) {
    uri
    status
    createdAt
  }
}
```

Variables:

```json
{
  "statusEmoji": "🚀",
  "timestamp": "2025-01-30T12:00:00Z"
}
```

## Multiple Variables

```graphql
query GetFilteredStatuses(
  $emoji: String!
  $pageSize: Int!
  $cursor: String
) {
  xyzStatusphereStatus(
    where: { status: { eq: $emoji } }
    first: $pageSize
    after: $cursor
    sortBy: [{ field: "createdAt", direction: DESC }]
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
  "emoji": "✨",
  "pageSize": 10,
  "cursor": null
}
```

## Optional Variables

Use default values for optional variables:

```graphql
query GetProfiles(
  $name: String = ""
  $pageSize: Int = 20
) {
  appBskyActorProfile(
    where: { displayName: { contains: $name } }
    first: $pageSize
  ) {
    edges {
      node {
        displayName
        description
      }
    }
  }
}
```

Variables:

```json
{
  "name": "alice"
}
```

Or omit variables to use defaults:

```json
{}
```

## Blob Upload with Variables

```graphql
mutation UploadImage($imageData: String!, $type: String!) {
  uploadBlob(
    data: $imageData
    mimeType: $type
  ) {
    ref
    mimeType
    size
  }
}
```

Variables:

```json
{
  "imageData": "base64EncodedImageData...",
  "type": "image/jpeg"
}
```

## Update Profile with Variables

```graphql
mutation UpdateProfile(
  $name: String!
  $bio: String!
  $avatarRef: String!
  $avatarType: String!
  $avatarSize: Int!
) {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: $name
      description: $bio
      avatar: {
        ref: $avatarRef
        mimeType: $avatarType
        size: $avatarSize
      }
    }
  ) {
    uri
    displayName
    description
    avatar {
      ref
      url(preset: "avatar")
    }
  }
}
```

Variables:

```json
{
  "name": "Alice Smith",
  "bio": "Software engineer & designer",
  "avatarRef": "bafkreiabc123...",
  "avatarType": "image/jpeg",
  "avatarSize": 125000
}
```

## Using in HTTP Requests

When making HTTP requests, send variables in the request body:

```bash
curl -X POST http://localhost:8000/graphql \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer <token>" \
  -d '{
    "query": "query GetStatus($emoji: String!) { xyzStatusphereStatus(where: { status: { eq: $emoji } }) { edges { node { status } } } }",
    "variables": {
      "emoji": "🎉"
    }
  }'
```
