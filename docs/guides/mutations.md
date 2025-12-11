# Mutations

Mutations write records to the authenticated user's repository. All mutations require authentication.

## Creating Records

```graphql
mutation {
  createXyzStatusphereStatus(
    input: {
      status: "🎉"
      createdAt: "2025-01-30T12:00:00Z"
    }
  ) {
    uri
    status
    createdAt
  }
}
```

Quickslice:

1. Writes the record to the user's PDS
2. Indexes locally for immediate query availability

### Custom Record Keys

By default, Quickslice generates a TID (timestamp-based ID) for the record key. You can specify a custom key:

```graphql
mutation {
  createXyzStatusphereStatus(
    input: {
      status: "✨"
      createdAt: "2025-01-30T12:00:00Z"
    }
    rkey: "my-custom-key"
  ) {
    uri
  }
}
```

Some Lexicons require specific key patterns. For example, profiles use `self` as the record key.

## Updating Records

Update an existing record by its record key:

```graphql
mutation {
  updateXyzStatusphereStatus(
    rkey: "3kvt7a2xyzw2a"
    input: {
      status: "🚀"
      createdAt: "2025-01-30T12:00:00Z"
    }
  ) {
    uri
    status
  }
}
```

The update replaces the entire record. Include all required fields, not just the ones you're changing.

## Deleting Records

Delete a record by its record key:

```graphql
mutation {
  deleteXyzStatusphereStatus(rkey: "3kvt7a2xyzw2a") {
    uri
  }
}
```

## Working with Blobs

Records can include binary data like images. Upload the blob first, then reference it.

### Upload a Blob

```graphql
mutation {
  uploadBlob(
    data: "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
    mimeType: "image/png"
  ) {
    ref
    mimeType
    size
  }
}
```

The `data` field accepts base64-encoded binary data. The response includes a `ref` (CID) for use in your record.

### Use the Blob in a Record

```graphql
mutation {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: "Alice"
      avatar: {
        ref: "bafkreiabc123..."
        mimeType: "image/png"
        size: 95
      }
    }
  ) {
    uri
    displayName
    avatar {
      url(preset: "avatar")
    }
  }
}
```

See the [Blobs Reference](../reference/blobs.md) for more details on blob handling and URL presets.

## Error Handling

Common mutation errors:

| Error | Cause |
|-------|-------|
| `401 Unauthorized` | Missing or invalid authentication token |
| `400 Bad Request` | Invalid input (missing required fields, wrong types) |
| `404 Not Found` | Record doesn't exist (for update/delete) |
| `403 Forbidden` | Trying to modify another user's record |

## Authentication

Mutations require authentication. Headers depend on the OAuth flow:

**DPoP flow** (recommended for browser apps):
```
Authorization: DPoP <access_token>
DPoP: <dpop_proof>
```

**Bearer token flow**:
```
Authorization: Bearer <access_token>
```

See the [Authentication Guide](authentication.md) for flow details and token acquisition.
