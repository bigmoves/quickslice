# Mutations

> **Note:** All mutations require authentication. Include your Bearer token in the `Authorization` header.

## Create Record

Create a new record:

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

### With Custom rkey

Provide a custom record key (rkey):

```graphql
mutation {
  createXyzStatusphereStatus(
    input: {
      status: "✨"
      createdAt: "2025-01-30T12:00:00Z"
    }
    rkey: "custom-key-123"
  ) {
    uri
    status
  }
}
```

## Update Record

Update an existing record:

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
    createdAt
  }
}
```

## Delete Record

Delete a record:

```graphql
mutation {
  deleteXyzStatusphereStatus(rkey: "3kvt7a2xyzw2a") {
    uri
    status
  }
}
```

## Working with Blobs

### Upload Blob

First, upload binary data as a blob:

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

Response:

```json
{
  "data": {
    "uploadBlob": {
      "ref": "bafkreiabc123...",
      "mimeType": "image/png",
      "size": 95
    }
  }
}
```

### Use Blob in Record

Use the blob reference in a record:

```graphql
mutation {
  createAppBskyActorProfile(
    input: {
      displayName: "Alice"
      description: "Builder of things"
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
      ref
      mimeType
      size
      url
    }
  }
}
```

### Update Profile with Avatar and Banner

```graphql
mutation {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: "Alice Smith"
      description: "Software engineer & designer"
      pronouns: "she/her"
      website: "https://alice.com"
      avatar: {
        ref: "bafkreiabc123avatar"
        mimeType: "image/jpeg"
        size: 125000
      }
      banner: {
        ref: "bafkreixyz789banner"
        mimeType: "image/jpeg"
        size: 450000
      }
    }
  ) {
    uri
    displayName
    description
    pronouns
    website
    avatar {
      ref
      url(preset: "avatar")
    }
    banner {
      ref
      url(preset: "banner")
    }
  }
}
```

## Blob URLs

Blobs automatically generate CDN URLs. You can specify a preset for different sizes:

```graphql
query {
  appBskyActorProfile {
    records {
      displayName
      avatar {
        ref
        url(preset: "avatar")      # Optimized for avatars
      }
      banner {
        ref
        url(preset: "banner")      # Optimized for banners
      }
    }
  }
}
```

Available presets:
- `avatar` - Small square image
- `banner` - Large horizontal image
- `feed_thumbnail` - Thumbnail for feed
- `feed_fullsize` - Full size for feed (default)
