# Working with Blobs

Blobs are used for binary data like images, videos, and files. They are uploaded separately and referenced by their CID (Content Identifier).

## Upload Blob

Upload binary data encoded as base64:

```graphql
mutation {
  uploadBlob(
    data: "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAY..."
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
      "ref": "bafkreiabc123xyz...",
      "mimeType": "image/png",
      "size": 1024
    }
  }
}
```

## Blob Reference

A blob reference contains:

- `ref`: CID of the blob content
- `mimeType`: MIME type (e.g., `image/jpeg`, `image/png`)
- `size`: Size in bytes

## Using Blobs in Records

### Profile Avatar

```graphql
mutation {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: "Alice"
      avatar: {
        ref: "bafkreiabc123..."
        mimeType: "image/jpeg"
        size: 125000
      }
    }
  ) {
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

### Profile Banner

```graphql
mutation {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: "Alice"
      banner: {
        ref: "bafkreixyz789..."
        mimeType: "image/jpeg"
        size: 450000
      }
    }
  ) {
    displayName
    banner {
      ref
      mimeType
      size
      url
    }
  }
}
```

## Blob URLs

Blobs automatically generate CDN URLs for serving. Use the `url` field with optional presets:

### Default URL

```graphql
query {
  appBskyActorProfile {
    edges {
      node {
        avatar {
          ref
          url
        }
      }
    }
  }
}
```

Returns: `https://cdn.bsky.app/img/feed_fullsize/plain/did:plc:.../bafkreiabc123@jpeg`

### Avatar Preset

```graphql
query {
  appBskyActorProfile {
    edges {
      node {
        avatar {
          ref
          url(preset: "avatar")
        }
      }
    }
  }
}
```

Returns: `https://cdn.bsky.app/img/avatar/plain/did:plc:.../bafkreiabc123@jpeg`

### Banner Preset

```graphql
query {
  appBskyActorProfile {
    edges {
      node {
        banner {
          url(preset: "banner")
        }
      }
    }
  }
}
```

Returns: `https://cdn.bsky.app/img/banner/plain/did:plc:.../bafkreixyz789@jpeg`

## Available Presets

- `avatar` - Optimized for profile avatars (square, small)
- `banner` - Optimized for profile banners (wide, medium)
- `feed_thumbnail` - Thumbnails in feed view
- `feed_fullsize` - Full size images in feed (default)

## Complete Example: Update Profile with Images

### Step 1: Upload Avatar

```graphql
mutation UploadAvatar($avatarData: String!) {
  uploadBlob(data: $avatarData, mimeType: "image/jpeg") {
    ref
    mimeType
    size
  }
}
```

Variables:
```json
{
  "avatarData": "base64EncodedJpegData..."
}
```

Response:
```json
{
  "data": {
    "uploadBlob": {
      "ref": "bafkreiabc123avatar",
      "mimeType": "image/jpeg",
      "size": 125000
    }
  }
}
```

### Step 2: Upload Banner

```graphql
mutation UploadBanner($bannerData: String!) {
  uploadBlob(data: $bannerData, mimeType: "image/jpeg") {
    ref
    mimeType
    size
  }
}
```

Variables:
```json
{
  "bannerData": "base64EncodedJpegData..."
}
```

Response:
```json
{
  "data": {
    "uploadBlob": {
      "ref": "bafkreixyz789banner",
      "mimeType": "image/jpeg",
      "size": 450000
    }
  }
}
```

### Step 3: Update Profile

```graphql
mutation UpdateProfileWithImages {
  updateAppBskyActorProfile(
    rkey: "self"
    input: {
      displayName: "Alice Smith"
      description: "Software engineer & designer"
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
    avatar {
      ref
      mimeType
      size
      url(preset: "avatar")
    }
    banner {
      ref
      mimeType
      size
      url(preset: "banner")
    }
  }
}
```

Response:
```json
{
  "data": {
    "updateAppBskyActorProfile": {
      "uri": "at://did:plc:xyz/app.bsky.actor.profile/self",
      "displayName": "Alice Smith",
      "description": "Software engineer & designer",
      "avatar": {
        "ref": "bafkreiabc123avatar",
        "mimeType": "image/jpeg",
        "size": 125000,
        "url": "https://cdn.bsky.app/img/avatar/plain/did:plc:xyz/bafkreiabc123avatar@jpeg"
      },
      "banner": {
        "ref": "bafkreixyz789banner",
        "mimeType": "image/jpeg",
        "size": 450000,
        "url": "https://cdn.bsky.app/img/banner/plain/did:plc:xyz/bafkreixyz789banner@jpeg"
      }
    }
  }
}
```

## JavaScript Example

```javascript
// Convert file to base64
async function fileToBase64(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.readAsDataURL(file);
    reader.onload = () => {
      const base64 = reader.result.split(',')[1]; // Remove data:image/jpeg;base64, prefix
      resolve(base64);
    };
    reader.onerror = reject;
  });
}

// Upload blob
async function uploadBlob(file, token) {
  const base64Data = await fileToBase64(file);

  const response = await fetch('/graphql', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      query: `
        mutation UploadBlob($data: String!, $mimeType: String!) {
          uploadBlob(data: $data, mimeType: $mimeType) {
            ref
            mimeType
            size
          }
        }
      `,
      variables: {
        data: base64Data,
        mimeType: file.type
      }
    })
  });

  const result = await response.json();
  return result.data.uploadBlob;
}

// Usage
const avatarFile = document.getElementById('avatar-input').files[0];
const blob = await uploadBlob(avatarFile, 'your-token');
console.log('Uploaded blob:', blob.ref);
```
