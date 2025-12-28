# Viewer State

Viewer state fields show the authenticated user's relationship to records. Has the current user favorited this photo? Do they follow this author? Viewer state answers these questions in a single query.

## How It Works

Viewer state fields find your records that reference the current record's URI or its author's DID. When you query a gallery, `viewerSocialGrainFavoriteViaSubject` returns your favorite for that gallery, if one exists.

The server identifies you from your access token. Authentication is required.

## Field Naming

Quickslice generates viewer state fields with the pattern:

```
viewer{CollectionName}Via{FieldName}
```

The field name comes from your lexicon. For example:
- A `subject` field generates `viewerSocialGrainFavoriteViaSubject`
- A `target` field generates `viewerSocialGrainFavoriteViaTarget`

## AT-URI Based (Records)

For lexicons with `format: "at-uri"` fields, viewer state checks if you have a record pointing to the current record's URI.

### Example: Check if User Favorited a Gallery

```graphql
query {
  socialGrainGallery(first: 10) {
    edges {
      node {
        uri
        title
        viewerSocialGrainFavoriteViaSubject {
          uri
        }
      }
    }
  }
}
```

Response when you've favorited the first gallery:

```json
{
  "data": {
    "socialGrainGallery": {
      "edges": [
        {
          "node": {
            "uri": "at://did:plc:author/social.grain.gallery/abc123",
            "title": "My Gallery",
            "viewerSocialGrainFavoriteViaSubject": {
              "uri": "at://did:plc:you/social.grain.favorite/fav456"
            }
          }
        },
        {
          "node": {
            "uri": "at://did:plc:author/social.grain.gallery/xyz789",
            "title": "Another Gallery",
            "viewerSocialGrainFavoriteViaSubject": null
          }
        }
      ]
    }
  }
}
```

The first gallery returns your favorite record. The second returns `null` because you have not favorited it.

## DID Based (Users)

For lexicons with `format: "did"` fields, viewer state checks if you have a record pointing to the current record's author DID.

### Example: Check if User Follows an Author

```graphql
query {
  socialGrainActorProfile(first: 10) {
    edges {
      node {
        uri
        did
        displayName
        viewerSocialGrainGraphFollowViaSubject {
          uri
        }
      }
    }
  }
}
```

Response when you follow the first user:

```json
{
  "data": {
    "socialGrainActorProfile": {
      "edges": [
        {
          "node": {
            "uri": "at://did:plc:alice/social.grain.actor.profile/self",
            "did": "did:plc:alice",
            "displayName": "Alice",
            "viewerSocialGrainGraphFollowViaSubject": {
              "uri": "at://did:plc:you/social.grain.graph.follow/follow123"
            }
          }
        },
        {
          "node": {
            "uri": "at://did:plc:bob/social.grain.actor.profile/self",
            "did": "did:plc:bob",
            "displayName": "Bob",
            "viewerSocialGrainGraphFollowViaSubject": null
          }
        }
      ]
    }
  }
}
```

## Combining with Other Joins

Viewer state works alongside other join types. Get engagement counts and your personal state together:

```graphql
query {
  socialGrainGallery(first: 5) {
    edges {
      node {
        uri
        title

        # Author profile
        socialGrainActorProfileByDid {
          displayName
        }

        # Total favorites count
        socialGrainFavoriteViaSubject {
          totalCount
        }

        # Did YOU favorite it?
        viewerSocialGrainFavoriteViaSubject {
          uri
        }
      }
    }
  }
}
```

## Authentication Required

Viewer state fields require authentication. Without a valid auth token:
- Fields return `null`
- No error is thrown

Use the [Quickslice client SDK](./authentication.md#using-the-client-sdk) to handle authentication automatically.

## Lexicon Requirements

Quickslice generates viewer state fields for any field with `format: "at-uri"` or `format: "did"`:

### AT-URI Format (for record references)

```json
{
  "properties": {
    "subject": {
      "type": "string",
      "format": "at-uri"
    }
  }
}
```

Generates: `viewer{Collection}ViaSubject`

### DID Format (for user references)

```json
{
  "properties": {
    "subject": {
      "type": "string",
      "format": "did"
    }
  }
}
```

Generates: `viewer{Collection}ViaSubject`

The field can have any name: `subject`, `target`, `ref`, or something else. The name becomes part of the generated field name.

## How Batching Works

Viewer state queries are batched like other joins. When fetching 100 galleries with viewer favorites:

1. Fetches 100 galleries
2. Collects all gallery URIs
3. Queries favorites where `did = viewer_did AND subject IN (uris)`
4. Maps results back to galleries

This avoids N+1 queries regardless of result size.
