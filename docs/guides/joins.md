# Joins

AT Protocol data is organized into collections. A user's status records live in one collection, their profile in another. Quickslice generates joins that let you query across collections, so you can fetch a status and its author's profile in a single request.

## Join Types

Quickslice generates three types of joins automatically:

| Type | What it does | Field naming |
|------|--------------|--------------|
| **Forward** | Follows a URI or strong ref to another record | `{fieldName}Resolved` |
| **Reverse** | Finds all records that reference a given record | `{SourceType}Via{FieldName}` |
| **DID** | Finds records by the same author | `{CollectionName}ByDid` |

## Forward Joins

Forward joins follow references from one record to another. When a record has a field containing an AT-URI or strong ref, Quickslice generates a `{fieldName}Resolved` field to fetch the referenced record.

### Example: Resolving a Favorite's Subject

A favorite record has a `subject` field containing an AT-URI. The `subjectResolved` field fetches the actual record:

```graphql
query {
  socialGrainFavorite(first: 5) {
    edges {
      node {
        subject
        createdAt
        subjectResolved {
          ... on SocialGrainPhoto {
            uri
            alt
          }
          ... on SocialGrainGallery {
            uri
            title
          }
        }
      }
    }
  }
}
```

Forward joins return a `Record` union type because the referenced record could be any type. Use inline fragments (`... on TypeName`) to access type-specific fields.

## Reverse Joins

Reverse joins work in the opposite direction: given a record, find all records that reference it. Quickslice analyzes your Lexicons and generates reverse join fields automatically.

Reverse joins return paginated connections with support for filtering, sorting, and cursor-based pagination.

### Example: Comments on a Photo

Find all comments that reference a specific photo:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        uri
        alt
        socialGrainCommentViaSubject(first: 10) {
          totalCount
          edges {
            node {
              text
              createdAt
            }
          }
          pageInfo {
            hasNextPage
            endCursor
          }
        }
      }
    }
  }
}
```

### Sorting and Filtering Reverse Joins

Reverse joins support the same sorting and filtering as top-level queries:

```graphql
query {
  socialGrainGallery(first: 3) {
    edges {
      node {
        title
        socialGrainGalleryItemViaGallery(
          first: 10
          sortBy: [{ field: position, direction: ASC }]
          where: { createdAt: { gt: "2025-01-01T00:00:00Z" } }
        ) {
          edges {
            node {
              position
            }
          }
        }
      }
    }
  }
}
```

## DID Joins

DID joins connect records by their author's identity. Every record has a `did` field identifying who created it. Quickslice generates `{CollectionName}ByDid` fields to find related records by the same author.

### Example: Author Profile from a Status

Get the author's profile alongside their status:

```graphql
query {
  xyzStatusphereStatus(first: 10) {
    edges {
      node {
        status
        createdAt
        appBskyActorProfileByDid {
          displayName
          avatar { url }
        }
      }
    }
  }
}
```

### Unique vs Non-Unique DID Joins

Some collections have exactly one record per DID (like profiles with a `literal:self` key). These return a single object:

```graphql
appBskyActorProfileByDid {
  displayName
}
```

Other collections can have multiple records per DID. These return paginated connections:

```graphql
socialGrainPhotoByDid(first: 10, sortBy: [{ field: createdAt, direction: DESC }]) {
  totalCount
  edges {
    node {
      alt
    }
  }
}
```

### Cross-Lexicon DID Joins

DID joins work across different Lexicon families. Get a user's Bluesky profile alongside their app-specific data:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        alt
        appBskyActorProfileByDid {
          displayName
          avatar { url }
        }
        socialGrainActorProfileByDid {
          description
        }
      }
    }
  }
}
```

## Common Patterns

### Profile Lookups

The most common pattern: joining author profiles to any record type.

```graphql
query {
  myAppPost(first: 20) {
    edges {
      node {
        content
        appBskyActorProfileByDid {
          displayName
          avatar { url }
        }
      }
    }
  }
}
```

### Engagement Counts

Use reverse joins to count likes, comments, or other engagement:

```graphql
query {
  socialGrainPhoto(first: 10) {
    edges {
      node {
        uri
        socialGrainFavoriteViaSubject {
          totalCount
        }
        socialGrainCommentViaSubject {
          totalCount
        }
      }
    }
  }
}
```

### User Activity

Get all records by a user across multiple collections:

```graphql
query {
  socialGrainActorProfile(first: 1, where: { actorHandle: { eq: "alice.bsky.social" } }) {
    edges {
      node {
        displayName
        socialGrainPhotoByDid(first: 5) {
          totalCount
          edges { node { alt } }
        }
        socialGrainGalleryByDid(first: 5) {
          totalCount
          edges { node { title } }
        }
      }
    }
  }
}
```

## How Batching Works

Quickslice batches join resolution to avoid the N+1 query problem. When you query 100 photos with author profiles:

1. Fetches 100 photos in one query
2. Collects all unique DIDs from those photos
3. Fetches all profiles in a single query: `WHERE did IN (...)`
4. Maps profiles back to their photos

This happens automatically for all join types.
