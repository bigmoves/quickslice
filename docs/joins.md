# Joins

quickslice automatically generates **forward joins**, **reverse joins**, and **DID joins** based on your lexicon schemas, allowing you to traverse relationships between records.

> **Note:** The examples in this document use [Grain](https://grain.social)'s `social.grain.*` lexicons. The same join patterns apply to any AT Protocol lexicons you load into quickslice.

## Overview

- **Forward Joins**: Follow references from one record to another (e.g., comment → photo it's about)
  - Returns: Single object or `Record` union
  - Naming: `{fieldName}Resolved`

- **Reverse Joins**: Discover records that reference a given record (e.g., photo → all favorites on it)
  - Returns: **Paginated Connection** with sorting, filtering, and pagination
  - Naming: `{SourceType}Via{FieldName}`

- **DID Joins**: Find records that share the same author (DID)
  - Returns: Single object (unique DID) or **Paginated Connection** (non-unique DID)
  - Naming: `{CollectionName}ByDid`

- **Union Types**: Forward joins return a `Record` union, allowing type-specific field access via inline fragments

## Forward Joins

Forward joins are generated for fields that reference other records via:
- `at-uri` format strings
- `strongRef` objects (containing `uri` and `cid`)

### Resolving Favorite Subjects

Favorites have a `subject` field containing an AT-URI. This gets a `subjectResolved` field to fetch the actual record:

```graphql
query {
  socialGrainFavorite(first: 3) {
    edges {
      node {
        uri
        subject
        createdAt
        # Resolve what was favorited
        subjectResolved {
          ... on SocialGrainPhoto {
            uri
            alt
            createdAt
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

### Union Types & Inline Fragments

Forward join fields return a `Record` union type because the referenced record could be any type. Use inline fragments to access type-specific fields:

```graphql
query {
  socialGrainComment(first: 5) {
    edges {
      node {
        uri
        text
        createdAt
        # The subject could be a photo or gallery
        subjectResolved {
          ... on SocialGrainPhoto {
            uri
            alt
          }
          ... on SocialGrainGallery {
            uri
            title
            description
          }
        }
      }
    }
  }
}
```

### Gallery Item Forward Joins

Gallery items link photos to galleries. Both references can be resolved:

```graphql
query {
  socialGrainGalleryItem(first: 3) {
    edges {
      node {
        uri
        position
        createdAt
        # Resolve the gallery this item belongs to
        galleryResolved {
          ... on SocialGrainGallery {
            title
            description
          }
        }
        # Resolve the photo
        itemResolved {
          ... on SocialGrainPhoto {
            uri
            alt
          }
        }
      }
    }
  }
}
```

## Reverse Joins

Reverse joins are automatically discovered by analyzing all lexicons. They allow you to find all records that reference a given record. **Reverse joins return paginated connections** with support for sorting, filtering, and cursor-based pagination.

### Favorites on a Photo

Find all users who favorited a specific photo:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        uri
        alt
        createdAt
        # Find all favorites on this photo
        socialGrainFavoriteViaSubject(first: 10) {
          totalCount
          edges {
            node {
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
    }
  }
}
```

### Comments on a Gallery

Find all comments on a gallery:

```graphql
query {
  socialGrainGallery(first: 3) {
    edges {
      node {
        uri
        title
        description
        # Find all comments on this gallery
        socialGrainCommentViaSubject(first: 20) {
          totalCount
          edges {
            node {
              text
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### Sorting Reverse Joins

Sort reverse join results by any field in the joined collection:

```graphql
query {
  socialGrainGallery(first: 3) {
    edges {
      node {
        uri
        title
        # Get items sorted by position
        socialGrainGalleryItemViaGallery(
          first: 10
          sortBy: [{ field: position, direction: ASC }]
        ) {
          edges {
            node {
              position
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### Filtering Reverse Joins

Use `where` filters to narrow down nested join results:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        uri
        alt
        # Only get favorites created after a certain date
        socialGrainFavoriteViaSubject(
          where: { createdAt: { gt: "2025-06-01T00:00:00Z" } }
        ) {
          totalCount
          edges {
            node {
              createdAt
            }
          }
        }
      }
    }
  }
}
```

## DID Joins

DID joins allow you to traverse relationships between records that share the same author (DID). These are automatically generated for all collection pairs and are named: `{CollectionName}ByDid`

### Two Types of DID Joins

#### 1. Unique DID Joins (literal:self key)

Collections with a `literal:self` key (like profiles) have only one record per DID. These return a **single nullable object** (no pagination needed):

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        uri
        alt
        did
        # Get the photographer's profile (single object, not paginated)
        socialGrainActorProfileByDid {
          displayName
          description
        }
      }
    }
  }
}
```

#### 2. Non-Unique DID Joins

Most collections can have multiple records per DID. These return **paginated connections** with full support for sorting, filtering, and pagination:

```graphql
query {
  socialGrainActorProfile(first: 3) {
    edges {
      node {
        displayName
        # Get all photos by this user (paginated)
        socialGrainPhotoByDid(
          first: 10
          sortBy: [{ field: createdAt, direction: DESC }]
        ) {
          totalCount
          edges {
            node {
              uri
              alt
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

### User's Photos

Get a user's recent photos from their profile:

```graphql
query {
  socialGrainActorProfile(first: 1) {
    edges {
      node {
        displayName
        description
        socialGrainPhotoByDid(first: 5, sortBy: [{ field: createdAt, direction: DESC }]) {
          totalCount
          edges {
            node {
              alt
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### User's Galleries

Get all galleries created by a user:

```graphql
query {
  socialGrainActorProfile(first: 1) {
    edges {
      node {
        displayName
        socialGrainGalleryByDid(first: 10, sortBy: [{ field: createdAt, direction: DESC }]) {
          totalCount
          edges {
            node {
              title
              description
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### User's Favorites

Get what a user has favorited:

```graphql
query {
  socialGrainActorProfile(first: 1) {
    edges {
      node {
        displayName
        socialGrainFavoriteByDid(first: 10, sortBy: [{ field: createdAt, direction: DESC }]) {
          totalCount
          edges {
            node {
              subject
              createdAt
              # Resolve what was favorited
              subjectResolved {
                ... on SocialGrainGallery {
                  title
                }
              }
            }
          }
        }
      }
    }
  }
}
```

### Cross-Collection DID Queries

DID joins work across all collection pairs, enabling powerful cross-collection queries:

```graphql
query {
  socialGrainActorProfile(first: 3) {
    edges {
      node {
        displayName
        # All their photos
        socialGrainPhotoByDid(first: 5) {
          totalCount
          edges {
            node {
              alt
            }
          }
        }
        # All their galleries
        socialGrainGalleryByDid(first: 5) {
          totalCount
          edges {
            node {
              title
            }
          }
        }
        # All their favorites
        socialGrainFavoriteByDid(first: 5) {
          totalCount
        }
      }
    }
  }
}
```

### From Photo to Author Profile

Navigate from any photo back to the author's full profile:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        alt
        createdAt
        socialGrainActorProfileByDid {
          displayName
          description
          # And back to their other photos
          socialGrainPhotoByDid(first: 3) {
            totalCount
          }
        }
      }
    }
  }
}
```

### Cross-Lexicon DID Joins (Bluesky Profile)

DID joins work across different lexicon families. Get a user's Bluesky profile alongside their Grain data:

```graphql
query {
  socialGrainPhoto(first: 5) {
    edges {
      node {
        alt
        createdAt
        # Get the Bluesky profile for avatar
        appBskyActorProfileByDid {
          displayName
          avatar {
            url(preset: "avatar")
          }
        }
        # Get the Grain profile for bio
        socialGrainActorProfileByDid {
          description
        }
      }
    }
  }
}
```

### DID Join Arguments

Non-unique DID joins support all standard connection arguments:

| Argument | Type | Description |
|----------|------|-------------|
| `first` | `Int` | Number of records to return (forward pagination) |
| `after` | `String` | Cursor for forward pagination |
| `last` | `Int` | Number of records to return (backward pagination) |
| `before` | `String` | Cursor for backward pagination |
| `sortBy` | `[SortFieldInput!]` | Sort by any field in the collection |
| `where` | `WhereInput` | Filter nested records |

## Complete Example

Combining forward joins, reverse joins, and DID joins to build a rich gallery view:

```graphql
query {
  socialGrainGallery(first: 3) {
    edges {
      node {
        uri
        title
        description
        createdAt

        # DID join: Get the author's Grain profile
        socialGrainActorProfileByDid {
          displayName
          description
        }

        # DID join: Get the author's Bluesky profile for avatar
        appBskyActorProfileByDid {
          avatar {
            url(preset: "avatar")
          }
        }

        # Reverse join: Get items in this gallery
        socialGrainGalleryItemViaGallery(
          first: 10
          sortBy: [{ field: position, direction: ASC }]
        ) {
          totalCount
          edges {
            node {
              position
              # Forward join: Resolve the photo
              itemResolved {
                ... on SocialGrainPhoto {
                  uri
                  alt
                }
              }
            }
          }
        }

        # Reverse join: Get favorites on this gallery
        socialGrainFavoriteViaSubject(first: 5) {
          totalCount
        }

        # Reverse join: Get comments on this gallery
        socialGrainCommentViaSubject(first: 5) {
          totalCount
          edges {
            node {
              text
              createdAt
              # DID join: Get commenter's profile
              socialGrainActorProfileByDid {
                displayName
              }
            }
          }
        }
      }
    }
  }
}
```

## DataLoader Batching

All joins use DataLoader for efficient batching:

```graphql
query {
  socialGrainPhoto(first: 100) {
    edges {
      node {
        uri
        alt
        socialGrainActorProfileByDid {
          displayName
        }
      }
    }
  }
}
```

**How it works:**
1. Fetches 100 photos
2. Collects all unique DIDs from those photos
3. Batches them into a single SQL query: `WHERE did IN (...)`
4. Returns resolved profiles efficiently

## Type Resolution

The `Record` union uses a type resolver that examines the `collection` field:

| Collection | GraphQL Type |
|------------|--------------|
| `social.grain.photo` | `SocialGrainPhoto` |
| `social.grain.gallery` | `SocialGrainGallery` |
| `social.grain.actor.profile` | `SocialGrainActorProfile` |
| `app.bsky.actor.profile` | `AppBskyActorProfile` |

This allows inline fragments to work correctly:

```graphql
query {
  socialGrainFavorite(first: 5) {
    edges {
      node {
        subjectResolved {
          ... on SocialGrainPhoto { alt }
          ... on SocialGrainGallery { title }
        }
      }
    }
  }
}
```

## Schema Introspection

Discover available joins using introspection:

```graphql
query {
  __type(name: "SocialGrainPhoto") {
    fields {
      name
      type {
        name
        kind
      }
    }
  }
}
```

Look for fields ending in:
- `Resolved` (forward joins)
- `Via*` (reverse joins)
- `ByDid` (DID joins)
