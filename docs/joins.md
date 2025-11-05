# Joins

quickslice automatically generates **forward joins**, **reverse joins**, and **DID joins** based on AT Protocol lexicon schemas, allowing you to traverse relationships between records.

## Overview

- **Forward Joins**: Follow references from one record to another (e.g., post → parent post)
  - Returns: Single object or `Record` union
  - Naming: `{fieldName}Resolved`

- **Reverse Joins**: Discover records that reference a given record (e.g., post → all likes on that post)
  - Returns: **Paginated Connection** with sorting, filtering, and pagination
  - Naming: `{SourceType}Via{FieldName}`

- **DID Joins**: Find records that share the same author (DID)
  - Returns: Single object (unique DID) or **Paginated Connection** (non-unique DID)
  - Naming: `{CollectionName}ByDid`

- **Union Types**: Forward joins return a `Record` union, allowing type-specific field access via inline fragments

## Forward Joins

Forward joins are generated for fields that reference other records via:
- `at-uri` format strings
- `strongRef` objects

### Basic Forward Join

When a field references another record, quickslice creates a `*Resolved` field:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        replyTo              # The at-uri string
        replyToResolved {    # The resolved record
          uri
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
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        replyToResolved {
          # Access fields based on the actual type
          ... on AppBskyFeedPost {
            uri
            text
            createdAt
          }
          ... on AppBskyFeedLike {
            uri
            subject
            createdAt
          }
        }
      }
    }
  }
}
```

### StrongRef Forward Joins

StrongRef fields (containing `uri` and `cid`) are resolved automatically:

```graphql
query {
  appBskyActorProfile {
    edges {
      node {
        displayName
        pinnedPost {
          uri    # Original strongRef uri
          cid    # Original strongRef cid
        }
        pinnedPostResolved {
          ... on AppBskyFeedPost {
            uri
            text
            likeCount
          }
        }
      }
    }
  }
}
```

## Reverse Joins

Reverse joins are automatically discovered by analyzing all lexicons. They allow you to find all records that reference a given record. **Reverse joins return paginated connections** with support for sorting, filtering, and cursor-based pagination.

### Basic Reverse Join

Reverse join fields are named: `{SourceType}Via{FieldName}` and return a Connection type:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Find all likes that reference this post via their 'subject' field
        appBskyFeedLikeViaSubject(first: 20) {
          totalCount  # Total number of likes
          edges {
            node {
              uri
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
        }
      }
    }
  }
}
```

### Multiple Reverse Joins

A record type can have multiple reverse join fields. You can request different page sizes for each:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Get first 10 replies
        appBskyFeedPostViaReplyTo(first: 10) {
          totalCount
          edges {
            node {
              uri
              text
            }
          }
        }
        # Get first 20 likes
        appBskyFeedLikeViaSubject(first: 20) {
          totalCount
          edges {
            node {
              uri
              createdAt
            }
          }
        }
        # Get first 20 reposts
        appBskyFeedRepostViaSubject(first: 20) {
          totalCount
          edges {
            node {
              uri
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### Reverse Joins with StrongRef

Reverse joins work with strongRef fields too. You can also use sorting and filtering:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Find all profiles that pinned this post
        appBskyActorProfileViaPinnedPost(
          sortBy: [{field: "indexedAt", direction: DESC}]
        ) {
          totalCount
          edges {
            node {
              uri
              displayName
            }
          }
        }
      }
    }
  }
}
```

### Sorting Reverse Joins

You can sort reverse join results by any field in the joined collection:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        # Get most recent likes first
        appBskyFeedLikeViaSubject(
          first: 10
          sortBy: [{field: "createdAt", direction: DESC}]
        ) {
          edges {
            node {
              uri
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
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Only get likes from a specific user
        appBskyFeedLikeViaSubject(
          where: { did: { eq: "did:plc:abc123" } }
        ) {
          totalCount  # Likes from this specific user
          edges {
            node {
              uri
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
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Get the author's profile (single object, not paginated)
        appBskyActorProfileByDid {
          uri
          displayName
          bio
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
  appBskyActorProfile {
    edges {
      node {
        displayName
        # Get all posts by this user (paginated)
        appBskyFeedPostByDid(
          first: 10
          sortBy: [{field: "indexedAt", direction: DESC}]
        ) {
          totalCount  # Total posts by this user
          edges {
            node {
              uri
              text
              indexedAt
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

### DID Join with Filtering

Combine DID joins with filters to find specific records:

```graphql
query {
  appBskyActorProfile(where: { did: { eq: "did:plc:abc123" } }) {
    edges {
      node {
        displayName
        # Get only posts containing "gleam"
        appBskyFeedPostByDid(
          where: { text: { contains: "gleam" } }
          sortBy: [{field: "indexedAt", direction: DESC}]
        ) {
          totalCount  # Posts mentioning "gleam"
          edges {
            node {
              text
              indexedAt
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
  appBskyActorProfile {
    edges {
      node {
        displayName
        # All their posts
        appBskyFeedPostByDid(first: 10) {
          totalCount
          edges {
            node {
              text
            }
          }
        }
        # All their likes
        appBskyFeedLikeByDid(first: 10) {
          totalCount
          edges {
            node {
              subject
            }
          }
        }
        # All their reposts
        appBskyFeedRepostByDid(first: 10) {
          totalCount
          edges {
            node {
              subject
            }
          }
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

Combining forward joins, reverse joins, and DID joins to build a rich thread view:

```graphql
query GetThread($postUri: String!) {
  appBskyFeedPost(where: { uri: { eq: $postUri } }) {
    edges {
      node {
        uri
        text
        createdAt

        # DID join: Get the author's profile
        appBskyActorProfileByDid {
          displayName
          bio
        }

        # Forward join: Get the parent post
        replyToResolved {
          ... on AppBskyFeedPost {
            uri
            text
            createdAt
          }
        }

        # Reverse join: Get first 10 replies
        appBskyFeedPostViaReplyTo(
          first: 10
          sortBy: [{field: "createdAt", direction: ASC}]
        ) {
          totalCount  # Total replies
          edges {
            node {
              uri
              text
              createdAt
            }
          }
          pageInfo {
            hasNextPage
          }
        }

        # Reverse join: Get first 20 likes
        appBskyFeedLikeViaSubject(first: 20) {
          totalCount  # Like count
          edges {
            node {
              uri
              createdAt
            }
          }
        }

        # Reverse join: Get reposts
        appBskyFeedRepostViaSubject(first: 20) {
          totalCount  # Repost count
          edges {
            node {
              uri
              createdAt
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
# This query will batch all replyToResolved lookups into a single database query
query {
  appBskyFeedPost(first: 100) {
    edges {
      node {
        uri
        text
        replyToResolved {
          ... on AppBskyFeedPost {
            uri
            text
          }
        }
      }
    }
  }
}
```

**How it works:**
1. Fetches 100 posts
2. Collects all unique `replyTo` URIs
3. Batches them into a single SQL query: `WHERE uri IN (...)`
4. Returns resolved records efficiently

## Performance Tips

### 1. Only Request What You Need

```graphql
# Good: Only request specific fields
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        appBskyFeedLikeViaSubject(first: 20) {
          totalCount  # Get count without fetching all records
          edges {
            node {
              uri  # Only need the URI
            }
          }
        }
      }
    }
  }
}
```

### 2. Use totalCount for Metrics

Get engagement counts efficiently without fetching all records:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Just get counts, no records
        likes: appBskyFeedLikeViaSubject(first: 0) {
          totalCount  # Like count
        }
        reposts: appBskyFeedRepostViaSubject(first: 0) {
          totalCount  # Repost count
        }
        replies: appBskyFeedPostViaReplyTo(first: 0) {
          totalCount  # Reply count
        }
      }
    }
  }
}
```

### 3. Use Pagination on Nested Joins

Nested joins are paginated by default. Always specify `first` or `last` for optimal performance:

```graphql
query {
  appBskyFeedPost(first: 10) {
    edges {
      node {
        uri
        text
        # Limit nested join results
        appBskyFeedLikeViaSubject(first: 20) {
          totalCount  # Total likes
          edges {
            node {
              uri
            }
          }
          pageInfo {
            hasNextPage  # Know if there are more
          }
        }
      }
    }
  }
}
```

### 4. Avoid Deep Nesting

```graphql
# Avoid: Deeply nested joins can be expensive
query {
  appBskyFeedPost {
    edges {
      node {
        replyToResolved {
          ... on AppBskyFeedPost {
            replyToResolved {
              ... on AppBskyFeedPost {
                replyToResolved {
                  # Too deep!
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

## Type Resolution

The `Record` union uses a type resolver that examines the `collection` field:

| Collection | GraphQL Type |
|------------|--------------|
| `app.bsky.feed.post` | `AppBskyFeedPost` |
| `app.bsky.feed.like` | `AppBskyFeedLike` |
| `app.bsky.actor.profile` | `AppBskyActorProfile` |

This allows inline fragments to work correctly:

```graphql
{
  appBskyFeedPost {
    edges {
      node {
        replyToResolved {
          # Runtime type is determined by the collection field
          ... on AppBskyFeedPost { text }
          ... on AppBskyFeedLike { subject }
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
  __type(name: "AppBskyFeedPost") {
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

## Common Patterns

### Thread Navigation

```graphql
# Get a post and its parent
query {
  appBskyFeedPost(where: { uri: { eq: $uri } }) {
    edges {
      node {
        uri
        text
        replyToResolved {
          ... on AppBskyFeedPost {
            uri
            text
          }
        }
      }
    }
  }
}
```

### Engagement Metrics

Use `totalCount` to get efficient engagement counts without fetching all records:

```graphql
# Get counts efficiently
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Get like count
        likes: appBskyFeedLikeViaSubject(first: 0) {
          totalCount
        }
        # Get repost count
        reposts: appBskyFeedRepostViaSubject(first: 0) {
          totalCount
        }
        # Get reply count
        replies: appBskyFeedPostViaReplyTo(first: 0) {
          totalCount
        }
      }
    }
  }
}
```

Or fetch recent engagement with pagination:

```graphql
query {
  appBskyFeedPost {
    edges {
      node {
        uri
        text
        # Get 10 most recent likes
        likes: appBskyFeedLikeViaSubject(
          first: 10
          sortBy: [{field: "createdAt", direction: DESC}]
        ) {
          totalCount  # Total like count
          edges {
            node {
              did  # Who liked it
              createdAt
            }
          }
        }
      }
    }
  }
}
```

### User's Pinned Content

```graphql
query {
  appBskyActorProfile(where: { did: { eq: $did } }) {
    edges {
      node {
        displayName
        pinnedPostResolved {
          ... on AppBskyFeedPost {
            uri
            text
            createdAt
          }
        }
      }
    }
  }
}
```
