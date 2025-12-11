# Common Patterns

Recipes for common use cases when building with Quickslice.

## Profile Lookups

Join author profiles to any record type to display names and avatars.

```graphql
query PostsWithAuthors {
  myAppPost(first: 20, sortBy: [{ field: createdAt, direction: DESC }]) {
    edges {
      node {
        content
        createdAt
        appBskyActorProfileByDid {
          displayName
          avatar { url(preset: "avatar") }
        }
      }
    }
  }
}
```

The `appBskyActorProfileByDid` field works on all records because every record has a `did` field.

## User Timelines

Fetch all records by a specific user using DID joins from their profile.

```graphql
query UserTimeline($handle: String!) {
  appBskyActorProfile(first: 1, where: { actorHandle: { eq: $handle } }) {
    edges {
      node {
        displayName
        myAppPostByDid(first: 20, sortBy: [{ field: createdAt, direction: DESC }]) {
          edges {
            node {
              content
              createdAt
            }
          }
        }
      }
    }
  }
}
```

## Engagement Counts

Use reverse joins with `totalCount` to show likes, comments, or other engagement metrics.

```graphql
query PhotosWithEngagement {
  socialGrainPhoto(first: 10) {
    edges {
      node {
        uri
        alt
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

## Feed with Nested Data

Build a rich feed by combining multiple join types.

```graphql
query Feed {
  myAppPost(first: 20, sortBy: [{ field: createdAt, direction: DESC }]) {
    edges {
      node {
        uri
        content
        createdAt

        # Author profile
        appBskyActorProfileByDid {
          displayName
          avatar { url(preset: "avatar") }
        }

        # Engagement counts
        myAppLikeViaSubject {
          totalCount
        }
        myAppCommentViaSubject {
          totalCount
        }

        # Preview of recent comments
        myAppCommentViaSubject(first: 3, sortBy: [{ field: createdAt, direction: DESC }]) {
          edges {
            node {
              text
              appBskyActorProfileByDid {
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

## Paginated Lists

Implement infinite scroll or "load more" with cursor-based pagination.

```graphql
query PaginatedStatuses($cursor: String) {
  xyzStatusphereStatus(
    first: 20
    after: $cursor
    sortBy: [{ field: createdAt, direction: DESC }]
  ) {
    edges {
      node {
        status
        createdAt
      }
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

First request: `{ "cursor": null }`

Subsequent requests: `{ "cursor": "endCursor_from_previous_response" }`

Continue until `hasNextPage` is `false`.

## Filtered Search

Combine multiple filters for search functionality.

```graphql
query SearchProfiles($query: String!) {
  appBskyActorProfile(
    first: 20
    where: { displayName: { contains: $query } }
    sortBy: [{ field: displayName, direction: ASC }]
  ) {
    edges {
      node {
        actorHandle
        displayName
        description
        avatar { url(preset: "avatar") }
      }
    }
  }
}
```

## Date Range Queries

Filter records within a time period.

```graphql
query RecentActivity($since: DateTime!, $until: DateTime!) {
  myAppPost(
    where: {
      createdAt: { gte: $since, lt: $until }
    }
    sortBy: [{ field: createdAt, direction: DESC }]
  ) {
    edges {
      node {
        content
        createdAt
      }
    }
    totalCount
  }
}
```

Variables:
```json
{
  "since": "2025-01-01T00:00:00Z",
  "until": "2025-02-01T00:00:00Z"
}
```

## Current User's Data

Use the `viewer` query to get the authenticated user's records.

```graphql
query MyProfile {
  viewer {
    did
    handle
    appBskyActorProfileByDid {
      displayName
      description
      avatar { url(preset: "avatar") }
    }
    myAppPostByDid(first: 10, sortBy: [{ field: createdAt, direction: DESC }]) {
      totalCount
      edges {
        node {
          content
          createdAt
        }
      }
    }
  }
}
```

## Real-Time Updates

Subscribe to new records and update your UI live.

```graphql
subscription NewStatuses {
  xyzStatusphereStatusCreated {
    uri
    status
    createdAt
    appBskyActorProfileByDid {
      displayName
      avatar { url(preset: "avatar") }
    }
  }
}
```

Combine with an initial query to show existing data, then append new records as they arrive via subscription.

## Aggregations

Get statistics like top items or activity over time.

```graphql
query TopArtists($user: String!) {
  fmTealAlphaFeedPlayAggregated(
    groupBy: [{ field: artists }]
    where: { actorHandle: { eq: $user } }
    orderBy: { count: DESC }
    limit: 10
  ) {
    artists
    count
  }
}
```

```graphql
query MonthlyActivity($user: String!) {
  myAppPostAggregated(
    groupBy: [{ field: createdAt, interval: MONTH }]
    where: { actorHandle: { eq: $user } }
    orderBy: { count: DESC }
  ) {
    createdAt
    count
  }
}
```
