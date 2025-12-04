# Following Feed Example

A simple HTML example demonstrating how to view Bluesky profile posts using Quickslice's GraphQL API.

## Features

- OAuth login with PKCE flow
- Client-side routing (`/profile/{handle}`)
- View any user's posts (excluding replies)
- Display post text and embedded images

## Setup

1. Start the Quickslice server on `localhost:8080`
2. Open `index.html` in a browser
3. Enter your OAuth Client ID and Bluesky handle
4. After login, you'll be redirected to your profile page

## Routes

- `/` - Home page (redirects to profile when logged in)
- `/profile/{handle}` - View posts from a specific user

## GraphQL Query Used

```graphql
query GetPosts($handle: String!) {
  appBskyFeedPost(
    sortBy: [{direction: DESC, field: createdAt}]
    where: {
      and: [
        {actorHandle: {eq: $handle}},
        {reply: {isNull: true}}
      ]
    }
  ) {
    edges {
      node {
        text
        createdAt
        appBskyActorProfileByDid {
          displayName
          actorHandle
          avatar { url }
        }
        embed {
          ... on AppBskyEmbedImages {
            images {
              image { url }
            }
          }
        }
      }
    }
  }
}
```

## Notes

- Requires authentication to view profiles
- Posts are sorted by creation date (newest first)
- Replies are filtered out to show only original posts
