# Subscriptions

> **Note:** Subscriptions require a WebSocket connection. Connect to `/graphql` using the [`graphql-ws`](https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md) protocol.

## Basic Subscription

Subscribe to new records:

```graphql
subscription {
  xyzStatusphereStatusCreated {
    uri
    status
    createdAt
  }
}
```

## Field Selection

Request only the fields you need:

```graphql
subscription {
  xyzStatusphereStatusCreated {
    status
  }
}
```

Response:

```json
{
  "data": {
    "xyzStatusphereStatusCreated": {
      "status": "🚀"
    }
  }
}
```

## Named Subscription

```graphql
subscription OnNewStatus {
  xyzStatusphereStatusCreated {
    uri
    status
    actorHandle
  }
}
```

## Subscription Types

Each collection has three subscription fields:

- `{collection}Created` - Fires when a new record is created
- `{collection}Updated` - Fires when a record is updated
- `{collection}Deleted` - Fires when a record is deleted

### Examples

```graphql
# New status created
subscription {
  xyzStatusphereStatusCreated {
    uri
    status
  }
}

# Status updated
subscription {
  xyzStatusphereStatusUpdated {
    uri
    status
  }
}

# Status deleted
subscription {
  xyzStatusphereStatusDeleted {
    uri
  }
}
```

## With Joins

Subscriptions support joins like queries:

```graphql
subscription {
  xyzStatusphereStatusCreated {
    uri
    status
    appBskyActorProfileByDid {
      displayName
      avatar {
        url
      }
    }
  }
}
```

Response:

```json
{
  "data": {
    "xyzStatusphereStatusCreated": {
      "uri": "at://did:plc:abc123/xyz.statusphere.status/3m4vk4wi",
      "status": "🎉 Just shipped!",
      "appBskyActorProfileByDid": {
        "displayName": "Alice",
        "avatar": {
          "url": "https://cdn.bsky.app/img/avatar/plain/did:plc:abc123/bafyrei..."
        }
      }
    }
  }
}
```

## WebSocket Protocol

### 1. Connect

```
ws://localhost:8080/graphql
```

### 2. Initialize

```json
{
  "type": "connection_init"
}
```

### 3. Subscribe

```json
{
  "id": "1",
  "type": "subscribe",
  "payload": {
    "query": "subscription { xyzStatusphereStatusCreated { uri status } }"
  }
}
```

### 4. Receive Events

```json
{
  "id": "1",
  "type": "next",
  "payload": {
    "data": {
      "xyzStatusphereStatusCreated": {
        "uri": "at://...",
        "status": "Hello!"
      }
    }
  }
}
```

### 5. Unsubscribe

```json
{
  "id": "1",
  "type": "complete"
}
```
