# Aggregations

> **Note:** Aggregation queries are public and do not require authentication.

## Basic Aggregation

Group records by a single field and count occurrences:

```graphql
query {
  xyzStatusphereStatusAggregated(groupBy: [{field: status}]) {
    status
    count
  }
}
```

Returns groups with their counts:

```json
{
  "data": {
    "xyzStatusphereStatusAggregated": [
      { "status": "👍", "count": 42 },
      { "status": "👎", "count": 18 }
    ]
  }
}
```

## Field Selection

Each collection has a type-safe `GroupByField` enum for available fields:

```graphql
query {
  appBskyFeedPostAggregated(groupBy: [{field: lang}]) {
    lang
    count
  }
}
```

## Multiple Fields

Group by multiple fields simultaneously:

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [
      {field: author}
      {field: lang}
    ]
  ) {
    author
    lang
    count
  }
}
```

## Date Truncation

Group datetime fields by time intervals using the `interval` parameter:

### Available Intervals

- `HOUR` - Truncate to hour
- `DAY` - Truncate to day
- `WEEK` - Truncate to week
- `MONTH` - Truncate to month

### Group by Day

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: createdAt, interval: DAY}]
  ) {
    createdAt
    count
  }
}
```

### Group by Month

```graphql
query {
  xyzStatusphereStatusAggregated(
    groupBy: [{field: indexedAt, interval: MONTH}]
  ) {
    indexedAt
    count
  }
}
```

> **Note:** Date intervals can only be applied to datetime fields. Applying intervals to other field types will return an error.

## Filtering with WHERE

Filter records before aggregation using the `where` argument:

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: lang}]
    where: {
      likes: { gte: 50 }
    }
  ) {
    lang
    count
  }
}
```

### Complex Filters

Combine multiple conditions with `and` and `or`:

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: author}]
    where: {
      and: [
        { likes: { gte: 100 } }
        { lang: { eq: "en" } }
      ]
    }
  ) {
    author
    count
  }
}
```

## Sorting Results

Sort aggregated results by count using the `orderBy` argument:

### Descending Order (Most Common First)

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: lang}]
    orderBy: {count: DESC}
  ) {
    lang
    count
  }
}
```

### Ascending Order (Least Common First)

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: lang}]
    orderBy: {count: ASC}
  ) {
    lang
    count
  }
}
```

## Limiting Results

Limit the number of groups returned using the `limit` parameter:

```graphql
query {
  appBskyFeedPostAggregated(
    groupBy: [{field: author}]
    orderBy: {count: DESC}
    limit: 10
  ) {
    author
    count
  }
}
```

## Table Columns

Group by database table columns in addition to JSON fields:

### Group by DID

```graphql
query {
  appBskyFeedPostAggregated(groupBy: [{field: did}]) {
    did
    count
  }
}
```

### Group by Indexed Date

```graphql
query {
  xyzStatusphereStatusAggregated(
    groupBy: [{field: indexedAt, interval: DAY}]
  ) {
    indexedAt
    count
  }
}
```

Available table columns: `uri`, `cid`, `did`, `collection`, `indexedAt`

## Array Fields

Array fields can be grouped by in aggregations:

```graphql
query {
  fmTealAlphaFeedPlayAggregated(
    groupBy: [{field: artists}]
  ) {
    artists
    count
  }
}
```

> **Note:** Array fields return JSON objects in the response, not strings.

## Complete Example

Combining all features - filtering, multiple groupBy fields, date truncation, ordering, and limiting:

```graphql
query GetTopLanguagesByDay($minLikes: Int!, $limit: Int!) {
  appBskyFeedPostAggregated(
    groupBy: [
      {field: createdAt, interval: DAY}
      {field: lang}
    ]
    where: {
      likes: { gte: $minLikes }
    }
    orderBy: {count: DESC}
    limit: $limit
  ) {
    createdAt
    lang
    count
  }
}
```

Variables:

```json
{
  "minLikes": 50,
  "limit": 20
}
```

Response:

```json
{
  "data": {
    "appBskyFeedPostAggregated": [
      {
        "createdAt": "2024-01-15",
        "lang": "en",
        "count": 342
      },
      {
        "createdAt": "2024-01-15",
        "lang": "fr",
        "count": 127
      },
      {
        "createdAt": "2024-01-14",
        "lang": "en",
        "count": 298
      }
    ]
  }
}
```

## Query Structure

Aggregated query fields follow this naming pattern:

- `{collection}Aggregated` - Returns aggregated results
- Parameters:
  - `groupBy` (required): Array of fields to group by with optional intervals
  - `where` (optional): Filter conditions
  - `orderBy` (optional): Sort by count (ASC or DESC)
  - `limit` (optional): Maximum number of groups to return (default: 100)
- Returns: Array of objects with group field values and `count`

## Validation

The server validates aggregation queries:

- **Date intervals**: Can only be applied to datetime fields
- **Query complexity**: Maximum 5 groupBy fields allowed per query
- **Field existence**: All groupBy fields must exist in the collection schema
