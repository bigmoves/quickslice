# Aggregations

All record types have a corresponding aggregation query: `{collectionName}Aggregated`. For example, `fm.teal.alpha.feed.play` records can be aggregated with `fmTealAlphaFeedPlayAggregated`.

Aggregation queries are public and do not require authentication.

## Basic Aggregation

Group by a field to count occurrences:

```graphql
query {
  fmTealAlphaFeedPlayAggregated(groupBy: [{ field: artists }]) {
    artists
    count
  }
}
```

```json
{
  "data": {
    "fmTealAlphaFeedPlayAggregated": [
      { "artists": [{ "artistName": "Radiohead" }], "count": 142 },
      { "artists": [{ "artistName": "Boards of Canada" }], "count": 87 }
    ]
  }
}
```

## Filtering & Sorting

Use `where` to filter records, `orderBy` to sort by count, and `limit` to cap results.

Get a user's top 10 artists for 2025:

```graphql
query {
  fmTealAlphaFeedPlayAggregated(
    groupBy: [{ field: artists }]
    where: {
      actorHandle: { eq: "baileytownsend.dev" }
      playedTime: { gte: "2025-01-01T00:00:00Z", lt: "2026-01-01T00:00:00Z" }
    }
    orderBy: { count: DESC }
    limit: 10
  ) {
    artists
    count
  }
}
```

```json
{
  "data": {
    "fmTealAlphaFeedPlayAggregated": [
      { "artists": [{ "artistName": "Radiohead" }], "count": 142 },
      { "artists": [{ "artistName": "Boards of Canada" }], "count": 87 }
    ]
  }
}
```

## Multiple Fields

Group by multiple fields simultaneously. Get top tracks with their artists:

```graphql
query {
  fmTealAlphaFeedPlayAggregated(
    groupBy: [{ field: trackName }, { field: artists }]
    where: {
      actorHandle: { eq: "baileytownsend.dev" }
      playedTime: { gte: "2025-01-01T00:00:00Z", lt: "2026-01-01T00:00:00Z" }
    }
    orderBy: { count: DESC }
    limit: 10
  ) {
    trackName
    artists
    count
  }
}
```

```json
{
  "data": {
    "fmTealAlphaFeedPlayAggregated": [
      { "trackName": "Everything In Its Right Place", "artists": [{ "artistName": "Radiohead" }], "count": 23 },
      { "trackName": "Roygbiv", "artists": [{ "artistName": "Boards of Canada" }], "count": 18 }
    ]
  }
}
```

## Date Truncation

Group datetime fields by time intervals: `HOUR`, `DAY`, `WEEK`, or `MONTH`.

Get plays per month:

```graphql
query {
  fmTealAlphaFeedPlayAggregated(
    groupBy: [{ field: playedTime, interval: MONTH }]
    where: {
      actorHandle: { eq: "baileytownsend.dev" }
      playedTime: { gte: "2025-01-01T00:00:00Z", lt: "2026-01-01T00:00:00Z" }
    }
    orderBy: { count: DESC }
  ) {
    playedTime
    count
  }
}
```

```json
{
  "data": {
    "fmTealAlphaFeedPlayAggregated": [
      { "playedTime": "2025-03-01", "count": 847 },
      { "playedTime": "2025-01-01", "count": 623 },
      { "playedTime": "2025-02-01", "count": 598 }
    ]
  }
}
```

## Reference

### Query Structure

- `{collectionName}Aggregated` - Aggregation query for any record type
- `groupBy` (required) - Array of fields to group by, with optional `interval` for datetime fields
- `where` (optional) - Filter conditions
- `orderBy` (optional) - Sort by `count` (`ASC` or `DESC`)
- `limit` (optional) - Maximum groups to return (default: 100)

### Available Columns

In addition to record fields, you can group by: `uri`, `cid`, `did`, `collection`, `indexedAt`, `actorHandle`

### Validation

- Date intervals can only be applied to datetime fields
- Maximum 5 groupBy fields per query
