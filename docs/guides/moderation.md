# Moderation

Quickslice provides AT Protocol-compatible moderation through labels and reports. Labels mark content; reports let users flag problems.

> **Note:** Admin operations use the `/admin/graphql` endpoint. User operations like `createReport` use the main `/graphql` endpoint.

## Labels

Labels attach metadata to records or accounts. Apply a `!takedown` label to hide content from queries. Apply `porn` or `gore` to trigger client-side warnings.

### Label Definitions

Each instance defines which labels it accepts. Quickslice seeds common defaults:

| Value | Severity | Effect |
|-------|----------|--------|
| `!takedown` | takedown | Hides from all queries |
| `!suspend` | takedown | Hides from all queries |
| `!warn` | alert | Clients show warning |
| `!hide` | alert | Hidden from feeds |
| `porn` | alert | Adult content warning |
| `sexual` | alert | Suggestive content warning |
| `nudity` | alert | Non-sexual nudity warning |
| `gore` | alert | Graphic violence warning |
| `graphic-media` | alert | Disturbing media warning |
| `spam` | inform | Spam indicator |
| `impersonation` | inform | Impersonation indicator |

Create custom labels through the admin API.

### Applying Labels

Admins apply labels via GraphQL:

```graphql
mutation {
  createLabel(
    uri: "at://did:plc:xyz/app.bsky.feed.post/abc123"
    val: "!takedown"
  ) {
    id
    uri
    val
    cts
  }
}
```

The `uri` identifies the target—a record URI or account DID. The `val` must match a defined label.

### Retracting Labels

Labels persist until negated. To remove a label, create a negation:

```graphql
mutation {
  negateLabel(
    uri: "at://did:plc:xyz/app.bsky.feed.post/abc123"
    val: "!takedown"
  ) {
    id
    neg
  }
}
```

The negation cancels the original label. Content reappears in queries.

### Listing Labels (Admin)

Admins can list all applied labels with optional filters:

```graphql
query {
  labels(first: 20) {
    edges {
      node {
        id
        uri
        val
        src
        neg
        cts
      }
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

Filter by subject URI or label value:

```graphql
query {
  labels(uri: "at://did:plc:xyz/app.bsky.feed.post/abc123", first: 10) {
    edges {
      node {
        val
        cts
      }
    }
  }
}
```

### Takedown Behavior

Records with `!takedown` or `!suspend` labels disappear from all queries. Quickslice filters them automatically—clients never see hidden content.

Pagination counts adjust for filtered records. A query for 10 items returns 10 visible items, not 10 minus takedowns.

### Querying Labels

Every record type exposes a `labels` field:

```graphql
query {
  xyzStatusphereStatuses(first: 10) {
    nodes {
      uri
      status
      labels {
        val
        src
        cts
      }
    }
  }
}
```

Only active labels appear. Negated and expired labels are excluded.

### Self-Labels

Authors can label their own content by including a `labels` field in their record with type `com.atproto.label.defs#selfLabels`. Quickslice automatically merges self-labels with moderator labels.

Example record with self-labels:

```json
{
  "text": "Adult content warning",
  "labels": {
    "$type": "com.atproto.label.defs#selfLabels",
    "values": [{"val": "porn"}]
  }
}
```

When querying, both self-labels and moderator labels appear in the `labels` field:

```graphql
query {
  xyzPosts(first: 10) {
    nodes {
      uri
      labels {
        val
        src
      }
    }
  }
}
```

Self-labels have the record author's DID as the `src`. Moderator labels have the moderator's DID.

## Reports

Reports let users flag content for moderator review.

### Creating Reports

Authenticated users submit reports:

```graphql
mutation {
  createReport(
    subjectUri: "at://did:plc:xyz/app.bsky.feed.post/abc123"
    reasonType: SPAM
    reason: "Promoting scam links"
  ) {
    id
    status
    createdAt
  }
}
```

Valid reason types: `SPAM`, `VIOLATION`, `MISLEADING`, `SEXUAL`, `RUDE`, `OTHER`.

Each user can report a URI once. Duplicate reports return the existing report.

### Reviewing Reports

Admins list pending reports:

```graphql
query {
  reports(status: PENDING, first: 20) {
    edges {
      node {
        id
        subjectUri
        reasonType
        reason
        reporterDid
        createdAt
      }
    }
  }
}
```

### Resolving Reports

Resolve a report by applying a label or dismissing it:

```graphql
mutation {
  resolveReport(id: 42, action: APPLY_LABEL, labelVal: "spam") {
    id
    status
    resolvedBy
    resolvedAt
  }
}
```

Actions:
- `APPLY_LABEL`: Creates a label on the reported content, marks report resolved
- `DISMISS`: Marks report dismissed without action

## Admin Access

Label and report management requires admin privileges. Configure admins by DID in your instance settings.

Non-admins can:
- View labels on records (via the `labels` field)
- Submit reports
- Configure their own label preferences

Non-admins cannot:
- Create or negate labels
- View or resolve reports

## Label Preferences

Users can configure how labeled content appears to them. This is exposed through the public `/graphql` endpoint, not the admin endpoint.

### Visibility Settings

| Setting | Behavior |
|---------|----------|
| `IGNORE` | Show content normally, no indicator |
| `SHOW` | Explicitly show (for adult content) |
| `WARN` | Blur with "Show anyway" option |
| `HIDE` | Do not display content |

### Querying Preferences

Authenticated users fetch their preferences:

```graphql
query {
  viewerLabelPreferences {
    val
    description
    severity
    visibility
    defaultVisibility
  }
}
```

This returns all non-system labels with their current visibility settings. If the user has not set a preference, `visibility` equals `defaultVisibility`.

### Setting Preferences

Update a preference:

```graphql
mutation {
  setLabelPreference(val: "spam", visibility: HIDE) {
    val
    visibility
  }
}
```

Reset to default by setting visibility to match `defaultVisibility`.

### System Labels

System labels (starting with `!`) cannot be configured. The server enforces them:

- `!takedown` and `!suspend` always hide content
- `!warn` and `!hide` always apply their effects

Attempting to set a preference for a system label returns an error.

### Client-Side Filtering

The server filters takedown labels automatically, but clients must apply user preferences for other labels. Here's the pattern:

```typescript
// 1. Fetch preferences once and cache
const prefs = await client.query(`{
  viewerLabelPreferences { val visibility }
}`)
const prefMap = new Map(prefs.map(p => [p.val, p.visibility]))

// 2. Check visibility for each record
function getVisibility(record) {
  for (const label of record.labels ?? []) {
    const vis = prefMap.get(label.val) ?? 'WARN'
    if (vis === 'HIDE') return { show: false }
    if (vis === 'WARN') return { show: true, blur: true }
  }
  return { show: true, blur: false }
}

// 3. Apply in your UI
function RecordCard({ record }) {
  const { show, blur } = getVisibility(record)

  if (!show) return null

  if (blur) {
    return (
      <BlurOverlay onReveal={() => setRevealed(true)}>
        <Content record={record} />
      </BlurOverlay>
    )
  }

  return <Content record={record} />
}
```

Key points:

- Cache preferences at session start or when user updates them
- Default unknown labels to `WARN` for safety
- Multiple labels on one record: apply the most restrictive
- `IGNORE` and `SHOW` both display normally; `SHOW` is for explicit opt-in to adult content
