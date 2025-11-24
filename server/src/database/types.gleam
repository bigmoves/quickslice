// Shared database type definitions

import gleam/option.{type Option}

/// A record stored in the database
pub type Record {
  Record(
    uri: String,
    cid: String,
    did: String,
    collection: String,
    json: String,
    indexed_at: String,
  )
}

/// An actor (user) stored in the database
pub type Actor {
  Actor(did: String, handle: String, indexed_at: String)
}

/// A lexicon schema definition
pub type Lexicon {
  Lexicon(id: String, json: String, created_at: String)
}

/// Collection statistics
pub type CollectionStat {
  CollectionStat(collection: String, count: Int)
}

/// Result of inserting a record
pub type InsertResult {
  /// Record was newly inserted or updated
  Inserted
  /// Record was skipped (duplicate CID or unchanged)
  Skipped
}

/// Date interval for date truncation in aggregations
pub type DateInterval {
  Hour
  Day
  Week
  Month
}

/// A field to group by with optional date truncation
pub type GroupByField {
  SimpleField(field: String)
  TruncatedField(field: String, interval: DateInterval)
}

/// A jetstream activity log entry
pub type ActivityEntry {
  ActivityEntry(
    id: Int,
    timestamp: String,
    operation: String,
    collection: String,
    did: String,
    status: String,
    error_message: Option(String),
    event_json: String,
  )
}

/// Activity bucket for aggregated data
pub type ActivityBucket {
  ActivityBucket(
    timestamp: String,
    create_count: Int,
    update_count: Int,
    delete_count: Int,
  )
}
