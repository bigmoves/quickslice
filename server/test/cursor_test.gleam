import cursor
import gleam/option.{None, Some}
import gleeunit/should

/// Test encoding a cursor with no sort fields (just CID)
pub fn encode_cursor_no_sort_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello world\",\"createdAt\":\"2025-01-15T12:00:00Z\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  let result = cursor.generate_cursor_from_record(record, None)

  // Decode the base64 to verify it's just the CID
  let decoded = cursor.decode_base64(result)
  should.be_ok(decoded)
  |> should.equal("bafytest123")
}

/// Test encoding a cursor with single sort field
pub fn encode_cursor_single_field_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello world\",\"createdAt\":\"2025-01-15T12:00:00Z\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  let sort_by = Some([#("indexed_at", "desc")])

  let result = cursor.generate_cursor_from_record(record, sort_by)

  // Decode the base64 to verify format
  let decoded = cursor.decode_base64(result)
  should.be_ok(decoded)
  |> should.equal("2025-01-15 12:00:00|bafytest123")
}

/// Test encoding a cursor with JSON field
pub fn encode_cursor_json_field_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello world\",\"createdAt\":\"2025-01-15T12:00:00Z\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  let sort_by = Some([#("text", "desc")])

  let result = cursor.generate_cursor_from_record(record, sort_by)

  let decoded = cursor.decode_base64(result)
  should.be_ok(decoded)
  |> should.equal("Hello world|bafytest123")
}

/// Test encoding a cursor with nested JSON field
pub fn encode_cursor_nested_json_field_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"author\":{\"name\":\"Alice\"},\"createdAt\":\"2025-01-15T12:00:00Z\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  let sort_by = Some([#("author.name", "asc")])

  let result = cursor.generate_cursor_from_record(record, sort_by)

  let decoded = cursor.decode_base64(result)
  should.be_ok(decoded)
  |> should.equal("Alice|bafytest123")
}

/// Test encoding a cursor with multiple sort fields
pub fn encode_cursor_multi_field_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello\",\"createdAt\":\"2025-01-15T12:00:00Z\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  let sort_by = Some([#("text", "desc"), #("createdAt", "desc")])

  let result = cursor.generate_cursor_from_record(record, sort_by)

  let decoded = cursor.decode_base64(result)
  should.be_ok(decoded)
  |> should.equal("Hello|2025-01-15T12:00:00Z|bafytest123")
}

/// Test decoding a valid cursor
pub fn decode_cursor_valid_test() {
  let sort_by = Some([#("indexed_at", "desc")])

  // Create a cursor: "2025-01-15 12:00:00|bafytest123"
  let cursor_str = cursor.encode_base64("2025-01-15 12:00:00|bafytest123")

  let result = cursor.decode_cursor(cursor_str, sort_by)

  should.be_ok(result)
  |> fn(decoded) {
    decoded.field_values
    |> should.equal(["2025-01-15 12:00:00"])

    decoded.cid
    |> should.equal("bafytest123")
  }
}

/// Test decoding a multi-field cursor
pub fn decode_cursor_multi_field_test() {
  let sort_by = Some([#("text", "desc"), #("createdAt", "desc")])

  let cursor_str =
    cursor.encode_base64("Hello|2025-01-15T12:00:00Z|bafytest123")

  let result = cursor.decode_cursor(cursor_str, sort_by)

  should.be_ok(result)
  |> fn(decoded) {
    decoded.field_values
    |> should.equal(["Hello", "2025-01-15T12:00:00Z"])

    decoded.cid
    |> should.equal("bafytest123")
  }
}

/// Test decoding with mismatched field count fails
pub fn decode_cursor_mismatch_test() {
  let sort_by = Some([#("text", "desc")])

  // Cursor has 2 fields but sort_by only has 1
  let cursor_str =
    cursor.encode_base64("Hello|2025-01-15T12:00:00Z|bafytest123")

  let result = cursor.decode_cursor(cursor_str, sort_by)

  should.be_error(result)
}

/// Test decoding invalid base64 fails
pub fn decode_cursor_invalid_base64_test() {
  let sort_by = Some([#("text", "desc")])

  let result = cursor.decode_cursor("not-valid-base64!!!", sort_by)

  should.be_error(result)
}

/// Test extracting table column values
pub fn extract_field_value_table_column_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{}",
      indexed_at: "2025-01-15 12:00:00",
    )

  cursor.extract_field_value(record, "uri")
  |> should.equal("at://did:plc:test/app.bsky.feed.post/123")

  cursor.extract_field_value(record, "cid")
  |> should.equal("bafytest123")

  cursor.extract_field_value(record, "did")
  |> should.equal("did:plc:test")

  cursor.extract_field_value(record, "collection")
  |> should.equal("app.bsky.feed.post")

  cursor.extract_field_value(record, "indexed_at")
  |> should.equal("2025-01-15 12:00:00")
}

/// Test extracting JSON field values
pub fn extract_field_value_json_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello world\",\"createdAt\":\"2025-01-15T12:00:00Z\",\"likeCount\":42}",
      indexed_at: "2025-01-15 12:00:00",
    )

  cursor.extract_field_value(record, "text")
  |> should.equal("Hello world")

  cursor.extract_field_value(record, "createdAt")
  |> should.equal("2025-01-15T12:00:00Z")

  cursor.extract_field_value(record, "likeCount")
  |> should.equal("42")
}

/// Test extracting nested JSON field values
pub fn extract_field_value_nested_json_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"author\":{\"name\":\"Alice\",\"did\":\"did:plc:alice\"}}",
      indexed_at: "2025-01-15 12:00:00",
    )

  cursor.extract_field_value(record, "author.name")
  |> should.equal("Alice")

  cursor.extract_field_value(record, "author.did")
  |> should.equal("did:plc:alice")
}

/// Test extracting missing JSON field returns NULL
pub fn extract_field_value_missing_test() {
  let record =
    cursor.RecordLike(
      uri: "at://did:plc:test/app.bsky.feed.post/123",
      cid: "bafytest123",
      did: "did:plc:test",
      collection: "app.bsky.feed.post",
      json: "{\"text\":\"Hello\"}",
      indexed_at: "2025-01-15 12:00:00",
    )

  cursor.extract_field_value(record, "nonexistent")
  |> should.equal("NULL")

  cursor.extract_field_value(record, "author.name")
  |> should.equal("NULL")
}

// WHERE Condition Builder Tests

/// Test building WHERE clause for single field DESC
pub fn build_where_single_field_desc_test() {
  let decoded =
    cursor.DecodedCursor(
      field_values: ["2025-01-15 12:00:00"],
      cid: "bafytest123",
    )

  let sort_by = Some([#("indexed_at", "desc")])

  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, False)

  // For DESC: indexed_at < cursor_value OR (indexed_at = cursor_value AND cid < cursor_cid)
  sql
  |> should.equal("((indexed_at < ?) OR (indexed_at = ? AND cid < ?))")

  params
  |> should.equal([
    "2025-01-15 12:00:00",
    "2025-01-15 12:00:00",
    "bafytest123",
  ])
}

/// Test building WHERE clause for single field ASC
pub fn build_where_single_field_asc_test() {
  let decoded =
    cursor.DecodedCursor(
      field_values: ["2025-01-15 12:00:00"],
      cid: "bafytest123",
    )

  let sort_by = Some([#("indexed_at", "asc")])

  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, False)

  // For ASC: indexed_at > cursor_value OR (indexed_at = cursor_value AND cid > cursor_cid)
  sql
  |> should.equal("((indexed_at > ?) OR (indexed_at = ? AND cid > ?))")

  params
  |> should.equal([
    "2025-01-15 12:00:00",
    "2025-01-15 12:00:00",
    "bafytest123",
  ])
}

/// Test building WHERE clause for JSON field
pub fn build_where_json_field_test() {
  let decoded =
    cursor.DecodedCursor(field_values: ["Hello world"], cid: "bafytest123")

  let sort_by = Some([#("text", "desc")])

  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, False)

  // JSON fields use json_extract
  sql
  |> should.equal(
    "((json_extract(json, '$.text') < ?) OR (json_extract(json, '$.text') = ? AND cid < ?))",
  )

  params
  |> should.equal(["Hello world", "Hello world", "bafytest123"])
}

/// Test building WHERE clause for nested JSON field
pub fn build_where_nested_json_field_test() {
  let decoded =
    cursor.DecodedCursor(field_values: ["Alice"], cid: "bafytest123")

  let sort_by = Some([#("author.name", "asc")])

  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, False)

  // Nested JSON fields use $.path.to.field
  sql
  |> should.equal(
    "((json_extract(json, '$.author.name') > ?) OR (json_extract(json, '$.author.name') = ? AND cid > ?))",
  )

  params
  |> should.equal(["Alice", "Alice", "bafytest123"])
}

/// Test building WHERE clause for multiple fields
pub fn build_where_multi_field_test() {
  let decoded =
    cursor.DecodedCursor(
      field_values: ["Hello", "2025-01-15T12:00:00Z"],
      cid: "bafytest123",
    )

  let sort_by = Some([#("text", "desc"), #("createdAt", "desc")])

  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, False)

  // Multi-field: progressive equality checks
  // (text < ?) OR (text = ? AND createdAt < ?) OR (text = ? AND createdAt = ? AND cid < ?)
  sql
  |> should.equal(
    "((json_extract(json, '$.text') < ?) OR (json_extract(json, '$.text') = ? AND json_extract(json, '$.createdAt') < ?) OR (json_extract(json, '$.text') = ? AND json_extract(json, '$.createdAt') = ? AND cid < ?))",
  )

  params
  |> should.equal([
    "Hello",
    "Hello",
    "2025-01-15T12:00:00Z",
    "Hello",
    "2025-01-15T12:00:00Z",
    "bafytest123",
  ])
}

/// Test building WHERE clause for backward pagination (before)
pub fn build_where_backward_test() {
  let decoded =
    cursor.DecodedCursor(
      field_values: ["2025-01-15 12:00:00"],
      cid: "bafytest123",
    )

  let sort_by = Some([#("indexed_at", "desc")])

  // is_before = True reverses the comparison operators
  let #(sql, params) = cursor.build_cursor_where_clause(decoded, sort_by, True)

  // For before with DESC: indexed_at > cursor_value OR (indexed_at = cursor_value AND cid > cursor_cid)
  sql
  |> should.equal("((indexed_at > ?) OR (indexed_at = ? AND cid > ?))")

  params
  |> should.equal([
    "2025-01-15 12:00:00",
    "2025-01-15 12:00:00",
    "bafytest123",
  ])
}
