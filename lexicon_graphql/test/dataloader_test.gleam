/// Tests for DataLoader batching logic
///
/// Verifies that URIs are correctly grouped and batched to prevent N+1 queries
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import lexicon_graphql/internal/lexicon/collection_meta
import lexicon_graphql/query/dataloader
import lexicon_graphql/types
import swell/value

// Test URI-to-collection extraction
pub fn uri_to_collection_test() {
  // Valid URI
  dataloader.uri_to_collection("at://did:plc:abc123/app.bsky.feed.post/3k7h8")
  |> should.equal(Some("app.bsky.feed.post"))

  // Valid URI with different collection
  dataloader.uri_to_collection(
    "at://did:plc:xyz789/app.bsky.actor.profile/self",
  )
  |> should.equal(Some("app.bsky.actor.profile"))

  // Invalid URI format
  dataloader.uri_to_collection("https://example.com")
  |> should.equal(None)

  // Empty string
  dataloader.uri_to_collection("")
  |> should.equal(None)
}

// Test batch fetching by URI with mock fetcher
pub fn batch_fetch_by_uri_test() {
  // Create a mock fetcher that returns records based on URI collection
  let mock_fetcher = fn(uris: List(String), collection: String, _field) {
    // Simulate fetching records - return one record per URI
    let _records =
      list.map(uris, fn(uri) {
        value.Object([
          #("uri", value.String(uri)),
          #("collection", value.String(collection)),
          #("text", value.String("Test post")),
        ])
      })

    // Group by URI for the result
    let result =
      list.fold(uris, dict.new(), fn(acc, uri) {
        let record =
          value.Object([
            #("uri", value.String(uri)),
            #("collection", value.String(collection)),
            #("text", value.String("Test post for " <> uri)),
          ])
        dict.insert(acc, uri, [record])
      })

    Ok(result)
  }

  // Test with URIs from the same collection
  let uris = [
    "at://did:plc:a/app.bsky.feed.post/1",
    "at://did:plc:b/app.bsky.feed.post/2",
  ]

  case dataloader.batch_fetch_by_uri(uris, mock_fetcher) {
    Ok(results) -> {
      // Should have 2 results
      dict.size(results)
      |> should.equal(2)

      // Each URI should have its record
      dict.has_key(results, "at://did:plc:a/app.bsky.feed.post/1")
      |> should.be_true

      dict.has_key(results, "at://did:plc:b/app.bsky.feed.post/2")
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}

// Test batch fetching with mixed collections
pub fn batch_fetch_by_uri_mixed_collections_test() {
  // Mock fetcher that tracks which collections were queried
  let mock_fetcher = fn(uris: List(String), collection: String, _field) {
    let result =
      list.fold(uris, dict.new(), fn(acc, uri) {
        let record =
          value.Object([
            #("uri", value.String(uri)),
            #("collection", value.String(collection)),
          ])
        dict.insert(acc, uri, [record])
      })
    Ok(result)
  }

  // URIs from different collections
  let uris = [
    "at://did:plc:a/app.bsky.feed.post/1",
    "at://did:plc:b/app.bsky.actor.profile/self",
  ]

  case dataloader.batch_fetch_by_uri(uris, mock_fetcher) {
    Ok(results) -> {
      // Should batch by collection and fetch both
      dict.size(results)
      |> should.equal(2)
    }
    Error(_) -> should.fail()
  }
}

// Test reverse join batching
pub fn batch_fetch_by_reverse_join_test() {
  // Mock fetcher that simulates finding records that reference parent URIs
  let mock_fetcher = fn(parent_uris: List(String), _collection: String, field) {
    case field {
      Some(ref_field) -> {
        // Simulate finding records that reference each parent URI
        let result =
          list.fold(parent_uris, dict.new(), fn(acc, parent_uri) {
            // Create 2 child records per parent
            let child1 =
              value.Object([
                #("uri", value.String("at://did:plc:child1/collection/key1")),
                #(ref_field, value.String(parent_uri)),
              ])
            let child2 =
              value.Object([
                #("uri", value.String("at://did:plc:child2/collection/key2")),
                #(ref_field, value.String(parent_uri)),
              ])
            dict.insert(acc, parent_uri, [child1, child2])
          })
        Ok(result)
      }
      None -> Error("Reference field required for reverse joins")
    }
  }

  let parent_uris = ["at://did:plc:parent/app.bsky.feed.post/1"]

  case
    dataloader.batch_fetch_by_reverse_join(
      parent_uris,
      "app.bsky.feed.like",
      "subject",
      mock_fetcher,
    )
  {
    Ok(results) -> {
      // Should have results for the parent URI
      case dict.get(results, "at://did:plc:parent/app.bsky.feed.post/1") {
        Ok(children) -> {
          // Should have 2 child records
          list.length(children)
          |> should.equal(2)
        }
        Error(_) -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test extracting URIs from records
pub fn extract_uris_from_records_test() {
  // Create a test lexicon for likes with subject field
  let lexicon =
    types.Lexicon(
      id: "app.bsky.feed.like",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "subject",
              types.Property(
                type_: "string",
                required: True,
                format: Some("at-uri"),
                ref: None,
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Create test records with at-uri subject fields
  let records = [
    value.Object([
      #("uri", value.String("at://did:plc:a/app.bsky.feed.like/1")),
      #("subject", value.String("at://did:plc:target/app.bsky.feed.post/1")),
    ]),
    value.Object([
      #("uri", value.String("at://did:plc:b/app.bsky.feed.like/2")),
      #("subject", value.String("at://did:plc:target/app.bsky.feed.post/2")),
    ]),
  ]

  let uris = dataloader.extract_uris_from_records(records, "subject", meta)

  // Should extract 2 URIs
  list.length(uris)
  |> should.equal(2)

  // Should contain the expected URIs
  list.contains(uris, "at://did:plc:target/app.bsky.feed.post/1")
  |> should.be_true

  list.contains(uris, "at://did:plc:target/app.bsky.feed.post/2")
  |> should.be_true
}

// Test extracting URIs from records with strongRef
pub fn extract_uris_from_records_with_strong_ref_test() {
  // Create a test lexicon with strongRef field
  let lexicon =
    types.Lexicon(
      id: "app.bsky.actor.profile",
      defs: types.Defs(
        main: Some(
          types.RecordDef(type_: "record", key: None, properties: [
            #(
              "pinnedPost",
              types.Property(
                type_: "ref",
                required: False,
                format: None,
                ref: Some("com.atproto.repo.strongRef"),
                refs: None,
                items: None,
              ),
            ),
          ]),
        ),
        others: dict.new(),
      ),
    )

  let meta = collection_meta.extract_metadata(lexicon)

  // Create test record with strongRef - using test_helpers to create the nested object
  // For now, we'll skip this test since it requires more complex Dynamic construction
  // TODO: Implement once we have better strongRef test helpers

  // Placeholder assertion
  meta.nsid
  |> should.equal("app.bsky.actor.profile")
}

// Test error handling when fetcher fails
pub fn batch_fetch_error_handling_test() {
  // Mock fetcher that always fails
  let failing_fetcher = fn(_uris, _collection, _field) {
    Error("Database connection failed")
  }

  let uris = ["at://did:plc:a/app.bsky.feed.post/1"]

  case dataloader.batch_fetch_by_uri(uris, failing_fetcher) {
    Ok(_) -> should.fail()
    Error(msg) -> msg |> should.equal("Database connection failed")
  }
}

// Test with empty URI list
pub fn batch_fetch_empty_list_test() {
  let mock_fetcher = fn(_uris, _collection, _field) { Ok(dict.new()) }

  case dataloader.batch_fetch_by_uri([], mock_fetcher) {
    Ok(results) -> {
      dict.size(results)
      |> should.equal(0)
    }
    Error(_) -> should.fail()
  }
}

// Test paginated reverse join batching
pub fn batch_fetch_by_reverse_join_paginated_test() {
  // Mock paginated fetcher that simulates paginated results
  let mock_paginated_fetcher = fn(
    parent_uri: String,
    _collection: String,
    field: option.Option(String),
    params: dataloader.PaginationParams,
  ) {
    case field {
      Some(_ref_field) -> {
        // Create mock edges based on pagination parameters
        let page_size = case params.first {
          Some(n) -> n
          None -> 10
        }

        // Create mock records with cursors
        let edges =
          list.range(1, page_size)
          |> list.map(fn(i) {
            let record =
              value.Object([
                #(
                  "uri",
                  value.String(
                    "at://did:plc:child"
                    <> int.to_string(i)
                    <> "/collection/key",
                  ),
                ),
                #("parentUri", value.String(parent_uri)),
              ])
            let cursor = "cursor_" <> int.to_string(i)
            #(record, cursor)
          })

        Ok(dataloader.PaginatedBatchResult(
          edges: edges,
          has_next_page: True,
          has_previous_page: False,
          total_count: Some(20),
        ))
      }
      None -> Error("Reference field required for paginated reverse joins")
    }
  }

  let parent_uri = "at://did:plc:parent/app.bsky.feed.post/1"
  let params =
    dataloader.PaginationParams(
      first: Some(5),
      after: None,
      last: None,
      before: None,
      sort_by: None,
      where: None,
    )

  case
    dataloader.batch_fetch_by_reverse_join_paginated(
      parent_uri,
      "app.bsky.feed.like",
      "subject",
      params,
      mock_paginated_fetcher,
    )
  {
    Ok(result) -> {
      // Should have 5 edges (as requested by first: 5)
      list.length(result.edges)
      |> should.equal(5)

      // Should have next page
      result.has_next_page
      |> should.be_true

      // Should not have previous page
      result.has_previous_page
      |> should.be_false

      // Should have total count
      result.total_count
      |> should.equal(Some(20))
    }
    Error(_) -> should.fail()
  }
}

// Test paginated DID batching
pub fn batch_fetch_by_did_paginated_test() {
  // Mock paginated fetcher for DID-based queries
  let mock_paginated_fetcher = fn(
    did: String,
    _collection: String,
    _field: option.Option(String),
    params: dataloader.PaginationParams,
  ) {
    let page_size = case params.first {
      Some(n) -> n
      None -> 10
    }

    // Simulate records belonging to the DID
    let edges =
      list.range(1, page_size)
      |> list.map(fn(i) {
        let record =
          value.Object([
            #(
              "uri",
              value.String(
                "at://" <> did <> "/app.bsky.feed.post/" <> int.to_string(i),
              ),
            ),
            #("did", value.String(did)),
            #("text", value.String("Post " <> int.to_string(i))),
          ])
        let cursor = "did_cursor_" <> int.to_string(i)
        #(record, cursor)
      })

    Ok(dataloader.PaginatedBatchResult(
      edges: edges,
      has_next_page: True,
      has_previous_page: False,
      total_count: Some(50),
    ))
  }

  let did = "did:plc:abc123"
  let params =
    dataloader.PaginationParams(
      first: Some(3),
      after: None,
      last: None,
      before: None,
      sort_by: None,
      where: None,
    )

  case
    dataloader.batch_fetch_by_did_paginated(
      did,
      "app.bsky.feed.post",
      params,
      mock_paginated_fetcher,
    )
  {
    Ok(result) -> {
      // Should have 3 edges (as requested by first: 3)
      list.length(result.edges)
      |> should.equal(3)

      // Should have next page
      result.has_next_page
      |> should.be_true

      // Should have total count
      result.total_count
      |> should.equal(Some(50))
    }
    Error(_) -> should.fail()
  }
}

// Test paginated error handling
pub fn batch_fetch_paginated_error_handling_test() {
  // Mock fetcher that always fails
  let failing_fetcher = fn(_key, _collection, _field, _params) {
    Error("Pagination query failed")
  }

  let params =
    dataloader.PaginationParams(
      first: Some(10),
      after: None,
      last: None,
      before: None,
      sort_by: None,
      where: None,
    )

  case
    dataloader.batch_fetch_by_did_paginated(
      "did:plc:test",
      "app.bsky.feed.post",
      params,
      failing_fetcher,
    )
  {
    Ok(_) -> should.fail()
    Error(msg) -> msg |> should.equal("Pagination query failed")
  }
}

// Test viewer state batch fetching
pub fn batch_fetch_viewer_state_test() {
  // Mock fetcher that returns records for specific viewer+subject combinations
  let fetcher = fn(
    viewer_did: String,
    collection: String,
    _reference_field: String,
    parent_keys: List(String),
  ) -> Result(dict.Dict(String, value.Value), String) {
    // Simulate: viewer "did:plc:viewer" liked gallery1 and gallery3
    case viewer_did, collection {
      "did:plc:viewer", "social.grain.favorite" -> {
        let results =
          list.fold(parent_keys, dict.new(), fn(acc, key) {
            case key {
              "at://did:plc:author/social.grain.gallery/gallery1" ->
                dict.insert(
                  acc,
                  key,
                  value.Object([
                    #(
                      "uri",
                      value.String(
                        "at://did:plc:viewer/social.grain.favorite/abc",
                      ),
                    ),
                    #("subject", value.String(key)),
                  ]),
                )
              "at://did:plc:author/social.grain.gallery/gallery3" ->
                dict.insert(
                  acc,
                  key,
                  value.Object([
                    #(
                      "uri",
                      value.String(
                        "at://did:plc:viewer/social.grain.favorite/def",
                      ),
                    ),
                    #("subject", value.String(key)),
                  ]),
                )
              _ -> acc
            }
          })
        Ok(results)
      }
      _, _ -> Ok(dict.new())
    }
  }

  let parent_uris = [
    "at://did:plc:author/social.grain.gallery/gallery1",
    "at://did:plc:author/social.grain.gallery/gallery2",
    "at://did:plc:author/social.grain.gallery/gallery3",
  ]

  let result =
    dataloader.batch_fetch_viewer_state(
      "did:plc:viewer",
      "social.grain.favorite",
      "subject",
      parent_uris,
      fetcher,
    )

  result
  |> should.be_ok

  let records = case result {
    Ok(r) -> r
    Error(_) -> dict.new()
  }

  // gallery1 should have a record
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery1")
  |> should.be_ok

  // gallery2 should NOT have a record (viewer didn't like it)
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery2")
  |> should.be_error

  // gallery3 should have a record
  dict.get(records, "at://did:plc:author/social.grain.gallery/gallery3")
  |> should.be_ok
}

// Test backward pagination parameters
pub fn batch_fetch_backward_pagination_test() {
  // Mock paginated fetcher that handles backward pagination
  let mock_paginated_fetcher = fn(
    _did: String,
    _collection: String,
    _field: option.Option(String),
    params: dataloader.PaginationParams,
  ) {
    // Check if backward pagination is requested
    let is_backward = case params.last {
      Some(_) -> True
      None -> False
    }

    let page_size = case params.last {
      Some(n) -> n
      None -> 5
    }

    let edges =
      list.range(1, page_size)
      |> list.map(fn(i) {
        let record =
          value.Object([
            #(
              "uri",
              value.String("at://did:plc:test/collection/" <> int.to_string(i)),
            ),
          ])
        let cursor = "cursor_" <> int.to_string(i)
        #(record, cursor)
      })

    Ok(dataloader.PaginatedBatchResult(
      edges: edges,
      has_next_page: False,
      has_previous_page: is_backward,
      total_count: Some(10),
    ))
  }

  let params =
    dataloader.PaginationParams(
      first: None,
      after: None,
      last: Some(3),
      before: Some("cursor_10"),
      sort_by: None,
      where: None,
    )

  case
    dataloader.batch_fetch_by_did_paginated(
      "did:plc:test",
      "app.bsky.feed.post",
      params,
      mock_paginated_fetcher,
    )
  {
    Ok(result) -> {
      // Should have 3 edges (as requested by last: 3)
      list.length(result.edges)
      |> should.equal(3)

      // Should not have next page (at the end)
      result.has_next_page
      |> should.be_false

      // Should have previous page (backward pagination)
      result.has_previous_page
      |> should.be_true
    }
    Error(_) -> should.fail()
  }
}
