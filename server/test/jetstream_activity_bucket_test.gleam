import database/repositories/jetstream_activity
import database/schema/tables
import gleam/list
import gleeunit
import gleeunit/should
import sqlight

pub fn main() {
  gleeunit.main()
}

fn setup_test_db() -> sqlight.Connection {
  let assert Ok(conn) = sqlight.open(":memory:")
  let assert Ok(_) = tables.create_jetstream_activity_table(conn)
  conn
}

pub fn bucket_1hr_returns_exactly_12_buckets_test() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_3hr_returns_exactly_12_buckets_test() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_3hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_6hr_returns_exactly_12_buckets_test() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_6hr(conn)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_1day_returns_exactly_24_buckets_test() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1day(conn)

  list.length(buckets)
  |> should.equal(24)
}

pub fn bucket_7day_returns_exactly_7_buckets_test() {
  let conn = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_7day(conn)

  list.length(buckets)
  |> should.equal(7)
}
