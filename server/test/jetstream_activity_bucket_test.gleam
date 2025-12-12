import database/executor.{type Executor}
import database/repositories/jetstream_activity
import gleam/list
import gleeunit
import gleeunit/should
import test_helpers

pub fn main() {
  gleeunit.main()
}

fn setup_test_db() -> Executor {
  let assert Ok(exec) = test_helpers.create_test_db()
  let assert Ok(_) = test_helpers.create_jetstream_activity_table(exec)
  exec
}

pub fn bucket_1hr_returns_exactly_12_buckets_test() {
  let exec = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1hr(exec)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_3hr_returns_exactly_12_buckets_test() {
  let exec = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_3hr(exec)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_6hr_returns_exactly_12_buckets_test() {
  let exec = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_6hr(exec)

  list.length(buckets)
  |> should.equal(12)
}

pub fn bucket_1day_returns_exactly_24_buckets_test() {
  let exec = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_1day(exec)

  list.length(buckets)
  |> should.equal(24)
}

pub fn bucket_7day_returns_exactly_7_buckets_test() {
  let exec = setup_test_db()

  let assert Ok(buckets) = jetstream_activity.get_activity_7day(exec)

  list.length(buckets)
  |> should.equal(7)
}
