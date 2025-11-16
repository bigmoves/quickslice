import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}

/// Result of an aggregation query - represents one group
pub type AggregateResult {
  AggregateResult(group_values: Dict(String, Dynamic), count: Int)
}
