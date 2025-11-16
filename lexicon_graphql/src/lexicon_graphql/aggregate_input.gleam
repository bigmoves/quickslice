import gleam/option.{type Option}

/// Date interval for truncating datetime fields in GROUP BY
pub type DateInterval {
  Hour
  Day
  Week
  Month
}

/// GraphQL input for specifying a field to group by
pub type GroupByFieldInput {
  GroupByFieldInput(field: String, interval: Option(DateInterval))
}

/// Order direction for aggregate results
pub type AggregationOrderBy {
  CountAsc
  CountDesc
}

/// Parse DateInterval from string
pub fn parse_date_interval(s: String) -> Result(DateInterval, Nil) {
  case s {
    "HOUR" -> Ok(Hour)
    "DAY" -> Ok(Day)
    "WEEK" -> Ok(Week)
    "MONTH" -> Ok(Month)
    _ -> Error(Nil)
  }
}

/// Parse AggregationOrderBy from string
pub fn parse_aggregation_order_by(s: String) -> Result(AggregationOrderBy, Nil) {
  case s {
    "asc" -> Ok(CountAsc)
    "desc" -> Ok(CountDesc)
    _ -> Error(Nil)
  }
}
