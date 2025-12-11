import atproto_car
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn parse_empty_returns_error_test() {
  atproto_car.parse_header(<<>>)
  |> should.be_error()
}

pub fn parse_invalid_header_returns_error_test() {
  // Invalid varint followed by garbage
  atproto_car.parse_header(<<0xFF, 0xFF, 0xFF>>)
  |> should.be_error()
}
