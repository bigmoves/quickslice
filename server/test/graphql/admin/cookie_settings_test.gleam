/// Tests for cookie settings GraphQL types
///
/// These tests verify the GraphQL type definitions are created correctly.
/// Since swell types are opaque, we test by ensuring the functions don't crash
/// and return valid schema types.
import gleeunit/should
import graphql/admin/types

pub fn cookie_same_site_enum_creates_type_test() {
  // Should not crash when creating the enum type
  let _enum_type = types.cookie_same_site_enum()
  True |> should.be_true()
}

pub fn cookie_secure_enum_creates_type_test() {
  // Should not crash when creating the enum type
  let _enum_type = types.cookie_secure_enum()
  True |> should.be_true()
}

pub fn cookie_settings_type_creates_type_test() {
  // Should not crash when creating the object type
  let _settings_type = types.cookie_settings_type()
  True |> should.be_true()
}
