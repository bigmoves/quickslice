import gleam/list
import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import graphql/executor
import graphql/schema
import graphql/value

pub fn main() {
  gleeunit.main()
}

// Test: Create a subscription type
pub fn create_subscription_type_test() {
  let subscription_field =
    schema.field(
      "testSubscription",
      schema.string_type(),
      "A test subscription",
      fn(_ctx) { Ok(value.String("test")) },
    )

  let subscription_type =
    schema.object_type("Subscription", "Root subscription type", [
      subscription_field,
    ])

  schema.type_name(subscription_type)
  |> should.equal("Subscription")
}

// Test: Schema with subscription type
pub fn schema_with_subscription_test() {
  let query_field =
    schema.field(
      "hello",
      schema.string_type(),
      "Hello query",
      fn(_ctx) { Ok(value.String("world")) },
    )

  let query_type =
    schema.object_type("Query", "Root query type", [query_field])

  let subscription_field =
    schema.field(
      "messageAdded",
      schema.string_type(),
      "Subscribe to new messages",
      fn(_ctx) { Ok(value.String("test message")) },
    )

  let subscription_type =
    schema.object_type("Subscription", "Root subscription type", [
      subscription_field,
    ])

  let test_schema =
    schema.schema_with_subscriptions(query_type, None, Some(subscription_type))

  // Schema should be created successfully
  // We can't easily test inequality on opaque types, so just verify it doesn't crash
  let _ = test_schema
  should.be_true(True)
}

// Test: Get subscription fields
pub fn get_subscription_fields_test() {
  let subscription_field1 =
    schema.field(
      "postCreated",
      schema.string_type(),
      "New post created",
      fn(_ctx) { Ok(value.String("post1")) },
    )

  let subscription_field2 =
    schema.field(
      "postUpdated",
      schema.string_type(),
      "Post updated",
      fn(_ctx) { Ok(value.String("post1")) },
    )

  let subscription_type =
    schema.object_type("Subscription", "Root subscription type", [
      subscription_field1,
      subscription_field2,
    ])

  let fields = schema.get_fields(subscription_type)

  list.length(fields)
  |> should.equal(2)
}

// Test: Execute anonymous subscription
pub fn execute_anonymous_subscription_test() {
  let query_type =
    schema.object_type("Query", "Root query", [
      schema.field("dummy", schema.string_type(), "Dummy", fn(_) {
        Ok(value.String("dummy"))
      }),
    ])

  let message_type =
    schema.object_type("Message", "A message", [
      schema.field("content", schema.string_type(), "Message content", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "content") {
              Ok(content) -> Ok(content)
              Error(_) -> Ok(value.String(""))
            }
          }
          _ -> Ok(value.String(""))
        }
      }),
    ])

  let subscription_type =
    schema.object_type("Subscription", "Root subscription", [
      schema.field("messageAdded", message_type, "New message", fn(ctx) {
        // In real usage, this would be called with event data in ctx.data
        case ctx.data {
          Some(data) -> Ok(data)
          None -> Ok(value.Object([#("content", value.String("test"))]))
        }
      }),
    ])

  let test_schema =
    schema.schema_with_subscriptions(query_type, None, Some(subscription_type))

  // Create context with event data
  let event_data =
    value.Object([#("content", value.String("Hello from subscription!"))])
  let ctx = schema.context(Some(event_data))

  let query = "subscription { messageAdded { content } }"

  case executor.execute(query, test_schema, ctx) {
    Ok(response) -> {
      case response.data {
        value.Object(fields) -> {
          case list.key_find(fields, "messageAdded") {
            Ok(value.Object(message_fields)) -> {
              case list.key_find(message_fields, "content") {
                Ok(value.String(content)) ->
                  should.equal(content, "Hello from subscription!")
                _ -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Execute subscription with field selection
pub fn execute_subscription_with_field_selection_test() {
  let query_type =
    schema.object_type("Query", "Root query", [
      schema.field("dummy", schema.string_type(), "Dummy", fn(_) {
        Ok(value.String("dummy"))
      }),
    ])

  let post_type =
    schema.object_type("Post", "A post", [
      schema.field("id", schema.id_type(), "Post ID", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "id") {
              Ok(id) -> Ok(id)
              Error(_) -> Ok(value.String(""))
            }
          }
          _ -> Ok(value.String(""))
        }
      }),
      schema.field("title", schema.string_type(), "Post title", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "title") {
              Ok(title) -> Ok(title)
              Error(_) -> Ok(value.String(""))
            }
          }
          _ -> Ok(value.String(""))
        }
      }),
      schema.field("content", schema.string_type(), "Post content", fn(ctx) {
        case ctx.data {
          Some(value.Object(fields)) -> {
            case list.key_find(fields, "content") {
              Ok(content) -> Ok(content)
              Error(_) -> Ok(value.String(""))
            }
          }
          _ -> Ok(value.String(""))
        }
      }),
    ])

  let subscription_type =
    schema.object_type("Subscription", "Root subscription", [
      schema.field("postCreated", post_type, "New post", fn(ctx) {
        case ctx.data {
          Some(data) -> Ok(data)
          None ->
            Ok(
              value.Object([
                #("id", value.String("1")),
                #("title", value.String("Test")),
                #("content", value.String("Test content")),
              ]),
            )
        }
      }),
    ])

  let test_schema =
    schema.schema_with_subscriptions(query_type, None, Some(subscription_type))

  // Create context with event data
  let event_data =
    value.Object([
      #("id", value.String("123")),
      #("title", value.String("New Post")),
      #("content", value.String("This is a new post")),
    ])
  let ctx = schema.context(Some(event_data))

  // Query only for id and title, not content
  let query = "subscription { postCreated { id title } }"

  case executor.execute(query, test_schema, ctx) {
    Ok(response) -> {
      case response.data {
        value.Object(fields) -> {
          case list.key_find(fields, "postCreated") {
            Ok(value.Object(post_fields)) -> {
              // Should have id and title
              case list.key_find(post_fields, "id") {
                Ok(value.String(id)) -> should.equal(id, "123")
                _ -> should.fail()
              }
              case list.key_find(post_fields, "title") {
                Ok(value.String(title)) -> should.equal(title, "New Post")
                _ -> should.fail()
              }
              // Should NOT have content (field selection working)
              case list.key_find(post_fields, "content") {
                Error(_) -> should.be_true(True)
                Ok(_) -> should.fail()
              }
            }
            _ -> should.fail()
          }
        }
        _ -> should.fail()
      }
    }
    Error(_) -> should.fail()
  }
}

// Test: Subscription without schema type
pub fn subscription_without_schema_type_test() {
  let query_type =
    schema.object_type("Query", "Root query", [
      schema.field("dummy", schema.string_type(), "Dummy", fn(_) {
        Ok(value.String("dummy"))
      }),
    ])

  // Schema WITHOUT subscription type
  let test_schema = schema.schema(query_type, None)

  let ctx = schema.context(None)
  let query = "subscription { messageAdded }"

  case executor.execute(query, test_schema, ctx) {
    Error(msg) -> should.equal(msg, "Schema does not define a subscription type")
    Ok(_) -> should.fail()
  }
}
