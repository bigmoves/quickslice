# Lexicon - AT Protocol Schema Validation for Gleam

A Gleam library for validating AT Protocol Lexicon schemas and data records, powered by the Rust [`slices-lexicon`](https://crates.io/crates/slices-lexicon) crate via Native Implemented Functions (NIFs).

## Features

- Validate AT Protocol Lexicon schema documents
- Validate data records against their schemas
- Check NSID (Namespaced Identifier) validity
- High-performance validation using native Rust code
- Type-safe Gleam API

## Prerequisites

- Rust toolchain (install via [rustup](https://rustup.rs/))
- Gleam compiler
- Make (for build automation)

## Building

The library requires building the Rust NIF before use:

```bash
cd lexicon
make build
```

This will:
1. Compile the Rust NIF library
2. Copy it to the `priv/` directory
3. Make it available for Gleam to load

### Platform-Specific Notes

- **macOS**: The `.dylib` file is automatically renamed to `.so` for BEAM compatibility
- **Linux**: Uses `.so` extension directly
- **Windows**: Uses `.dll` extension

## Usage

Add the lexicon library to your `gleam.toml`:

```toml
[dependencies]
lexicon = { path = "../lexicon" }
```

### Example: Validating Lexicon Schemas

```gleam
import lexicon
import gleam/io

pub fn main() {
  let schema_json = "{\"lexicon\": 1, \"id\": \"com.example.post\", ...}"

  case lexicon.validate_schemas([schema_json]) {
    Ok(Nil) -> io.println("Schema is valid!")
    Error(err) -> io.println("Validation failed: " <> lexicon.describe_error(err))
  }
}
```

### Example: Validating Records

```gleam
import lexicon

pub fn validate_post(record_json: String, schema_json: String) {
  case lexicon.validate_record(record_json, schema_json) {
    Ok(Nil) -> {
      // Record is valid, safe to store in database
      store_record(record_json)
    }
    Error(err) -> {
      // Handle validation error
      log_error(lexicon.describe_error(err))
    }
  }
}
```

### Example: Checking NSIDs

```gleam
import lexicon
import gleam/io

pub fn check_collection_name(collection: String) {
  case lexicon.is_valid_nsid(collection) {
    True -> io.println("Valid NSID: " <> collection)
    False -> io.println("Invalid NSID: " <> collection)
  }
}

// Valid NSIDs
check_collection_name("com.atproto.repo.createRecord")  // Valid
check_collection_name("app.bsky.feed.post")             // Valid

// Invalid NSIDs
check_collection_name("invalid nsid")                   // Invalid
check_collection_name("UPPERCASE.NOT.ALLOWED")          // Invalid
```

## API Reference

### `validate_schemas(json_strings: List(String)) -> Result(Nil, ValidationError)`

Validates one or more lexicon schema documents. Returns `Ok(Nil)` if all schemas are valid.

**Parameters:**
- `json_strings`: List of JSON strings representing lexicon schemas

**Returns:**
- `Ok(Nil)`: All schemas are valid
- `Error(ValidationError)`: One or more schemas failed validation

### `validate_record(record_json: String, schema_json: String) -> Result(Nil, ValidationError)`

Validates a data record against its lexicon schema.

**Parameters:**
- `record_json`: JSON string of the record to validate
- `schema_json`: JSON string of the lexicon schema

**Returns:**
- `Ok(Nil)`: Record is valid according to the schema
- `Error(ValidationError)`: Record validation failed

### `is_valid_nsid(nsid: String) -> Bool`

Checks if a string is a valid NSID (Namespaced Identifier).

**Parameters:**
- `nsid`: String to check

**Returns:**
- `True`: String is a valid NSID
- `False`: String is not a valid NSID

### `describe_error(error: ValidationError) -> String`

Converts a `ValidationError` to a human-readable string.

## Testing

Run the test suite:

```bash
make test
```

## Development

### Project Structure

```
lexicon/
├── gleam.toml              # Gleam package configuration
├── Makefile                # Build automation
├── README.md               # This file
├── src/
│   ├── lexicon.gleam       # Main Gleam API
│   └── lexicon_nif.erl     # Erlang NIF loader
├── native/
│   └── lexicon_nif/        # Rust NIF implementation
│       ├── Cargo.toml
│       └── src/
│           └── lib.rs
└── priv/                   # Compiled NIF library (created by build)
```

### Cleaning

To remove build artifacts:

```bash
make clean
```

## Important Notes

### NIF Safety

Native Implemented Functions (NIFs) run in the same OS process as the BEAM VM. If a NIF crashes, it can bring down the entire runtime rather than just an isolated process. This library includes error handling to minimize this risk, but you should be aware of this limitation.

### Performance

Because validation runs in native Rust code, it's significantly faster than a pure Erlang/Gleam implementation, making it suitable for validating large numbers of schemas or records.

## License

This library uses the MIT-licensed `slices-lexicon` Rust crate.

## Resources

- [AT Protocol Specification](https://atproto.com/)
- [Lexicon Schema Language](https://atproto.com/specs/lexicon)
- [slices-lexicon Rust Crate](https://crates.io/crates/slices-lexicon)
- [Rustler Documentation](https://github.com/rusterlium/rustler)
