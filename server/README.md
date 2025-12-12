# Quickslice Server

ATProto/Bluesky data indexer and API server.

## Features

- Multi-database support (SQLite and PostgreSQL)
- OAuth 2.0 authentication with ATProto
- GraphQL API for querying indexed data
- MCP (Model Context Protocol) support
- Jetstream integration for real-time data sync

## Quick Start

### 1. Set up environment

```sh
cp .env.example .env
# Edit .env with your configuration
```

### 2. Set up the database

**SQLite (default, simplest):**
```sh
make db-setup-sqlite
```

**PostgreSQL:**
```sh
# Ensure PostgreSQL is running
export DATABASE_URL=postgres://localhost:5432/quickslice
make db-setup-postgres
```

### 3. Run the server

```sh
gleam run
```

## Database Configuration

The server supports both SQLite and PostgreSQL via the `DATABASE_URL` environment variable:

| Database   | URL Format                                      | Example                                    |
|------------|------------------------------------------------|---------------------------------------------|
| SQLite     | `sqlite:path/to/file.db`                       | `sqlite:data/quickslice.db`                |
| SQLite     | `sqlite::memory:`                              | In-memory database (testing)               |
| PostgreSQL | `postgres://user:pass@host:port/dbname`        | `postgres://localhost:5432/quickslice`     |

### Database Commands (via Makefile)

```sh
make db-create      # Create the database
make db-migrate     # Run pending migrations
make db-rollback    # Rollback the last migration
make db-status      # Show migration status
make db-reset       # Drop and recreate (DESTRUCTIVE)
```

All commands require `DATABASE_URL` to be set and automatically use the correct migrations directory for the database type.

## Development

```sh
gleam build  # Build the project
gleam test   # Run the tests
gleam run    # Run the server
```

### Running PostgreSQL Integration Tests

To run PostgreSQL integration tests, set the `POSTGRES_TEST_URL` environment variable:

```sh
export POSTGRES_TEST_URL=postgres://localhost:5432/quickslice_test
gleam test
```

If `POSTGRES_TEST_URL` is not set, PostgreSQL tests are automatically skipped.

## Architecture

### Database Layer

The database layer uses an Executor abstraction that provides a unified interface for both SQLite and PostgreSQL:

```
database/
├── executor.gleam        # Unified Executor type and operations
├── connection.gleam      # Connection factory (parses DATABASE_URL)
├── sqlite/
│   ├── executor.gleam    # SQLite-specific implementation
│   └── connection.gleam  # SQLite connection handling
├── postgres/
│   ├── executor.gleam    # PostgreSQL-specific implementation
│   └── connection.gleam  # PostgreSQL connection handling
├── queries/
│   ├── where_clause.gleam   # Dialect-aware WHERE clause builder
│   └── pagination.gleam     # Pagination utilities
└── repositories/         # Data access layer
```

### Migrations

Database migrations are managed with [dbmate](https://github.com/amacneil/dbmate):

- SQLite migrations: `db/migrations/`
- PostgreSQL migrations: `db/migrations_postgres/`

## License

See LICENSE file.
