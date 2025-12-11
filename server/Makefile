# Quickslice Server Makefile
# Database operations and development helpers

.PHONY: help build test clean db-create db-migrate db-rollback db-status db-reset db-setup-sqlite db-setup-postgres

# Default target
help:
	@echo "Quickslice Server Commands"
	@echo ""
	@echo "Build:"
	@echo "  make build          - Build the Gleam project"
	@echo "  make test           - Run tests"
	@echo "  make clean          - Clean build artifacts"
	@echo ""
	@echo "Database (requires DATABASE_URL env var):"
	@echo "  make db-create      - Create the database"
	@echo "  make db-migrate     - Run pending migrations"
	@echo "  make db-rollback    - Rollback the last migration"
	@echo "  make db-status      - Show migration status"
	@echo "  make db-reset       - Drop and recreate database (DESTRUCTIVE)"
	@echo ""
	@echo "Database Setup Helpers:"
	@echo "  make db-setup-sqlite   - Set up SQLite database"
	@echo "  make db-setup-postgres - Set up PostgreSQL database"
	@echo ""
	@echo "Environment Variables:"
	@echo "  DATABASE_URL - Connection string (required for db-* commands)"
	@echo "    SQLite:     sqlite:data/quickslice.db"
	@echo "    PostgreSQL: postgres://user:pass@localhost:5432/quickslice"

# Build targets
build:
	gleam build

test:
	gleam test

clean:
	rm -rf build/

# Database targets (use dbmate)
# These require DATABASE_URL to be set

db-create:
	@if [ -z "$$DATABASE_URL" ]; then echo "ERROR: DATABASE_URL not set"; exit 1; fi
	@echo "Creating database..."
	@if echo "$$DATABASE_URL" | grep -q "^postgres"; then \
		dbmate --migrations-dir ./db/migrations_postgres create; \
	else \
		dbmate create; \
	fi

db-migrate:
	@if [ -z "$$DATABASE_URL" ]; then echo "ERROR: DATABASE_URL not set"; exit 1; fi
	@echo "Running migrations..."
	@if echo "$$DATABASE_URL" | grep -q "^postgres"; then \
		dbmate --migrations-dir ./db/migrations_postgres up; \
	else \
		dbmate up; \
	fi

db-rollback:
	@if [ -z "$$DATABASE_URL" ]; then echo "ERROR: DATABASE_URL not set"; exit 1; fi
	@echo "Rolling back last migration..."
	@if echo "$$DATABASE_URL" | grep -q "^postgres"; then \
		dbmate --migrations-dir ./db/migrations_postgres down; \
	else \
		dbmate down; \
	fi

db-status:
	@if [ -z "$$DATABASE_URL" ]; then echo "ERROR: DATABASE_URL not set"; exit 1; fi
	@echo "Migration status:"
	@if echo "$$DATABASE_URL" | grep -q "^postgres"; then \
		dbmate --migrations-dir ./db/migrations_postgres status; \
	else \
		dbmate status; \
	fi

db-reset:
	@if [ -z "$$DATABASE_URL" ]; then echo "ERROR: DATABASE_URL not set"; exit 1; fi
	@echo "WARNING: This will destroy all data!"
	@read -p "Are you sure? [y/N] " confirm && [ "$$confirm" = "y" ] || exit 1
	@echo "Dropping and recreating database..."
	@if echo "$$DATABASE_URL" | grep -q "^postgres"; then \
		dbmate --migrations-dir ./db/migrations_postgres drop && \
		dbmate --migrations-dir ./db/migrations_postgres up; \
	else \
		dbmate drop && dbmate up; \
	fi

# Convenience targets for local development
db-setup-sqlite:
	@mkdir -p data
	DATABASE_URL=sqlite:data/quickslice.db $(MAKE) db-create db-migrate
	@echo "SQLite database ready at data/quickslice.db"
	@echo "Set DATABASE_URL=sqlite:data/quickslice.db to use it"

db-setup-postgres:
	@echo "Setting up PostgreSQL database..."
	@echo "Ensure PostgreSQL is running and DATABASE_URL is set"
	@if [ -z "$$DATABASE_URL" ]; then \
		echo "Example: DATABASE_URL=postgres://localhost:5432/quickslice make db-setup-postgres"; \
		exit 1; \
	fi
	$(MAKE) db-create db-migrate
	@echo "PostgreSQL database ready"
