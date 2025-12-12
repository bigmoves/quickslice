# Docker Auto-Migrations Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Automatically run dbmate migrations when the Docker container starts, before the Gleam server begins accepting connections.

**Architecture:** Create a wrapper entrypoint script that runs `dbmate --wait up` (with appropriate migrations directory based on DATABASE_URL), then executes the Gleam-generated `entrypoint.sh`. Install dbmate binary in the Docker image and copy migration files.

**Tech Stack:** dbmate, shell script, Docker

---

## Task 1: Create Docker Entrypoint Wrapper Script

**Files:**
- Create: `server/docker-entrypoint.sh`

**Step 1: Create the entrypoint script**

```bash
#!/bin/sh
set -e

if [ -n "$DATABASE_URL" ]; then
    echo "Running database migrations..."
    if echo "$DATABASE_URL" | grep -q "^postgres"; then
        dbmate --wait --migrations-dir /app/db/migrations_postgres up
    else
        dbmate --wait up
    fi
    echo "Migrations complete."
fi

exec /app/entrypoint.sh "$@"
```

**Step 2: Make it executable locally**

Run: `chmod +x server/docker-entrypoint.sh`

**Step 3: Commit**

```bash
git add server/docker-entrypoint.sh
git commit -m "feat: add docker entrypoint script for auto-migrations"
```

---

## Task 2: Update Dockerfile to Install dbmate and Copy Migration Files

**Files:**
- Modify: `Dockerfile`

**Step 1: Update the runtime stage to install dbmate**

In the runtime stage, change the `apk add` line and add dbmate download:

**Before:**
```dockerfile
# Install runtime dependencies (SQLite and PostgreSQL client libraries)
RUN apk add --no-cache sqlite-libs sqlite libpq
```

**After:**
```dockerfile
# Install runtime dependencies and dbmate for migrations
RUN apk add --no-cache sqlite-libs sqlite libpq curl \
    && curl -fsSL -o /usr/local/bin/dbmate https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-amd64 \
    && chmod +x /usr/local/bin/dbmate
```

**Step 2: Copy migration files and entrypoint wrapper**

Add these lines after the existing `COPY --from=builder` line:

```dockerfile
# Copy database migrations and config
COPY --from=builder /build/server/db /app/db
COPY --from=builder /build/server/.dbmate.yml /app/.dbmate.yml
COPY --from=builder /build/server/docker-entrypoint.sh /app/docker-entrypoint.sh
```

**Step 3: Update CMD to use wrapper entrypoint**

**Before:**
```dockerfile
CMD ["./entrypoint.sh", "run"]
```

**After:**
```dockerfile
CMD ["/app/docker-entrypoint.sh", "run"]
```

**Step 4: Commit**

```bash
git add Dockerfile
git commit -m "feat: add dbmate and auto-migrations to Docker build"
```

---

## Task 3: Test the Docker Build

**Step 1: Build the Docker image**

Run: `docker build -t quickslice-test .`
Expected: Build completes successfully

**Step 2: Verify dbmate is installed**

Run: `docker run --rm quickslice-test dbmate --version`
Expected: Outputs dbmate version (e.g., `2.28.0`)

**Step 3: Verify migration files are present**

Run: `docker run --rm quickslice-test ls -la /app/db/migrations/`
Expected: Shows `20241210000001_initial_schema.sql`

Run: `docker run --rm quickslice-test ls -la /app/db/migrations_postgres/`
Expected: Shows `20241210000001_initial_schema.sql`

**Step 4: Verify entrypoint script is present and executable**

Run: `docker run --rm quickslice-test ls -la /app/docker-entrypoint.sh`
Expected: Shows `-rwxr-xr-x` permissions

---

## Task 4: Update Documentation

**Files:**
- Modify: `server/README.md`

**Step 1: Add section about Docker auto-migrations**

Add to the README under an appropriate section:

```markdown
## Docker Deployment

When running in Docker, migrations are automatically applied on container startup:

1. The container waits for the database to be ready (`dbmate --wait`)
2. Migrations are applied based on DATABASE_URL:
   - PostgreSQL URLs (`postgres://...`) use `db/migrations_postgres/`
   - SQLite URLs use `db/migrations/`
3. The Gleam server starts after migrations complete

To skip auto-migrations (e.g., if using an external migration tool), unset DATABASE_URL and manage the database separately.
```

**Step 2: Commit**

```bash
git add server/README.md
git commit -m "docs: add Docker auto-migrations documentation"
```

---

## Summary

| Task | Description |
|------|-------------|
| 1 | Create `docker-entrypoint.sh` wrapper script |
| 2 | Update `Dockerfile` to install dbmate and copy files |
| 3 | Test the Docker build |
| 4 | Update documentation |

**Total: 4 tasks**
