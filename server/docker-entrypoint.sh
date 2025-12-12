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
