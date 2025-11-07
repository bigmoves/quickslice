#!/bin/bash
# Apply patches to dependencies after gleam build
# Run this after: gleam build

set -e

cd "$(dirname "$0")"

echo "Applying patches..."

# Check if the mist package exists
if [ ! -f "server/build/packages/mist/src/mist/internal/http.gleam" ]; then
    echo "Error: mist package not found. Run 'cd server && gleam build' first."
    exit 1
fi

# Apply the WebSocket protocol patch
if patch -p1 --dry-run --silent < patches/mist-websocket-protocol.patch 2>/dev/null; then
    patch -p1 < patches/mist-websocket-protocol.patch
    echo "✓ Applied mist WebSocket protocol patch"
else
    echo "⚠ Patch already applied or failed to apply"
fi

echo "Done!"
