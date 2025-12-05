#!/usr/bin/env bash
set -euo pipefail

# Bunny CDN Deploy Script
# Syncs www/priv/ to Bunny Storage Zone with full sync and cache purge

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
UPLOADED=0
DELETED=0
SKIPPED=0

# Parse arguments
DRY_RUN=false
VERBOSE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=true
            shift
            ;;
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Required environment variables
: "${BUNNY_API_KEY:?BUNNY_API_KEY environment variable is required}"
: "${BUNNY_STORAGE_ZONE:?BUNNY_STORAGE_ZONE environment variable is required}"
: "${BUNNY_STORAGE_HOST:?BUNNY_STORAGE_HOST environment variable is required (e.g., storage.bunnycdn.com)}"
: "${BUNNY_PULLZONE_ID:?BUNNY_PULLZONE_ID environment variable is required}"

# Configuration
LOCAL_DIR="www/priv"
STORAGE_URL="https://${BUNNY_STORAGE_HOST}/${BUNNY_STORAGE_ZONE}"

echo "Bunny CDN Deploy"
echo "================"
echo "Storage Zone: ${BUNNY_STORAGE_ZONE}"
echo "Storage Host: ${BUNNY_STORAGE_HOST}"
echo "Local Dir:    ${LOCAL_DIR}"
if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}DRY RUN MODE - No changes will be made${NC}"
fi
echo ""
