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

# Load .env file if it exists
if [ -f .env ]; then
    set -a
    source .env
    set +a
fi

# Required environment variables
: "${BUNNY_API_KEY:?BUNNY_API_KEY environment variable is required (Account API key for cache purge)}"
: "${BUNNY_STORAGE_PASSWORD:?BUNNY_STORAGE_PASSWORD environment variable is required (Storage Zone password)}"
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

# Get content-type based on file extension
get_content_type() {
    local file="$1"
    case "${file##*.}" in
        html) echo "text/html" ;;
        css)  echo "text/css" ;;
        js)   echo "application/javascript" ;;
        json) echo "application/json" ;;
        png)  echo "image/png" ;;
        jpg|jpeg) echo "image/jpeg" ;;
        gif)  echo "image/gif" ;;
        svg)  echo "image/svg+xml" ;;
        ico)  echo "image/x-icon" ;;
        woff) echo "font/woff" ;;
        woff2) echo "font/woff2" ;;
        *)    echo "application/octet-stream" ;;
    esac
}

# Upload a single file
upload_file() {
    local local_path="$1"
    local remote_path="$2"
    local content_type
    content_type=$(get_content_type "$local_path")

    if [ "$VERBOSE" = true ]; then
        echo "  Uploading: ${remote_path} (${content_type})"
    fi

    if [ "$DRY_RUN" = true ]; then
        ((UPLOADED++))
        return 0
    fi

    local response
    local http_code

    response=$(curl -s -w "\n%{http_code}" -X PUT \
        "${STORAGE_URL}/${remote_path}" \
        -H "AccessKey: ${BUNNY_STORAGE_PASSWORD}" \
        -H "Content-Type: ${content_type}" \
        --data-binary "@${local_path}")

    http_code=$(echo "$response" | tail -n1)

    if [[ "$http_code" =~ ^2 ]]; then
        ((UPLOADED++))
        return 0
    else
        echo -e "${RED}Failed to upload ${remote_path}: HTTP ${http_code}${NC}"
        echo "$response" | head -n -1
        return 1
    fi
}

# List all files in remote storage (recursively)
list_remote_files() {
    local path="${1:-}"
    local url="${STORAGE_URL}/${path}"

    local response
    response=$(curl -s -X GET "$url" \
        -H "AccessKey: ${BUNNY_STORAGE_PASSWORD}" \
        -H "Accept: application/json")

    # Parse JSON response - each item has ObjectName and IsDirectory
    echo "$response" | jq -r '.[] |
        if .IsDirectory then
            .ObjectName + "/"
        else
            .ObjectName
        end' 2>/dev/null | while read -r item; do
        if [[ "$item" == */ ]]; then
            # It's a directory, recurse
            local subdir="${item%/}"
            if [ -n "$path" ]; then
                list_remote_files "${path}${subdir}/"
            else
                list_remote_files "${subdir}/"
            fi
        else
            # It's a file, print full path
            echo "${path}${item}"
        fi
    done
}

# Delete a single file from remote
delete_file() {
    local remote_path="$1"

    if [ "$VERBOSE" = true ]; then
        echo "  Deleting: ${remote_path}"
    fi

    if [ "$DRY_RUN" = true ]; then
        ((DELETED++))
        return 0
    fi

    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" -X DELETE \
        "${STORAGE_URL}/${remote_path}" \
        -H "AccessKey: ${BUNNY_STORAGE_PASSWORD}")

    if [[ "$http_code" =~ ^2 ]]; then
        ((DELETED++))
        return 0
    else
        echo -e "${RED}Failed to delete ${remote_path}: HTTP ${http_code}${NC}"
        return 1
    fi
}

# Purge pull zone cache
purge_cache() {
    echo "Purging CDN cache..."

    if [ "$DRY_RUN" = true ]; then
        echo -e "${YELLOW}  Would purge pull zone ${BUNNY_PULLZONE_ID}${NC}"
        return 0
    fi

    local http_code
    http_code=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
        "https://api.bunny.net/pullzone/${BUNNY_PULLZONE_ID}/purgeCache" \
        -H "AccessKey: ${BUNNY_API_KEY}" \
        -H "Content-Type: application/json")

    if [[ "$http_code" =~ ^2 ]]; then
        echo -e "${GREEN}  Cache purged successfully${NC}"
        return 0
    else
        echo -e "${RED}  Failed to purge cache: HTTP ${http_code}${NC}"
        return 1
    fi
}

# ============================================
# MAIN EXECUTION
# ============================================

# Check local directory exists
if [ ! -d "$LOCAL_DIR" ]; then
    echo -e "${RED}Error: Local directory ${LOCAL_DIR} does not exist${NC}"
    echo "Run 'make docs' first to generate documentation"
    exit 1
fi

# Step 1: Upload all local files
echo "Uploading files..."
LOCAL_FILES_LIST=$(mktemp)
trap "rm -f $LOCAL_FILES_LIST" EXIT

find "$LOCAL_DIR" -type f -print0 | while IFS= read -r -d '' file; do
    # Get path relative to LOCAL_DIR
    relative_path="${file#${LOCAL_DIR}/}"
    echo "$relative_path" >> "$LOCAL_FILES_LIST"
    upload_file "$file" "$relative_path"
done

echo ""

# Step 2: Delete orphaned remote files (full sync)
echo "Checking for orphaned files..."
REMOTE_FILES=$(list_remote_files)

if [ -n "$REMOTE_FILES" ]; then
    while IFS= read -r remote_file; do
        if [ -z "$remote_file" ]; then
            continue
        fi
        if ! grep -qxF "$remote_file" "$LOCAL_FILES_LIST"; then
            delete_file "$remote_file"
        fi
    done <<< "$REMOTE_FILES"
fi

echo ""

# Step 3: Purge CDN cache
purge_cache

# Summary
echo ""
echo "============================================"
echo -e "${GREEN}Deploy complete!${NC}"
echo "  Uploaded: ${UPLOADED} files"
echo "  Deleted:  ${DELETED} files"
if [ "$DRY_RUN" = true ]; then
    echo -e "${YELLOW}  (DRY RUN - no actual changes made)${NC}"
fi
echo "============================================"
