# Bunny CDN Deploy Script Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create a bash script that syncs `www/priv/` to Bunny Storage Zone with full sync (upload + delete orphans) and cache purge.

**Architecture:** Single bash script using curl for all Bunny API calls. Environment variables for credentials. Makefile integration runs docs generation before deploy.

**Tech Stack:** Bash, curl, jq (for JSON parsing)

---

## Task 1: Create Script Skeleton with Environment Validation

**Files:**
- Create: `scripts/deploy-cdn.sh`

**Step 1: Create the script file with shebang and strict mode**

```bash
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
```

**Step 2: Make the script executable**

Run: `chmod +x scripts/deploy-cdn.sh`

**Step 3: Test environment validation**

Run: `./scripts/deploy-cdn.sh`
Expected: Error message about missing BUNNY_API_KEY

**Step 4: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add deploy-cdn.sh skeleton with env validation"
```

---

## Task 2: Add Content-Type Detection Helper

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add content-type function after configuration section**

Add this after the `echo ""` line:

```bash
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
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add content-type detection helper"
```

---

## Task 3: Add Upload Function

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add upload function after get_content_type**

```bash
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
        -H "AccessKey: ${BUNNY_API_KEY}" \
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
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add upload_file function"
```

---

## Task 4: Add List Remote Files Function

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add list_remote function after upload_file**

```bash
# List all files in remote storage (recursively)
list_remote_files() {
    local path="${1:-}"
    local url="${STORAGE_URL}/${path}"

    local response
    response=$(curl -s -X GET "$url" \
        -H "AccessKey: ${BUNNY_API_KEY}" \
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
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add list_remote_files function"
```

---

## Task 5: Add Delete Function

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add delete function after list_remote_files**

```bash
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
        -H "AccessKey: ${BUNNY_API_KEY}")

    if [[ "$http_code" =~ ^2 ]]; then
        ((DELETED++))
        return 0
    else
        echo -e "${RED}Failed to delete ${remote_path}: HTTP ${http_code}${NC}"
        return 1
    fi
}
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add delete_file function"
```

---

## Task 6: Add Cache Purge Function

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add purge function after delete_file**

```bash
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
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add purge_cache function"
```

---

## Task 7: Add Main Upload Logic

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add main upload logic at end of script**

```bash
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
declare -A LOCAL_FILES

while IFS= read -r -d '' file; do
    # Get path relative to LOCAL_DIR
    relative_path="${file#${LOCAL_DIR}/}"
    LOCAL_FILES["$relative_path"]=1
    upload_file "$file" "$relative_path"
done < <(find "$LOCAL_DIR" -type f -print0)

echo ""
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add main upload logic"
```

---

## Task 8: Add Sync Delete Logic

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add delete orphans logic after upload section**

```bash
# Step 2: Delete orphaned remote files (full sync)
echo "Checking for orphaned files..."
REMOTE_FILES=$(list_remote_files)

if [ -n "$REMOTE_FILES" ]; then
    while IFS= read -r remote_file; do
        if [ -z "$remote_file" ]; then
            continue
        fi
        if [ -z "${LOCAL_FILES[$remote_file]+x}" ]; then
            delete_file "$remote_file"
        fi
    done <<< "$REMOTE_FILES"
fi

echo ""
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add sync delete logic for orphaned files"
```

---

## Task 9: Add Cache Purge and Summary

**Files:**
- Modify: `scripts/deploy-cdn.sh`

**Step 1: Add cache purge and summary at end**

```bash
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
```

**Step 2: Commit**

```bash
git add scripts/deploy-cdn.sh
git commit -m "feat: add cache purge and summary output"
```

---

## Task 10: Add Makefile Integration

**Files:**
- Modify: `Makefile`

**Step 1: Add deploy-www target to .PHONY line**

Change line 1 from:
```makefile
.PHONY: help test build clean run css format-examples docs
```
To:
```makefile
.PHONY: help test build clean run css format-examples docs deploy-www
```

**Step 2: Add deploy-www target at end of Makefile**

```makefile

# Deploy www to Bunny CDN
deploy-www: docs
	@scripts/deploy-cdn.sh
```

**Step 3: Update help target**

Add this line in the help section after the format-examples line:
```makefile
	@echo "  make deploy-www - Deploy www/priv to Bunny CDN"
```

**Step 4: Commit**

```bash
git add Makefile
git commit -m "feat: add deploy-www Makefile target"
```

---

## Task 11: Manual Testing

**Step 1: Test dry-run mode**

Run:
```bash
export BUNNY_API_KEY="test"
export BUNNY_STORAGE_ZONE="test"
export BUNNY_STORAGE_HOST="storage.bunnycdn.com"
export BUNNY_PULLZONE_ID="123"
./scripts/deploy-cdn.sh --dry-run --verbose
```

Expected: Script runs, shows files it would upload, no actual API calls.

**Step 2: Test with real credentials (if available)**

Run: `make deploy-www`

Expected: Files upload to Bunny CDN, cache purges.

---

## API Reference

**Upload File:**
```bash
curl -X PUT "https://{host}/{zone}/{path}" \
  -H "AccessKey: {api_key}" \
  -H "Content-Type: {mime_type}" \
  --data-binary @file
```

**List Files:**
```bash
curl -X GET "https://{host}/{zone}/{path}/" \
  -H "AccessKey: {api_key}"
```

**Delete File:**
```bash
curl -X DELETE "https://{host}/{zone}/{path}" \
  -H "AccessKey: {api_key}"
```

**Purge Cache:**
```bash
curl -X POST "https://api.bunny.net/pullzone/{id}/purgeCache" \
  -H "AccessKey: {api_key}"
```

## Sources

- [Bunny Storage API Overview](https://docs.bunny.net/reference/storage-api)
- [Upload File API](https://docs.bunny.net/reference/put_-storagezonename-path-filename)
- [Purge Cache API](https://docs.bunny.net/reference/pullzonepublic_purgecachepostbytag)
