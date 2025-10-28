#!/bin/bash

# Create Record Script - Takes access token as argument
# Usage: ./create_record_with_token.sh <ACCESS_TOKEN>

set -e

LOCALHOST_API="http://localhost:8000"

# Check if token was provided
if [ -z "$1" ]; then
  echo "❌ Access token required"
  echo ""
  echo "Usage: $0 <ACCESS_TOKEN>"
  echo ""
  echo "Get your access token from the AIP server first, then run:"
  echo "  $0 YOUR_ACCESS_TOKEN"
  exit 1
fi

ACCESS_TOKEN="$1"

echo "🔐 QuickSlice Record Creation Tool"
echo "Using access token: ${ACCESS_TOKEN:0:20}..."
echo ""

# Get ATProto session
echo "🔍 Getting ATProto session from AIP..."
AIP_BASE_URL="${AIP_BASE_URL:-https://tunnel.chadtmiller.com}"
SESSION_RESPONSE=$(curl -s "$AIP_BASE_URL/api/atprotocol/session" \
  -H "Authorization: Bearer $ACCESS_TOKEN")

echo "📋 Full ATProto Session Response:"
echo "$SESSION_RESPONSE" | jq '.'
echo ""

DID=$(echo "$SESSION_RESPONSE" | jq -r '.did // empty')
HANDLE=$(echo "$SESSION_RESPONSE" | jq -r '.handle // empty')
PDS_ENDPOINT=$(echo "$SESSION_RESPONSE" | jq -r '.pds_endpoint // empty')

if [ -z "$DID" ]; then
  echo "❌ Failed to get ATProto session"
  echo "Response: $SESSION_RESPONSE"
  exit 1
fi

echo "✅ ATProto session retrieved!"
echo "   DID: $DID"
echo "   Handle: $HANDLE"
echo "   PDS: $PDS_ENDPOINT"
echo ""

# Create a status record
echo "📝 Creating status record..."

# Prompt for status emoji
read -p "Enter status emoji (single emoji, or press Enter for default): " STATUS_EMOJI
if [ -z "$STATUS_EMOJI" ]; then
  STATUS_EMOJI="✨"
fi

# Create the record JSON
RECORD_JSON=$(cat <<EOF
{
  "repo": "$DID",
  "collection": "xyz.statusphere.status",
  "record": {
    "status": "$STATUS_EMOJI",
    "createdAt": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")"
  }
}
EOF
)

# Make the request to localhost:8000
echo "🚀 Sending request to $LOCALHOST_API..."
echo "📤 Request body:"
echo "$RECORD_JSON" | jq '.'
echo ""

CREATE_RESPONSE=$(curl -s -w "\n%{http_code}" -X POST \
  "$LOCALHOST_API/xrpc/xyz.statusphere.status.createRecord" \
  -H "Authorization: Bearer $ACCESS_TOKEN" \
  -H "Content-Type: application/json" \
  -d "$RECORD_JSON")

# Extract status code (last line) and body (everything else)
HTTP_CODE=$(echo "$CREATE_RESPONSE" | tail -n 1)
RESPONSE_BODY=$(echo "$CREATE_RESPONSE" | sed '$d')

if [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "201" ]; then
  echo "✅ Record created successfully!"
  echo ""
  echo "Response:"
  echo "$RESPONSE_BODY" | jq '.'
  echo ""

  RECORD_URI=$(echo "$RESPONSE_BODY" | jq -r '.uri // empty')
  if [ -n "$RECORD_URI" ]; then
    echo "📍 Record URI: $RECORD_URI"
  fi
else
  echo "❌ Failed to create record (HTTP $HTTP_CODE)"
  echo ""
  echo "Response:"
  echo "$RESPONSE_BODY" | jq '.' 2>/dev/null || echo "$RESPONSE_BODY"
  exit 1
fi

echo ""
echo "🎉 Done!"
