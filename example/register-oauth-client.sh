#!/bin/bash

# OAuth Dynamic Client Registration Script for AT Protocol
# Registers a new OAuth client with the AIP server per RFC 7591
# Usage: bash scripts/register-oauth-client.sh

set -e  # Exit on any error

# Configuration
AIP_BASE="${AIP_BASE_URL:-http://localhost:8081}"
CLIENT_BASE_URL="${CLIENT_BASE_URL:-http://localhost:8000}"
CLIENT_NAME="${CLIENT_NAME:-quickslice client}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR" && pwd)"
CONFIG_FILE="$ROOT_DIR/.env"

echo "🚀 OAuth Dynamic Client Registration for quickslice"
echo "AIP Server: $AIP_BASE"
echo "Client Base URL: $CLIENT_BASE_URL"
echo "Client Name: $CLIENT_NAME"
echo

# Check if client is already registered
if [ -f "$CONFIG_FILE" ]; then
    echo "⚠️  Existing OAuth client configuration found at $CONFIG_FILE"
    echo -n "Do you want to register a new client? This will overwrite the existing config. (y/N): "
    read -r OVERWRITE
    if [ "$OVERWRITE" != "y" ] && [ "$OVERWRITE" != "Y" ]; then
        echo "❌ Registration cancelled"
        exit 1
    fi
fi

echo "🔍 Using OAuth registration endpoint..."
REGISTRATION_ENDPOINT="$AIP_BASE/oauth/clients/register"

echo "✅ Registration endpoint: $REGISTRATION_ENDPOINT"
echo

# Create client registration request
echo "📝 Creating client registration request..."
REDIRECT_URI="$CLIENT_BASE_URL/oauth/callback"

REGISTRATION_REQUEST=$(cat <<EOF
{
    "client_name": "$CLIENT_NAME",
    "redirect_uris": ["$REDIRECT_URI"],
    "scope": "openid profile atproto transition:generic",
    "grant_types": ["authorization_code", "refresh_token"],
    "response_types": ["code"],
    "token_endpoint_auth_method": "client_secret_basic"
}
EOF
)

echo "Registration request:"
echo "$REGISTRATION_REQUEST" | jq '.' 2>/dev/null || echo "$REGISTRATION_REQUEST"
echo

# Register the client
echo "🔄 Registering client with AIP server..."
REGISTRATION_RESPONSE=$(curl -s -X POST "$REGISTRATION_ENDPOINT" \
    -H "Content-Type: application/json" \
    -d "$REGISTRATION_REQUEST" || {
        echo "❌ Failed to register client with AIP server"
        echo "Make sure the AIP server is running at $AIP_BASE"
        exit 1
    })

echo "Registration response:"
echo "$REGISTRATION_RESPONSE" | jq '.' 2>/dev/null || echo "$REGISTRATION_RESPONSE"
echo

# Extract client credentials
CLIENT_ID=$(echo "$REGISTRATION_RESPONSE" | grep -o '"client_id":"[^"]*' | cut -d'"' -f4)
CLIENT_SECRET=$(echo "$REGISTRATION_RESPONSE" | grep -o '"client_secret":"[^"]*' | cut -d'"' -f4)

if [ -z "$CLIENT_ID" ] || [ -z "$CLIENT_SECRET" ]; then
    echo "❌ Failed to extract client credentials from registration response"
    echo "Expected client_id and client_secret in response"
    echo "Response was: $REGISTRATION_RESPONSE"
    exit 1
fi

echo "✅ Client registered successfully!"
echo "Client ID: $CLIENT_ID"
echo "Client Secret: [REDACTED]"
echo

# Save credentials to .env.oauth file
echo "💾 Saving client credentials to $CONFIG_FILE..."
cat > "$CONFIG_FILE" <<EOF
# OAuth Client Credentials for Slice AT Proto Client
# Generated on $(date)
# AIP Server: $AIP_BASE

OAUTH_CLIENT_ID=$CLIENT_ID
OAUTH_CLIENT_SECRET=$CLIENT_SECRET
OAUTH_REDIRECT_URI=$REDIRECT_URI
AIP_BASE_URL=$AIP_BASE
EOF

echo "✅ Client registration complete!"
echo
echo "📋 Summary:"
echo "  - Client ID: $CLIENT_ID"
echo "  - Client Name: $CLIENT_NAME"
echo "  - Redirect URI: $REDIRECT_URI"
echo "  - Scopes: openid email profile atproto transition:generic account:email blob:image/* repo:network.slices.slice repo:network.slices.lexicon repo:network.slices.actor.profile repo:network.slices.waitlist.request"
echo "  - Config saved to: $CONFIG_FILE"
echo
echo "🔧 Environment variables saved to $CONFIG_FILE:"
echo "  OAUTH_CLIENT_ID"
echo "  OAUTH_CLIENT_SECRET"
echo "  OAUTH_REDIRECT_URI"
echo "  OAUTH_AIP_BASE_URL"
echo
echo "💡 To use these credentials in your application:"
echo "  source $CONFIG_FILE"
echo "  # Or load them in your .env file"
echo
echo "🧪 To test the OAuth flow, you can now use the registered credentials"
echo "    with your AtProtoClient in TypeScript/Deno."
