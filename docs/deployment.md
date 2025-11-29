# Deployment (WIP)

This guide covers deploying quickslice on Fly.io and Railway. Both platforms support Docker deployments with persistent volumes for SQLite.

## Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `DATABASE_URL` | No | `quickslice.db` | Path to SQLite database file. Use `/data/quickslice.db` with volume mount |
| `HOST` | No | `127.0.0.1` | Server bind address. Set to `0.0.0.0` for containers |
| `PORT` | No | `8080` | Server port |
| `SECRET_KEY_BASE` | Recommended | Auto-generated | Session encryption key (64+ chars). **Must persist across restarts** |
| `ADMIN_DIDS` | Optional | - | Comma-separated DIDs for admin access (e.g., `did:plc:abc,did:plc:xyz`) |
| `EXTERNAL_BASE_URL` | Optional | `http://localhost:8080` | Base URL of your application (used for OAuth redirect URIs and client metadata). Use `http://127.0.0.1:8080` for loopback mode |
| `OAUTH_LOOPBACK_MODE` | Optional | `false` | Set to `true` for local development without ngrok. Uses loopback client IDs instead of client metadata URLs |
| `OAUTH_SUPPORTED_SCOPES` | Optional | `atproto transition:generic` | Space-separated OAuth scopes to request (used in loopback client IDs and client metadata) |
| `JETSTREAM_URL` | No | `wss://jetstream2.us-west.bsky.network/subscribe` | Jetstream WebSocket endpoint |
| `RELAY_URL` | No | `https://relay1.us-west.bsky.network` | AT Protocol relay URL |
| `PLC_DIRECTORY_URL` | No | `https://plc.directory` | PLC directory URL |

### Critical Environment Variables

- **DATABASE_URL**: Must point to a persistent volume location
- **SECRET_KEY_BASE**: Generate with `openssl rand -base64 48`. Store as a secret and keep persistent
- **HOST**: Set to `0.0.0.0` in container environments
- **ADMIN_DIDS**: Required for backfill and settings page access

## SQLite Volume Setup

SQLite requires persistent storage for three files:
- `{DATABASE_URL}` - Main database file
- `{DATABASE_URL}-shm` - Shared memory file
- `{DATABASE_URL}-wal` - Write-ahead log

**IMPORTANT**: Without persistent storage, all data will be lost on container restart.

## Fly.io

### 1. Create a volume

```bash
fly volumes create app_data --size 10
```

### 2. Configure fly.toml

Create `fly.toml` in your project root:

```toml
app = 'your-app-name'
primary_region = 'sjc'

[build]
  dockerfile = "Dockerfile"

[env]
  DATABASE_URL = '/data/quickslice.db'
  HOST = '0.0.0.0'
  PORT = '8080'

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = 'stop'
  auto_start_machines = true
  min_machines_running = 1

[[mounts]]
  source = 'app_data'
  destination = '/data'

[[vm]]
  memory = '1gb'
  cpu_kind = 'shared'
  cpus = 1
```

### 3. Set secrets

```bash
fly secrets set SECRET_KEY_BASE=$(openssl rand -base64 48)

# Optional: Admin access
fly secrets set ADMIN_DIDS=did:plc:your_did
```

### 4. Deploy

```bash
fly deploy
```

### 5. Verify health

```bash
fly status
curl https://your-app.fly.dev/health
```

## Railway

### 1. Create a new project

Connect your GitHub repository or deploy from the CLI.

### 2. Configure environment variables

In the Railway dashboard, add these variables:

```
DATABASE_URL=/data/quickslice.db
HOST=0.0.0.0
PORT=8080
SECRET_KEY_BASE=<generate-with-openssl-rand>
```

Optional variables:
```
ADMIN_DIDS=did:plc:your_did
```

### 3. Add a volume

In the Railway dashboard:
1. Go to your service settings
2. Add a volume mount
3. Mount path: `/data`
4. Size: 10GB (or as needed)

### 4. Configure health check

In the service settings, set the health check path to `/health`.

### 5. Deploy

Railway will automatically deploy when you push to your connected branch.

### Optional: railway.json

Create `railway.json` for declarative configuration:

```json
{
  "$schema": "https://railway.app/railway.schema.json",
  "build": {
    "builder": "DOCKERFILE",
    "dockerfilePath": "Dockerfile"
  },
  "deploy": {
    "numReplicas": 1,
    "restartPolicyType": "ON_FAILURE",
    "restartPolicyMaxRetries": 10,
    "healthcheckPath": "/health",
    "healthcheckTimeout": 100
  }
}
```

## Docker Compose (Self-Hosted)

For self-hosted deployments, use the published Docker image:

```yaml
version: "3.8"

services:
  quickslice:
    image: ghcr.io/bigmoves/quickslice:latest
    ports:
      - "8080:8080"
    volumes:
      - quickslice-data:/data
      - ./lexicons:/app/priv/lexicons:ro  # Optional: custom lexicons
    environment:
      - HOST=0.0.0.0
      - PORT=8080
      - DATABASE_URL=/data/quickslice.db
      - SECRET_KEY_BASE=${SECRET_KEY_BASE}
      - ADMIN_DIDS=${ADMIN_DIDS}
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "wget", "--spider", "-q", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3

volumes:
  quickslice-data:
```

Create a `.env` file for secrets:

```bash
SECRET_KEY_BASE=<generate-with-openssl-rand>
ADMIN_DIDS=did:plc:your_did
```

Start the service:

```bash
docker compose up -d
```

## Post-Deployment

### Health check

Verify the service is running:

```bash
curl https://your-app-url/health
```

Expected response:
```json
{"status":"healthy"}
```

### Access GraphiQL

Navigate to `/graphiql` (requires `ADMIN_DIDS` configuration).

### Database access

**Fly.io**:
```bash
fly ssh console
sqlite3 /data/quickslice.db
```

**Railway**:
Use the Railway CLI or connect via SSH from the dashboard.

**Docker**:
```bash
docker exec -it <container-name> sqlite3 /data/quickslice.db
```

### Logs

**Fly.io**:
```bash
fly logs
```

**Railway**:
View logs in the dashboard or use `railway logs`.

**Docker**:
```bash
docker compose logs -f quickslice
```

## Backfill Configuration

Control memory usage during backfill operations with these environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `BACKFILL_MAX_PDS_WORKERS` | 10 | Max concurrent PDS endpoints being processed |
| `BACKFILL_PDS_CONCURRENCY` | 4 | Max concurrent repo fetches per PDS |
| `BACKFILL_MAX_HTTP_CONCURRENT` | 50 | Global HTTP request limit |
| `BACKFILL_REPO_TIMEOUT` | 60 | Timeout per repo fetch (seconds) |

### Recommended Settings by VPS Size

**1GB RAM (e.g., Railway starter):**
```
BACKFILL_MAX_PDS_WORKERS=8
BACKFILL_PDS_CONCURRENCY=2
BACKFILL_MAX_HTTP_CONCURRENT=30
```

**2GB RAM:**
```
BACKFILL_MAX_PDS_WORKERS=15
BACKFILL_PDS_CONCURRENCY=4
BACKFILL_MAX_HTTP_CONCURRENT=50
```

**4GB+ RAM:**
```
BACKFILL_MAX_PDS_WORKERS=25
BACKFILL_PDS_CONCURRENCY=6
BACKFILL_MAX_HTTP_CONCURRENT=100
```

## Resource Requirements

**Minimum**:
- Memory: 1GB
- CPU: 1 shared core
- Disk: 10GB volume (for SQLite database)

**Recommendations**:
- Scale memory for high-traffic deployments
- Use SSD-backed volumes for SQLite performance
- Monitor database size and scale volume as needed

## Security

1. **Always set SECRET_KEY_BASE** - Generate a strong random key and keep it persistent
2. **Use HTTPS in production** - Both Fly.io and Railway handle this automatically
3. **Restrict admin access** - Set `ADMIN_DIDS` to limit who can access GraphiQL and backfill endpoints
4. **Store secrets securely** - Use platform secret management, never commit secrets to git
