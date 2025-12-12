# Deployment

Deploy Quickslice to production. Railway with one-click deploy is fastest.

## Railway (Recommended)

### 1. Deploy

Click the button to create a new Quickslice instance:

[![Deploy on Railway](https://railway.com/button.svg)](https://railway.com/deploy/quickslice?referralCode=Ofii6e&utm_medium=integration&utm_source=template&utm_campaign=generic)

Railway prompts you to configure environment variables. Leave the form open while you generate a signing key.

### 2. Generate OAuth Signing Key

Quickslice needs a private key to sign OAuth tokens:

```bash
brew install goat
goat key generate -t p256
```

Paste the output into the `OAUTH_SIGNING_KEY` field in Railway, then click **Save Config**.

### 3. Configure Your Domain

After deployment completes:

1. Click on your quickslice service
2. Go to **Settings**
3. Click **Generate Domain** under Networking

Railway creates a public URL like `quickslice-production-xxxx.up.railway.app`.

**Redeploy to apply the domain:**

1. Go to **Deployments**
2. Click the three-dot menu on the latest deployment
3. Select **Redeploy**

### 4. Create Admin Account

Visit your domain. The welcome screen prompts you to create an admin account:

1. Enter your AT Protocol handle (e.g., `yourname.bsky.social`)
2. Click **Authenticate**
3. Authorize Quickslice on your PDS
4. You're now the instance admin

### 5. Configure Your Instance

From the homepage, go to **Settings**:

1. Enter your **Domain Authority** in reverse-domain format (e.g., `xyz.statusphere`)
2. Upload your Lexicons as a `.zip` file (JSON format, directory structure doesn't matter):
   ```
   lexicons.zip
   └── lexicons/
       └── xyz/
           └── statusphere/
               ├── status.json
               └── follow.json
   ```
3. Click **Trigger Backfill** to import existing records from the network. The Quickslice logo enters a loading state during backfill and the page refreshes when complete. Check Railway logs to monitor progress:
   ```
   INFO [backfill] PDS worker 67/87 done (1898 records)
   INFO [backfill] PDS worker 68/87 done (1117 records)
   INFO [backfill] PDS worker 69/87 done (746 records)
   ...
   ```

## Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `OAUTH_SIGNING_KEY` | Yes | - | P-256 private key for signing OAuth tokens |
| `DATABASE_URL` | No | `quickslice.db` | Path to SQLite database |
| `HOST` | No | `127.0.0.1` | Server bind address (use `0.0.0.0` for containers) |
| `PORT` | No | `8080` | Server port |
| `SECRET_KEY_BASE` | Recommended | Auto-generated | Session encryption key (64+ chars) |
| `EXTERNAL_BASE_URL` | No | Auto-detected | Public URL for OAuth redirects |

## Fly.io

### 1. Create a Volume

```bash
fly volumes create app_data --size 10
```

### 2. Configure fly.toml

```toml
app = 'your-app-name'
primary_region = 'sjc'

[build]
  dockerfile = "Dockerfile"

[env]
  DATABASE_URL = 'sqlite:/data/quickslice.db'
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

### 3. Set Secrets

```bash
fly secrets set SECRET_KEY_BASE=$(openssl rand -base64 48)
fly secrets set OAUTH_SIGNING_KEY="$(goat key generate -t p256)"
```

### 4. Deploy

```bash
fly deploy
```

## Docker Compose

For self-hosted deployments:

```yaml
version: "3.8"

services:
  quickslice:
    image: ghcr.io/bigmoves/quickslice:latest
    ports:
      - "8080:8080"
    volumes:
      - quickslice-data:/data
    environment:
      - HOST=0.0.0.0
      - PORT=8080
      - DATABASE_URL=sqlite:/data/quickslice.db
      - SECRET_KEY_BASE=${SECRET_KEY_BASE}
      - OAUTH_SIGNING_KEY=${OAUTH_SIGNING_KEY}
    restart: unless-stopped

volumes:
  quickslice-data:
```

Create a `.env` file:

```bash
SECRET_KEY_BASE=$(openssl rand -base64 48)
OAUTH_SIGNING_KEY=$(goat key generate -t p256)
```

Start:

```bash
docker compose up -d
```

## Backfill Configuration

NOTE: These configurations are evolving. If your container runs low on memory or crashes, reduce concurrent workers and requests.

Control memory usage during backfill with these variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `BACKFILL_MAX_PDS_WORKERS` | 10 | Max concurrent PDS endpoints |
| `BACKFILL_PDS_CONCURRENCY` | 4 | Max concurrent repo fetches per PDS |
| `BACKFILL_MAX_HTTP_CONCURRENT` | 50 | Global HTTP request limit |

**1GB RAM:**
```
BACKFILL_MAX_PDS_WORKERS=8
BACKFILL_PDS_CONCURRENCY=2
BACKFILL_MAX_HTTP_CONCURRENT=30
```

**2GB+ RAM:** Use defaults or increase values.

## Resource Requirements

**Minimum:**
- Memory: 1GB
- CPU: 1 shared core
- Disk: 10GB volume

**Recommendations:**
- Use SSD-backed volumes for SQLite performance
- Monitor database size and scale volume as needed
