# quickslice Docker Example

This example demonstrates how to run quickslice using the published Docker image with custom lexicons.

## Directory Structure

```
example/
├── Dockerfile              # Custom Dockerfile (optional)
├── docker-compose.yml      # Docker Compose configuration
├── lexicons/              # Custom lexicons directory
│   └── xyz/
│       └── statusphere/
│           └── status.json
└── README.md              # This file
```

## Quick Start

### Using Docker Compose (Recommended)

```bash
cd example
docker-compose up
```

The server will be available at `http://localhost:8000`

### Using Docker Directly

```bash
cd example
docker run -p 8000:8000 \
  -v $(pwd)/lexicons:/app/server/priv/lexicons:ro \
  ghcr.io/bigmoves/quickslice:latest
```

## Custom Lexicons

The `lexicons/` directory is mounted to `/app/server/priv/lexicons` in the container. This allows you to provide custom AT Protocol lexicons without rebuilding the image.

### Adding Your Own Lexicon

1. Create a directory structure following the NSID format:
   ```
   lexicons/
   └── com/
       └── example/
           └── myrecord.json
   ```

2. Create your lexicon JSON file:
   ```json
   {
     "lexicon": 1,
     "id": "com.example.myrecord",
     "defs": {
       "main": {
         "type": "record",
         "key": "tid",
         "record": {
           "type": "object",
           "required": ["text"],
           "properties": {
             "text": {
               "type": "string",
               "maxLength": 300
             },
             "createdAt": {
               "type": "string",
               "format": "datetime"
             }
           }
         }
       }
     }
   }
   ```

3. Restart the container to load the new lexicon

## Environment Variables

You can customize the server configuration using environment variables in `docker-compose.yml`:

- `HOST` - Server host (default: `0.0.0.0`)
- `PORT` - Server port (default: `8000`)

## GraphQL Endpoint

Once running, access the GraphQL endpoint at:
- `http://localhost:8000/graphql`

## Volume Mounts

- `./lexicons:/app/server/priv/lexicons:ro` - Mounts local lexicons directory as read-only

The `:ro` flag makes the mount read-only for security.

## Stopping the Server

```bash
docker compose down
```

## Pulling Latest Image

```bash
docker compose pull
docker compose up -d
```

## Troubleshooting

### Port Already in Use

If port 8000 is already in use, change it in `docker-compose.yml`:

```yaml
ports:
  - "3000:8000"  # Maps local port 3000 to container port 8000
```

### Lexicons Not Loading

Ensure your lexicon files:
1. Follow the correct directory structure (namespace/path/file.json)
2. Have valid JSON syntax
3. Match the NSID in the filename path

### Viewing Logs

```bash
docker-compose logs -f quickslice
```
