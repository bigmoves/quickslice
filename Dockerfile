ARG GLEAM_VERSION=v1.13.0

# Build stage - compile the application
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

# Install build dependencies (including PostgreSQL client for multi-database support)
RUN apk add --no-cache \
    bash \
    git \
    nodejs \
    npm \
    build-base \
    sqlite-dev \
    postgresql-dev \
    && npm install -g bun

# Configure git for non-interactive use
ENV GIT_TERMINAL_PROMPT=0

# Add local dependencies first (these change less frequently)
COPY ./lexicon_graphql /build/lexicon_graphql
COPY ./client /build/client
COPY ./atproto_car /build/atproto_car

# Add server code
COPY ./server /build/server

# Add patches directory
COPY ./patches /build/patches

# Install dependencies for all projects
RUN cd /build/client && gleam deps download
RUN cd /build/lexicon_graphql && gleam deps download
RUN cd /build/server && gleam deps download

# Apply patches to dependencies
RUN cd /build && patch -p1 < patches/mist-websocket-protocol.patch

# Install JavaScript dependencies for client
RUN cd /build/client && npm install

# Compile the client code and output to server's static directory
RUN cd /build/client \
    && gleam add --dev lustre_dev_tools \
    && gleam run -m lustre/dev build quickslice_client --minify --outdir=/build/server/priv/static

# Compile the server code
RUN cd /build/server \
    && gleam export erlang-shipment

# Runtime stage - slim image with only what's needed to run
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine

# Install runtime dependencies and dbmate for migrations
ARG TARGETARCH
RUN apk add --no-cache sqlite-libs sqlite libpq curl \
    && DBMATE_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "arm64" || echo "amd64") \
    && curl -fsSL -o /usr/local/bin/dbmate https://github.com/amacneil/dbmate/releases/latest/download/dbmate-linux-${DBMATE_ARCH} \
    && chmod +x /usr/local/bin/dbmate

# Copy the compiled server code from the builder stage
COPY --from=builder /build/server/build/erlang-shipment /app

# Copy database migrations and config
COPY --from=builder /build/server/db /app/db
COPY --from=builder /build/server/.dbmate.yml /app/.dbmate.yml
COPY --from=builder /build/server/docker-entrypoint.sh /app/docker-entrypoint.sh

# Set up the entrypoint
WORKDIR /app

# Create the data directory for the SQLite database and Fly.io volume mount
RUN mkdir -p /data && chmod 755 /data

# Set environment variables
ENV HOST=0.0.0.0
ENV PORT=8080

# Expose the port the server will run on
EXPOSE $PORT

# Run the server
CMD ["/app/docker-entrypoint.sh", "run"]
