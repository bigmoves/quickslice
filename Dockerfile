ARG GLEAM_VERSION=v1.13.0

# Build stage - compile the application
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

# Install build dependencies
RUN apk add --no-cache \
    bash \
    git \
    nodejs \
    npm \
    build-base \
    sqlite-dev

# Configure git for non-interactive use
ENV GIT_TERMINAL_PROMPT=0

# Add local dependencies first (these change less frequently)
COPY ./lexicon_graphql /build/lexicon_graphql
COPY ./client /build/client

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

# Install runtime dependencies
RUN apk add --no-cache sqlite-libs sqlite

# Copy the compiled server code from the builder stage
COPY --from=builder /build/server/build/erlang-shipment /app

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
CMD ["./entrypoint.sh", "run"]
