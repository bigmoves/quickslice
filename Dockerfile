ARG GLEAM_VERSION=v1.13.0

# Build stage - compile the application
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

# Install build dependencies including Rust for NIFs
RUN apk add --no-cache \
    bash \
    git \
    build-base \
    sqlite-dev \
    rust \
    cargo

# Configure git for non-interactive use
ENV GIT_TERMINAL_PROMPT=0

# Add local dependencies first (these change less frequently)
COPY ./lexicon /build/lexicon
COPY ./graphql /build/graphql
COPY ./lexicon_graphql /build/lexicon_graphql

# Add server code
COPY ./server /build/server

# Build Rust NIFs for lexicon package (Linux build produces .so)
RUN cd /build/lexicon/native/lexicon_nif && cargo build --release && \
    mkdir -p /build/lexicon/priv && \
    cp /build/lexicon/native/lexicon_nif/target/release/liblexicon_nif.so /build/lexicon/priv/liblexicon_nif.so

# Install dependencies for all projects
RUN cd /build/lexicon && gleam deps download
RUN cd /build/graphql && gleam deps download
RUN cd /build/lexicon_graphql && gleam deps download
RUN cd /build/server && gleam deps download

# Compile the server code
RUN cd /build/server \
    && gleam export erlang-shipment

# Runtime stage - slim image with only what's needed to run
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine

# Install runtime dependencies
RUN apk add --no-cache sqlite-libs

# Copy the compiled server code from the builder stage
COPY --from=builder /build/server/build/erlang-shipment /app

# Copy lexicons directory to the runtime image
COPY --from=builder /build/server/priv/lexicons /app/priv/lexicons

# Set up the entrypoint
WORKDIR /app

# Set environment variables
ENV HOST=0.0.0.0
ENV PORT=8000

# Expose the port the server will run on
EXPOSE $PORT

# Run the server in foreground mode without requiring a TTY
# The 'foreground' command keeps the process running without an interactive shell
CMD ["./entrypoint.sh", "foreground"]
