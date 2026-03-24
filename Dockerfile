# Synapse - Plexus RPC CLI Client
# Multi-stage build for minimal production image

# Build stage
FROM haskell:9.6.7-slim AS builder

WORKDIR /build

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git \
    libgmp-dev \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Copy cabal files first for better layer caching
COPY plexus-synapse.cabal cabal.project* ./

# Copy dependencies
COPY ../plexus-protocol /build/plexus-protocol
COPY ../synapse-types /build/synapse-types

# Update cabal and build dependencies only
RUN cabal update && cabal build --only-dependencies

# Copy source code
COPY . .

# Build synapse
RUN cabal build exe:synapse && \
    cabal install exe:synapse --install-method=copy --overwrite-policy=always --installdir=/usr/local/bin

# Runtime stage - minimal image
FROM debian:bookworm-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libgmp10 \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Copy binary from builder
COPY --from=builder /usr/local/bin/synapse /usr/local/bin/synapse

# Create non-root user
RUN useradd -m -u 1000 -s /bin/bash synapse
USER synapse
WORKDIR /home/synapse

# Set default log level to error (quiet)
ENV LOG_LEVEL=error

# Default command: show help
CMD ["synapse", "--help"]

# Usage examples:
# Build:
#   docker build -t synapse:latest .
#
# Run help:
#   docker run --rm synapse:latest
#
# Call a method:
#   docker run --rm --network host synapse:latest substrate echo.echo --message "hello"
#
# Enable debug logging:
#   docker run --rm --network host -e LOG_LEVEL=debug synapse:latest substrate hash
#
# Connect to custom host:
#   docker run --rm synapse:latest -H 192.168.1.100 -P 5001 backend method
