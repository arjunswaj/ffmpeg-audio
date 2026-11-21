# ffmpeg-audio development container
FROM debian:trixie-slim

# System deps (Debian Trixie ships FFmpeg 7.1+ with native libavutil/iamf.h and AVStreamGroup)
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    pkg-config \
    curl \
    ca-certificates \
    xz-utils \
    git \
    openssh-client \
    ripgrep \
    fd-find \
    vim \
    tmux \
    less \
    libavcodec-dev \
    libavformat-dev \
    libavutil-dev \
    libswresample-dev \
    ffmpeg \
    libgmp-dev \
    libffi-dev \
    libnuma-dev \
    && rm -rf /var/lib/apt/lists/*

# Build git from source: jj 0.46 needs git >= 2.42 for `workspace add --colocate`.
ARG GIT_VERSION=2.54.0
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libexpat1-dev \
    libssl-dev \
    zlib1g-dev \
    && curl -fsSL --retry 5 --retry-all-errors --http1.1 -o /tmp/git.tar.gz \
       "https://github.com/git/git/archive/refs/tags/v${GIT_VERSION}.tar.gz" \
    && tar -C /tmp -xf /tmp/git.tar.gz \
    && make -C "/tmp/git-${GIT_VERSION}" -j"$(nproc)" prefix=/usr/local \
       NO_TCLTK=1 NO_GETTEXT=1 NO_PYTHON=1 all \
    && make -C "/tmp/git-${GIT_VERSION}" prefix=/usr/local \
       NO_TCLTK=1 NO_GETTEXT=1 NO_PYTHON=1 install \
    && rm -rf /tmp/git.tar.gz "/tmp/git-${GIT_VERSION}" /var/lib/apt/lists/* \
    && git --version

# GHC + cabal via ghcup
RUN ARCH=$(uname -m) \
    && case "$ARCH" in aarch64) GHCUP_ARCH=aarch64 ;; *) GHCUP_ARCH=x86_64 ;; esac \
    && curl -fsSL --retry 5 --retry-all-errors --http1.1 "https://downloads.haskell.org/~ghcup/${GHCUP_ARCH}-linux-ghcup" -o /usr/local/bin/ghcup \
    && chmod +x /usr/local/bin/ghcup \
    && ghcup install ghc 9.10.3 --set \
    && ghcup install cabal 3.16.1.0 --set \
    && ghcup gc \
    && rm -rf /root/.ghcup/ghc/*/share/doc /root/.ghcup/cache /root/.ghcup/tmp

ENV PATH="/root/.local/bin:/root/.ghcup/bin:/root/.cabal/bin:${PATH}"
ENV LANG=C.UTF-8

# Install ormolu and hlint from Debian Trixie apt (fast, low-memory, avoids building ghc-lib-parser)
RUN apt-get update && apt-get install -y --no-install-recommends \
    ormolu \
    hlint \
    && rm -rf /var/lib/apt/lists/*

# jj 0.46 adds `jj workspace add --colocate` (a Git worktree per workspace).
ARG JJ_VERSION=v0.46.0
RUN ARCH=$(uname -m) \
    && case "$ARCH" in aarch64) JJ_ARCH=aarch64 ;; x86_64) JJ_ARCH=x86_64 ;; esac \
    && curl -fsSL --retry 5 --retry-all-errors --http1.1 -o /tmp/jj.tar.gz "https://github.com/jj-vcs/jj/releases/download/${JJ_VERSION}/jj-${JJ_VERSION}-${JJ_ARCH}-unknown-linux-musl.tar.gz" \
    && tar xzf /tmp/jj.tar.gz -C /usr/local/bin ./jj \
    && rm /tmp/jj.tar.gz

WORKDIR /workspace

CMD ["/bin/bash"]
