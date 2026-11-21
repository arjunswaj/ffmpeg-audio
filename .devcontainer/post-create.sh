#!/bin/bash
# Prepare the devcontainer. You can run it more than once.
set -e

cd "$(dirname "$0")/.."
echo "=== ffmpeg-audio DevContainer Setup ==="

if [ ! -d .jj ]; then
    echo "Initializing jj (colocated with git backend)..."
    jj git init --colocate
fi

echo "Updating cabal package index and warming dependencies..."
cabal update
cabal build -j2 --only-dependencies --enable-tests
cabal build -j2 --enable-tests

# Marker read by .devcontainer/exec.sh so a fresh Compose container is prepared
# exactly once. Lives on the dist-newstyle volume, so it survives container
# recreation but is discarded when the build cache is wiped.
touch /workspace/dist-newstyle/.dev-ready

echo ""
echo "=== Setup Complete ==="
echo "git:    $(git --version)"
echo "jj:     $(jj --version)"
echo "cabal:  $(cabal --version | head -n1)"
echo "ghc:    $(ghc --version)"
echo "ffmpeg: $(ffmpeg -version | head -n1)"
