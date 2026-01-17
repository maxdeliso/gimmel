#!/usr/bin/env bash
set -euo pipefail

# Smart build script that automatically uses static builds on Linux when Nix is available
# Falls back to regular builds otherwise (e.g., Windows, or Linux without Nix)

OS="$(uname -s)"
USE_STATIC=false

# Check if we should use static builds
if [[ "$OS" == "Linux" ]]; then
  # Check if Nix is available
  if command -v nix &> /dev/null 2>&1 || command -v nix-build &> /dev/null 2>&1; then
    # Check if flake.nix exists and we can use the static shell
    if [[ -f flake.nix ]]; then
      # Try to verify the static shell exists (don't fail if nix-command is disabled)
      if nix develop .#static --help &> /dev/null 2>&1 || \
         nix-shell --help &> /dev/null 2>&1; then
        USE_STATIC=true
        echo "Detected Nix on Linux - using static build"
      fi
    fi
  fi
fi

if [[ "$USE_STATIC" == "true" ]]; then
  # Use static build via haskell.nix
  echo "Building static binary with haskell.nix (musl)..."
  if nix build .#static 2>&1; then
    echo "✓ Static build successful"
    echo "Binary location: $(nix build .#static --print-out-paths 2>/dev/null || echo 'result/bin/gimmel-exe')"
  else
    echo "⚠ Static build failed, falling back to regular dynamic build..."
    stack build
  fi
else
  # Regular build (Windows or Linux without Nix)
  echo "Building with regular dynamic linking..."
  stack build
fi
