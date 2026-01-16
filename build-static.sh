#!/usr/bin/env bash
set -euo pipefail

# Build static binary using haskell.nix with musl
# This script uses haskell.nix to build a fully static binary

echo "Building static binary with haskell.nix (musl-based)..."

# Build using haskell.nix
if nix build .#static 2>&1; then
  BIN=$(nix build .#static --print-out-paths 2>/dev/null || echo "result/bin/gimmel-exe")
  
  if [ -f "$BIN" ] || [ -f "result/bin/gimmel-exe" ]; then
    ACTUAL_BIN="${BIN:-result/bin/gimmel-exe}"
    echo ''
    echo '✓ Build successful!'
    echo "Binary location: $ACTUAL_BIN"
    echo ''
    echo 'Checking if binary is static...'
    if ldd "$ACTUAL_BIN" 2>&1 | grep -q 'not a dynamic executable'; then
      echo '✓ Binary is fully static'
    else
      echo '⚠ Warning: Binary may have dynamic dependencies'
      echo 'Dynamic dependencies:'
      ldd "$ACTUAL_BIN" 2>&1 || true
    fi
    echo ''
    echo 'Binary info:'
    file "$ACTUAL_BIN"
  else
    echo '✗ Binary not found'
    exit 1
  fi
else
  echo '✗ Static build failed'
  exit 1
fi
