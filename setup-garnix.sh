#!/bin/bash

# 1. Verify this is a Nix Flake project (Required for Garnix)
if [ ! -f "flake.nix" ]; then
  echo "❌ Error: No 'flake.nix' found in the current directory."
  echo "   Garnix requires the project to be a Nix Flake."
  echo "   Please convert your project to a flake or run this script from the root."
  exit 1
fi

# 2. (Optional) Create a basic garnix.yaml to be explicit
# This tells Garnix to cache everything, which solves your disk space issues.
if [ ! -f "garnix.yaml" ]; then
  echo "Creating garnix.yaml configuration..."
  cat <<EOF > garnix.yaml
# Garnix Configuration
# Documentation: https://garnix.io/docs/yaml
builds:
  # Build and cache all packages, checks, and devShells
  include:
    - "*"
EOF
  echo "✅ 'garnix.yaml' created."
  echo "👉 ACTION REQUIRED: Run 'git add garnix.yaml' and commit it."
else
  echo "✅ 'garnix.yaml' already exists."
fi

# 3. Print the Integration Instructions
echo ""
echo "=========================================================="
echo "   🚀 READY TO INTEGRATE WITH GARNIX (Free for OSS)"
echo "=========================================================="
echo ""
echo "1. Ensure your repository is PUBLIC (Garnix is free for public repos)."
echo "2. Commit and push your changes (including the new garnix.yaml)."
echo "3. Visit one of the links below to install the Garnix GitHub App:"
echo ""
echo "   🔗 Recommended: https://garnix.io/signup"
echo "   (This will guide you through the installation process)"
echo ""
echo "   🔗 Direct install: https://github.com/apps/garnix/installations/new"
echo ""
echo "   - Click 'Install'"
echo "   - Select 'Only select repositories'"
echo "   - Choose this repository"
echo "   - Click 'Install' again"
echo ""
echo "🎉 Once installed, the first build will start automatically."
echo "   You can view the logs at: https://garnix.io/dash"
echo "=========================================================="
