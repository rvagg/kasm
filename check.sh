#!/bin/bash
# Run all checks for KASM project

set -e  # Exit on error

echo "=== Running cargo check ==="
cargo check

echo -e "\n=== Running rustfmt check ==="
cargo fmt -- --check

echo -e "\n=== Running clippy ==="
cargo clippy -- -D warnings

echo -e "\n=== Running tests ==="
cargo test

echo -e "\n=== All checks passed! ==="