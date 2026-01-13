#!/bin/bash
# Run all checks for KASM project

set -e  # Exit on error

FUZZ_DURATION=60  # Default fuzz duration in seconds
RUN_FUZZ=false

usage() {
    echo "Usage: $0 [-f [duration]] [-h]"
    echo "  -f [duration]  Run fuzzing after checks (default: ${FUZZ_DURATION}s)"
    echo "  -h             Show this help message"
    exit 0
}

while getopts "f:h" opt; do
    case $opt in
        f)
            RUN_FUZZ=true
            if [[ -n "$OPTARG" && "$OPTARG" =~ ^[0-9]+$ ]]; then
                FUZZ_DURATION="$OPTARG"
            fi
            ;;
        h)
            usage
            ;;
        *)
            usage
            ;;
    esac
done

echo "=== Running cargo check ==="
cargo check

echo -e "\n=== Running rustfmt check ==="
cargo fmt -- --check

echo -e "\n=== Running clippy ==="
cargo clippy -- -D warnings

echo -e "\n=== Running tests ==="
cargo test

echo -e "\n=== All checks passed! ==="

if $RUN_FUZZ; then
    echo -e "\n=== Running fuzzer for ${FUZZ_DURATION}s ==="
    if ! command -v cargo-fuzz &> /dev/null; then
        echo "cargo-fuzz not installed. Install with: cargo install cargo-fuzz"
        exit 1
    fi
    cd fuzz
    if cargo +nightly fuzz run parse_module -- -max_total_time="$FUZZ_DURATION"; then
        echo -e "\n=== Fuzzing complete, no issues found ==="
    else
        echo -e "\n=== Fuzzing found issues (see above) ==="
        exit 1
    fi
fi