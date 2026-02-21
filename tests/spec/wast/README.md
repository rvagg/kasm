# WebAssembly Spec Test Fixtures (.wast)

These `.wast` files are copied from the official WebAssembly spec test suite.
They are checked in directly rather than using git submodules.

## Source

- Repository: https://github.com/WebAssembly/spec
- Commit: `cfdbf3b53fc66cb6b01eb5d0a9ae7e618f87fb2b`
- Date: 2024-02-15

## Contents

- `*.wast` — 90 core WebAssembly 1.0 spec tests
- `simd/*.wast` — 57 SIMD proposal spec tests

## Updating

To update these fixtures from a newer spec commit:

1. Clone or fetch the spec repo
2. Diff the new tests against these copies to review changes
3. Copy updated files and update the commit hash above
4. Run `cargo test --test wast_tests -- --ignored` to verify
