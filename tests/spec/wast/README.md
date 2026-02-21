# WebAssembly Spec Test Fixtures (.wast)

These `.wast` files are copied from the official WebAssembly spec test suite.
They are checked in directly rather than using git submodules.

## Source

- Repository: https://github.com/WebAssembly/spec
- Tag: `wg-2.0` (W3C Working Group 2.0 release)
- Date: 2025-08-28

This tag covers WebAssembly 1.0 plus the bulk memory, reference types, and SIMD
proposals — matching the feature set implemented by kasm.

## Contents

- `*.wast` — 90 core WebAssembly spec tests
- `simd/*.wast` — 58 SIMD proposal spec tests

## Updating

To update these fixtures from a newer spec tag:

1. Clone or fetch the spec repo
2. Diff the new tests against these copies to review changes
3. Copy updated files and update the tag reference above
4. Run `cargo test --test wast_tests` to verify
