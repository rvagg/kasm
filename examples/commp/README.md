# CommP - Filecoin Piece Commitment

A WASI example implementing Filecoin's CommP (Piece Commitment) algorithm. This is a real-world cryptographic hash used in Filecoin storage deals.

## Usage

```bash
# Build for WASI
cargo build --target wasm32-wasip1 --release

# Run with kasm
cat file.bin | kasm run target/wasm32-wasip1/release/commp.wasm

# Output: 64-character hex CommP root
ea94b28b4c72336a925aa555376cbca087b9aae7cf16bc69eb19e913106f6f0c
```

## Algorithm Overview

CommP is a SHA256-based binary merkle tree with two quirks required for Filecoin's proof system:

1. **FR32 expansion**: Input is expanded from 127 to 128 bytes per block by inserting 2 zero bits every 254 bits. This ensures all 32-byte chunks are valid BLS12-381 field elements.

2. **254-bit truncation**: All SHA256 hashes have their top 2 bits zeroed, keeping values within the field.

```
Input bytes (streaming)
       ↓
FR32 expand (127 → 128 bytes)
       ↓
Split into 32-byte leaves (no leaf hashing - chunks ARE leaves)
       ↓
Binary merkle tree with SHA256-trunc254 internal nodes
       ↓
32-byte CommP root
```

## Why This Example

- **Real-world algorithm**: Used in production Filecoin since 2020
- **SHA256-heavy**: Good stress test for interpreter performance
- **Streaming input**: Processes stdin in 8KB chunks
- **Pure Rust**: No external dependencies beyond `sha2` crate
- **SIMD-enabled**: Compiled with `+simd128` for SIMD-accelerated SHA256
- **Compact WASM**: ~67KB release binary

## Test Vectors

From the [reference implementation](https://github.com/filecoin-project/go-fil-commp-hashhash):

| Input | CommP |
|-------|-------|
| 127 × 0x42 | `ea94b28b4c72336a925aa555376cbca087b9aae7cf16bc69eb19e913106f6f0c` |
| 254 × 0x42 | `3f3019433e31133007948d56fe896fdbb42b6ecfe430e22728b49ca9355af30b` |
| 508 × 0x42 | `004f6f290bdcc62e84ed8f2c88a3fa713709a5382f70d79ae473c0cdcca7d131` |

## Building

```bash
# Native (for testing)
cargo test --release

# WASI target (with SIMD)
rustup target add wasm32-wasip1
RUSTFLAGS="-C target-feature=+simd128" cargo build --target wasm32-wasip1 --release
```
