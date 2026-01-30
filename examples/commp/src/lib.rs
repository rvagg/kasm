//! Filecoin CommP (Piece Commitment) Implementation
//!
//! CommP is a content identifier for Filecoin pieces, computed as:
//! 1. FR32 padding: insert 2 zero bits every 254 bits
//! 2. SHA256 merkle tree with 254-bit truncation (top 2 bits zeroed)
//!
//! Based on the Filecoin specification and go-fil-commp-hashhash.

use sha2::{Digest, Sha256};

/// Size of a merkle tree node (32 bytes)
const NODE_SIZE: usize = 32;

/// Input bytes per quad (127 bytes = 4 * 254 bits / 8)
const IN_BYTES_PER_QUAD: usize = 127;

/// Output bytes per quad after FR32 padding (128 bytes)
const OUT_BYTES_PER_QUAD: usize = 128;

/// Minimum payload size (65 bytes) - kept for spec reference
#[allow(dead_code)]
const MIN_PAYLOAD_SIZE: usize = 65;

/// Maximum tree levels (supports up to 64 GiB)
const MAX_LEVEL: usize = 64;

/// Pre-computed zero commitment nodes for each level
fn get_zero_comm(level: usize) -> [u8; NODE_SIZE] {
    static ZERO_COMMS: std::sync::OnceLock<[[u8; NODE_SIZE]; MAX_LEVEL]> = std::sync::OnceLock::new();

    ZERO_COMMS.get_or_init(|| {
        let mut comms = [[0u8; NODE_SIZE]; MAX_LEVEL];
        let mut concat = [0u8; NODE_SIZE * 2];

        for i in 1..MAX_LEVEL {
            concat[..NODE_SIZE].copy_from_slice(&comms[i - 1]);
            concat[NODE_SIZE..].copy_from_slice(&comms[i - 1]);
            comms[i] = truncated_hash(&concat);
        }
        comms
    })[level]
}

/// Compute SHA256 hash with top 2 bits zeroed (254-bit truncation)
#[inline(always)]
fn truncated_hash(data: &[u8]) -> [u8; NODE_SIZE] {
    let mut hasher = Sha256::new();
    hasher.update(data);
    let mut result: [u8; NODE_SIZE] = hasher.finalize().into();
    // Zero top 2 bits for BLS12-381 field element compatibility
    result[NODE_SIZE - 1] &= 0b00111111;
    result
}

/// FR32 pad a 127-byte quad into a 128-byte output buffer
///
/// FR32 inserts 2 zero bits every 254 bits (31.75 bytes), ensuring
/// all 32-byte chunks are valid BLS12-381 field elements.
#[inline(always)]
fn fr32_pad(source: &[u8], output: &mut [u8; OUT_BYTES_PER_QUAD]) {
    // First Fr element (bytes 0-31): copy directly, clear top 2 bits
    output[..32].copy_from_slice(&source[..32]);
    output[31] &= 0b00111111;

    // Second Fr element (bytes 32-63): shift left by 2 bits
    for i in 32..64 {
        output[i] = (source[i] << 2) | (source[i - 1] >> 6);
    }
    output[63] &= 0b00111111;

    // Third Fr element (bytes 64-95): shift left by 4 bits
    for i in 64..96 {
        output[i] = (source[i] << 4) | (source[i - 1] >> 4);
    }
    output[95] &= 0b00111111;

    // Fourth Fr element (bytes 96-127): shift left by 6 bits
    for i in 96..127 {
        output[i] = (source[i] << 6) | (source[i - 1] >> 2);
    }
    // Last byte: top 6 bits of source[126] shifted right
    output[127] = source[126] >> 2;
}

/// Process a 127-byte quad: FR32 pad and hash to produce 2 leaf nodes
#[inline(always)]
fn process_quad(source: &[u8], padded: &mut [u8; OUT_BYTES_PER_QUAD]) -> ([u8; NODE_SIZE], [u8; NODE_SIZE]) {
    fr32_pad(source, padded);
    (truncated_hash(&padded[..64]), truncated_hash(&padded[64..]))
}

/// Streaming CommP hasher
pub struct CommPHasher {
    /// Buffer for accumulating partial quads
    buffer: [u8; IN_BYTES_PER_QUAD],
    /// Current offset into buffer
    offset: usize,
    /// Total bytes written
    bytes_written: u64,
    /// Collected leaf nodes
    leaves: Vec<[u8; NODE_SIZE]>,
    /// Reusable FR32 padding buffer
    pad_buffer: [u8; OUT_BYTES_PER_QUAD],
}

impl CommPHasher {
    /// Create a new hasher
    pub fn new() -> Self {
        CommPHasher {
            buffer: [0u8; IN_BYTES_PER_QUAD],
            offset: 0,
            bytes_written: 0,
            leaves: Vec::with_capacity(16384),
            pad_buffer: [0u8; OUT_BYTES_PER_QUAD],
        }
    }

    /// Write bytes into the hasher
    pub fn write(&mut self, bytes: &[u8]) {
        if bytes.is_empty() {
            return;
        }

        let len = bytes.len();
        self.bytes_written += len as u64;

        // Fast path: if we can't complete a quad, just buffer
        if self.offset + len < IN_BYTES_PER_QUAD {
            self.buffer[self.offset..self.offset + len].copy_from_slice(bytes);
            self.offset += len;
            return;
        }

        let mut read_pos = 0;

        // Complete the buffered quad if we have partial data
        if self.offset > 0 {
            let bytes_needed = IN_BYTES_PER_QUAD - self.offset;
            self.buffer[self.offset..].copy_from_slice(&bytes[..bytes_needed]);
            read_pos = bytes_needed;

            let (leaf1, leaf2) = process_quad(&self.buffer, &mut self.pad_buffer);
            self.leaves.push(leaf1);
            self.leaves.push(leaf2);
            self.offset = 0;
        }

        // Process full quads directly from input
        while read_pos + IN_BYTES_PER_QUAD <= len {
            let (leaf1, leaf2) = process_quad(
                &bytes[read_pos..read_pos + IN_BYTES_PER_QUAD],
                &mut self.pad_buffer,
            );
            self.leaves.push(leaf1);
            self.leaves.push(leaf2);
            read_pos += IN_BYTES_PER_QUAD;
        }

        // Buffer remaining bytes
        let remaining = len - read_pos;
        if remaining > 0 {
            self.buffer[..remaining].copy_from_slice(&bytes[read_pos..]);
            self.offset = remaining;
        }
    }

    /// Build final tree and return (height, root)
    fn build(&mut self) -> (u8, [u8; NODE_SIZE]) {
        // Process any remaining buffered data
        if self.offset > 0 || self.bytes_written == 0 {
            // Zero-fill the rest of the buffer
            self.buffer[self.offset..].fill(0);
            let (leaf1, leaf2) = process_quad(&self.buffer, &mut self.pad_buffer);
            self.leaves.push(leaf1);
            self.leaves.push(leaf2);
        }

        let num_leaves = self.leaves.len();
        if num_leaves == 0 {
            return (0, [0u8; NODE_SIZE]);
        }

        // Build tree level by level using double-buffering
        let mut current = std::mem::take(&mut self.leaves);
        let mut next: Vec<[u8; NODE_SIZE]> = Vec::with_capacity(num_leaves / 2 + 1);
        let mut concat_buf = [0u8; 64];
        let mut height: u8 = 0;

        while current.len() > 1 {
            // Pad with zero commitment if odd number
            if current.len() % 2 == 1 {
                current.push(get_zero_comm(height as usize + 1));
            }

            // Combine pairs into parent nodes
            next.clear();
            next.reserve(current.len() / 2);

            let mut i = 0;
            while i < current.len() {
                concat_buf[..NODE_SIZE].copy_from_slice(&current[i]);
                concat_buf[NODE_SIZE..].copy_from_slice(&current[i + 1]);
                next.push(truncated_hash(&concat_buf));
                i += 2;
            }

            // Swap buffers
            std::mem::swap(&mut current, &mut next);
            height += 1;
        }

        let root = if current.is_empty() {
            [0u8; NODE_SIZE]
        } else {
            current[0]
        };

        // Restore leaves vector for potential reuse
        self.leaves = current;
        self.leaves.clear();

        (height, root)
    }

    /// Get the 32-byte CommP root hash
    pub fn root(&mut self) -> [u8; NODE_SIZE] {
        let (_, root) = self.build();
        root
    }

    /// Get the tree height
    pub fn height(&mut self) -> u8 {
        let (height, _) = self.build();
        height
    }

    /// Get bytes written count
    pub fn count(&self) -> u64 {
        self.bytes_written
    }

    /// Reset the hasher for reuse
    pub fn reset(&mut self) {
        self.buffer.fill(0);
        self.offset = 0;
        self.bytes_written = 0;
        self.leaves.clear();
    }
}

impl Default for CommPHasher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hex_to_bytes(hex: &str) -> [u8; 32] {
        let mut bytes = [0u8; 32];
        for (i, chunk) in hex.as_bytes().chunks(2).enumerate() {
            let s = std::str::from_utf8(chunk).unwrap();
            bytes[i] = u8::from_str_radix(s, 16).unwrap();
        }
        bytes
    }

    #[test]
    fn test_basic() {
        let mut hasher = CommPHasher::new();
        hasher.write(&[0x42u8; 127]);
        let root = hasher.root();
        assert_eq!(root.len(), 32);
    }

    #[test]
    fn test_pow2_vectors() {
        // Test vectors from Go reference implementation
        let vectors = [
            (127, 0x42u8, "ea94b28b4c72336a925aa555376cbca087b9aae7cf16bc69eb19e913106f6f0c"),
            (254, 0x42u8, "3f3019433e31133007948d56fe896fdbb42b6ecfe430e22728b49ca9355af30b"),
            (508, 0x42u8, "004f6f290bdcc62e84ed8f2c88a3fa713709a5382f70d79ae473c0cdcca7d131"),
        ];

        for (size, fill, expected_hex) in vectors {
            let data: Vec<u8> = vec![fill; size];
            let mut hasher = CommPHasher::new();
            hasher.write(&data);
            let root = hasher.root();
            let expected = hex_to_bytes(expected_hex);
            assert_eq!(root, expected, "Mismatch for size {}", size);
        }
    }

    #[test]
    fn test_non_pow2_vectors() {
        // Test vectors for non-power-of-2 leaf counts
        let vectors = [
            (381, 0x42u8, "967973c08aa3536fd80f4e4d9b5fa80d1c5152370a4fee45d10d7d6b91063926"),
            (635, 0x42u8, "41b56a1128bf6c1e6ce307f7f2630ab53548fa85a12f176c590cb21aa158f01e"),
            (762, 0x42u8, "978a7244b8a81fb61081444fa4fa51a73b33fb546471548de2a74f667da68f01"),
        ];

        for (size, fill, expected_hex) in vectors {
            let data: Vec<u8> = vec![fill; size];
            let mut hasher = CommPHasher::new();
            hasher.write(&data);
            let root = hasher.root();
            let expected = hex_to_bytes(expected_hex);
            assert_eq!(root, expected, "Mismatch for size {}", size);
        }
    }
}
