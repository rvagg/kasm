//! Common test utilities shared between integration tests

use std::io::Write;
use std::sync::{Arc, Mutex};

/// Captured writer that stores output for testing
pub struct CapturedWriter(pub Arc<Mutex<Vec<u8>>>);

impl Write for CapturedWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
