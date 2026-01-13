//! Implementation limits for WebAssembly parsing and execution.
//!
//! These limits are aligned with V8's limits from src/wasm/wasm-limits.h.
//! They prevent OOM attacks from malformed input claiming unrealistic counts,
//! and ensure compatibility with mainstream WebAssembly runtimes.

// =============================================================================
// Module-level limits
// =============================================================================

/// Maximum number of type definitions in a module
pub const MAX_TYPES: u32 = 1_000_000;

/// Maximum number of defined functions in a module
pub const MAX_FUNCTIONS: u32 = 1_000_000;

/// Maximum number of imports in a module
pub const MAX_IMPORTS: u32 = 1_000_000;

/// Maximum number of exports in a module
pub const MAX_EXPORTS: u32 = 1_000_000;

/// Maximum number of globals in a module
pub const MAX_GLOBALS: u32 = 1_000_000;

/// Maximum number of data segments in a module
pub const MAX_DATA_SEGMENTS: u32 = 100_000;

/// Maximum number of element segments in a module
pub const MAX_ELEMENT_SEGMENTS: u32 = 100_000;

/// Maximum number of tables in a module
pub const MAX_TABLES: u32 = 100_000;

// =============================================================================
// Function-level limits
// =============================================================================

/// Maximum function body size in bytes
pub const MAX_FUNCTION_SIZE: u32 = 7_654_321;

/// Maximum number of function parameters
pub const MAX_FUNCTION_PARAMS: u32 = 1_000;

/// Maximum number of function return values
pub const MAX_FUNCTION_RETURNS: u32 = 1_000;

/// Maximum number of local variables in a function
pub const MAX_FUNCTION_LOCALS: u32 = 50_000;

// =============================================================================
// Instruction-level limits
// =============================================================================

/// Maximum number of labels in a br_table instruction
pub const MAX_BR_TABLE_LABELS: u32 = 65_536;

/// Maximum number of value types in a select typed instruction
pub const MAX_SELECT_TYPED_VALUES: u32 = 1_000;

// =============================================================================
// Table limits
// =============================================================================

/// Maximum table size (number of elements)
pub const MAX_TABLE_SIZE: u32 = 10_000_000;

/// Maximum number of table init entries
pub const MAX_TABLE_INIT_ENTRIES: u32 = 10_000_000;

// =============================================================================
// Memory limits
// =============================================================================

/// Maximum memory pages for 32-bit addressing (4 GB)
pub const MAX_MEMORY_PAGES_32: u32 = 65_536;

// =============================================================================
// V8 also defines these limits for proposals not yet implemented:
//
// Exception handling proposal:
//   - MAX_TAGS = 1,000,000
//
// Multi-memory proposal:
//   - MAX_MEMORIES = 100,000
//
// Memory64 proposal (64-bit addressing):
//   - MAX_MEMORY_PAGES_64 = 262,144  (16 GB)
//
// GC proposal:
//   - MAX_STRUCT_FIELDS = 10,000
//   - MAX_RTT_SUBTYPING_DEPTH = 63  (runtime type subtyping depth)
//   - MAX_ARRAY_NEW_FIXED_LENGTH = 10,000
// =============================================================================
