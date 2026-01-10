//! WebAssembly table implementation
//!
//! Tables are typed vectors of references (function refs or external refs) that enable
//! indirect function calls and reference storage in WebAssembly. They are a critical
//! component for dynamic dispatch and polymorphism.
//!
//! ## Security Considerations
//!
//! Tables are security-critical because they enable indirect function calls. The type
//! checking in `call_indirect` must be rigorous - any type mismatch must trap to prevent
//! calling functions with incorrect signatures.
//!
//! ## Usage
//!
//! ```rust,ignore
//! use crate::runtime::table::Table;
//! use crate::parser::module::{RefType, Limits};
//!
//! // Create a table with min 10, max 100 elements
//! let limits = Limits { min: 10, max: Some(100) };
//! let mut table = Table::new(RefType::FuncRef, limits)?;
//!
//! // Get/set elements
//! table.set(0, Some(Value::FuncRef(Some(5))))?;
//! let elem = table.get(0)?;
//!
//! // Grow table
//! let old_size = table.grow(5, None)?;
//! ```

use super::{RuntimeError, Value};
use crate::parser::module::{Limits, RefType};

/// A WebAssembly table - a typed vector of references
pub struct Table {
    /// The type of references this table holds (FuncRef or ExternRef)
    ref_type: RefType,

    /// The actual elements (Option because slots can be null)
    /// - For FuncRef: Some(Value::FuncRef(Some(func_idx))) or None/Value::FuncRef(None)
    /// - For ExternRef: Some(Value::ExternRef(...)) or None
    elements: Vec<Option<Value>>,

    /// Size limits (min is initial size, max is optional maximum)
    limits: Limits,
}

impl Table {
    /// Create a new table with the given reference type and size limits
    ///
    /// The table is initialised with `limits.min` null references.
    pub fn new(ref_type: RefType, limits: Limits) -> Result<Self, RuntimeError> {
        let initial_size = limits.min as usize;
        let elements = vec![None; initial_size]; // Initialise with null references

        Ok(Table {
            ref_type,
            elements,
            limits,
        })
    }

    /// Get the current table size (number of elements)
    pub fn size(&self) -> u32 {
        self.elements.len() as u32
    }

    /// Get the element at the given index
    ///
    /// Returns the value or an appropriate null value if the slot is empty.
    ///
    /// # Errors
    ///
    /// Returns `TableIndexOutOfBounds` if the index is out of bounds.
    pub fn get(&self, index: u32) -> Result<Value, RuntimeError> {
        let elem = self
            .elements
            .get(index as usize)
            .ok_or(RuntimeError::TableIndexOutOfBounds(index))?;

        // Return the value or appropriate null based on table type
        Ok(match elem {
            Some(val) => val.clone(),
            None => match self.ref_type {
                RefType::FuncRef => Value::FuncRef(None),
                RefType::ExternRef => Value::ExternRef(None),
            },
        })
    }

    /// Set the element at the given index
    ///
    /// # Errors
    ///
    /// - Returns `TableIndexOutOfBounds` if the index is out of bounds.
    /// - Returns `TypeMismatch` if the value type doesn't match the table's reference type.
    pub fn set(&mut self, index: u32, value: Option<Value>) -> Result<(), RuntimeError> {
        // Validate type if value is present
        if let Some(val) = &value {
            self.validate_element(val)?;
        }

        let elem = self
            .elements
            .get_mut(index as usize)
            .ok_or(RuntimeError::TableIndexOutOfBounds(index))?;

        *elem = value;
        Ok(())
    }

    /// Grow the table by delta elements, initialising new slots with init_value
    ///
    /// Returns the old size on success, or u32::MAX (representing -1 as i32) if growth fails.
    ///
    /// # Errors
    ///
    /// - Returns `TypeMismatch` if init_value type doesn't match the table's reference type.
    pub fn grow(&mut self, delta: u32, init_value: Option<Value>) -> Result<u32, RuntimeError> {
        let old_size = self.elements.len() as u32;

        // Check for overflow - return failure (-1) instead of error
        let new_size = match old_size.checked_add(delta) {
            Some(size) => size,
            None => return Ok(u32::MAX), // Overflow: return -1
        };

        // Check against max limit if present
        if let Some(max) = self.limits.max
            && new_size > max
        {
            return Ok(u32::MAX); // Exceeds max: return -1
        }

        // Validate init value type
        if let Some(val) = &init_value {
            self.validate_element(val)?;
        }

        // Grow the table
        self.elements.resize(new_size as usize, init_value);
        Ok(old_size)
    }

    /// Fill a range of table elements with a value
    ///
    /// # Errors
    ///
    /// - Returns `TableIndexOutOfBounds` if the range is out of bounds or would overflow.
    /// - Returns `TypeMismatch` if the value type doesn't match the table's reference type.
    pub fn fill(&mut self, start: u32, count: u32, value: Option<Value>) -> Result<(), RuntimeError> {
        let end = start
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;

        if end > self.size() {
            return Err(RuntimeError::TableIndexOutOfBounds(end));
        }

        // Validate value type
        if let Some(val) = &value {
            self.validate_element(val)?;
        }

        for i in start..end {
            self.elements[i as usize] = value.clone();
        }
        Ok(())
    }

    /// Initialize table from element segment data
    ///
    /// # Errors
    ///
    /// - Returns `TableIndexOutOfBounds` if the range is out of bounds.
    /// - Returns `TypeMismatch` if any value type doesn't match the table's reference type.
    pub fn init(&mut self, dst_idx: u32, src: &[Option<Value>], src_idx: u32, count: u32) -> Result<(), RuntimeError> {
        let src_end = (src_idx + count) as usize;
        let dst_end = dst_idx
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;

        if src_end > src.len() || dst_end > self.size() {
            return Err(RuntimeError::TableIndexOutOfBounds(dst_end));
        }

        for i in 0..count {
            let src_val = &src[(src_idx + i) as usize];
            if let Some(val) = src_val {
                self.validate_element(val)?;
            }
            self.elements[(dst_idx + i) as usize] = src_val.clone();
        }
        Ok(())
    }

    /// Copy within the same table or from another table
    ///
    /// # Errors
    ///
    /// - Returns `TableIndexOutOfBounds` if ranges are out of bounds.
    pub fn copy_within(&mut self, dst_idx: u32, src_idx: u32, count: u32) -> Result<(), RuntimeError> {
        let src_end = src_idx
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;
        let dst_end = dst_idx
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;

        if src_end > self.size() || dst_end > self.size() {
            return Err(RuntimeError::TableIndexOutOfBounds(src_end.max(dst_end)));
        }

        if count > 0 {
            // Clone the range first to handle overlaps correctly
            let temp: Vec<_> = self.elements[src_idx as usize..src_end as usize].to_vec();
            self.elements[dst_idx as usize..dst_end as usize].clone_from_slice(&temp);
        }

        Ok(())
    }

    /// Copy from another table into this table
    ///
    /// This is optimised for cross-table copies, using bulk slice operations
    /// instead of element-by-element copying.
    ///
    /// # Errors
    ///
    /// - Returns `TableIndexOutOfBounds` if ranges are out of bounds.
    /// - Returns `TypeMismatch` if the table reference types don't match.
    pub fn copy_from(&mut self, dst_idx: u32, src: &Table, src_idx: u32, count: u32) -> Result<(), RuntimeError> {
        // Validate types match (ONE type check)
        if std::mem::discriminant(&self.ref_type) != std::mem::discriminant(&src.ref_type) {
            return Err(RuntimeError::TypeMismatch {
                expected: format!("{:?}", self.ref_type),
                actual: format!("{:?}", src.ref_type),
            });
        }

        // Bounds checking (TWO bounds checks total)
        let src_end = src_idx
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;
        let dst_end = dst_idx
            .checked_add(count)
            .ok_or(RuntimeError::TableIndexOutOfBounds(u32::MAX))?;

        if src_end > src.size() || dst_end > self.size() {
            return Err(RuntimeError::TableIndexOutOfBounds(src_end.max(dst_end)));
        }

        // Direct bulk copy - O(n) clone only, no per-element overhead
        if count > 0 {
            let temp: Vec<_> = src.elements[src_idx as usize..src_end as usize].to_vec();
            self.elements[dst_idx as usize..dst_end as usize].clone_from_slice(&temp);
        }

        Ok(())
    }

    /// Validate that a value matches this table's reference type
    fn validate_element(&self, value: &Value) -> Result<(), RuntimeError> {
        match (&self.ref_type, value) {
            (RefType::FuncRef, Value::FuncRef(_)) => Ok(()),
            (RefType::ExternRef, Value::ExternRef(_)) => Ok(()),
            _ => Err(RuntimeError::TypeMismatch {
                expected: format!("{:?}", self.ref_type),
                actual: format!("{:?}", value.typ()),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::FuncAddr;

    #[test]
    fn test_table_creation() {
        // Create table with min 10, max 20
        let limits = Limits { min: 10, max: Some(20) };
        let table = Table::new(RefType::FuncRef, limits).unwrap();
        assert_eq!(table.size(), 10);

        // Create table with no max
        let limits = Limits { min: 5, max: None };
        let table = Table::new(RefType::ExternRef, limits).unwrap();
        assert_eq!(table.size(), 5);
    }

    #[test]
    fn test_table_get_set() {
        let limits = Limits { min: 10, max: Some(20) };
        let mut table = Table::new(RefType::FuncRef, limits).unwrap();

        // Initially all elements return FuncRef(None)
        assert_eq!(table.get(0).unwrap(), Value::FuncRef(None));

        // Set an element
        table.set(0, Some(Value::FuncRef(Some(FuncAddr(42))))).unwrap();
        assert_eq!(table.get(0).unwrap(), Value::FuncRef(Some(FuncAddr(42))));

        // Set to null explicitly
        table.set(0, Some(Value::FuncRef(None))).unwrap();
        assert_eq!(table.get(0).unwrap(), Value::FuncRef(None));

        // Set to None directly (clears the slot)
        table.set(0, None).unwrap();
        assert_eq!(table.get(0).unwrap(), Value::FuncRef(None));
    }

    #[test]
    fn test_table_bounds() {
        let limits = Limits { min: 10, max: Some(20) };
        let mut table = Table::new(RefType::FuncRef, limits).unwrap();

        // Out of bounds get
        let result = table.get(10);
        assert!(matches!(result, Err(RuntimeError::TableIndexOutOfBounds(10))));

        // Out of bounds set
        let result = table.set(10, Some(Value::FuncRef(Some(FuncAddr(1)))));
        assert!(matches!(result, Err(RuntimeError::TableIndexOutOfBounds(10))));
    }

    #[test]
    fn test_table_grow_success() {
        let limits = Limits { min: 10, max: Some(20) };
        let mut table = Table::new(RefType::FuncRef, limits).unwrap();

        // Grow by 5 (within max)
        let old_size = table.grow(5, None).unwrap();
        assert_eq!(old_size, 10);
        assert_eq!(table.size(), 15);

        // New elements are initialised to null
        assert_eq!(table.get(14).unwrap(), Value::FuncRef(None));

        // Grow with init value
        let old_size = table.grow(3, Some(Value::FuncRef(Some(FuncAddr(99))))).unwrap();
        assert_eq!(old_size, 15);
        assert_eq!(table.size(), 18);
        assert_eq!(table.get(17).unwrap(), Value::FuncRef(Some(FuncAddr(99))));
    }

    #[test]
    fn test_table_grow_failure() {
        let limits = Limits { min: 10, max: Some(20) };
        let mut table = Table::new(RefType::FuncRef, limits).unwrap();

        // Try to grow beyond max (10 + 11 = 21 > 20)
        let result = table.grow(11, None).unwrap();
        assert_eq!(result, u32::MAX); // Failure returns -1
        assert_eq!(table.size(), 10); // Size unchanged
    }

    #[test]
    fn test_table_type_validation() {
        let limits = Limits { min: 10, max: Some(20) };
        let mut table = Table::new(RefType::FuncRef, limits).unwrap();

        // Try to set wrong type (I32 instead of FuncRef)
        let result = table.set(0, Some(Value::I32(42)));
        assert!(matches!(result, Err(RuntimeError::TypeMismatch { .. })));

        // Try to grow with wrong type
        let result = table.grow(1, Some(Value::I64(100)));
        assert!(matches!(result, Err(RuntimeError::TypeMismatch { .. })));

        // Correct type works
        assert!(table.set(0, Some(Value::FuncRef(Some(FuncAddr(1))))).is_ok());
    }
}
