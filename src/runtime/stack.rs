//! WebAssembly value stack implementation

use super::{RuntimeError, Value};
use crate::parser::module::ValueType;

/// The WebAssembly value stack
#[derive(Debug, Default)]
pub struct Stack {
    values: Vec<Value>,
}

impl Stack {
    /// Create a new empty stack
    pub fn new() -> Self {
        Stack { values: Vec::new() }
    }

    /// Push a value onto the stack
    pub fn push(&mut self, value: Value) {
        self.values.push(value);
    }

    /// Push multiple values onto the stack
    pub fn push_all(&mut self, values: impl IntoIterator<Item = Value>) {
        self.values.extend(values);
    }

    /// Pop a value from the stack
    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.values.pop().ok_or(RuntimeError::StackUnderflow)
    }

    /// Pop a value and check its type
    pub fn pop_typed(&mut self, expected_type: ValueType) -> Result<Value, RuntimeError> {
        let value = self.pop()?;
        if value.typ() != expected_type {
            return Err(RuntimeError::TypeMismatch {
                expected: format!("{expected_type:?}"),
                actual: format!("{:?}", value.typ()),
            });
        }
        Ok(value)
    }

    /// Pop an i32 value
    pub fn pop_i32(&mut self) -> Result<i32, RuntimeError> {
        self.pop_typed(ValueType::I32)?
            .as_i32()
            .ok_or(RuntimeError::TypeMismatch {
                expected: "i32".to_string(),
                actual: "non-i32".to_string(),
            })
    }

    /// Pop an i64 value
    pub fn pop_i64(&mut self) -> Result<i64, RuntimeError> {
        self.pop_typed(ValueType::I64)?
            .as_i64()
            .ok_or(RuntimeError::TypeMismatch {
                expected: "i64".to_string(),
                actual: "non-i64".to_string(),
            })
    }

    /// Pop an f32 value
    pub fn pop_f32(&mut self) -> Result<f32, RuntimeError> {
        self.pop_typed(ValueType::F32)?
            .as_f32()
            .ok_or(RuntimeError::TypeMismatch {
                expected: "f32".to_string(),
                actual: "non-f32".to_string(),
            })
    }

    /// Pop an f64 value
    pub fn pop_f64(&mut self) -> Result<f64, RuntimeError> {
        self.pop_typed(ValueType::F64)?
            .as_f64()
            .ok_or(RuntimeError::TypeMismatch {
                expected: "f64".to_string(),
                actual: "non-f64".to_string(),
            })
    }

    /// Get the current stack depth
    pub fn depth(&self) -> usize {
        self.values.len()
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// Clear the stack
    pub fn clear(&mut self) {
        self.values.clear();
    }

    /// Get all values (used for returning results)
    pub fn drain(&mut self) -> Vec<Value> {
        self.values.drain(..).collect()
    }

    /// Peek at the top value without popping
    pub fn peek(&self) -> Option<&Value> {
        self.values.last()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_pop() {
        let mut stack = Stack::new();

        stack.push(Value::I32(42));
        stack.push(Value::I64(100));

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.pop().unwrap(), Value::I64(100));
        assert_eq!(stack.pop().unwrap(), Value::I32(42));
        assert!(stack.pop().is_err());
    }

    #[test]
    fn test_push_all() {
        let mut stack = Stack::new();
        let values = vec![Value::I32(1), Value::I32(2), Value::I32(3)];

        stack.push_all(values);
        assert_eq!(stack.depth(), 3);
        assert_eq!(stack.pop().unwrap(), Value::I32(3));
    }

    #[test]
    fn test_pop_typed() {
        let mut stack = Stack::new();
        stack.push(Value::I32(42));

        // Correct type
        assert_eq!(stack.pop_typed(ValueType::I32).unwrap(), Value::I32(42));

        // Wrong type
        stack.push(Value::I32(42));
        assert!(stack.pop_typed(ValueType::I64).is_err());
    }

    #[test]
    fn test_typed_pop_methods() {
        let mut stack = Stack::new();

        stack.push(Value::I32(42));
        assert_eq!(stack.pop_i32().unwrap(), 42);

        stack.push(Value::I64(100));
        assert_eq!(stack.pop_i64().unwrap(), 100);

        stack.push(Value::F32(1.5));
        assert_eq!(stack.pop_f32().unwrap(), 1.5);

        stack.push(Value::F64(2.5));
        assert_eq!(stack.pop_f64().unwrap(), 2.5);
    }

    #[test]
    fn test_peek() {
        let mut stack = Stack::new();
        assert!(stack.peek().is_none());

        stack.push(Value::I32(42));
        assert_eq!(stack.peek(), Some(&Value::I32(42)));
        assert_eq!(stack.depth(), 1); // Peek doesn't remove
    }

    #[test]
    fn test_drain() {
        let mut stack = Stack::new();
        stack.push(Value::I32(1));
        stack.push(Value::I32(2));
        stack.push(Value::I32(3));

        let values = stack.drain();
        assert_eq!(values, vec![Value::I32(1), Value::I32(2), Value::I32(3)]);
        assert!(stack.is_empty());
    }

    #[test]
    fn test_clear() {
        let mut stack = Stack::new();
        stack.push(Value::I32(42));
        assert!(!stack.is_empty());

        stack.clear();
        assert!(stack.is_empty());
    }
}
