//! AST types for WebAssembly Script Test (.wast) files.
//!
//! A .wast file is a sequence of commands that define modules, register them
//! for cross-module linking, invoke exported functions, and assert expected
//! behaviour (return values, traps, validation failures, etc.).

use crate::wat::Span;

/// A parsed .wast script containing a sequence of test commands.
#[derive(Debug)]
pub struct WastScript {
    pub commands: Vec<WastCommand>,
}

/// A top-level command in a .wast file.
#[derive(Debug)]
pub enum WastCommand {
    /// Define a module, optionally named.
    Module {
        span: Span,
        name: Option<String>,
        module: WastModule,
    },

    /// Register the most recent (or named) module under a string name for imports.
    Register {
        span: Span,
        as_name: String,
        module_name: Option<String>,
    },

    /// Top-level action (invoke without assertion).
    Action { span: Span, action: WastAction },

    /// Assert that an action returns expected values.
    AssertReturn {
        span: Span,
        action: WastAction,
        expected: Vec<WastValue>,
    },

    /// Assert that an action traps with the expected message.
    AssertTrap {
        span: Span,
        action: WastAction,
        message: String,
    },

    /// Assert that module instantiation traps.
    AssertModuleTrap {
        span: Span,
        module: WastModule,
        message: String,
    },

    /// Assert that a module fails validation.
    AssertInvalid {
        span: Span,
        module: WastModule,
        message: String,
    },

    /// Assert that a module fails to parse.
    AssertMalformed {
        span: Span,
        module: WastModule,
        message: String,
    },

    /// Assert that a module fails to link (import resolution).
    AssertUnlinkable {
        span: Span,
        module: WastModule,
        message: String,
    },

    /// Assert that module instantiation fails (after linking).
    AssertUninstantiable {
        span: Span,
        module: WastModule,
        message: String,
    },

    /// Assert that an action exhausts resources (e.g. call stack).
    AssertExhaustion {
        span: Span,
        action: WastAction,
        message: String,
    },
}

/// How a module is provided in a .wast file.
#[derive(Debug)]
pub enum WastModule {
    /// Inline WAT text: `(module ...)`.
    /// Contains the raw source text, to be parsed by `kasm::wat::parse()`.
    Wat(String),

    /// Binary module: `(module binary "\00asm" ...)`.
    /// Concatenated byte values from the string literals.
    Binary(Vec<u8>),

    /// Quoted text module: `(module quote "(func ...)" ...)`.
    /// Concatenated string contents, to be parsed as WAT.
    Quote(String),
}

impl WastModule {
    /// Return the WAT source text for this module, if available.
    ///
    /// - `Wat` returns its source directly.
    /// - `Quote` normalises by wrapping in `(module ...)` if the text lacks it.
    /// - `Binary` returns `None` (no text source).
    pub fn to_wat_source(&self) -> Option<String> {
        match self {
            WastModule::Wat(source) => Some(source.clone()),
            WastModule::Quote(source) => {
                if source.trim_start().starts_with("(module") {
                    Some(source.clone())
                } else {
                    Some(format!("(module {source})"))
                }
            }
            WastModule::Binary(_) => None,
        }
    }
}

/// An action within an assertion or at top level.
#[derive(Debug)]
pub enum WastAction {
    /// Invoke an exported function: `(invoke $mod? "name" args...)`.
    Invoke {
        module: Option<String>,
        name: String,
        args: Vec<WastValue>,
    },

    /// Read an exported global: `(get $mod? "name")`.
    Get { module: Option<String>, name: String },
}

/// A typed constant value used as an argument or expected result.
#[derive(Debug)]
pub enum WastValue {
    I32(u32),
    I64(u64),
    F32(WastFloat<u32>),
    F64(WastFloat<u64>),
    V128 { lane_type: String, lanes: Vec<WastLane> },
    RefNull(String),
    RefFunc,
    RefExtern(Option<u32>),
}

/// A floating-point value that may be a NaN pattern.
#[derive(Debug)]
pub enum WastFloat<T> {
    Value(T),
    NanCanonical,
    NanArithmetic,
}

/// A single lane value within a v128 constant.
#[derive(Debug)]
pub enum WastLane {
    /// Integer lane (i8, i16, i32, i64).
    Integer(u64),
    /// f32 lane (may be NaN pattern).
    F32(WastFloat<u32>),
    /// f64 lane (may be NaN pattern).
    F64(WastFloat<u64>),
}
