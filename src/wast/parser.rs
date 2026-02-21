//! Parser for WebAssembly Script Test (.wast) files.
//!
//! Uses the S-expression reader to parse .wast files into a sequence of test
//! commands. Module bodies are extracted as raw source text (for WAT) or raw
//! bytes (for binary modules); actual parsing is deferred to the test runner,
//! since assertion commands like `assert_malformed` expect parsing to fail.

use super::command::*;
use crate::wat::sexpr::{self, ReadError, SExpr, SExprList};
use crate::wat::{FloatLit, SignedValue, Span, Token, TokenKind};
use std::fmt;

/// Errors encountered while parsing a .wast script.
#[derive(Debug)]
pub enum WastParseError {
    /// S-expression read error (lexer or structure).
    Read(ReadError),
    /// Invalid .wast command syntax.
    Syntax(WastSyntaxError),
}

/// A syntax error specific to the .wast command layer.
#[derive(Debug)]
pub struct WastSyntaxError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for WastParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WastParseError::Read(e) => write!(f, "{e}"),
            WastParseError::Syntax(e) => write!(f, "{}:{}: {}", e.span.line, e.span.column, e.message),
        }
    }
}

impl std::error::Error for WastParseError {}

impl From<ReadError> for WastParseError {
    fn from(e: ReadError) -> Self {
        WastParseError::Read(e)
    }
}

fn syntax_err(message: impl Into<String>, span: Span) -> WastParseError {
    WastParseError::Syntax(WastSyntaxError {
        message: message.into(),
        span,
    })
}

/// Keywords that indicate a bare module definition (no `(module ...)` wrapper).
const BARE_MODULE_KEYWORDS: &[&str] = &[
    "func", "memory", "table", "global", "type", "import", "export", "start", "elem", "data",
];

fn is_bare_module_keyword(kw: &str) -> bool {
    BARE_MODULE_KEYWORDS.contains(&kw)
}

/// Parse a .wast source string into a script of test commands.
pub fn parse_script(source: &str) -> Result<WastScript, WastParseError> {
    let sexprs = sexpr::read_all(source)?;
    let mut commands = Vec::new();
    let mut i = 0;

    while i < sexprs.len() {
        let sexpr = &sexprs[i];
        let list = sexpr
            .as_list()
            .ok_or_else(|| syntax_err("expected top-level command", sexpr.span()))?;

        let keyword = list
            .head_keyword()
            .ok_or_else(|| syntax_err("expected command keyword", list.span))?;

        let cmd = match keyword {
            "module" => parse_module_def(source, list)?,
            "register" => parse_register(list)?,
            "invoke" => WastCommand::Action {
                span: list.span,
                action: parse_action_from_list(source, list)?,
            },
            "assert_return" => parse_assert_return(source, list)?,
            "assert_trap" => parse_assert_trap(source, list)?,
            "assert_invalid" => parse_assert_module_cmd(source, list, "assert_invalid")?,
            "assert_malformed" => parse_assert_module_cmd(source, list, "assert_malformed")?,
            "assert_unlinkable" => parse_assert_module_cmd(source, list, "assert_unlinkable")?,
            "assert_uninstantiable" => parse_assert_module_cmd(source, list, "assert_uninstantiable")?,
            "assert_exhaustion" => parse_assert_exhaustion(source, list)?,
            kw if is_bare_module_keyword(kw) => {
                // Bare module syntax: consecutive top-level definitions form one module.
                let start_span = list.span;
                let mut parts = vec![&source[list.span.start..list.span.end]];
                while i + 1 < sexprs.len() {
                    if let Some(next_list) = sexprs[i + 1].as_list()
                        && let Some(next_kw) = next_list.head_keyword()
                        && is_bare_module_keyword(next_kw)
                    {
                        i += 1;
                        parts.push(&source[next_list.span.start..next_list.span.end]);
                        continue;
                    }
                    break;
                }
                let module_source = format!("(module {})", parts.join(" "));
                WastCommand::Module {
                    span: start_span,
                    name: None,
                    module: WastModule::Wat(module_source),
                }
            }
            other => return Err(syntax_err(format!("unknown command: {other}"), list.span)),
        };
        commands.push(cmd);
        i += 1;
    }

    Ok(WastScript { commands })
}

// ---------------------------------------------------------------------------
// Module definition
// ---------------------------------------------------------------------------

/// Parse `(module $name? ...)` / `(module $name? binary ...)` / `(module $name? quote ...)`.
fn parse_module_def(source: &str, list: SExprList<'_>) -> Result<WastCommand, WastParseError> {
    let mut idx = 1; // skip "module"

    // Optional $name
    let name = match list.get(idx) {
        Some(SExpr::Atom(token)) if matches!(&token.kind, TokenKind::Id(_)) => {
            idx += 1;
            if let TokenKind::Id(id) = &token.kind {
                Some(format!("${id}"))
            } else {
                None
            }
        }
        _ => None,
    };

    // Check for "binary" or "quote" keyword
    let module = match list.get(idx).and_then(|s| s.as_keyword()) {
        Some("binary") => {
            idx += 1;
            parse_module_binary(list, idx)?
        }
        Some("quote") => {
            idx += 1;
            parse_module_quote(list, idx)?
        }
        _ => {
            // Inline WAT module, extract the source text using span
            WastModule::Wat(source[list.span.start..list.span.end].to_string())
        }
    };

    Ok(WastCommand::Module {
        span: list.span,
        name,
        module,
    })
}

/// Parse `(module binary "str1" "str2" ...)`, concatenate string bytes.
fn parse_module_binary(list: SExprList<'_>, from: usize) -> Result<WastModule, WastParseError> {
    let mut bytes = Vec::new();
    for item in list.iter_from(from) {
        match item.as_atom().map(|t| &t.kind) {
            Some(TokenKind::String(b)) => bytes.extend_from_slice(b),
            _ => return Err(syntax_err("expected string literal in binary module", item.span())),
        }
    }
    Ok(WastModule::Binary(bytes))
}

/// Parse `(module quote "str1" "str2" ...)`, concatenate as UTF-8 text.
fn parse_module_quote(list: SExprList<'_>, from: usize) -> Result<WastModule, WastParseError> {
    let mut text = String::new();
    for item in list.iter_from(from) {
        match item.as_atom().map(|t| &t.kind) {
            Some(TokenKind::String(b)) => {
                // Quote strings represent WAT source text; interpret bytes as UTF-8
                // (with possible invalid sequences for malformed tests)
                text.push_str(&String::from_utf8_lossy(b));
            }
            _ => return Err(syntax_err("expected string literal in quoted module", item.span())),
        }
    }
    Ok(WastModule::Quote(text))
}

/// Extract a module from inside an assertion: `(module ...)` / `(module binary ...)` / `(module quote ...)`.
fn parse_inline_module(source: &str, sexpr: &SExpr) -> Result<WastModule, WastParseError> {
    let list = sexpr
        .as_list()
        .ok_or_else(|| syntax_err("expected module", sexpr.span()))?;
    list.expect_head("module").map_err(WastParseError::Read)?;

    let mut idx = 1;

    // Skip optional $name
    if let Some(SExpr::Atom(token)) = list.get(idx)
        && matches!(&token.kind, TokenKind::Id(_))
    {
        idx += 1;
    }

    match list.get(idx).and_then(|s| s.as_keyword()) {
        Some("binary") => parse_module_binary(list, idx + 1),
        Some("quote") => parse_module_quote(list, idx + 1),
        _ => Ok(WastModule::Wat(source[list.span.start..list.span.end].to_string())),
    }
}

// ---------------------------------------------------------------------------
// Register
// ---------------------------------------------------------------------------

/// Parse `(register "as_name" $module_name?)`.
fn parse_register(list: SExprList<'_>) -> Result<WastCommand, WastParseError> {
    let as_name = list
        .get(1)
        .and_then(|s| s.as_atom())
        .and_then(|t| match &t.kind {
            TokenKind::String(b) => String::from_utf8(b.clone()).ok(),
            _ => None,
        })
        .ok_or_else(|| syntax_err("expected string in register", list.span))?;

    let module_name = list.get(2).and_then(|s| s.as_id()).map(|id| format!("${id}"));

    Ok(WastCommand::Register {
        span: list.span,
        as_name,
        module_name,
    })
}

// ---------------------------------------------------------------------------
// Actions
// ---------------------------------------------------------------------------

/// Parse an action S-expression: `(invoke ...)` or `(get ...)`.
fn parse_action(source: &str, sexpr: &SExpr) -> Result<WastAction, WastParseError> {
    let list = sexpr
        .as_list()
        .ok_or_else(|| syntax_err("expected action", sexpr.span()))?;
    parse_action_from_list(source, list)
}

/// Parse action from an already-opened list.
fn parse_action_from_list(source: &str, list: SExprList<'_>) -> Result<WastAction, WastParseError> {
    let keyword = list
        .head_keyword()
        .ok_or_else(|| syntax_err("expected action keyword", list.span))?;

    match keyword {
        "invoke" => parse_invoke(source, list),
        "get" => parse_get(list),
        other => Err(syntax_err(format!("unknown action: {other}"), list.span)),
    }
}

/// Parse `(invoke $mod? "name" args...)`.
fn parse_invoke(source: &str, list: SExprList<'_>) -> Result<WastAction, WastParseError> {
    let mut idx = 1; // skip "invoke"

    // Optional $module reference
    let module = match list.get(idx).and_then(|s| s.as_id()) {
        Some(id) => {
            idx += 1;
            Some(format!("${id}"))
        }
        _ => None,
    };

    // Function name (string)
    let name = expect_string(list.get(idx), list.span)?;
    idx += 1;

    // Arguments: remaining items are (type.const value) expressions
    let mut args = Vec::new();
    for item in list.iter_from(idx) {
        args.push(parse_value(source, item)?);
    }

    Ok(WastAction::Invoke { module, name, args })
}

/// Parse `(get $mod? "name")`.
fn parse_get(list: SExprList<'_>) -> Result<WastAction, WastParseError> {
    let mut idx = 1; // skip "get"

    let module = match list.get(idx).and_then(|s| s.as_id()) {
        Some(id) => {
            idx += 1;
            Some(format!("${id}"))
        }
        _ => None,
    };

    let name = expect_string(list.get(idx), list.span)?;

    Ok(WastAction::Get { module, name })
}

// ---------------------------------------------------------------------------
// Assertions
// ---------------------------------------------------------------------------

/// Parse `(assert_return (action) expected...)`.
fn parse_assert_return(source: &str, list: SExprList<'_>) -> Result<WastCommand, WastParseError> {
    let action_sexpr = list
        .get(1)
        .ok_or_else(|| syntax_err("expected action in assert_return", list.span))?;
    let action = parse_action(source, action_sexpr)?;

    let mut expected = Vec::new();
    for item in list.iter_from(2) {
        expected.push(parse_value(source, item)?);
    }

    Ok(WastCommand::AssertReturn {
        span: list.span,
        action,
        expected,
    })
}

/// Parse `(assert_trap (action|module) "message")`.
fn parse_assert_trap(source: &str, list: SExprList<'_>) -> Result<WastCommand, WastParseError> {
    let inner = list
        .get(1)
        .ok_or_else(|| syntax_err("expected action or module in assert_trap", list.span))?;
    let message = expect_string(list.get(2), list.span)?;

    // Determine whether inner is an action (invoke/get) or a module
    if let Some(inner_list) = inner.as_list()
        && let Some(kw) = inner_list.head_keyword()
        && kw == "module"
    {
        let module = parse_inline_module(source, inner)?;
        return Ok(WastCommand::AssertModuleTrap {
            span: list.span,
            module,
            message,
        });
    }

    let action = parse_action(source, inner)?;
    Ok(WastCommand::AssertTrap {
        span: list.span,
        action,
        message,
    })
}

/// Parse assertion commands that take a module and an expected error message:
/// `(assert_invalid (module ...) "msg")`, `(assert_malformed ...)`, etc.
fn parse_assert_module_cmd(source: &str, list: SExprList<'_>, kind: &str) -> Result<WastCommand, WastParseError> {
    let module_sexpr = list
        .get(1)
        .ok_or_else(|| syntax_err(format!("expected module in {kind}"), list.span))?;
    let module = parse_inline_module(source, module_sexpr)?;
    let message = expect_string(list.get(2), list.span)?;

    let span = list.span;
    Ok(match kind {
        "assert_invalid" => WastCommand::AssertInvalid { span, module, message },
        "assert_malformed" => WastCommand::AssertMalformed { span, module, message },
        "assert_unlinkable" => WastCommand::AssertUnlinkable { span, module, message },
        "assert_uninstantiable" => WastCommand::AssertUninstantiable { span, module, message },
        _ => unreachable!(),
    })
}

/// Parse `(assert_exhaustion (action) "message")`.
fn parse_assert_exhaustion(source: &str, list: SExprList<'_>) -> Result<WastCommand, WastParseError> {
    let action_sexpr = list
        .get(1)
        .ok_or_else(|| syntax_err("expected action in assert_exhaustion", list.span))?;
    let action = parse_action(source, action_sexpr)?;
    let message = expect_string(list.get(2), list.span)?;

    Ok(WastCommand::AssertExhaustion {
        span: list.span,
        action,
        message,
    })
}

// ---------------------------------------------------------------------------
// Value parsing
// ---------------------------------------------------------------------------

/// Parse a typed constant value: `(i32.const N)`, `(f64.const nan:canonical)`,
/// `(v128.const i32x4 1 2 3 4)`, `(ref.null func)`, `(ref.func)`, `(ref.extern)`.
fn parse_value(_source: &str, sexpr: &SExpr) -> Result<WastValue, WastParseError> {
    let list = sexpr
        .as_list()
        .ok_or_else(|| syntax_err("expected value expression", sexpr.span()))?;

    let keyword = list
        .head_keyword()
        .ok_or_else(|| syntax_err("expected value keyword", list.span))?;

    match keyword {
        "i32.const" => {
            let val = expect_integer(list.get(1), list.span)?;
            let bits = if val.negative {
                (-(val.value as i64)) as u32
            } else {
                val.value as u32
            };
            Ok(WastValue::I32(bits))
        }
        "i64.const" => {
            let val = expect_integer(list.get(1), list.span)?;
            let bits = if val.negative {
                (-(val.value as i128)) as u64
            } else {
                val.value
            };
            Ok(WastValue::I64(bits))
        }
        "f32.const" => {
            if let Some((negative, payload)) = extract_nan_payload(list.get(1)) {
                let mut bits = nan_payload_to_f32(payload);
                if negative {
                    bits |= 1u32 << 31;
                }
                return Ok(WastValue::F32(WastFloat::Value(bits)));
            }
            let f = parse_f32_float_value(list, 1)?;
            Ok(WastValue::F32(f))
        }
        "f64.const" => {
            if let Some((negative, payload)) = extract_nan_payload(list.get(1)) {
                let mut bits = nan_payload_to_f64(payload);
                if negative {
                    bits |= 1u64 << 63;
                }
                return Ok(WastValue::F64(WastFloat::Value(bits)));
            }
            let f = parse_f64_float_value(list, 1)?;
            Ok(WastValue::F64(f.map_value(|v| v.to_bits())))
        }
        "v128.const" => parse_v128_value(list),
        "ref.null" => {
            let reftype = list.get(1).and_then(|s| s.as_keyword()).unwrap_or("func");
            Ok(WastValue::RefNull(reftype.to_string()))
        }
        "ref.func" => Ok(WastValue::RefFunc),
        "ref.extern" => {
            let idx = list.get(1).and_then(|s| s.as_atom()).and_then(|t| {
                if let TokenKind::Integer(sv) = &t.kind {
                    sv.to_u64().map(|n| n as u32)
                } else {
                    None
                }
            });
            Ok(WastValue::RefExtern(idx))
        }
        other => Err(syntax_err(format!("unknown value type: {other}"), list.span)),
    }
}

/// Parse a float value, handling NaN patterns and special keywords.
fn parse_f64_float_value(list: SExprList<'_>, idx: usize) -> Result<WastFloat<f64>, WastParseError> {
    let item = list
        .get(idx)
        .ok_or_else(|| syntax_err("expected float value", list.span))?;

    parse_f64_float_token(item)
}

/// Parse an f32 float value, using `FloatLit::to_f32()` to avoid double-rounding
/// when converting hex float literals that have more precision than f32.
fn parse_f32_float_value(list: SExprList<'_>, idx: usize) -> Result<WastFloat<u32>, WastParseError> {
    let item = list
        .get(idx)
        .ok_or_else(|| syntax_err("expected float value", list.span))?;

    parse_f32_float_token(item)
}

/// Parse an f32 float token, returning the f32 bit pattern directly.
fn parse_f32_float_token(item: &SExpr) -> Result<WastFloat<u32>, WastParseError> {
    let token = item
        .as_atom()
        .ok_or_else(|| syntax_err("expected float atom", item.span()))?;

    match &token.kind {
        TokenKind::Keyword(kw) => {
            let f = parse_nan_keyword(kw, token.span)?;
            Ok(match f {
                WastFloat::NanCanonical => WastFloat::NanCanonical,
                WastFloat::NanArithmetic => WastFloat::NanArithmetic,
                WastFloat::Value(v) => WastFloat::Value((v as f32).to_bits()),
            })
        }
        TokenKind::Float(flit) => match flit {
            FloatLit::Nan {
                negative,
                payload: None,
            } => {
                let mut bits = 0x7FC00000u32;
                if *negative {
                    bits |= 1u32 << 31;
                }
                Ok(WastFloat::Value(bits))
            }
            FloatLit::Nan {
                negative,
                payload: Some(p),
            } => {
                let mut bits = 0x7F800000u32 | (*p as u32);
                if *negative {
                    bits |= 1u32 << 31;
                }
                Ok(WastFloat::Value(bits))
            }
            _ => Ok(WastFloat::Value(flit.to_f32().to_bits())),
        },
        TokenKind::Integer(ival) => {
            let v = if ival.negative {
                -(ival.value as f64)
            } else {
                ival.value as f64
            };
            Ok(WastFloat::Value((v as f32).to_bits()))
        }
        _ => Err(syntax_err("expected float value", token.span)),
    }
}

/// Parse a single float/NaN token from an S-expression.
fn parse_f64_float_token(item: &SExpr) -> Result<WastFloat<f64>, WastParseError> {
    let token = item
        .as_atom()
        .ok_or_else(|| syntax_err("expected float atom", item.span()))?;

    match &token.kind {
        TokenKind::Keyword(kw) => parse_nan_keyword(kw, token.span),
        TokenKind::Float(flit) => match flit {
            FloatLit::Nan {
                negative,
                payload: None,
            } => {
                // Bare `nan` / `-nan`, canonical NaN with correct sign
                let mut bits = 0x7FF8000000000000u64;
                if *negative {
                    bits |= 1u64 << 63;
                }
                Ok(WastFloat::Value(f64::from_bits(bits)))
            }
            FloatLit::Nan { .. } => {
                // Payload NaN handled by extract_nan_payload at call site
                Ok(WastFloat::Value(flit.to_f64()))
            }
            _ => Ok(WastFloat::Value(flit.to_f64())),
        },
        TokenKind::Integer(ival) => {
            let v = if ival.negative {
                -(ival.value as f64)
            } else {
                ival.value as f64
            };
            Ok(WastFloat::Value(v))
        }
        _ => Err(syntax_err("expected float value", token.span)),
    }
}

/// Parse NaN keywords: `nan:canonical`, `nan:arithmetic`, `nan:0xHH_HH`.
fn parse_nan_keyword(kw: &str, span: Span) -> Result<WastFloat<f64>, WastParseError> {
    match kw {
        "nan:canonical" => Ok(WastFloat::NanCanonical),
        "nan:arithmetic" => Ok(WastFloat::NanArithmetic),
        _ if kw.starts_with("nan:0x") => {
            // NaN with specific payload: nan:0x7f_ffff
            // Return payload as f64, caller must construct proper NaN for target type
            let hex_str = &kw[6..].replace('_', "");
            let payload =
                u64::from_str_radix(hex_str, 16).map_err(|e| syntax_err(format!("bad nan payload: {e}"), span))?;
            Ok(WastFloat::Value(payload as f64))
        }
        _ => Err(syntax_err(format!("unexpected float keyword: {kw}"), span)),
    }
}

/// Extract NaN payload from a FloatLit token (returns negative flag and payload value).
fn extract_nan_payload(sexpr: Option<&SExpr>) -> Option<(bool, u64)> {
    let token = sexpr?.as_atom()?;
    if let TokenKind::Float(FloatLit::Nan {
        negative,
        payload: Some(p),
    }) = &token.kind
    {
        Some((*negative, *p))
    } else {
        None
    }
}

/// Construct f32 NaN bit pattern from payload
fn nan_payload_to_f32(payload: u64) -> u32 {
    0x7F800000 | (payload as u32)
}

/// Construct f64 NaN bit pattern from payload
fn nan_payload_to_f64(payload: u64) -> u64 {
    0x7FF0000000000000 | payload
}

/// Parse `(v128.const lane_type val0 val1 ...)`.
fn parse_v128_value(list: SExprList<'_>) -> Result<WastValue, WastParseError> {
    let lane_type = list
        .get(1)
        .and_then(|s| s.as_keyword())
        .ok_or_else(|| syntax_err("expected v128 lane type", list.span))?;

    let lane_count = match lane_type {
        "i8x16" => 16,
        "i16x8" => 8,
        "i32x4" => 4,
        "i64x2" => 2,
        "f32x4" => 4,
        "f64x2" => 2,
        _ => return Err(syntax_err(format!("unknown v128 lane type: {lane_type}"), list.span)),
    };

    let mut lanes = Vec::with_capacity(lane_count);
    for i in 0..lane_count {
        let item = list.get(2 + i).ok_or_else(|| {
            syntax_err(
                format!("expected {lane_count} lanes for {lane_type}, got {i}"),
                list.span,
            )
        })?;
        lanes.push(parse_lane_value(item, lane_type)?);
    }

    Ok(WastValue::V128 {
        lane_type: lane_type.to_string(),
        lanes,
    })
}

/// Parse a single lane value for v128.
fn parse_lane_value(sexpr: &SExpr, lane_type: &str) -> Result<WastLane, WastParseError> {
    let token = sexpr
        .as_atom()
        .ok_or_else(|| syntax_err("expected lane value", sexpr.span()))?;

    match lane_type {
        "f32x4" => {
            if let TokenKind::Float(FloatLit::Nan {
                negative,
                payload: Some(p),
            }) = &token.kind
            {
                let mut bits = nan_payload_to_f32(*p);
                if *negative {
                    bits |= 1u32 << 31;
                }
                return Ok(WastLane::F32(WastFloat::Value(bits)));
            }
            let f = parse_f32_lane_float(token)?;
            Ok(WastLane::F32(f))
        }
        "f64x2" => {
            if let TokenKind::Float(FloatLit::Nan {
                negative,
                payload: Some(p),
            }) = &token.kind
            {
                let mut bits = nan_payload_to_f64(*p);
                if *negative {
                    bits |= 1u64 << 63;
                }
                return Ok(WastLane::F64(WastFloat::Value(bits)));
            }
            let f = parse_f64_lane_float(token)?;
            Ok(WastLane::F64(f.map_value(|v| v.to_bits())))
        }
        _ => {
            // Integer lane (store as wrapping u64 bit pattern)
            let val = match &token.kind {
                TokenKind::Integer(ival) => {
                    if ival.negative {
                        (-(ival.value as i128)) as u64
                    } else {
                        ival.value
                    }
                }
                _ => return Err(syntax_err("expected integer lane value", token.span)),
            };
            Ok(WastLane::Integer(val))
        }
    }
}

/// Parse a float token into a WastFloat, handling NaN patterns.
fn parse_f64_lane_float(token: &Token) -> Result<WastFloat<f64>, WastParseError> {
    match &token.kind {
        TokenKind::Keyword(kw) => parse_nan_keyword(kw, token.span),
        TokenKind::Float(flit) => match flit {
            FloatLit::Nan {
                negative,
                payload: None,
            } => {
                let mut bits = 0x7FF8000000000000u64;
                if *negative {
                    bits |= 1u64 << 63;
                }
                Ok(WastFloat::Value(f64::from_bits(bits)))
            }
            FloatLit::Nan { .. } => Ok(WastFloat::Value(flit.to_f64())),
            _ => Ok(WastFloat::Value(flit.to_f64())),
        },
        TokenKind::Integer(ival) => {
            let v = if ival.negative {
                -(ival.value as f64)
            } else {
                ival.value as f64
            };
            Ok(WastFloat::Value(v))
        }
        _ => Err(syntax_err("expected float value", token.span)),
    }
}

/// Parse an f32 float lane token, returning f32 bit pattern directly.
fn parse_f32_lane_float(token: &Token) -> Result<WastFloat<u32>, WastParseError> {
    match &token.kind {
        TokenKind::Keyword(kw) => {
            let f = parse_nan_keyword(kw, token.span)?;
            Ok(match f {
                WastFloat::NanCanonical => WastFloat::NanCanonical,
                WastFloat::NanArithmetic => WastFloat::NanArithmetic,
                WastFloat::Value(v) => WastFloat::Value((v as f32).to_bits()),
            })
        }
        TokenKind::Float(flit) => match flit {
            FloatLit::Nan {
                negative,
                payload: None,
            } => {
                let mut bits = 0x7FC00000u32;
                if *negative {
                    bits |= 1u32 << 31;
                }
                Ok(WastFloat::Value(bits))
            }
            FloatLit::Nan {
                negative,
                payload: Some(p),
            } => {
                let mut bits = 0x7F800000u32 | (*p as u32);
                if *negative {
                    bits |= 1u32 << 31;
                }
                Ok(WastFloat::Value(bits))
            }
            _ => Ok(WastFloat::Value(flit.to_f32().to_bits())),
        },
        TokenKind::Integer(ival) => {
            let v = if ival.negative {
                -(ival.value as f64)
            } else {
                ival.value as f64
            };
            Ok(WastFloat::Value((v as f32).to_bits()))
        }
        _ => Err(syntax_err("expected float value", token.span)),
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract a string value from an optional S-expression.
fn expect_string(sexpr: Option<&SExpr>, fallback_span: Span) -> Result<String, WastParseError> {
    let item = sexpr.ok_or_else(|| syntax_err("expected string", fallback_span))?;
    let token = item
        .as_atom()
        .ok_or_else(|| syntax_err("expected string atom", item.span()))?;
    match &token.kind {
        TokenKind::String(b) => Ok(String::from_utf8_lossy(b).to_string()),
        _ => Err(syntax_err("expected string", token.span)),
    }
}

/// Extract an integer value from an optional S-expression.
fn expect_integer(sexpr: Option<&SExpr>, fallback_span: Span) -> Result<SignedValue<u64>, WastParseError> {
    let item = sexpr.ok_or_else(|| syntax_err("expected integer", fallback_span))?;
    let token = item
        .as_atom()
        .ok_or_else(|| syntax_err("expected integer atom", item.span()))?;
    match &token.kind {
        TokenKind::Integer(ival) => Ok(*ival),
        _ => Err(syntax_err("expected integer", token.span)),
    }
}

impl<T: std::fmt::Debug> WastFloat<T> {
    /// Map the Value variant, preserving NaN patterns.
    fn map_value<U: std::fmt::Debug>(self, f: impl FnOnce(T) -> U) -> WastFloat<U> {
        match self {
            WastFloat::Value(v) => WastFloat::Value(f(v)),
            WastFloat::NanCanonical => WastFloat::NanCanonical,
            WastFloat::NanArithmetic => WastFloat::NanArithmetic,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_module_and_assert_return() {
        let source = r#"
(module
  (func (export "add") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add))
(assert_return (invoke "add" (i32.const 1) (i32.const 2)) (i32.const 3))
"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 2);
        assert!(matches!(&script.commands[0], WastCommand::Module { .. }));
        assert!(matches!(&script.commands[1], WastCommand::AssertReturn { .. }));
    }

    #[test]
    fn parse_binary_module() {
        let source = r#"(module binary "\00asm\01\00\00\00")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        if let WastCommand::Module {
            module: WastModule::Binary(bytes),
            ..
        } = &script.commands[0]
        {
            assert_eq!(bytes, b"\x00asm\x01\x00\x00\x00");
        } else {
            panic!("expected binary module");
        }
    }

    #[test]
    fn parse_named_module_and_register() {
        let source = r#"
(module $Mf (func (export "call") (result i32) (i32.const 42)))
(register "Mf" $Mf)
(assert_return (invoke $Mf "call") (i32.const 42))
"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 3);
        if let WastCommand::Module { name, .. } = &script.commands[0] {
            assert_eq!(name.as_deref(), Some("$Mf"));
        }
        if let WastCommand::Register {
            as_name, module_name, ..
        } = &script.commands[1]
        {
            assert_eq!(as_name, "Mf");
            assert_eq!(module_name.as_deref(), Some("$Mf"));
        }
    }

    #[test]
    fn parse_assert_trap() {
        let source = r#"(assert_trap (invoke "div" (i32.const 1) (i32.const 0)) "integer divide by zero")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        if let WastCommand::AssertTrap { message, .. } = &script.commands[0] {
            assert_eq!(message, "integer divide by zero");
        }
    }

    #[test]
    fn parse_assert_invalid() {
        let source = r#"
(assert_invalid
  (module (func (result i32) (f32.const 1.0)))
  "type mismatch"
)
"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        assert!(
            matches!(&script.commands[0], WastCommand::AssertInvalid { message, .. } if message == "type mismatch")
        );
    }

    #[test]
    fn parse_assert_malformed_binary() {
        let source = r#"(assert_malformed (module binary "") "unexpected end")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        if let WastCommand::AssertMalformed {
            module: WastModule::Binary(b),
            message,
            ..
        } = &script.commands[0]
        {
            assert!(b.is_empty());
            assert_eq!(message, "unexpected end");
        }
    }

    #[test]
    fn parse_assert_malformed_quote() {
        let source = r#"(assert_malformed (module quote "(memory 1)" "(func)") "some error")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        if let WastCommand::AssertMalformed {
            module: WastModule::Quote(text),
            ..
        } = &script.commands[0]
        {
            assert_eq!(text, "(memory 1)(func)");
        }
    }

    #[test]
    fn parse_assert_module_trap() {
        let source = r#"(assert_trap (module (func $main (unreachable)) (start $main)) "unreachable")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        assert!(matches!(&script.commands[0], WastCommand::AssertModuleTrap { .. }));
    }

    #[test]
    fn parse_top_level_invoke() {
        let source = r#"
(module (func (export "inc") (nop)))
(invoke "inc")
"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 2);
        assert!(matches!(&script.commands[1], WastCommand::Action { .. }));
    }

    #[test]
    fn parse_assert_exhaustion() {
        let source = r#"(assert_exhaustion (invoke "runaway") "call stack exhausted")"#;
        let script = parse_script(source).unwrap();
        assert_eq!(script.commands.len(), 1);
        assert!(
            matches!(&script.commands[0], WastCommand::AssertExhaustion { message, .. } if message == "call stack exhausted")
        );
    }

    #[test]
    fn parse_ref_null_value() {
        let source = r#"(assert_return (invoke "f") (ref.null func))"#;
        let script = parse_script(source).unwrap();
        if let WastCommand::AssertReturn { expected, .. } = &script.commands[0] {
            assert!(matches!(&expected[0], WastValue::RefNull(t) if t == "func"));
        }
    }

    #[test]
    fn parse_real_i32_wast() {
        let source = std::fs::read_to_string("tests/spec/wast/i32.wast").unwrap();
        let script = parse_script(&source).unwrap();
        // i32.wast has 1 module + many assert_return + some assert_trap
        assert!(script.commands.len() > 400);
        assert!(matches!(&script.commands[0], WastCommand::Module { .. }));
    }

    #[test]
    fn parse_real_binary_wast() {
        let source = std::fs::read_to_string("tests/spec/wast/binary.wast").unwrap();
        let script = parse_script(&source).unwrap();
        // binary.wast has binary modules and many assert_malformed
        assert!(script.commands.len() > 50);
        let binary_count = script
            .commands
            .iter()
            .filter(|c| {
                matches!(
                    c,
                    WastCommand::Module {
                        module: WastModule::Binary(_),
                        ..
                    }
                )
            })
            .count();
        assert!(binary_count >= 2, "expected binary modules, got {binary_count}");
    }

    #[test]
    fn parse_real_linking_wast() {
        let source = std::fs::read_to_string("tests/spec/wast/linking.wast").unwrap();
        let script = parse_script(&source).unwrap();
        // linking.wast has modules, register, assert_return, assert_unlinkable, assert_trap
        let register_count = script
            .commands
            .iter()
            .filter(|c| matches!(c, WastCommand::Register { .. }))
            .count();
        assert!(register_count > 0, "expected register commands");
        let assert_return_count = script
            .commands
            .iter()
            .filter(|c| matches!(c, WastCommand::AssertReturn { .. }))
            .count();
        assert!(
            assert_return_count > 30,
            "expected many assert_return, got {assert_return_count}"
        );
    }

    #[test]
    fn parse_real_start_wast() {
        let source = std::fs::read_to_string("tests/spec/wast/start.wast").unwrap();
        let script = parse_script(&source).unwrap();
        // start.wast has top-level invokes, assert_return, assert_invalid, assert_trap, assert_malformed
        let invoke_count = script
            .commands
            .iter()
            .filter(|c| matches!(c, WastCommand::Action { .. }))
            .count();
        assert!(invoke_count > 0, "expected top-level invocations");
    }

    #[test]
    fn parse_real_comments_wast() {
        let source = std::fs::read_to_string("tests/spec/wast/comments.wast").unwrap();
        let script = parse_script(&source).unwrap();
        // comments.wast has modules, module quote, assert_return, assert_malformed
        assert!(script.commands.len() > 5);
    }

    #[test]
    fn parse_all_core_wast_files() {
        // endianness.wast is corrupt in the repo (starts with "endianness." prefix)
        let skip = ["endianness.wast"];

        let mut failures = Vec::new();
        for entry in std::fs::read_dir("tests/spec/wast").unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().map(|e| e == "wast").unwrap_or(false) {
                let name = path.file_name().unwrap().to_str().unwrap();
                if skip.contains(&name) {
                    continue;
                }
                let source = std::fs::read_to_string(&path).unwrap();
                if let Err(e) = parse_script(&source) {
                    failures.push(format!("{}: {e}", path.display()));
                }
            }
        }
        assert!(failures.is_empty(), "Failed to parse:\n{}", failures.join("\n"));
    }

    #[test]
    fn parse_all_simd_wast_files() {
        let mut failures = Vec::new();
        for entry in std::fs::read_dir("tests/spec/wast/simd").unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.extension().map(|e| e == "wast").unwrap_or(false) {
                let source = std::fs::read_to_string(&path).unwrap();
                if let Err(e) = parse_script(&source) {
                    failures.push(format!("{}: {e}", path.display()));
                }
            }
        }
        assert!(failures.is_empty(), "Failed to parse:\n{}", failures.join("\n"));
    }
}
