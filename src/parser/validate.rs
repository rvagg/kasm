use super::module::{ElementMode, FunctionType, GlobalType, ValueType, ValueType::*};
use crate::parser::instruction::{BlockType, Instruction, InstructionKind};
use MaybeValue::{Unknown, Val};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("constant expression required")]
    ConstantExpressionRequired,

    #[error("type mismatch")]
    TypeMismatch,

    #[error("unknown type")]
    UnknownFunctionType,

    #[error("unknown function {0}")]
    UnknownFunction(u32),

    #[error("unknown table")]
    UnknownTable,

    #[error("unknown table {0}")]
    UnknownTableWithIndex(u32),

    #[error("unknown memory")]
    UnknownMemory,

    #[error("unknown memory {0}")]
    UnknownMemoryWithIndex(u32),

    #[error("unknown element")]
    UnknownElement,

    #[error("unknown elem segment {0}")]
    UnknownElemSegment(u32),

    #[error("unexpected token")]
    UnexpectedToken,

    #[error("unknown local {0}")]
    UnknownLocal(u32),

    #[error("unknown global {0}")]
    UnknownGlobal(u32),

    #[error("unknown label")]
    UnknownLabel,

    #[error("invalid instruction")]
    InvalidInstruction,

    #[error("unexpected end of section or function")]
    UnexpectedEndOfFunction,

    #[error("data count section required")]
    DataCountSectionRequired,

    #[error("unknown data segment")]
    UnknownDataSegment,

    #[error("unknown data segment {0}")]
    UnknownDataSegmentWithIndex(u32),

    #[error("unknown block type")]
    UnknownBlockType,

    #[error("undeclared function reference")]
    UndeclaredFunctionReference,

    #[error("unimplemented instruction")]
    UnimplementedInstruction,

    #[error("alignment must not be larger than natural")]
    BadAlignment,

    #[error("global is immutable")]
    GlobalIsImmutable,

    #[error("invalid result arity")]
    InvalidResultArity,
}

pub trait Validator {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError>;
    fn ended(&mut self) -> bool;
    fn finalise(&mut self) -> Result<(), ValidationError> {
        Ok(())
    }
}

pub struct ConstantExpressionValidator<'a> {
    // globals: &'a Vec<GlobalType>,
    imports: &'a super::module::ImportSection,
    return_type: ValueType,
    instr_count: u32,
    has_end: bool,
    total_functions: Option<u32>, // Total number of functions (imports + declared)
}

impl ConstantExpressionValidator<'_> {
    pub fn new(
        imports: &super::module::ImportSection,
        return_type: super::module::ValueType,
    ) -> ConstantExpressionValidator<'_> {
        ConstantExpressionValidator {
            imports,
            return_type,
            instr_count: 0,
            has_end: false,
            total_functions: None,
        }
    }

    pub fn with_function_count(mut self, count: u32) -> Self {
        self.total_functions = Some(count);
        self
    }

    fn global(&mut self, global_index: u32) -> Result<&GlobalType, ValidationError> {
        // constant expressions can only reference imported globals
        self.imports
            .get_global_import(global_index)
            .map(|import| match &import.external_kind {
                super::module::ExternalKind::Global(global_type) => {
                    if global_type.mutable {
                        Err(ValidationError::ConstantExpressionRequired)
                    } else {
                        Ok(global_type)
                    }
                }
                _ => Err(ValidationError::ConstantExpressionRequired),
            })
            .unwrap_or(Err(ValidationError::UnknownGlobal(global_index)))
    }
}

impl Validator for ConstantExpressionValidator<'_> {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError> {
        let kind = &inst.kind;
        use InstructionKind::*;

        match kind {
            I32Const { .. }
            | I64Const { .. }
            | F32Const { .. }
            | F64Const { .. }
            | RefNull { .. }
            | RefFunc { .. }
            | GlobalGet { .. }
            | End => {}
            _ => return Err(ValidationError::ConstantExpressionRequired),
        }
        self.instr_count += 1;
        if self.instr_count >= 2 && !matches!(kind, End) {
            return Err(ValidationError::TypeMismatch);
        }
        match &kind {
            // const type must match self.return_type
            I32Const { .. } => (self.return_type == I32)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            I64Const { .. } => (self.return_type == I64)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            F32Const { .. } => (self.return_type == F32)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            F64Const { .. } => (self.return_type == F64)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            RefNull { ref_type } => {
                if self.return_type == FuncRef && *ref_type == FuncRef
                    || self.return_type == ExternRef && *ref_type == ExternRef
                {
                    Ok(())
                } else {
                    Err(ValidationError::TypeMismatch)
                }
            }
            RefFunc { func_idx } => {
                // First check the return type
                if self.return_type != FuncRef && self.return_type != ExternRef {
                    return Err(ValidationError::TypeMismatch);
                }
                // Then check if the function index is valid
                if let Some(total) = self.total_functions
                    && *func_idx >= total
                {
                    return Err(ValidationError::UnknownFunction(*func_idx));
                }
                Ok(())
            }
            GlobalGet { global_idx } => {
                let value_type = self.global(*global_idx)?.value_type;
                if value_type == self.return_type {
                    Ok(())
                } else {
                    Err(ValidationError::TypeMismatch)
                }
            }
            End => match self.instr_count {
                1 => Err(ValidationError::TypeMismatch),
                2 => {
                    self.has_end = true;
                    Ok(())
                }
                _ => Err(ValidationError::ConstantExpressionRequired),
            },
            _ => Err(ValidationError::ConstantExpressionRequired),
        }
    }

    fn ended(&mut self) -> bool {
        self.has_end
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum MaybeValue {
    Val(ValueType),
    Unknown,
}

#[allow(dead_code)]
impl MaybeValue {
    fn is_num(&self) -> bool {
        match self {
            Val(v) => *v == I32 || *v == I64 || *v == F32 || *v == F64,
            Unknown => true,
        }
    }

    fn is_vec(&self) -> bool {
        match self {
            Val(v) => *v == V128,
            Unknown => true,
        }
    }

    fn is_ref(&self) -> bool {
        match self {
            Val(v) => *v == FuncRef || *v == ExternRef,
            Unknown => true,
        }
    }
}

const I32_VALUE: ValueType = I32;
const I64_VALUE: ValueType = I64;
const F32_VALUE: ValueType = F32;
const F64_VALUE: ValueType = F64;
#[allow(dead_code)]
const V128_VALUE: ValueType = V128;

#[derive(Clone)]
struct CtrlFrame {
    instruction: InstructionKind,
    start_types: Vec<MaybeValue>,
    end_types: Vec<MaybeValue>,
    height: u32,
    unreachable: bool,
}

pub struct CodeValidator<'a> {
    module: &'a super::module::Module,
    ctx: &'a super::module::ValidationContext,
    locals: &'a super::module::Locals,
    function_type: &'a FunctionType,
    vals: Vec<MaybeValue>,
    ctrls: Vec<CtrlFrame>,
    need_end: bool,
    current_function_index: u32,
}

impl<'a> CodeValidator<'a> {
    #[inline]
    fn validate_memory_exists(&self) -> Result<(), ValidationError> {
        if !self.ctx.has_memory {
            return Err(ValidationError::UnknownMemory);
        }
        Ok(())
    }

    pub fn new(
        module: &'a super::module::Module,
        ctx: &'a super::module::ValidationContext,
        locals: &'a super::module::Locals,
        function_type: &'a FunctionType,
        current_function_index: u32,
    ) -> CodeValidator<'a> {
        let mut v = CodeValidator {
            module,
            ctx,
            locals,
            function_type,
            vals: vec![],
            ctrls: vec![],
            need_end: false,
            current_function_index,
        };

        // push the return types but leave the parameters blank as they aren't on the stack
        // until explicitly loaded with local.get
        let end_types = function_type.return_types.iter().map(|v| Val(*v)).collect();
        // Use a dummy block for the function body
        v.push_ctrl(
            InstructionKind::Block {
                block_type: BlockType::Empty,
            },
            vec![],
            end_types,
        );

        v
    }

    fn push_val(&mut self, val_type: MaybeValue) -> Option<()> {
        self.vals.push(val_type);
        Some(())
    }

    fn push_vals(&mut self, val_types: Vec<MaybeValue>) -> Option<()> {
        for val_type in val_types {
            self.push_val(val_type)?;
        }
        Some(())
    }

    fn pop_val(&mut self) -> Option<MaybeValue> {
        if self.vals.len() as u32 == self.ctrls.last()?.height && self.ctrls.last()?.unreachable {
            Some(Unknown)
        } else if self.vals.len() as u32 == self.ctrls.last()?.height || self.vals.is_empty() {
            None
        } else {
            self.vals.pop()
        }
    }

    fn pop_expected(&mut self, val_type: MaybeValue) -> Option<MaybeValue> {
        let popped = self.pop_val()?;
        if popped != val_type && popped != Unknown && val_type != Unknown {
            None
        } else {
            Some(popped)
        }
    }

    fn pop_expecteds(&mut self, mut val_types: Vec<MaybeValue>) -> Option<Vec<MaybeValue>> {
        let mut popped = Vec::new();
        val_types.reverse();
        for val_type in val_types {
            popped.push(self.pop_expected(val_type)?);
        }
        Some(popped)
    }

    fn push_ctrl(&mut self, instruction: InstructionKind, start_types: Vec<MaybeValue>, end_types: Vec<MaybeValue>) {
        self.ctrls.push(CtrlFrame {
            instruction,
            start_types: start_types.clone(),
            end_types,
            height: self.vals.len() as u32,
            unreachable: false,
        });
        self.push_vals(start_types);
        self.need_end = true;
    }

    fn pop_ctrl(&mut self) -> Result<CtrlFrame, ValidationError> {
        if self.ctrls.is_empty() {
            Err(ValidationError::UnexpectedToken)
        } else {
            let ctrl = self.ctrls.last().unwrap();
            let end_types_clone = ctrl.end_types.clone();
            let height = ctrl.height as usize;

            if self.pop_expecteds(end_types_clone).is_none() || self.vals.len() != height {
                Err(ValidationError::TypeMismatch)
            } else {
                Ok(self.ctrls.pop().unwrap())
            }
        }
    }

    fn frame_types(&mut self, frame: CtrlFrame) -> Vec<MaybeValue> {
        if matches!(frame.instruction, InstructionKind::Loop { .. }) {
            frame.start_types
        } else {
            frame.end_types
        }
    }

    fn unreachable(&mut self) -> Option<()> {
        if let Some(ctrl) = self.ctrls.last_mut() {
            let h = ctrl.height as usize;
            if self.vals.len() > h {
                self.vals.truncate(h);
            }
            ctrl.unreachable = true;
            Some(())
        } else {
            None
        }
    }

    fn sig_unary(&mut self, in_type: MaybeValue, out_type: MaybeValue) -> Result<(), ValidationError> {
        self.pop_expected(in_type).ok_or(ValidationError::TypeMismatch)?;
        self.push_val(out_type).ok_or(ValidationError::TypeMismatch)
    }

    fn sig_binary(
        &mut self,
        in1_type: MaybeValue,
        in2_type: MaybeValue,
        out_type: MaybeValue,
    ) -> Result<(), ValidationError> {
        self.pop_expected(in1_type).ok_or(ValidationError::TypeMismatch)?;
        self.pop_expected(in2_type).ok_or(ValidationError::TypeMismatch)?;
        self.push_val(out_type).ok_or(ValidationError::TypeMismatch)
    }

    fn local(&mut self, local_index: u32) -> Result<&ValueType, ValidationError> {
        let li = local_index;
        let param_count = self.function_type.parameters.len();
        let local: Option<&ValueType> = if (li as usize) < param_count {
            self.function_type.parameters.get(li as usize)
        } else {
            let li = li - param_count as u32;
            self.locals.get(li)
        };

        local.ok_or(ValidationError::UnknownLocal(local_index))
    }

    fn global(&mut self, global_index: u32) -> Result<&GlobalType, ValidationError> {
        // TODO: get the imported global type
        let imported_global_count = self.module.imports.global_count();
        let global = if global_index < imported_global_count as u32 {
            self.module
                .imports
                .get_global_import(global_index)
                .and_then(|import| match &import.external_kind {
                    super::module::ExternalKind::Global(global_type) => Some(global_type),
                    _ => None,
                })
        } else {
            self.module
                .globals
                .get(global_index - imported_global_count as u32)
                .map(|g| &g.global_type)
        };
        match global {
            Some(global) => Ok(global),
            None => Err(ValidationError::UnknownGlobal(global_index)),
        }
    }

    fn label_types_at(&mut self, li: u32) -> Result<Vec<MaybeValue>, ValidationError> {
        if self.ctrls.len() <= li as usize {
            return Err(ValidationError::UnknownLabel);
        }
        let index = self.ctrls.len() - li as usize - 1;
        let frame = self.ctrls.get(index).ok_or(ValidationError::UnknownLabel)?.clone();
        Ok(self.frame_types(frame))
    }

    fn get_function_type(&self, fi: u32) -> Result<&FunctionType, ValidationError> {
        // Use pre-computed function type indices for O(1) lookup
        self.ctx
            .function_type_indices
            .get(fi as usize)
            .ok_or(ValidationError::UnknownFunction(fi))
            .and_then(|&type_idx| self.get_type(type_idx))
    }

    fn get_type(&self, ti: u32) -> Result<&FunctionType, ValidationError> {
        self.module.types.get(ti).ok_or(ValidationError::UnknownFunctionType)
    }
}

impl Validator for CodeValidator<'_> {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError> {
        let kind = &inst.kind;
        use InstructionKind::*;

        match kind {
            I32Const { .. } => self.push_val(Val(I32_VALUE)).ok_or(ValidationError::TypeMismatch),

            // iunop (i32):i32
            I32Clz | I32Ctz | I32Popcnt => self.sig_unary(Val(I32_VALUE), Val(I32_VALUE)),

            // ibinop (i32,i32):i32
            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or | I32Xor | I32Shl
            | I32ShrS | I32ShrU | I32Rotr | I32Rotl => self.sig_binary(Val(I32_VALUE), Val(I32_VALUE), Val(I32_VALUE)),

            // itestop (i32):i32
            I32Eqz => self.sig_unary(Val(I32_VALUE), Val(I32_VALUE)),

            // irelop (i32,i32):i32
            I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU => {
                self.sig_binary(Val(I32_VALUE), Val(I32_VALUE), Val(I32_VALUE))
            }

            // cvtop (i32):i32
            I32Extend8S | I32Extend16S => self.sig_unary(Val(I32_VALUE), Val(I32_VALUE)),

            // cvtop (i64):i32
            I32WrapI64 => self.sig_unary(Val(I64_VALUE), Val(I32_VALUE)),

            // cvtop (f32):i32
            I32TruncF32S | I32TruncF32U | I32TruncSatF32S | I32TruncSatF32U | I32ReinterpretF32 => {
                self.sig_unary(Val(F32_VALUE), Val(I32_VALUE))
            }

            // cvtop (f64):i32
            I32TruncF64S | I32TruncF64U | I32TruncSatF64S | I32TruncSatF64U => {
                self.sig_unary(Val(F64_VALUE), Val(I32_VALUE))
            }

            I64Const { .. } => self.push_val(Val(I64_VALUE)).ok_or(ValidationError::TypeMismatch),

            // iunop (i64):i64
            I64Clz | I64Ctz | I64Popcnt => self.sig_unary(Val(I64_VALUE), Val(I64_VALUE)),

            // ibinop (i64,i64):i64
            I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or | I64Xor | I64Shl
            | I64ShrS | I64ShrU | I64Rotr | I64Rotl => self.sig_binary(Val(I64_VALUE), Val(I64_VALUE), Val(I64_VALUE)),

            // itestop (i64):i32
            I64Eqz => self.sig_unary(Val(I64_VALUE), Val(I32_VALUE)),

            // irelop (i64,i64):i32
            I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU => {
                self.sig_binary(Val(I64_VALUE), Val(I64_VALUE), Val(I32_VALUE))
            }

            // cvtop (i64):i64
            I64Extend8S | I64Extend16S | I64Extend32S => self.sig_unary(Val(I64_VALUE), Val(I64_VALUE)),

            // cvtop (i32):i64
            I64ExtendI32S | I64ExtendI32U => self.sig_unary(Val(I32_VALUE), Val(I64_VALUE)),

            // cvtop (f32):i64
            I64TruncF32S | I64TruncF32U | I64TruncSatF32S | I64TruncSatF32U => {
                self.sig_unary(Val(F32_VALUE), Val(I64_VALUE))
            }

            // cvtop (f64):i64
            I64TruncF64S | I64TruncF64U | I64TruncSatF64S | I64TruncSatF64U | I64ReinterpretF64 => {
                self.sig_unary(Val(F64_VALUE), Val(I64_VALUE))
            }

            F32Const { .. } => self.push_val(Val(F32_VALUE)).ok_or(ValidationError::TypeMismatch),

            // funop (f32):f32
            F32Abs | F32Neg | F32Sqrt | F32Ceil | F32Floor | F32Trunc | F32Nearest => {
                self.sig_unary(Val(F32_VALUE), Val(F32_VALUE))
            }

            // fbinop (f32,f32):f32
            F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign => {
                self.sig_binary(Val(F32_VALUE), Val(F32_VALUE), Val(F32_VALUE))
            }

            // frelop (f32,f32):i32
            F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge => {
                self.sig_binary(Val(F32_VALUE), Val(F32_VALUE), Val(I32_VALUE))
            }

            // cvtop (f64):f32
            F32DemoteF64 => self.sig_unary(Val(F64_VALUE), Val(F32_VALUE)),

            // cvtop (i32):f32
            F32ConvertI32S | F32ConvertI32U | F32ReinterpretI32 => self.sig_unary(Val(I32_VALUE), Val(F32_VALUE)),

            // cvtop (i64):f32
            F32ConvertI64S | F32ConvertI64U => self.sig_unary(Val(I64_VALUE), Val(F32_VALUE)),

            F64Const { .. } => self.push_val(Val(F64_VALUE)).ok_or(ValidationError::TypeMismatch),

            // funop (f64):f64
            F64Abs | F64Neg | F64Sqrt | F64Ceil | F64Floor | F64Trunc | F64Nearest => {
                self.sig_unary(Val(F64_VALUE), Val(F64_VALUE))
            }

            // fbinop (f64,f64):f64
            F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign => {
                self.sig_binary(Val(F64_VALUE), Val(F64_VALUE), Val(F64_VALUE))
            }

            // frelop (f64,f64):i32
            F64Eq | F64Ne | F64Lt | F64Gt | F64Le | F64Ge => {
                self.sig_binary(Val(F64_VALUE), Val(F64_VALUE), Val(I32_VALUE))
            }

            // cvtop (f32):f64
            F64PromoteF32 => self.sig_unary(Val(F32_VALUE), Val(F64_VALUE)),

            // cvtop (i32):f64
            F64ConvertI32S | F64ConvertI32U => self.sig_unary(Val(I32_VALUE), Val(F64_VALUE)),

            // cvtop (i64):f64
            F64ConvertI64S | F64ConvertI64U | F64ReinterpretI64 => self.sig_unary(Val(I64_VALUE), Val(F64_VALUE)),

            LocalGet { local_idx } => {
                let local = *self.local(*local_idx)?;
                self.push_val(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            LocalSet { local_idx } => {
                let local = *self.local(*local_idx)?;
                self.pop_expected(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            LocalTee { local_idx } => {
                let local = *self.local(*local_idx)?;
                self.pop_expected(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            GlobalGet { global_idx } => {
                let global = *self.global(*global_idx)?;
                self.push_val(Val(global.value_type))
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            GlobalSet { global_idx } => {
                let global = *self.global(*global_idx)?;
                if !global.mutable {
                    return Err(ValidationError::GlobalIsImmutable);
                }
                self.pop_expected(Val(global.value_type))
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            I32Load { memarg }
            | I32Load8S { memarg }
            | I32Load8U { memarg }
            | I32Load16S { memarg }
            | I32Load16U { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                let align_exp = match &kind {
                    I32Load { .. } => 2,                        // 4 bytes = 2^2
                    I32Load8S { .. } | I32Load8U { .. } => 0,   // 1 byte = 2^0
                    I32Load16S { .. } | I32Load16U { .. } => 1, // 2 bytes = 2^1
                    _ => unreachable!(),
                };
                check_alignment(memarg, align_exp)
            }

            I64Load { memarg }
            | I64Load8S { memarg }
            | I64Load8U { memarg }
            | I64Load16S { memarg }
            | I64Load16U { memarg }
            | I64Load32S { memarg }
            | I64Load32U { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I64)).ok_or(ValidationError::TypeMismatch)?;
                let align_exp = match &kind {
                    I64Load { .. } => 3,                        // 8 bytes = 2^3
                    I64Load8S { .. } | I64Load8U { .. } => 0,   // 1 byte = 2^0
                    I64Load16S { .. } | I64Load16U { .. } => 1, // 2 bytes = 2^1
                    I64Load32S { .. } | I64Load32U { .. } => 2, // 4 bytes = 2^2
                    _ => unreachable!(),
                };
                check_alignment(memarg, align_exp)
            }

            F32Load { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(memarg, 2 /* 4 bytes = 2^2 */)
            }

            F64Load { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F64)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(memarg, 3 /* 8 bytes = 2^3 */)
            }

            I32Store { memarg } | I32Store8 { memarg } | I32Store16 { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                let align_exp = match &kind {
                    I32Store { .. } => 2,   // 4 bytes = 2^2
                    I32Store8 { .. } => 0,  // 1 byte = 2^0
                    I32Store16 { .. } => 1, // 2 bytes = 2^1
                    _ => unreachable!(),
                };
                check_alignment(memarg, align_exp)
            }

            I64Store { memarg } | I64Store8 { memarg } | I64Store16 { memarg } | I64Store32 { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I64)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                let align_exp = match &kind {
                    I64Store { .. } => 3,   // 8 bytes = 2^3
                    I64Store8 { .. } => 0,  // 1 byte = 2^0
                    I64Store16 { .. } => 1, // 2 bytes = 2^1
                    I64Store32 { .. } => 2, // 4 bytes = 2^2
                    _ => unreachable!(),
                };
                check_alignment(memarg, align_exp)
            }

            F32Store { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(F32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(memarg, 2 /* 4 bytes = 2^2 */)
            }

            F64Store { memarg } => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(F64)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(memarg, 3 /* 8 bytes = 2^3 */)
            }

            MemorySize => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryGrow => {
                // Check that memory exists (imported or declared)
                self.validate_memory_exists()?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryFill | MemoryCopy => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemoryWithIndex(0));
                }
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryInit { data_idx } => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemoryWithIndex(0));
                }

                // memory.init always requires a DataCount section for bulk memory operations
                let has_data_count_section =
                    self.module.data_count.position.start != 0 || self.module.data_count.position.end != 0;
                if !has_data_count_section {
                    return Err(ValidationError::DataCountSectionRequired);
                }

                // Check if data_idx >= data_count
                if *data_idx >= self.module.data_count.count {
                    return Err(ValidationError::UnknownDataSegmentWithIndex(*data_idx));
                }
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableInit { elem_idx, table_idx } => {
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                let table = self
                    .module
                    .table
                    .tables
                    .get(*table_idx as usize)
                    .ok_or(ValidationError::UnknownTableWithIndex(*table_idx))?;
                let elem = self
                    .module
                    .elements
                    .elements
                    .get(*elem_idx as usize)
                    .ok_or(ValidationError::UnknownElement)?;
                if elem.ref_type != table.ref_type {
                    return Err(ValidationError::TypeMismatch);
                }
                Ok(())
            }

            TableCopy { src_table, dst_table } => {
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                let src_table_type = self.module.get_table(*src_table).ok_or(ValidationError::UnknownTable)?;
                let dst_table_type = self.module.get_table(*dst_table).ok_or(ValidationError::UnknownTable)?;
                if src_table_type.ref_type != dst_table_type.ref_type {
                    return Err(ValidationError::TypeMismatch);
                }
                Ok(())
            }

            TableGet { table_idx } => {
                let table = self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(table.ref_type.into()))
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableSet { table_idx } => {
                let table = self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;
                self.pop_expected(Val(table.ref_type.into()))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            ElemDrop { elem_idx } => {
                self.module
                    .elements
                    .elements
                    .get(*elem_idx as usize)
                    .ok_or(ValidationError::UnknownElemSegment(*elem_idx))?;
                Ok(())
            }

            DataDrop { data_idx } => {
                let has_data_count_section =
                    self.module.data_count.position.start != 0 || self.module.data_count.position.end != 0;
                let has_memory = !self.module.memory.is_empty() || self.module.imports.memory_count() > 0;

                if !has_data_count_section {
                    // If module has memory, bulk memory ops require DataCount section
                    // If no memory, then it's just an unknown data segment
                    if has_memory {
                        return Err(ValidationError::DataCountSectionRequired);
                    } else {
                        return Err(ValidationError::UnknownDataSegment);
                    }
                }

                // DataCount section exists, check the index
                if *data_idx >= self.module.data_count.count {
                    Err(ValidationError::UnknownDataSegmentWithIndex(*data_idx))
                } else {
                    Ok(())
                }
            }

            Block { block_type } | Loop { block_type } | If { block_type } => {
                // special case for If we need to pop an i32
                if matches!(kind, If { .. }) {
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                }

                let mut start_types = Vec::new();
                let mut end_types = Vec::new();

                match block_type {
                    BlockType::Empty => {
                        // Do nothing, types remain empty
                    }
                    BlockType::Value(t) => {
                        end_types.push(Val(*t));
                    }
                    BlockType::FuncType(ti) => {
                        let ftype = self.module.types.get(*ti).ok_or(ValidationError::UnknownBlockType)?;
                        // parameters are stack ordered, so pick them in reverse
                        ftype.parameters.iter().rev().try_for_each(|v| {
                            match self.pop_expected(Val(*v)) {
                                Some(_) => {
                                    // insert at start because we're working in reverse
                                    start_types.insert(0, Val(*v));
                                    Ok(())
                                }
                                None => Err(ValidationError::TypeMismatch),
                            }
                        })?;
                        ftype.return_types.iter().for_each(|v| {
                            end_types.push(Val(*v));
                        });
                    }
                };

                self.push_ctrl(kind.clone(), start_types, end_types);
                Ok(())
            }

            Else => {
                let ctrl = self.pop_ctrl()?;
                if !matches!(ctrl.instruction, If { .. }) {
                    Err(ValidationError::InvalidInstruction)
                } else {
                    // Mark that we had an else by pushing Else control frame
                    self.push_ctrl(Else, ctrl.start_types, ctrl.end_types);
                    Ok(())
                }
            }

            End => {
                let ctrl = self.pop_ctrl()?;

                // Special check: typed if without else is invalid UNLESS
                // the start types match the end types (parameters flow through)
                if matches!(ctrl.instruction, If { .. }) && !ctrl.end_types.is_empty() && !ctrl.unreachable {
                    // Check if start_types == end_types (parameters flow through)
                    if ctrl.start_types != ctrl.end_types {
                        return Err(ValidationError::TypeMismatch);
                    }
                }

                self.push_vals(ctrl.end_types).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Return => {
                // Return targets the outermost label
                let li = self.ctrls.len() as u32 - 1;
                let label_types = self.label_types_at(li)?;
                self.pop_expecteds(label_types).ok_or(ValidationError::TypeMismatch)?;
                self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Br { label_idx } => {
                let label_types = self.label_types_at(*label_idx)?;
                self.pop_expecteds(label_types).ok_or(ValidationError::TypeMismatch)?;
                self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            BrIf { label_idx } => {
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                let label_types = self.label_types_at(*label_idx)?;
                self.pop_expecteds(label_types.clone())
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_vals(label_types).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            BrTable { labels, default } => {
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                if self.ctrls.len() < *default as usize {
                    return Err(ValidationError::UnknownLabel);
                }

                // First, check that all labels exist
                for &li in labels.iter() {
                    if self.ctrls.len() <= li as usize {
                        return Err(ValidationError::UnknownLabel);
                    }
                }

                // Get the result types for the default label
                let default_label_types = self.label_types_at(*default)?;
                let arity = default_label_types.len();

                // All branch targets must have the same arity
                for &li in labels.iter() {
                    let label_types = self.label_types_at(li)?;
                    if label_types.len() != arity {
                        return Err(ValidationError::TypeMismatch);
                    }
                }

                // Check if we're in an unreachable state
                let is_unreachable = self.ctrls.last().map(|c| c.unreachable).unwrap_or(false);

                if is_unreachable {
                    // In unreachable state, we need to be more careful
                    // We still validate, but we allow polymorphic values
                    let saved_stack_height = self.vals.len();

                    // Try to validate each branch
                    let mut validation_failed = false;
                    for &li in labels.iter() {
                        let label_types = self.label_types_at(li)?;

                        // Try to pop expected types
                        let mut can_satisfy = true;
                        for expected_type in label_types.iter().rev() {
                            if let Some(actual) = self.pop_val() {
                                // If we get Unknown (polymorphic), it can satisfy any type
                                // If we get a concrete type, it must match
                                if actual != Unknown && actual != *expected_type {
                                    can_satisfy = false;
                                    break;
                                }
                            } else {
                                can_satisfy = false;
                                break;
                            }
                        }

                        // Restore stack for next iteration
                        self.vals.truncate(saved_stack_height);

                        if !can_satisfy {
                            validation_failed = true;
                            break;
                        }
                    }

                    if validation_failed {
                        return Err(ValidationError::TypeMismatch);
                    }

                    // Also validate the default branch
                    for expected_type in default_label_types.iter().rev() {
                        if let Some(actual) = self.pop_val() {
                            if actual != Unknown && actual != *expected_type {
                                self.vals.truncate(saved_stack_height);
                                return Err(ValidationError::TypeMismatch);
                            }
                        } else {
                            self.vals.truncate(saved_stack_height);
                            return Err(ValidationError::TypeMismatch);
                        }
                    }
                } else {
                    // Normal validation
                    labels.iter().try_for_each(|&li| {
                        let label_types = self.label_types_at(li)?;
                        let popped = self.pop_expecteds(label_types.clone());
                        if popped.is_none() {
                            Err(ValidationError::TypeMismatch)
                        } else {
                            self.push_vals(label_types).ok_or(ValidationError::TypeMismatch)?;
                            Ok(())
                        }
                    })?;

                    // Pop for the default branch
                    self.pop_expecteds(default_label_types)
                        .ok_or(ValidationError::TypeMismatch)?;
                }
                self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Unreachable => {
                self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Call { func_idx } => {
                // Get type index directly from pre-computed data
                let type_idx = self
                    .ctx
                    .function_type_indices
                    .get(*func_idx as usize)
                    .ok_or(ValidationError::UnknownFunction(*func_idx))?;

                let ftype = self
                    .module
                    .types
                    .get(*type_idx)
                    .ok_or(ValidationError::UnknownFunctionType)?;

                // parameters are stack ordered, so pick them in reverse
                for param in ftype.parameters.iter().rev() {
                    self.pop_expected(Val(*param)).ok_or(ValidationError::TypeMismatch)?;
                }

                for return_type in &ftype.return_types {
                    self.push_val(Val(*return_type)).ok_or(ValidationError::TypeMismatch)?;
                }
                Ok(())
            }

            CallIndirect { type_idx, table_idx } => {
                // Validate table exists using pre-computed data
                if *table_idx as usize >= self.ctx.has_table.len() || !self.ctx.has_table[*table_idx as usize] {
                    return Err(ValidationError::UnknownTable);
                }

                let table = self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;

                if ValueType::from(table.ref_type) != FuncRef {
                    return Err(ValidationError::TypeMismatch);
                }

                // operand that directs us to the table entry
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;

                // table entry must have this function signature
                let ftype = self
                    .module
                    .types
                    .get(*type_idx)
                    .ok_or(ValidationError::UnknownFunctionType)?;

                // parameters are stack ordered, so pick them in reverse
                for param in ftype.parameters.iter().rev() {
                    self.pop_expected(Val(*param)).ok_or(ValidationError::TypeMismatch)?;
                }

                for return_type in &ftype.return_types {
                    self.push_val(Val(*return_type)).ok_or(ValidationError::TypeMismatch)?;
                }
                Ok(())
            }

            Select => {
                // Select pops: [t t i32] and pushes: [t]
                // Note: We report TypeMismatch for insufficient operands rather than InvalidResultArity.
                // This is technically correct at the binary level, though some validators (like the
                // reference interpreter) may report InvalidResultArity when the original WAT had
                // explicit result arity declarations that are lost during WAT→WASM compilation.

                // Pop condition (i32)
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                // Pop two values of same type
                let t2 = self.pop_val().ok_or(ValidationError::TypeMismatch)?.clone();
                let t1 = self.pop_val().ok_or(ValidationError::TypeMismatch)?.clone();

                // Check if types are compatible
                if (t1.is_num() && t2.is_num()) || (t1.is_vec() && t2.is_vec()) {
                    if t1 != t2 && t1 != Unknown && t2 != Unknown {
                        Err(ValidationError::TypeMismatch)
                    } else {
                        // Push the result type
                        self.push_val(if t1 == Unknown { t2.clone() } else { t1.clone() })
                            .ok_or(ValidationError::TypeMismatch)?;
                        Ok(())
                    }
                } else {
                    Err(ValidationError::TypeMismatch)
                }
            }

            SelectTyped { val_types } => {
                // SelectT has explicit type immediates in the instruction data
                if val_types.is_empty() {
                    // Empty result arity is a specific error
                    return Err(ValidationError::InvalidResultArity);
                }
                if val_types.len() != 1 {
                    // Select should have exactly one result type.
                    return Err(ValidationError::InvalidResultArity);
                }
                let result_type = val_types[0];

                // Pop condition (i32)
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;

                // Pop two values of the result type
                let t2 = self.pop_val().ok_or(ValidationError::TypeMismatch)?;
                let t1 = self.pop_val().ok_or(ValidationError::TypeMismatch)?;

                // Check that popped values match the expected type
                let expected = Val(result_type);
                if !matches!(t1, Unknown) && t1 != expected {
                    return Err(ValidationError::TypeMismatch);
                }
                if !matches!(t2, Unknown) && t2 != expected {
                    return Err(ValidationError::TypeMismatch);
                }

                // Push result type
                self.push_val(Val(result_type)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            RefFunc { func_idx } => {
                self.get_function_type(*func_idx)?;
                // Check if this is a self-reference
                if *func_idx == self.current_function_index {
                    // Allow self-reference if the function is declared in a declarative element
                    // Declarative elements exist specifically to allow functions to be referenced
                    // before they are fully validated, enabling self-references and forward references
                    let is_declared_in_declarative = self.module.elements.elements.iter().any(|elem| {
                        matches!(elem.mode, ElementMode::Declarative)
                            && elem.init.iter().any(|init_exprs| {
                                init_exprs.iter().any(|instr| {
                                    matches!(instr.kind, InstructionKind::RefFunc { func_idx: idx } if idx == *func_idx)
                                })
                            })
                    });

                    if !is_declared_in_declarative {
                        return Err(ValidationError::UndeclaredFunctionReference);
                    }
                }
                self.push_val(Val(FuncRef)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            RefNull { ref_type } => {
                self.push_val(Val(*ref_type)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            RefIsNull => {
                // ref.is_null pops a reference type and pushes i32
                let ref_type = self.pop_val().ok_or(ValidationError::TypeMismatch)?;
                // Check that popped value is a reference type
                if !ref_type.is_ref() {
                    return Err(ValidationError::TypeMismatch);
                }
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Drop => {
                self.pop_val().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Nop => Ok(()),

            TableSize { table_idx } => {
                // Verify table exists
                self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;
                // table.size pushes the current size as i32
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableGrow { table_idx } => {
                let table = self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;
                // table.grow pops: [n:i32, init:reftype] and pushes: [prev_size:i32]
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(table.ref_type.into()))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableFill { table_idx } => {
                let table = self.module.get_table(*table_idx).ok_or(ValidationError::UnknownTable)?;
                // table.fill pops: [i:i32, val:reftype, n:i32]
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(table.ref_type.into()))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            _ => Err(ValidationError::UnimplementedInstruction),
        }
    }

    fn ended(&mut self) -> bool {
        self.need_end && self.ctrls.is_empty()
    }

    fn finalise(&mut self) -> Result<(), ValidationError> {
        // After all instructions are processed, check that the stack state
        // matches the expected function return types
        if !self.ctrls.is_empty() {
            return Err(ValidationError::UnexpectedEndOfFunction);
        }

        // Check that the stack has exactly the expected return values
        let expected_count = self.function_type.return_types.len();
        let actual_count = self.vals.len();

        if actual_count != expected_count {
            return Err(ValidationError::InvalidResultArity);
        }

        // Check that the types match
        for (i, expected_type) in self.function_type.return_types.iter().enumerate() {
            if let Some(Val(actual_type)) = self.vals.get(i)
                && actual_type != expected_type
            {
                return Err(ValidationError::TypeMismatch);
            }
        }

        Ok(())
    }
}

fn check_alignment(memarg: &crate::parser::instruction::MemArg, align_exponent: u32) -> Result<(), ValidationError> {
    if memarg.align > align_exponent {
        return Err(ValidationError::BadAlignment);
    }
    Ok(())
}
