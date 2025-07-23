use super::module::{FunctionType, GlobalType, ValueType, ValueType::*};
use crate::parser::ast;
use ast::InstructionType::*;
use ast::{BlockType, Instruction, InstructionData, InstructionType};
use thiserror::Error;
use MaybeValue::{Unknown, Val};

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

    #[error("unknown memory")]
    UnknownMemory,

    #[error("unknown element")]
    UnknownElement,

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

    #[error("data count section required")]
    DataCountSectionRequired,

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
    fn finalize(&mut self) -> Result<(), ValidationError> {
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
            .map(|global_type| match global_type.external_kind {
                super::module::ExternalKind::Global(ref global_type) => {
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
        match *inst.get_type() {
            I32Const | I64Const | F32Const | F64Const | RefNull | RefFunc | GlobalGet | End => {}
            _ => return Err(ValidationError::ConstantExpressionRequired),
        }
        self.instr_count += 1;
        if self.instr_count >= 2 && *inst.get_type() != End {
            return Err(ValidationError::TypeMismatch);
        }
        match inst.get_type() {
            // const type must match self.return_type
            I32Const => (self.return_type == I32)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            I64Const => (self.return_type == I64)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            F32Const => (self.return_type == F32)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            F64Const => (self.return_type == F64)
                .then_some(())
                .ok_or(ValidationError::TypeMismatch),
            RefNull => {
                if let InstructionData::RefType { ref_type } = *inst.get_data() {
                    if self.return_type == FuncRef && ref_type == FuncRef
                        || self.return_type == ExternRef && ref_type == ExternRef
                    {
                        Ok(())
                    } else {
                        Err(ValidationError::TypeMismatch)
                    }
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }
            RefFunc => {
                // First check the return type
                if self.return_type != FuncRef && self.return_type != ExternRef {
                    return Err(ValidationError::TypeMismatch);
                }
                // Then check if the function index is valid
                if let InstructionData::Function { function_index } = *inst.get_data() {
                    if let Some(total) = self.total_functions {
                        if function_index >= total {
                            return Err(ValidationError::UnknownFunction(function_index));
                        }
                    }
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }
            GlobalGet => {
                if let InstructionData::Global { global_index: gi } = *inst.get_data() {
                    let value_type = self.global(gi)?.value_type;
                    if value_type == self.return_type {
                        Ok(())
                    } else {
                        Err(ValidationError::TypeMismatch)
                    }
                } else {
                    Err(ValidationError::InvalidInstruction)
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
enum MaybeValue {
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
    instruction: InstructionType,
    start_types: Vec<MaybeValue>,
    end_types: Vec<MaybeValue>,
    height: u32,
    unreachable: bool,
}

// TODO: this should probably just augment the Module Unit itself so we have access to all of this
pub struct CodeValidator<'a> {
    module: &'a super::module::Module,
    locals: &'a super::module::Locals,
    params: Vec<ValueType>,
    return_types: Vec<ValueType>,
    vals: Vec<MaybeValue>,
    ctrls: Vec<CtrlFrame>,
    need_end: bool,
    current_function_index: u32,
}

impl<'a> CodeValidator<'a> {
    pub fn new(
        module: &'a super::module::Module,
        locals: &'a super::module::Locals,
        function_type: &'a FunctionType,
        current_function_index: u32,
    ) -> CodeValidator<'a> {
        let mut v = CodeValidator {
            module,
            locals,
            params: function_type.parameters.clone(),
            return_types: function_type.return_types.clone(),
            vals: vec![],
            ctrls: vec![],
            need_end: false,
            current_function_index,
        };

        // push the return types but leave the parameters blank as they aren't on the stack
        // until explicitly loaded with local.get
        let end_types = function_type.return_types.iter().map(|v| Val(*v)).collect();
        v.push_ctrl(Block, vec![], end_types);

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

    fn push_ctrl(&mut self, instruction: InstructionType, start_types: Vec<MaybeValue>, end_types: Vec<MaybeValue>) {
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
        if frame.instruction == Loop {
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
        let local: Option<&ValueType> = if (li as usize) < self.params.len() {
            self.params.get(li as usize)
        } else {
            let li = li - self.params.len() as u32;
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
                    super::module::ExternalKind::Global(ref global_type) => Some(global_type),
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
        if (fi as usize) < self.module.imports.function_count() {
            self.module
                .imports
                .get_function_type_index(fi)
                .ok_or(ValidationError::UnknownFunction(fi))
                .and_then(|ti| self.get_type(ti))
        } else {
            self.module
                .functions
                .get((fi as u8) - (self.module.imports.function_count() as u8))
                .ok_or(ValidationError::UnknownFunction(fi))
                .and_then(|f| self.get_type(f.ftype_index))
        }
    }

    fn get_type(&self, ti: u32) -> Result<&FunctionType, ValidationError> {
        self.module
            .types
            .get(ti as u8)
            .ok_or(ValidationError::UnknownFunctionType)
    }
}

impl Validator for CodeValidator<'_> {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError> {
        match inst.get_type() {
            I32Const => self.push_val(Val(I32_VALUE)).ok_or(ValidationError::TypeMismatch),

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

            I64Const => self.push_val(Val(I64_VALUE)).ok_or(ValidationError::TypeMismatch),

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

            F32Const => self.push_val(Val(F32_VALUE)).ok_or(ValidationError::TypeMismatch),

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

            F64Const => self.push_val(Val(F64_VALUE)).ok_or(ValidationError::TypeMismatch),

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

            LocalGet => {
                if let InstructionData::Local { local_index: li } = *inst.get_data() {
                    let local = *self.local(li)?;
                    self.push_val(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            LocalSet => {
                if let InstructionData::Local { local_index: li } = *inst.get_data() {
                    let local = *self.local(li)?;
                    self.pop_expected(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            LocalTee => {
                if let InstructionData::Local { local_index: li } = *inst.get_data() {
                    let local = *self.local(li)?;
                    self.pop_expected(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                    self.push_val(Val(local)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            GlobalGet => {
                if let InstructionData::Global { global_index: gi } = *inst.get_data() {
                    let global = *self.global(gi)?;
                    self.push_val(Val(global.value_type))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            GlobalSet => {
                if let InstructionData::Global { global_index: gi } = *inst.get_data() {
                    let global = *self.global(gi)?;
                    if !global.mutable {
                        return Err(ValidationError::GlobalIsImmutable);
                    }
                    self.pop_expected(Val(global.value_type))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            I32Load | I32Load8S | I32Load8U | I32Load16S | I32Load16U => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    inst,
                    match inst.get_type() {
                        &I32Load => 2,                  // 4 bytes = 2^2
                        &I32Load8S | &I32Load8U => 0,   // 1 byte = 2^0
                        &I32Load16S | &I32Load16U => 1, // 2 bytes = 2^1
                        _ => unreachable!(),
                    },
                )
            }

            I64Load | I64Load8S | I64Load8U | I64Load16S | I64Load16U | I64Load32S | I64Load32U => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I64)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    inst,
                    match inst.get_type() {
                        &I64Load => 3,                  // 8 bytes = 2^3
                        &I64Load8S | &I64Load8U => 0,   // 1 byte = 2^0
                        &I64Load16S | &I64Load16U => 1, // 2 bytes = 2^1
                        &I64Load32S | &I64Load32U => 2, // 4 bytes = 2^2
                        _ => unreachable!(),
                    },
                )
            }

            F32Load => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(inst, 2 /* 4 bytes = 2^2 */)
            }

            F64Load => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F64)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(inst, 3 /* 8 bytes = 2^3 */)
            }

            I32Store | I32Store8 | I32Store16 => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    inst,
                    match inst.get_type() {
                        I32Store => 2,   // 4 bytes = 2^2
                        I32Store8 => 0,  // 1 byte = 2^0
                        I32Store16 => 1, // 2 bytes = 2^1
                        _ => unreachable!(),
                    },
                )
            }

            I64Store | I64Store8 | I64Store16 | I64Store32 => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I64)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    inst,
                    match inst.get_type() {
                        I64Store => 3,   // 8 bytes = 2^3
                        I64Store8 => 0,  // 1 byte = 2^0
                        I64Store16 => 1, // 2 bytes = 2^1
                        I64Store32 => 2, // 4 bytes = 2^2
                        _ => unreachable!(),
                    },
                )
            }

            F32Store => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(F32)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(inst, 2 /* 4 bytes = 2^2 */)
            }

            F64Store => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(F64)).ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                check_alignment(inst, 3 /* 8 bytes = 2^3 */)
            }

            MemorySize => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryGrow => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryFill | MemoryCopy => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryInit => {
                // Check that memory exists (imported or declared)
                if self.module.memory.is_empty() && self.module.imports.memory_count() == 0 {
                    return Err(ValidationError::UnknownMemory);
                }
                if self.module.data_count.count == 0 {
                    return Err(ValidationError::DataCountSectionRequired);
                }
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableInit => {
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                if let InstructionData::TableInit {
                    table_index,
                    elem_index,
                    ..
                } = *inst.get_data()
                {
                    let table = self
                        .module
                        .table
                        .tables
                        .get(table_index as usize)
                        .ok_or(ValidationError::UnknownTable)?;
                    let elem = self
                        .module
                        .elements
                        .elements
                        .get(elem_index as usize)
                        .ok_or(ValidationError::UnknownElement)?;
                    if elem.ref_type != table.ref_type {
                        return Err(ValidationError::TypeMismatch);
                    }
                }
                Ok(())
            }

            TableCopy => {
                // TODO: check that the destination table index exists
                // TODO: check that the source element index exists (for TableInit)
                // TODO: check that the source table index exists (for TableInit)
                // TODO: check that the reference type of the source and destination match
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableGet => {
                if let InstructionData::Table { table_index, .. } = *inst.get_data() {
                    let table = self
                        .module
                        .get_table(table_index)
                        .ok_or(ValidationError::UnknownTable)?;
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                    self.push_val(Val(table.ref_type.into()))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            TableSet => {
                if let InstructionData::Table { table_index, .. } = *inst.get_data() {
                    let table = self
                        .module
                        .get_table(table_index)
                        .ok_or(ValidationError::UnknownTable)?;
                    self.pop_expected(Val(table.ref_type.into()))
                        .ok_or(ValidationError::TypeMismatch)?;
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            ElemDrop => {
                // TODO: check that the element index exists
                Ok(())
            }

            DataDrop => {
                if self.module.data_count.count == 0 {
                    Err(ValidationError::DataCountSectionRequired)
                } else {
                    Ok(())
                }
            }

            Block | Loop | If => {
                // special case for If we need to pop an i32
                if inst.get_type() == &If {
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                }

                if let InstructionData::Block { blocktype: bt } = *inst.get_data() {
                    let mut start_types = Vec::new();
                    let mut end_types = Vec::new();
                    match bt {
                        // simple []->[type] version
                        BlockType::Type(t) => {
                            end_types.push(Val(t));
                            Ok(())
                        }
                        BlockType::Empty => Ok(()),
                        // complex [type*]->[type*] version
                        BlockType::TypeIndex(ti) => {
                            let ftype = self
                                .module
                                .types
                                .get(ti as u8)
                                .ok_or(ValidationError::UnknownBlockType)?;
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
                            Ok(())
                        }
                    }?;
                    self.push_ctrl(*inst.get_type(), start_types, end_types);
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            Else => {
                let ctrl = self.pop_ctrl()?;
                if ctrl.instruction != If {
                    Err(ValidationError::InvalidInstruction)
                } else {
                    // Mark that we had an else by pushing Else control frame
                    self.push_ctrl(Else, ctrl.start_types, ctrl.end_types);
                    Ok(())
                }
            }

            End => {
                let ctrl = self.pop_ctrl()?;

                // Special check: typed if without else is invalid
                // This happens when we pop an If control (not Else) that has result types
                if ctrl.instruction == If && !ctrl.end_types.is_empty() && !ctrl.unreachable {
                    return Err(ValidationError::TypeMismatch);
                }

                self.push_vals(ctrl.end_types).ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Return | Br | BrIf => {
                let li: u32 = if let InstructionData::Labelled { label_index: li } = *inst.get_data() {
                    li
                } else {
                    // Return, outermost label
                    self.ctrls.len() as u32 - 1
                };

                if inst.get_type() == &BrIf {
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                }

                let label_types = self.label_types_at(li)?;
                self.pop_expecteds(label_types.clone())
                    .ok_or(ValidationError::TypeMismatch)?;
                match inst.get_type() {
                    &Return | &Br => {
                        self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                        Ok(())
                    }
                    &BrIf => {
                        self.push_vals(label_types).ok_or(ValidationError::TypeMismatch)?;
                        Ok(())
                    }
                    _ => Err(ValidationError::InvalidInstruction),
                }
            }

            BrTable => {
                if let InstructionData::TableLabelled {
                    ref labels,
                    label_index: li,
                } = *inst.get_data()
                {
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;
                    if self.ctrls.len() < li as usize {
                        return Err(ValidationError::UnknownLabel);
                    }
                    let label_types = self.label_types_at(li)?;
                    let arity = label_types.len();
                    labels.iter().try_for_each(|&li| {
                        if self.ctrls.len() < li as usize {
                            Err(ValidationError::UnknownLabel)
                        } else {
                            let label_types = self.label_types_at(li)?;
                            if label_types.len() != arity {
                                Err(ValidationError::TypeMismatch)
                            } else {
                                let popped = self.pop_expecteds(label_types.clone());
                                if popped.is_none() {
                                    Err(ValidationError::TypeMismatch)
                                } else {
                                    self.push_vals(label_types).ok_or(ValidationError::TypeMismatch)?;
                                    Ok(())
                                }
                            }
                        }
                    })?;
                    self.pop_expecteds(label_types).ok_or(ValidationError::TypeMismatch)?;
                    self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            Unreachable => {
                self.unreachable().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Call => {
                if let InstructionData::Function { function_index: fi } = *inst.get_data() {
                    let ftype: &FunctionType = self.get_function_type(fi)?;
                    let parameters: Vec<_> = ftype.parameters.to_vec();
                    let return_types: Vec<_> = ftype.return_types.to_vec();

                    // parameters are stack ordered, so pick them in reverse
                    parameters
                        .iter()
                        .rev()
                        .try_for_each(|v| match self.pop_expected(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    return_types.iter().try_for_each(|v| match self.push_val(Val(*v)) {
                        Some(_) => Ok(()),
                        None => Err(ValidationError::TypeMismatch),
                    })?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            CallIndirect => {
                if let InstructionData::Indirect {
                    type_index: typi,
                    table_index: tabi,
                } = *inst.get_data()
                {
                    // TOOD: this probably should check that table_index exists in the table space

                    let table = self.module.get_table(tabi).ok_or(ValidationError::UnknownTable)?;

                    if ValueType::from(table.ref_type) != FuncRef {
                        return Err(ValidationError::TypeMismatch);
                    }

                    // operand that directs us to the table entry
                    self.pop_expected(Val(I32)).ok_or(ValidationError::TypeMismatch)?;

                    // table entry must have this function signature
                    let ftype = self.get_type(typi)?;
                    let parameters: Vec<_> = ftype.parameters.to_vec();
                    let return_types: Vec<_> = ftype.return_types.to_vec();

                    // parameters are stack ordered, so pick them in reverse
                    parameters
                        .iter()
                        .rev()
                        .try_for_each(|v| match self.pop_expected(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    return_types.iter().try_for_each(|v| match self.push_val(Val(*v)) {
                        Some(_) => Ok(()),
                        None => Err(ValidationError::TypeMismatch),
                    })?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            Select => {
                // Select pops: [t t i32] and pushes: [t]
                // Note: If there aren't enough values on the stack, we report TypeMismatch
                // rather than InvalidResultArity. This differs from some validators but
                // both errors are valid for malformed select instructions.

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

            SelectT => {
                // SelectT has explicit type immediates in the instruction data
                if let InstructionData::ValueType { value_types } = inst.get_data() {
                    if value_types.is_empty() {
                        // Empty result arity is a specific error
                        return Err(ValidationError::InvalidResultArity);
                    }
                    if value_types.len() != 1 {
                        // Select should have exactly one result type.
                        return Err(ValidationError::InvalidResultArity);
                    }
                    // Decode the value type from byte
                    let result_type = ValueType::decode(value_types[0]).map_err(|_| ValidationError::TypeMismatch)?;

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
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            RefFunc => {
                if let InstructionData::Function { function_index: fi } = *inst.get_data() {
                    self.get_function_type(fi)?;
                    // Check if this is a self-reference
                    // During validation of a function body, that function is not yet declared
                    // So it cannot reference itself with ref.func
                    if fi == self.current_function_index {
                        return Err(ValidationError::UndeclaredFunctionReference);
                    }
                    self.push_val(Val(FuncRef)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            RefNull => {
                if let InstructionData::RefType { ref_type } = *inst.get_data() {
                    self.push_val(Val(ref_type)).ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
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

            _ => Err(ValidationError::UnimplementedInstruction),
        }
    }

    fn ended(&mut self) -> bool {
        self.need_end && self.ctrls.is_empty()
    }

    fn finalize(&mut self) -> Result<(), ValidationError> {
        // After all instructions are processed, check that the stack state
        // matches the expected function return types
        if !self.ctrls.is_empty() {
            return Err(ValidationError::InvalidInstruction);
        }

        // Check that the stack has exactly the expected return values
        let expected_count = self.return_types.len();
        let actual_count = self.vals.len();

        if actual_count != expected_count {
            return Err(ValidationError::InvalidResultArity);
        }

        // Check that the types match
        for (i, expected_type) in self.return_types.iter().enumerate() {
            if let Some(Val(actual_type)) = self.vals.get(i) {
                if actual_type != expected_type {
                    return Err(ValidationError::TypeMismatch);
                }
            }
        }

        Ok(())
    }
}

fn check_alignment(inst: &Instruction, align_exponent: u32) -> Result<(), ValidationError> {
    let align: u32 = if let InstructionData::Memory {
        subopcode_bytes: _,
        memarg,
    } = *inst.get_data()
    {
        memarg.0
    } else {
        return Err(ValidationError::InvalidInstruction);
    };

    if align > align_exponent {
        return Err(ValidationError::BadAlignment);
    }

    Ok(())
}
