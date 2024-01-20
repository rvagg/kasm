use super::module::{FunctionType, GlobalType, ValueType, ValueType::*};
use crate::parser::ast;
use ast::InstructionType::*;
use ast::{BlockType, Instruction, InstructionData, InstructionType};
use thiserror::Error;
use MaybeValue::{Unknown, Val};

#[derive(Error, Debug)]
pub enum ValidationError {
    #[error("type mismatch")]
    TypeMismatch,

    #[error("unknown function type")]
    UnknownFunctionType,

    #[error("unexpected token")]
    UnexpectedToken,

    #[error("unknown local")]
    UnknownLocal,

    #[error("unknown global")]
    UnknownGlobal,

    #[error("unknown label")]
    UnknownLabel,

    #[error("invalid instruction")]
    InvalidInstruction,

    #[error("data count section required")]
    DataCountSectionRequired,

    #[error("unknown block type")]
    UnknownBlockType,

    #[error("unimplemented instruction")]
    UnimplementedInstruction,

    #[error("alignment must not be larger than natural")]
    BadAlignment,
}

pub trait Validator {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError>;
    fn ended(&mut self) -> bool;
}

pub struct ConstantExpressionValidator<'a> {
    globals: &'a Vec<GlobalType>,
    return_type: ValueType,
    has_end: bool,
}

impl ConstantExpressionValidator<'_> {
    pub fn new<'a>(
        globals: &'a Vec<GlobalType>,
        return_type: super::module::ValueType,
    ) -> ConstantExpressionValidator<'a> {
        ConstantExpressionValidator {
            globals,
            return_type,
            has_end: false,
        }
    }
}

impl Validator for ConstantExpressionValidator<'_> {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError> {
        match inst.get_type() {
            // const type must match self.return_type
            I32Const => (self.return_type == I32)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch),
            I64Const => (self.return_type == I64)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch),
            F32Const => (self.return_type == F32)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch),
            F64Const => (self.return_type == F64)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch),
            RefNull => (self.return_type == FuncRef || self.return_type == ExternRef)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch),
            RefFunc => (self.return_type == FuncRef || self.return_type == ExternRef)
                .then(|| ())
                .ok_or(ValidationError::TypeMismatch), // TODO: should check that the function index exists? but this implies the functions section is required
            GlobalGet => {
                // TODO: globals are further constrained in that they must be imported, do that here by requiring imports too?
                if let InstructionData::GlobalInstruction { global_index: gi } = *inst.get_data() {
                    match self.globals.get(gi as usize) {
                        Some(global) if !global.mutable => Ok(()),
                        Some(_) => Err(ValidationError::InvalidInstruction),
                        None => Err(ValidationError::UnknownGlobal),
                    }
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }
            End => {
                self.has_end = true;
                Ok(())
            }
            _ => Err(ValidationError::InvalidInstruction),
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
    types: &'a super::module::TypeSection,
    functions: &'a super::module::FunctionSection,
    globals: &'a Vec<GlobalType>,
    data_count: u32,
    locals: &'a super::module::Locals,
    params: Vec<ValueType>,
    vals: Vec<MaybeValue>,
    ctrls: Vec<CtrlFrame>,
    need_end: bool,
}

pub fn function_type<'a>(
    types: &'a super::module::TypeSection,
    functions: &'a super::module::FunctionSection,
    fi: u32,
) -> Result<&'a super::module::FunctionType, ValidationError> {
    let ftype = functions
        .get(fi as u8)
        .and_then(|f| types.get(f.ftype_index as u8))
        .ok_or(ValidationError::UnknownFunctionType)?;
    Ok(ftype)
}

impl<'a> CodeValidator<'a> {
    pub fn new<'b>(
        types: &'b super::module::TypeSection,
        functions: &'b super::module::FunctionSection,
        globals: &'b Vec<GlobalType>,
        data_count: u32,
        locals: &'b super::module::Locals,
        function_type: &'b FunctionType,
    ) -> CodeValidator<'b> {
        // TODO: should we Result<> this?
        // let ftype: &FunctionType = function_type(types, functions, function_index).unwrap();

        let mut v = CodeValidator {
            types,
            functions,
            globals,
            data_count,
            locals,
            params: function_type.parameters.clone(),
            vals: vec![],
            ctrls: vec![],
            need_end: false,
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
        } else if self.vals.len() as u32 == self.ctrls.last()?.height {
            None
        } else if self.vals.len() == 0 {
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

    fn push_ctrl(
        &mut self,
        instruction: InstructionType,
        start_types: Vec<MaybeValue>,
        end_types: Vec<MaybeValue>,
    ) {
        self.ctrls.push(CtrlFrame {
            instruction: instruction,
            start_types: start_types.clone(),
            end_types: end_types,
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
            let end_types_clone = self.ctrls.last().unwrap().end_types.clone();
            if self.pop_expecteds(end_types_clone).is_none() {
                Err(ValidationError::TypeMismatch)
            } else {
                if self.vals.len() != self.ctrls.last().unwrap().height as usize {
                    Err(ValidationError::TypeMismatch)
                } else {
                    Ok(self.ctrls.pop().unwrap())
                }
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

    fn sig_unary(
        &mut self,
        in_type: MaybeValue,
        out_type: MaybeValue,
    ) -> Result<(), ValidationError> {
        self.pop_expected(in_type)
            .ok_or(ValidationError::TypeMismatch)?;
        self.push_val(out_type).ok_or(ValidationError::TypeMismatch)
    }

    fn sig_binary(
        &mut self,
        in1_type: MaybeValue,
        in2_type: MaybeValue,
        out_type: MaybeValue,
    ) -> Result<(), ValidationError> {
        self.pop_expected(in1_type)
            .ok_or(ValidationError::TypeMismatch)?;
        self.pop_expected(in2_type)
            .ok_or(ValidationError::TypeMismatch)?;
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

        local.ok_or(ValidationError::UnknownLocal)
    }

    fn global(&mut self, global_index: u32) -> Result<&GlobalType, ValidationError> {
        self.globals
            .get(global_index as usize)
            .ok_or(ValidationError::UnknownGlobal)
    }

    fn label_types_at(&mut self, li: u32) -> Result<Vec<MaybeValue>, ValidationError> {
        if self.ctrls.len() <= li as usize {
            return Err(ValidationError::UnknownLabel);
        }
        let index = self.ctrls.len() - li as usize - 1;
        let frame = self
            .ctrls
            .get(index)
            .ok_or(ValidationError::UnknownLabel)?
            .clone();
        Ok(self.frame_types(frame))
    }

    fn function_type(&mut self, fi: u32) -> Result<&FunctionType, ValidationError> {
        return function_type(self.types, self.functions, fi);
    }
}

impl Validator for CodeValidator<'_> {
    fn validate(&mut self, inst: &Instruction) -> Result<(), ValidationError> {
        match inst.get_type() {
            I32Const => self
                .push_val(Val(I32_VALUE))
                .ok_or(ValidationError::TypeMismatch),

            // iunop (i32):i32
            I32Clz | I32Ctz | I32Popcnt => self.sig_unary(Val(I32_VALUE), Val(I32_VALUE)),

            // ibinop (i32,i32):i32
            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
            | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotr | I32Rotl => {
                self.sig_binary(Val(I32_VALUE), Val(I32_VALUE), Val(I32_VALUE))
            }

            // itestop (i32):i32
            I32Eqz => self.sig_unary(Val(I32_VALUE), Val(I32_VALUE)),

            // irelop (i32,i32):i32
            I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS
            | I32GeU => self.sig_binary(Val(I32_VALUE), Val(I32_VALUE), Val(I32_VALUE)),

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

            I64Const => self
                .push_val(Val(I64_VALUE))
                .ok_or(ValidationError::TypeMismatch),

            // iunop (i64):i64
            I64Clz | I64Ctz | I64Popcnt => self.sig_unary(Val(I64_VALUE), Val(I64_VALUE)),

            // ibinop (i64,i64):i64
            I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
            | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotr | I64Rotl => {
                self.sig_binary(Val(I64_VALUE), Val(I64_VALUE), Val(I64_VALUE))
            }

            // itestop (i64):i32
            I64Eqz => self.sig_unary(Val(I64_VALUE), Val(I32_VALUE)),

            // irelop (i64,i64):i32
            I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS
            | I64GeU => self.sig_binary(Val(I64_VALUE), Val(I64_VALUE), Val(I32_VALUE)),

            // cvtop (i64):i64
            I64Extend8S | I64Extend16S | I64Extend32S => {
                self.sig_unary(Val(I64_VALUE), Val(I64_VALUE))
            }

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

            F32Const => self
                .push_val(Val(F32_VALUE))
                .ok_or(ValidationError::TypeMismatch),

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
            F32ConvertI32S | F32ConvertI32U | F32ReinterpretI32 => {
                self.sig_unary(Val(I32_VALUE), Val(F32_VALUE))
            }

            // cvtop (i64):f32
            F32ConvertI64S | F32ConvertI64U => self.sig_unary(Val(I64_VALUE), Val(F32_VALUE)),

            F64Const => self
                .push_val(Val(F64_VALUE))
                .ok_or(ValidationError::TypeMismatch),

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
            F64ConvertI64S | F64ConvertI64U | F64ReinterpretI64 => {
                self.sig_unary(Val(I64_VALUE), Val(F64_VALUE))
            }

            LocalGet => {
                if let InstructionData::LocalInstruction { local_index: li } = *inst.get_data() {
                    let local = self.local(li)?.clone();
                    self.push_val(Val(local))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            LocalSet => {
                if let InstructionData::LocalInstruction { local_index: li } = *inst.get_data() {
                    let local = self.local(li)?.clone();
                    self.pop_expected(Val(local))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            LocalTee => {
                if let InstructionData::LocalInstruction { local_index: li } = *inst.get_data() {
                    let local = self.local(li)?.clone();
                    self.pop_expected(Val(local))
                        .ok_or(ValidationError::TypeMismatch)?;
                    self.push_val(Val(local))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            GlobalGet => {
                if let InstructionData::GlobalInstruction { global_index: gi } = *inst.get_data() {
                    let global = self.global(gi)?.clone();
                    self.push_val(Val(global.value_type))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            GlobalSet => {
                if let InstructionData::GlobalInstruction { global_index: gi } = *inst.get_data() {
                    let global = self.global(gi)?.clone();
                    self.pop_expected(Val(global.value_type))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            I32Load | I32Load8S | I32Load8U | I32Load16S | I32Load16U => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    &inst,
                    match inst.get_type() {
                        &I32Load => 2,                  // 4 bytes = 2^2
                        &I32Load8S | &I32Load8U => 0,   // 1 byte = 2^0
                        &I32Load16S | &I32Load16U => 1, // 2 bytes = 2^1
                        _ => unreachable!(),
                    },
                )
            }

            I64Load | I64Load8S | I64Load8U | I64Load16S | I64Load16U | I64Load32S | I64Load32U => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I64))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    &inst,
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
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(&inst, 2 /* 4 bytes = 2^2 */)
            }

            F64Load => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(F64))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(&inst, 3 /* 8 bytes = 2^3 */)
            }

            I32Store | I32Store8 | I32Store16 => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    &inst,
                    match inst.get_type() {
                        &I32Store => 2,   // 4 bytes = 2^2
                        &I32Store8 => 0,  // 1 byte = 2^0
                        &I32Store16 => 1, // 2 bytes = 2^1
                        _ => unreachable!(),
                    },
                )
            }

            I64Store | I64Store8 | I64Store16 | I64Store32 => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I64))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(
                    &inst,
                    match inst.get_type() {
                        &I64Store => 3,   // 8 bytes = 2^3
                        &I64Store8 => 0,  // 1 byte = 2^0
                        &I64Store16 => 1, // 2 bytes = 2^1
                        &I64Store32 => 2, // 4 bytes = 2^2
                        _ => unreachable!(),
                    },
                )
            }

            F32Store => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(F32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(&inst, 2 /* 4 bytes = 2^2 */)
            }

            F64Store => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(F64))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                check_alignment(&inst, 3 /* 8 bytes = 2^3 */)
            }

            MemoryGrow => {
                // TODO: check that the memory index exists
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                self.push_val(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryFill | MemoryCopy => {
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            MemoryInit => {
                // TODO: check that the memory index exists
                if self.data_count == 0 {
                    return Err(ValidationError::DataCountSectionRequired);
                }
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            TableInit | TableCopy => {
                // TODO: check that the destination table index exists
                // TODO: check that the source element index exists (for TableInit)
                // TODO: check that the source table index exists (for TableInit)
                // TODO: check that the reference type of the source and destination match
                self.pop_expecteds(vec![Val(I32), Val(I32), Val(I32)])
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            ElemDrop => {
                // TODO: check that the element index exists
                Ok(())
            }

            DataDrop => {
                if self.data_count == 0 {
                    Err(ValidationError::DataCountSectionRequired)
                } else {
                    Ok(())
                }
            }

            Block | Loop | If => {
                // special case for If we need to pop an i32
                if inst.get_type() == &If {
                    self.pop_expected(Val(I32))
                        .ok_or(ValidationError::TypeMismatch)?;
                }

                if let InstructionData::BlockInstruction { blocktype: bt } = *inst.get_data() {
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
                                .types
                                .get(ti as u8)
                                .ok_or(ValidationError::UnknownBlockType)?;
                            ftype.parameters.iter().try_for_each(|v| {
                                start_types.push(Val(*v));
                                match self.pop_expected(Val(*v)) {
                                    Some(_) => Ok(()),
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
                    self.push_ctrl(Else, ctrl.start_types, ctrl.end_types);
                    Ok(())
                }
            }

            End => {
                let ctrl = self.pop_ctrl()?;
                self.push_vals(ctrl.end_types)
                    .ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Return | Br | BrIf => {
                let li: u32 = if let InstructionData::LabelledInstruction { label_index: li } =
                    *inst.get_data()
                {
                    li
                } else {
                    // Return, outermost label
                    self.ctrls.len() as u32 - 1
                };

                if inst.get_type() == &BrIf {
                    self.pop_expected(Val(I32))
                        .ok_or(ValidationError::TypeMismatch)?;
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
                        self.push_vals(label_types)
                            .ok_or(ValidationError::TypeMismatch)?;
                        Ok(())
                    }
                    _ => Err(ValidationError::InvalidInstruction),
                }
            }

            BrTable => {
                if let InstructionData::TableLabelledInstruction {
                    ref labels,
                    label_index: li,
                } = *inst.get_data()
                {
                    self.pop_expected(Val(I32))
                        .ok_or(ValidationError::TypeMismatch)?;
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
                                    self.push_vals(label_types)
                                        .ok_or(ValidationError::TypeMismatch)?;
                                    Ok(())
                                }
                            }
                        }
                    })?;
                    self.pop_expecteds(label_types)
                        .ok_or(ValidationError::TypeMismatch)?;
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
                if let InstructionData::FunctionInstruction { function_index: fi } =
                    *inst.get_data()
                {
                    let ftype: &FunctionType = self.function_type(fi)?;
                    let parameters: Vec<_> = ftype.parameters.iter().cloned().collect();
                    let return_types: Vec<_> = ftype.return_types.iter().cloned().collect();

                    // parameters are stack ordered, so pick them in reverse
                    parameters
                        .iter()
                        .rev()
                        .try_for_each(|v| match self.pop_expected(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    return_types
                        .iter()
                        .try_for_each(|v| match self.push_val(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            CallIndirect => {
                if let InstructionData::IndirectInstruction {
                    type_index: ti,
                    table_index: _,
                } = *inst.get_data()
                {
                    // TOOD: this probably should check that table_index exists in the table space

                    // operand that directs us to the table entry
                    self.pop_expected(Val(I32))
                        .ok_or(ValidationError::TypeMismatch)?;

                    // table entry must have this function signature
                    let ftype = self
                        .types
                        .get(ti as u8)
                        .ok_or(ValidationError::UnknownFunctionType)?;
                    let parameters: Vec<_> = ftype.parameters.iter().cloned().collect();
                    let return_types: Vec<_> = ftype.return_types.iter().cloned().collect();

                    parameters
                        .iter()
                        .try_for_each(|v| match self.pop_expected(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    return_types
                        .iter()
                        .try_for_each(|v| match self.push_val(Val(*v)) {
                            Some(_) => Ok(()),
                            None => Err(ValidationError::TypeMismatch),
                        })?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            Select => {
                self.pop_expected(Val(I32))
                    .ok_or(ValidationError::TypeMismatch)?;
                let t1 = self.pop_val().ok_or(ValidationError::TypeMismatch)?.clone();
                let t2 = self.pop_val().ok_or(ValidationError::TypeMismatch)?.clone();
                if (t1.is_num() && t2.is_num()) || (t1.is_vec() && t2.is_vec()) {
                    if t1 != t2 && t1 != Unknown && t2 != Unknown {
                        Err(ValidationError::TypeMismatch)
                    } else {
                        self.push_val(if t1 == Unknown {
                            t2.clone()
                        } else {
                            t1.clone()
                        })
                        .ok_or(ValidationError::TypeMismatch)?;
                        Ok(())
                    }
                } else {
                    Err(ValidationError::TypeMismatch)
                }
            }

            RefFunc => {
                if let InstructionData::FunctionInstruction { function_index: fi } =
                    *inst.get_data()
                {
                    self.function_type(fi)?;
                    self.push_val(Val(FuncRef))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            RefNull => {
                if let InstructionData::RefTypeInstruction { ref_type } = *inst.get_data() {
                    self.push_val(Val(ref_type))
                        .ok_or(ValidationError::TypeMismatch)?;
                    Ok(())
                } else {
                    Err(ValidationError::InvalidInstruction)
                }
            }

            Drop => {
                self.pop_val().ok_or(ValidationError::TypeMismatch)?;
                Ok(())
            }

            Nop => Ok(()),

            _ => {
                println!("validate: unimplemented instruction {:?}", inst.to_string());
                Err(ValidationError::UnimplementedInstruction)
            }
        }
    }

    fn ended(&mut self) -> bool {
        self.need_end && self.ctrls.is_empty()
    }
}

fn check_alignment(inst: &Instruction, align_exponent: u32) -> Result<(), ValidationError> {
    let align: u32 = if let InstructionData::MemoryInstruction {
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
