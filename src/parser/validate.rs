use super::module::ValueType;
use super::module::ValueType::*;
use crate::parser::ast;
use ast::InstructionType::*;
use ast::{BlockType, Instruction, InstructionData, InstructionType};
use MaybeValue::{Unknown, Val};

#[derive(PartialEq, Debug, Clone)]
enum MaybeValue {
    Val(ValueType),
    Unknown,
}

#[allow(dead_code)]
impl MaybeValue {
    fn is_num(self) -> bool {
        match self {
            Val(v) => v == I32 || v == I64 || v == F32 || v == F64,
            Unknown => true,
        }
    }

    fn is_vec(self) -> bool {
        match self {
            Val(v) => v == V128,
            Unknown => true,
        }
    }

    fn is_ref(self) -> bool {
        match self {
            Val(v) => v == FuncRef || v == ExternRef,
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

pub struct Validator<'a> {
    types: &'a super::module::TypeSection,
    params: &'a Vec<ValueType>,
    locals: &'a Vec<ValueType>,
    vals: Vec<MaybeValue>,
    ctrls: Vec<CtrlFrame>,
}

impl<'a> Validator<'a> {
    pub fn new<'b>(
        types: &'b super::module::TypeSection,
        locals: &'b Vec<ValueType>,
        ftype: &'b super::module::FunctionType,
    ) -> Validator<'b> {
        let mut v = Validator {
            types,
            params: &ftype.parameters,
            locals,
            vals: vec![],
            ctrls: vec![],
        };
        let start_types = ftype.parameters.iter().map(|v| Val(*v)).collect();
        let end_types = ftype.return_types.iter().map(|v| Val(*v)).collect();
        v.push_ctrl(Block, start_types, end_types);

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
        // TODO: turns out we don't need to push the start types on, they're for locals, but we
        // do probably need to validate that the end type is right somehow?
        // self.push_vals(start_types);
    }

    fn pop_ctrl(&mut self) -> Result<CtrlFrame, &'static str> {
        if self.ctrls.is_empty() {
            Err("unexpected token")
        } else {
            let end_types_clone = self.ctrls.last().unwrap().end_types.clone();
            if self.pop_expecteds(end_types_clone).is_none() {
                Err("type mismatch")
            } else {
                if self.vals.len() != self.ctrls.last().unwrap().height as usize {
                    Err("type mismatch")
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
            if self.vals.len() <= h {
                self.vals.truncate(h);
                ctrl.unreachable = true;
                Some(())
            } else {
                None
            }
        } else {
            None
        }
    }

    fn sig_unary(&mut self, in_type: MaybeValue, out_type: MaybeValue) -> Result<(), &'static str> {
        self.pop_expected(in_type).ok_or("type mismatch")?;
        self.push_val(out_type).ok_or("type mismatch")
    }

    fn sig_binary(
        &mut self,
        in1_type: MaybeValue,
        in2_type: MaybeValue,
        out_type: MaybeValue,
    ) -> Result<(), &'static str> {
        self.pop_expected(in1_type).ok_or("type mismatch")?;
        self.pop_expected(in2_type).ok_or("type mismatch")?;
        self.push_val(out_type).ok_or("type mismatch")
    }

    fn local(&mut self, local_index: u32) -> Result<&ValueType, &'static str> {
        let li = local_index as usize;
        let local: Option<&ValueType> = if li < self.params.len() {
            self.params.get(li)
        } else {
            let li = li - self.params.len();
            self.locals.get(li)
        };

        local.ok_or("unknown local")
    }

    fn label_types_at(&mut self, li: u32) -> Result<Vec<MaybeValue>, &'static str> {
        let index = self.ctrls.len() - li as usize - 1;
        let frame = self.ctrls.get(index).ok_or("invalid label index")?.clone();
        Ok(self.frame_types(frame))
    }

    pub fn validate(&mut self, inst: &Instruction) -> Result<(), &'static str> {
        match inst.get_type() {
            I32Const => self.push_val(Val(I32_VALUE)).ok_or("type mismatch"),

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
            I32TruncF32S | I32TruncF32U | I32TruncSatF32S | I32TruncSatF32U => {
                self.sig_unary(Val(F32_VALUE), Val(I32_VALUE))
            }

            // cvtop (f64):i32
            I32TruncF64S | I32TruncF64U | I32TruncSatF64S | I32TruncSatF64U => {
                self.sig_unary(Val(F64_VALUE), Val(I32_VALUE))
            }

            I64Const => self.push_val(Val(I64_VALUE)).ok_or("type mismatch"),

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
            I64TruncF64S | I64TruncF64U | I64TruncSatF64S | I64TruncSatF64U => {
                self.sig_unary(Val(F64_VALUE), Val(I64_VALUE))
            }

            F32Const => self.push_val(Val(F32_VALUE)).ok_or("type mismatch"),

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

            F64Const => self.push_val(Val(F64_VALUE)).ok_or("type mismatch"),

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
                    self.push_val(Val(local)).ok_or("type mismatch")?;
                    Ok(())
                } else {
                    Err("invalid instruction")
                }
            }

            LocalSet => {
                if let InstructionData::LocalInstruction { local_index: li } = *inst.get_data() {
                    let local = self.local(li)?.clone();
                    self.pop_expected(Val(local)).ok_or("type mismatch")?;
                    Ok(())
                } else {
                    Err("invalid instruction")
                }
            }

            Block | Loop | If => {
                // special case for If we need to pop an i32
                if inst.get_type() == &If {
                    self.pop_expected(Val(I32)).ok_or("type mismatch")?;
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
                            let ftype = self.types.get(ti as u8).ok_or("unknown block type")?;
                            ftype.parameters.iter().try_for_each(|v| {
                                start_types.push(Val(*v));
                                match self.pop_expected(Val(*v)) {
                                    Some(_) => Ok(()),
                                    None => Err("type mismatch"),
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
                    Err("invalid instruction")
                }
            }

            Else => {
                let ctrl = self.pop_ctrl()?;
                if ctrl.instruction != If {
                    Err("invalid instruction")
                } else {
                    self.push_ctrl(Else, ctrl.start_types, ctrl.end_types);
                    Ok(())
                }
            }

            End => {
                let ctrl = self.pop_ctrl()?;
                self.push_vals(ctrl.end_types).ok_or("type mismatch")?;
                Ok(())
            }

            Return | Br | BrIf => {
                let li: u32 = if let InstructionData::LabelledInstruction { label_index: li } =
                    *inst.get_data()
                {
                    li
                } else {
                    // Return
                    self.ctrls.len() as u32 - 1
                };

                if inst.get_type() == &BrIf {
                    self.pop_expected(Val(I32)).ok_or("type mismatch")?;
                }

                if self.ctrls.len() < li as usize {
                    Err("invalid instruction")
                } else {
                    let label_types = self.label_types_at(li)?;
                    self.pop_expecteds(label_types.clone())
                        .ok_or("type mismatch")?;
                    match inst.get_type() {
                        &Return | &Br => {
                            self.unreachable().ok_or("type mismatch")?;
                            Ok(())
                        }
                        &BrIf => {
                            self.push_vals(label_types).ok_or("type mismatch")?;
                            Ok(())
                        }
                        _ => Err("invalid instruction"),
                    }
                }
            }

            BrTable => {
                if let InstructionData::TableLabelledInstruction {
                    ref labels,
                    label_index: li,
                } = *inst.get_data()
                {
                    self.pop_expected(Val(I32)).ok_or("type mismatch")?;
                    if self.ctrls.len() < li as usize {
                        return Err("invalid instruction");
                    }
                    let label_types = self.label_types_at(li)?;
                    let arity = label_types.len();
                    labels.iter().try_for_each(|&li| {
                        if self.ctrls.len() < li as usize {
                            Err("invalid instruction")
                        } else {
                            let label_types = self.label_types_at(li)?;
                            if label_types.len() != arity {
                                Err("invalid instruction")
                            } else {
                                let popped = self.pop_expecteds(label_types.clone());
                                if popped.is_none() {
                                    Err("invalid instruction")
                                } else {
                                    self.push_vals(label_types).ok_or("type mismatch")?;
                                    Ok(())
                                }
                            }
                        }
                    })?;
                    self.pop_expecteds(label_types).ok_or("type mismatch")?;
                    self.unreachable().ok_or("type mismatch")?;
                    Ok(())
                } else {
                    Err("invalid instruction")
                }
            }

            Drop => {
                self.pop_val().ok_or("type mismatch")?;
                Ok(())
            }

            Nop => Ok(()),

            _ => {
                println!("validate: unimplemented instruction {:?}", inst.to_string());
                Err("unimplemented instruction")
            }
        }
    }
}
