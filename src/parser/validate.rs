use crate::parser::ast;

#[derive(PartialEq, Debug, Clone)]
enum MaybeValue {
    Val(super::parsed_unit::ValueType),
    Unknown,
}

impl MaybeValue {
    fn is_num(self) -> bool {
        match self {
            MaybeValue::Val(v) => {
                v == super::parsed_unit::ValueType::I32
                    || v == super::parsed_unit::ValueType::I64
                    || v == super::parsed_unit::ValueType::F32
                    || v == super::parsed_unit::ValueType::F64
            }
            MaybeValue::Unknown => true,
        }
    }

    fn is_vec(self) -> bool {
        match self {
            MaybeValue::Val(v) => v == super::parsed_unit::ValueType::V128,
            MaybeValue::Unknown => true,
        }
    }

    fn is_ref(self) -> bool {
        match self {
            MaybeValue::Val(v) => {
                v == super::parsed_unit::ValueType::FuncRef
                    || v == super::parsed_unit::ValueType::ExternRef
            }
            MaybeValue::Unknown => true,
        }
    }
}

const I32_VALUE: super::parsed_unit::ValueType = super::parsed_unit::ValueType::I32;
const I64_VALUE: super::parsed_unit::ValueType = super::parsed_unit::ValueType::I64;
const F32_VALUE: super::parsed_unit::ValueType = super::parsed_unit::ValueType::F32;
const F64_VALUE: super::parsed_unit::ValueType = super::parsed_unit::ValueType::F64;
const V128_VALUE: super::parsed_unit::ValueType = super::parsed_unit::ValueType::V128;

struct CtrlFrame {
    instruction: ast::InstructionType,
    start_types: Vec<MaybeValue>,
    end_types: Vec<MaybeValue>,
    height: u32,
    unreachable: bool,
}

pub struct Validator<'a> {
    ftype: &'a super::parsed_unit::FunctionType,
    vals: Vec<MaybeValue>,
    ctrls: Vec<CtrlFrame>,
}

impl<'a> Validator<'a> {
    pub fn new(ftype: &super::parsed_unit::FunctionType) -> Validator {
        let mut v = Validator {
            ftype: ftype,
            vals: Vec::new(),
            ctrls: Vec::new(),
        };
        let start_types = ftype
            .parameters
            .iter()
            .map(|v| MaybeValue::Val(*v))
            .collect();
        let end_types = ftype
            .return_types
            .iter()
            .map(|v| MaybeValue::Val(*v))
            .collect();
        v.push_ctrl(ast::InstructionType::Block, start_types, end_types);

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
        if self.vals.len() == 0 {
            println!("pop_val: empty val stack");
            None
        } else if self.ctrls.len() == 0 {
            println!("pop_val: empty ctrl stack");
            None
        } else if self.vals.len() as u32 == self.ctrls.last()?.height
            && self.ctrls.last()?.unreachable
        {
            Some(MaybeValue::Unknown)
        } else if self.vals.len() as u32 == self.ctrls.last()?.height {
            println!("pop_val: height = {}", self.ctrls.last()?.height);
            None
        } else {
            self.vals.pop()
        }
    }

    fn pop_expected(&mut self, val_type: MaybeValue) -> Option<MaybeValue> {
        let popped = self.pop_val()?;
        if popped != val_type && popped != MaybeValue::Unknown && val_type != MaybeValue::Unknown {
            println!("pop_expected {:?}, got {:?}", val_type, popped);
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
        instruction: ast::InstructionType,
        start_types: Vec<MaybeValue>,
        end_types: Vec<MaybeValue>,
    ) {
        self.push_vals(start_types.clone());
        self.ctrls.push(CtrlFrame {
            instruction: instruction,
            start_types: start_types,
            end_types: end_types,
            height: self.vals.len() as u32,
            unreachable: false,
        });
    }

    fn pop_ctrl(&mut self) -> Result<CtrlFrame, &'static str> {
        if self.ctrls.is_empty() {
            println!("pop_ctrl: empty ctrl stack");
            Err("unexpected token")
        } else {
            let end_types_clone = self.ctrls.last().unwrap().end_types.clone();
            if self.pop_expecteds(end_types_clone).is_none() {
                println!("pop_ctrl: invalid end types");
                Err("type mismatch")
            } else {
                if self.vals.len() != self.ctrls.last().unwrap().height as usize {
                    println!(
                        "pop_ctrl: invalid height {} != {}",
                        self.vals.len(),
                        self.ctrls.last().unwrap().height
                    );
                    Err("unexpected token")
                } else {
                    Ok(self.ctrls.pop().unwrap())
                }
            }
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

    pub fn validate(&mut self, inst: &ast::Instruction) -> Result<(), &'static str> {
        match inst.get_type() {
            ast::InstructionType::I32Const => self
                .push_val(MaybeValue::Val(I32_VALUE))
                .ok_or("type mismatch"),

            // iunop (i32):i32
            ast::InstructionType::I32Clz
            | ast::InstructionType::I32Ctz
            | ast::InstructionType::I32Popcnt => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // ibinop (i32,i32):i32
            ast::InstructionType::I32Add
            | ast::InstructionType::I32Sub
            | ast::InstructionType::I32Mul
            | ast::InstructionType::I32DivS
            | ast::InstructionType::I32DivU
            | ast::InstructionType::I32RemS
            | ast::InstructionType::I32RemU
            | ast::InstructionType::I32And
            | ast::InstructionType::I32Or
            | ast::InstructionType::I32Xor
            | ast::InstructionType::I32Shl
            | ast::InstructionType::I32ShrS
            | ast::InstructionType::I32ShrU
            | ast::InstructionType::I32Rotr
            | ast::InstructionType::I32Rotl => self.sig_binary(
                MaybeValue::Val(I32_VALUE),
                MaybeValue::Val(I32_VALUE),
                MaybeValue::Val(I32_VALUE),
            ),

            // itestop (i32):i32
            ast::InstructionType::I32Eqz => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // irelop (i32,i32):i32
            ast::InstructionType::I32Eq
            | ast::InstructionType::I32Ne
            | ast::InstructionType::I32LtS
            | ast::InstructionType::I32LtU
            | ast::InstructionType::I32GtS
            | ast::InstructionType::I32GtU
            | ast::InstructionType::I32LeS
            | ast::InstructionType::I32LeU
            | ast::InstructionType::I32GeS
            | ast::InstructionType::I32GeU => self.sig_binary(
                MaybeValue::Val(I32_VALUE),
                MaybeValue::Val(I32_VALUE),
                MaybeValue::Val(I32_VALUE),
            ),

            // cvtop (i32):i32
            ast::InstructionType::I32Extend8S | ast::InstructionType::I32Extend16S => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // cvtop (i64):i32
            ast::InstructionType::I32WrapI64 => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // cvtop (f32):i32
            ast::InstructionType::I32TruncF32S
            | ast::InstructionType::I32TruncF32U
            | ast::InstructionType::I32TruncSatF32S
            | ast::InstructionType::I32TruncSatF32U => {
                self.sig_unary(MaybeValue::Val(F32_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // cvtop (f64):i32
            ast::InstructionType::I32TruncF64S
            | ast::InstructionType::I32TruncF64U
            | ast::InstructionType::I32TruncSatF64S
            | ast::InstructionType::I32TruncSatF64U => {
                self.sig_unary(MaybeValue::Val(F64_VALUE), MaybeValue::Val(I32_VALUE))
            }

            ast::InstructionType::I64Const => self
                .push_val(MaybeValue::Val(I64_VALUE))
                .ok_or("type mismatch"),

            // iunop (i64):i32
            ast::InstructionType::I64Clz
            | ast::InstructionType::I64Ctz
            | ast::InstructionType::I64Popcnt => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // ibinop (i64,i64):i64
            ast::InstructionType::I64Add
            | ast::InstructionType::I64Sub
            | ast::InstructionType::I64Mul
            | ast::InstructionType::I64DivS
            | ast::InstructionType::I64DivU
            | ast::InstructionType::I64RemS
            | ast::InstructionType::I64RemU
            | ast::InstructionType::I64And
            | ast::InstructionType::I64Or
            | ast::InstructionType::I64Xor
            | ast::InstructionType::I64Shl
            | ast::InstructionType::I64ShrS
            | ast::InstructionType::I64ShrU
            | ast::InstructionType::I64Rotr
            | ast::InstructionType::I64Rotl => self.sig_binary(
                MaybeValue::Val(I64_VALUE),
                MaybeValue::Val(I64_VALUE),
                MaybeValue::Val(I64_VALUE),
            ),

            // itestop (i64):i32
            ast::InstructionType::I64Eqz => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // irelop (i64,i64):i32
            ast::InstructionType::I64Eq
            | ast::InstructionType::I64Ne
            | ast::InstructionType::I64LtS
            | ast::InstructionType::I64LtU
            | ast::InstructionType::I64GtS
            | ast::InstructionType::I64GtU
            | ast::InstructionType::I64LeS
            | ast::InstructionType::I64LeU
            | ast::InstructionType::I64GeS
            | ast::InstructionType::I64GeU => self.sig_binary(
                MaybeValue::Val(I64_VALUE),
                MaybeValue::Val(I64_VALUE),
                MaybeValue::Val(I32_VALUE),
            ),

            // cvtop (i64):i64
            ast::InstructionType::I64Extend8S
            | ast::InstructionType::I64Extend16S
            | ast::InstructionType::I64Extend32S => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(I32_VALUE))
            }

            // cvtop (i32):i64
            ast::InstructionType::I64ExtendI32S | ast::InstructionType::I64ExtendI32U => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(I64_VALUE))
            }

            // cvtop (f32):i64
            ast::InstructionType::I64TruncF32S
            | ast::InstructionType::I64TruncF32U
            | ast::InstructionType::I64TruncSatF32S
            | ast::InstructionType::I64TruncSatF32U => {
                self.sig_unary(MaybeValue::Val(F32_VALUE), MaybeValue::Val(I64_VALUE))
            }

            // cvtop (f64):i64
            ast::InstructionType::I64TruncF64S
            | ast::InstructionType::I64TruncF64U
            | ast::InstructionType::I64TruncSatF64S
            | ast::InstructionType::I64TruncSatF64U => {
                self.sig_unary(MaybeValue::Val(F64_VALUE), MaybeValue::Val(I64_VALUE))
            }

            ast::InstructionType::F32Const => self
                .push_val(MaybeValue::Val(F32_VALUE))
                .ok_or("type mismatch"),

            // funop (f32):f32
            ast::InstructionType::F32Abs
            | ast::InstructionType::F32Neg
            | ast::InstructionType::F32Sqrt
            | ast::InstructionType::F32Ceil
            | ast::InstructionType::F32Floor
            | ast::InstructionType::F32Trunc
            | ast::InstructionType::F32Nearest => {
                self.sig_unary(MaybeValue::Val(F32_VALUE), MaybeValue::Val(F32_VALUE))
            }

            // fbinop (f32,f32):f32
            ast::InstructionType::F32Add
            | ast::InstructionType::F32Sub
            | ast::InstructionType::F32Mul
            | ast::InstructionType::F32Div
            | ast::InstructionType::F32Min
            | ast::InstructionType::F32Max
            | ast::InstructionType::F32Copysign => self.sig_binary(
                MaybeValue::Val(F32_VALUE),
                MaybeValue::Val(F32_VALUE),
                MaybeValue::Val(F32_VALUE),
            ),

            // frelop (f32,f32):i32
            ast::InstructionType::F32Eq
            | ast::InstructionType::F32Ne
            | ast::InstructionType::F32Lt
            | ast::InstructionType::F32Gt
            | ast::InstructionType::F32Le
            | ast::InstructionType::F32Ge => self.sig_binary(
                MaybeValue::Val(F32_VALUE),
                MaybeValue::Val(F32_VALUE),
                MaybeValue::Val(I32_VALUE),
            ),

            // cvtop (f64):f32
            ast::InstructionType::F32DemoteF64 => {
                self.sig_unary(MaybeValue::Val(F64_VALUE), MaybeValue::Val(F32_VALUE))
            }

            // cvtop (i32):f32
            ast::InstructionType::F32ConvertI32S
            | ast::InstructionType::F32ConvertI32U
            | ast::InstructionType::F32ReinterpretI32 => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(F32_VALUE))
            }

            // cvtop (i64):f32
            ast::InstructionType::F32ConvertI64S | ast::InstructionType::F32ConvertI64U => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(F32_VALUE))
            }

            ast::InstructionType::F64Const => self
                .push_val(MaybeValue::Val(F64_VALUE))
                .ok_or("type mismatch"),

            // funop (f64):f64
            ast::InstructionType::F64Abs
            | ast::InstructionType::F64Neg
            | ast::InstructionType::F64Sqrt
            | ast::InstructionType::F64Ceil
            | ast::InstructionType::F64Floor
            | ast::InstructionType::F64Trunc
            | ast::InstructionType::F64Nearest => {
                self.sig_unary(MaybeValue::Val(F64_VALUE), MaybeValue::Val(F64_VALUE))
            }

            // fbinop (f64,f64):f64
            ast::InstructionType::F64Add
            | ast::InstructionType::F64Sub
            | ast::InstructionType::F64Mul
            | ast::InstructionType::F64Div
            | ast::InstructionType::F64Min
            | ast::InstructionType::F64Max
            | ast::InstructionType::F64Copysign => self.sig_binary(
                MaybeValue::Val(F64_VALUE),
                MaybeValue::Val(F64_VALUE),
                MaybeValue::Val(F64_VALUE),
            ),

            // frelop (f64,f64):i32
            ast::InstructionType::F64Eq
            | ast::InstructionType::F64Ne
            | ast::InstructionType::F64Lt
            | ast::InstructionType::F64Gt
            | ast::InstructionType::F64Le
            | ast::InstructionType::F64Ge => self.sig_binary(
                MaybeValue::Val(F64_VALUE),
                MaybeValue::Val(F64_VALUE),
                MaybeValue::Val(I32_VALUE),
            ),

            // cvtop (f32):f64
            ast::InstructionType::F64PromoteF32 => {
                self.sig_unary(MaybeValue::Val(F32_VALUE), MaybeValue::Val(F64_VALUE))
            }

            // cvtop (i32):f64
            ast::InstructionType::F64ConvertI32S | ast::InstructionType::F64ConvertI32U => {
                self.sig_unary(MaybeValue::Val(I32_VALUE), MaybeValue::Val(F64_VALUE))
            }

            // cvtop (i64):f64
            ast::InstructionType::F64ConvertI64S
            | ast::InstructionType::F64ConvertI64U
            | ast::InstructionType::F64ReinterpretI64 => {
                self.sig_unary(MaybeValue::Val(I64_VALUE), MaybeValue::Val(F64_VALUE))
            }

            ast::InstructionType::LocalGet => {
                if let ast::InstructionData::LocalInstruction { local_index: li } = *inst.get_data()
                {
                    self.ftype
                        .parameters
                        .get(li as usize)
                        .map(|v| self.push_val(MaybeValue::Val(*v)));
                    Ok(())
                } else {
                    println!("validate: LocalGet: invalid data");
                    Err("invalid instruction")
                }
            }

            ast::InstructionType::Block | ast::InstructionType::Loop | ast::InstructionType::If => {
                // special case for If we need to pop an i32
                if inst.get_type() == &ast::InstructionType::If {
                    self.pop_expected(MaybeValue::Val(super::parsed_unit::ValueType::I32))
                        .ok_or("type mismatch")?;
                }

                if let ast::InstructionData::BlockInstruction { blocktype: bt } = *inst.get_data() {
                    let mut end_types = Vec::new();
                    match bt {
                        // simple []->[type] version
                        ast::BlockType::Type(t) => {
                            end_types.push(MaybeValue::Val(t));
                        }
                        ast::BlockType::Empty => {}
                        // TODO: TypeIndex form with start_types too
                        ast::BlockType::TypeIndex(ti) => {}
                    }
                    self.push_ctrl(*inst.get_type(), Vec::new(), end_types);
                    Ok(())
                } else {
                    println!("validate: Block: invalid data");
                    Err("invalid instruction")
                }
            }

            ast::InstructionType::Else => {
                let ctrl = self.pop_ctrl()?;
                if ctrl.instruction != ast::InstructionType::If {
                    println!("validate: Else: invalid instruction");
                    Err("invalid instruction")
                } else {
                    let mut end_types = Vec::new();
                    for t in ctrl.start_types {
                        end_types.push(t);
                    }
                    self.push_ctrl(ast::InstructionType::Else, Vec::new(), end_types);
                    Ok(())
                }
            }

            ast::InstructionType::End => {
                let ctrl = self.pop_ctrl()?;
                if ctrl.end_types.len() != 0 {
                    self.push_vals(ctrl.end_types).ok_or("type mismatch")
                } else {
                    Ok(())
                }
            }

            ast::InstructionType::Drop => {
                self.pop_val().ok_or("type mismatch")?;
                Ok(())
            }

            _ => {
                println!("validate: unimplemented instruction {:?}", inst.to_string());
                Err("unimplemented instruction")
            }
        }
    }
}
