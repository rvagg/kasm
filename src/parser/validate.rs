use crate::parser::ast;

#[derive(PartialEq, Debug, Clone)]
enum ValOrUnknown {
    Val(super::parsed_unit::ValueType),
    Unknown,
}

impl ValOrUnknown {
    fn is_num(self) -> bool {
        match self {
            ValOrUnknown::Val(v) => {
                v == super::parsed_unit::ValueType::I32
                    || v == super::parsed_unit::ValueType::I64
                    || v == super::parsed_unit::ValueType::F32
                    || v == super::parsed_unit::ValueType::F64
            }
            ValOrUnknown::Unknown => true,
        }
    }

    fn is_vec(self) -> bool {
        match self {
            ValOrUnknown::Val(v) => v == super::parsed_unit::ValueType::V128,
            ValOrUnknown::Unknown => true,
        }
    }

    fn is_ref(self) -> bool {
        match self {
            ValOrUnknown::Val(v) => {
                v == super::parsed_unit::ValueType::FuncRef
                    || v == super::parsed_unit::ValueType::ExternRef
            }
            ValOrUnknown::Unknown => true,
        }
    }
}

struct CtrlFrame {
    instruction: ast::InstructionType,
    start_types: Vec<ValOrUnknown>,
    end_types: Vec<ValOrUnknown>,
    height: u32,
    unreachable: bool,
}

pub struct Validator<'a> {
    ftype: &'a super::parsed_unit::FunctionType,
    vals: Vec<ValOrUnknown>,
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
            .map(|v| ValOrUnknown::Val(*v))
            .collect();
        let end_types = ftype
            .return_types
            .iter()
            .map(|v| ValOrUnknown::Val(*v))
            .collect();
        v.push_ctrl(ast::InstructionType::Block, start_types, end_types);

        v
    }

    fn push_val(&mut self, val_type: ValOrUnknown) -> Option<()> {
        self.vals.push(val_type);
        Some(())
    }

    fn push_vals(&mut self, val_types: Vec<ValOrUnknown>) -> Option<()> {
        for val_type in val_types {
            self.push_val(val_type)?;
        }
        Some(())
    }

    fn pop_val(&mut self) -> Option<ValOrUnknown> {
        if self.vals.len() == 0 {
            println!("pop_val: empty val stack");
            None
        } else if self.ctrls.len() == 0 {
            println!("pop_val: empty ctrl stack");
            None
        } else if self.vals.len() as u32 == self.ctrls.last()?.height
            && self.ctrls.last()?.unreachable
        {
            Some(ValOrUnknown::Unknown)
        } else if self.vals.len() as u32 == self.ctrls.last()?.height {
            println!("pop_val: height = {}", self.ctrls.last()?.height);
            None
        } else {
            self.vals.pop()
        }
    }

    fn pop_expected(&mut self, val_type: ValOrUnknown) -> Option<ValOrUnknown> {
        let popped = self.pop_val()?;
        if popped != val_type
            && popped != ValOrUnknown::Unknown
            && val_type != ValOrUnknown::Unknown
        {
            println!("pop_expected {:?}, got {:?}", val_type, popped);
            None
        } else {
            Some(popped)
        }
    }

    fn pop_expecteds(&mut self, mut val_types: Vec<ValOrUnknown>) -> Option<Vec<ValOrUnknown>> {
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
        start_types: Vec<ValOrUnknown>,
        end_types: Vec<ValOrUnknown>,
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

    pub fn validate(&mut self, inst: &ast::Instruction) -> Result<(), &'static str> {
        match inst.get_type() {
            // (i32,i32):i32
            ast::InstructionType::I32Eq
            | ast::InstructionType::I32Ne
            | ast::InstructionType::I32LtS
            | ast::InstructionType::I32LtU
            | ast::InstructionType::I32GtS
            | ast::InstructionType::I32GtU
            | ast::InstructionType::I32LeS
            | ast::InstructionType::I32LeU
            | ast::InstructionType::I32GeS
            | ast::InstructionType::I32GeU
            | ast::InstructionType::I32Add
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
            | ast::InstructionType::I32Rotl => {
                self.pop_expected(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                    .ok_or("type mismatch")?;
                self.pop_expected(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                    .ok_or("type mismatch")?;
                self.push_val(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                    .ok_or("type mismatch")
            }

            // (i32):i32
            ast::InstructionType::I32Eqz
            | ast::InstructionType::I32Clz
            | ast::InstructionType::I32Ctz
            | ast::InstructionType::I32Popcnt
            | ast::InstructionType::I32Extend8S
            | ast::InstructionType::I32Extend16S => {
                self.pop_expected(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                    .ok_or("type mismatch")?;
                self.push_val(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                    .ok_or("type mismatch")
            }

            ast::InstructionType::I32Const => self
                .push_val(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                .ok_or("type mismatch"),

            ast::InstructionType::LocalGet => {
                if let ast::InstructionData::LocalInstruction { local_index: li } = *inst.get_data()
                {
                    self.ftype
                        .parameters
                        .get(li as usize)
                        .map(|v| self.push_val(ValOrUnknown::Val(*v)));
                    Ok(())
                } else {
                    println!("validate: LocalGet: invalid data");
                    Err("invalid instruction")
                }
            }

            ast::InstructionType::Block | ast::InstructionType::Loop | ast::InstructionType::If => {
                // special case for If we need to pop an i32
                if inst.get_type() == &ast::InstructionType::If {
                    self.pop_expected(ValOrUnknown::Val(super::parsed_unit::ValueType::I32))
                        .ok_or("type mismatch")?;
                }

                if let ast::InstructionData::BlockInstruction { blocktype: bt } = *inst.get_data() {
                    let mut end_types = Vec::new();
                    match bt {
                        // simple []->[type] version
                        ast::BlockType::Type(t) => {
                            end_types.push(ValOrUnknown::Val(t));
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
            _ => {
                println!("validate: unimplemented instruction {:?}", inst.to_string());
                Err("unimplemented instruction")
            }
        }
    }
}
