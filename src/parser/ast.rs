use std::slice::Iter;
use std::fmt;


#[derive(PartialEq)]
pub enum Node {
    Nop
  , Block { children : Vec<Node> }
  , Loop { children : Vec<Node> }
  , If { children : Vec<Node> }
  , Else { children : Vec<Node> }
  , Select
  , Br { argument_count : u8, relative_depth : u32 }
  , BrIf { argument_count : u8, relative_depth : u32 }
  , BrTable
  , Return
  , Unreachable
  , Drop
  , End
  , I32Const { value : i32 }
  , I64Const { value : i64 }
  , F64Const { value : f64 }
  , F32Const { value : f32 }
  , GetLocal { local_index : u32 }
  , SetLocal { local_index : u32 }
  , Call { argument_count : u8, function_index : u32 }
  , CallIndirect { argument_count : u8, type_index : u32 }
  , CallImport { argument_count : u8, import_index : u32 }
  , TeeLocal { local_index : u32 }
  , I32Load8S
  , I32Load8U
  , I32Load16S
  , I32Load16U
  , I64Load8S
  , I64Load8U
  , I64Load16S
  , I64Load16U
  , I64Load32S
  , I64Load32U
  , I32Load
  , I64Load
  , F32Load
  , F64Load
  , I32Store8
  , I32Store16
  , I64Store8
  , I64Store16
  , I64Store32
  , I32Store
  , I64Store
  , F32Store
  , F64Store
  , GrowMemory
  , CurrentMemory
  , I32Add
  , I32Sub
  , I32Mul
  , I32DivS
  , I32DivU
  , I32RemS
  , I32RemU
  , I32And
  , I32Or
  , I32Xor
  , I32Shl
  , I32ShrU
  , I32ShrS
  , I32Rotr
  , I32Rotl
  , I32Eq
  , I32Ne
  , I32LtS
  , I32LeS
  , I32LtU
  , I32LeU
  , I32GtS
  , I32GeS
  , I32GtU
  , I32GeU
  , I32Clz
  , I32Ctz
  , I32Popcnt
  , I32Eqz
  , I64Add
  , I64Sub
  , I64Mul
  , I64DivS
  , I64DivU
  , I64RemS
  , I64RemU
  , I64And
  , I64Or
  , I64Xor
  , I64Shl
  , I64ShrU
  , I64ShrS
  , I64Rotr
  , I64Rotl
  , I64Eq
  , I64Ne
  , I64LtS
  , I64LeS
  , I64LtU
  , I64LeU
  , I64GtS
  , I64GeS
  , I64GtU
  , I64GeU
  , I64Clz
  , I64Ctz
  , I64Popcnt
  , I64Eqz
  , F32Add
  , F32Sub
  , F32Mul
  , F32Div
  , F32Min
  , F32Max
  , F32Abs
  , F32Neg
  , F32Copysign
  , F32Ceil
  , F32Floor
  , F32Trunc
  , F32Nearest
  , F32Sqrt
  , F32Eq
  , F32Ne
  , F32Lt
  , F32Le
  , F32Gt
  , F32Ge
  , F64Add
  , F64Sub
  , F64Mul
  , F64Div
  , F64Min
  , F64Max
  , F64Abs
  , F64Neg
  , F64Copysign
  , F64Ceil
  , F64Floor
  , F64Trunc
  , F64Nearest
  , F64Sqrt
  , F64Eq
  , F64Ne
  , F64Lt
  , F64Le
  , F64Gt
  , F64Ge
  , I32TruncSF32
  , I32TruncSF64
  , I32TruncUF32
  , I32TruncUF64
  , I32Wrap_i64
  , I64TruncSF32
  , I64TruncSF64
  , I64TruncUF32
  , I64TruncUF64
  , I64ExtendSI32
  , I64ExtendUI32
  , F32ConvertSI32
  , F32ConvertUI32
  , F32ConvertSI64
  , F32ConvertUI64
  , F32DemoteF64
  , F32ReinterpretI32
  , F64ConvertSI32
  , F64ConvertUI32
  , F64ConvertSI64
  , F64ConvertUI64
  , F64PromoteF32
  , F64ReinterpretI64
  , I32ReinterpretF32
  , I64ReinterpretF64

  , UNKNOWN
}


pub fn to_ast(bytes : &Vec<u8>) -> Node {
  let mut iter = bytes.iter();
  Node::Block { children : consume_block(&mut iter) }
  //println!("{:?}", bytes);
}


fn consume_block(iter : &mut Iter<u8>) -> Vec<Node> {
  let mut nodes : Vec<Node> = vec!();

  loop {
    let expr = consume_expression(iter);
    if expr == Node::End {
      break;
    }
    nodes.push(expr);
    let (left, _) = iter.size_hint();
    if left == 0 {
      //TODO: error if End expected? Return?
      break;
    }
  }

  nodes
}


fn consume_expression(iter : &mut Iter<u8>) -> Node {
  let b = iter.next().unwrap();

  match *b {
      0x00 => Node::Nop
    , 0x01 => Node::Block { children : consume_block(iter) }
    , 0x02 => Node::Loop { children : consume_block(iter) }
    , 0x03 => Node::If { children : consume_block(iter) }
    , 0x04 => Node::Else { children : consume_block(iter) }
    , 0x05 => Node::Select //TODO: ? what's after this?
    , 0x06 => Node::Br { argument_count : consume_vu1(iter), relative_depth : consume_vu32(iter) }
    , 0x07 => Node::BrIf { argument_count : consume_vu1(iter), relative_depth : consume_vu32(iter) }
    , 0x08 => Node::BrTable //TODO: ?
    , 0x09 => Node::Return
    , 0x0a => Node::Unreachable
    , 0x0b => Node::Drop
    // 0x0c, 0x0d, 0x0e
    , 0x0f => Node::End

    , 0x10 => Node::I32Const { value : consume_vs32(iter) }
    , 0x11 => Node::I64Const { value : consume_vs64(iter) }
    , 0x12 => Node::F64Const { value : consume_f64(iter) }
    , 0x13 => Node::F32Const { value : consume_f32(iter) }
    , 0x14 => Node::GetLocal { local_index : consume_vu32(iter) }
    , 0x15 => Node::SetLocal { local_index : consume_vu32(iter) }
    , 0x16 => Node::Call { argument_count : consume_vu1(iter), function_index : consume_vu32(iter) }
    , 0x17 => Node::CallIndirect { argument_count : consume_vu1(iter), type_index : consume_vu32(iter) }
    , 0x18 => Node::CallImport { argument_count : consume_vu1(iter), import_index : consume_vu32(iter) }
    , 0x19 => Node::TeeLocal { local_index : consume_vu32(iter) }

    , 0x20 => Node::I32Load8S
    , 0x21 => Node::I32Load8U
    , 0x22 => Node::I32Load16S
    , 0x23 => Node::I32Load16U
    , 0x24 => Node::I64Load8S
    , 0x25 => Node::I64Load8U
    , 0x26 => Node::I64Load16S
    , 0x27 => Node::I64Load16U
    , 0x28 => Node::I64Load32S
    , 0x29 => Node::I64Load32U
    , 0x2a => Node::I32Load
    , 0x2b => Node::I64Load
    , 0x2c => Node::F32Load
    , 0x2d => Node::F64Load
    , 0x2e => Node::I32Store8
    , 0x2f => Node::I32Store16
    , 0x30 => Node::I64Store8
    , 0x31 => Node::I64Store16
    , 0x32 => Node::I64Store32
    , 0x33 => Node::I32Store
    , 0x34 => Node::I64Store
    , 0x35 => Node::F32Store
    , 0x36 => Node::F64Store
    // 0x37, 0x38
    , 0x39 => Node::GrowMemory
    // 0x3a
    , 0x3b => Node::CurrentMemory

    , 0x40 => Node::I32Add
    , 0x41 => Node::I32Sub
    , 0x42 => Node::I32Mul
    , 0x43 => Node::I32DivS
    , 0x44 => Node::I32DivU
    , 0x45 => Node::I32RemS
    , 0x46 => Node::I32RemU
    , 0x47 => Node::I32And
    , 0x48 => Node::I32Or
    , 0x49 => Node::I32Xor
    , 0x4a => Node::I32Shl
    , 0x4b => Node::I32ShrU
    , 0x4c => Node::I32ShrS
    , 0xb6 => Node::I32Rotr
    , 0xb7 => Node::I32Rotl
    , 0x4d => Node::I32Eq
    , 0x4e => Node::I32Ne
    , 0x4f => Node::I32LtS
    , 0x50 => Node::I32LeS
    , 0x51 => Node::I32LtU
    , 0x52 => Node::I32LeU
    , 0x53 => Node::I32GtS
    , 0x54 => Node::I32GeS
    , 0x55 => Node::I32GtU
    , 0x56 => Node::I32GeU
    , 0x57 => Node::I32Clz
    , 0x58 => Node::I32Ctz
    , 0x59 => Node::I32Popcnt
    , 0x5a => Node::I32Eqz
    , 0x5b => Node::I64Add
    , 0x5c => Node::I64Sub
    , 0x5d => Node::I64Mul
    , 0x5e => Node::I64DivS
    , 0x5f => Node::I64DivU
    , 0x60 => Node::I64RemS
    , 0x61 => Node::I64RemU
    , 0x62 => Node::I64And
    , 0x63 => Node::I64Or
    , 0x64 => Node::I64Xor
    , 0x65 => Node::I64Shl
    , 0x66 => Node::I64ShrU
    , 0x67 => Node::I64ShrS
    , 0xb8 => Node::I64Rotr
    , 0xb9 => Node::I64Rotl
    , 0x68 => Node::I64Eq
    , 0x69 => Node::I64Ne
    , 0x6a => Node::I64LtS
    , 0x6b => Node::I64LeS
    , 0x6c => Node::I64LtU
    , 0x6d => Node::I64LeU
    , 0x6e => Node::I64GtS
    , 0x6f => Node::I64GeS
    , 0x70 => Node::I64GtU
    , 0x71 => Node::I64GeU
    , 0x72 => Node::I64Clz
    , 0x73 => Node::I64Ctz
    , 0x74 => Node::I64Popcnt
    , 0xba => Node::I64Eqz
    , 0x75 => Node::F32Add
    , 0x76 => Node::F32Sub
    , 0x77 => Node::F32Mul
    , 0x78 => Node::F32Div
    , 0x79 => Node::F32Min
    , 0x7a => Node::F32Max
    , 0x7b => Node::F32Abs
    , 0x7c => Node::F32Neg
    , 0x7d => Node::F32Copysign
    , 0x7e => Node::F32Ceil
    , 0x7f => Node::F32Floor
    , 0x80 => Node::F32Trunc
    , 0x81 => Node::F32Nearest
    , 0x82 => Node::F32Sqrt
    , 0x83 => Node::F32Eq
    , 0x84 => Node::F32Ne
    , 0x85 => Node::F32Lt
    , 0x86 => Node::F32Le
    , 0x87 => Node::F32Gt
    , 0x88 => Node::F32Ge
    , 0x89 => Node::F64Add
    , 0x8a => Node::F64Sub
    , 0x8b => Node::F64Mul
    , 0x8c => Node::F64Div
    , 0x8d => Node::F64Min
    , 0x8e => Node::F64Max
    , 0x8f => Node::F64Abs
    , 0x90 => Node::F64Neg
    , 0x91 => Node::F64Copysign
    , 0x92 => Node::F64Ceil
    , 0x93 => Node::F64Floor
    , 0x94 => Node::F64Trunc
    , 0x95 => Node::F64Nearest
    , 0x96 => Node::F64Sqrt
    , 0x97 => Node::F64Eq
    , 0x98 => Node::F64Ne
    , 0x99 => Node::F64Lt
    , 0x9a => Node::F64Le
    , 0x9b => Node::F64Gt
    , 0x9c => Node::F64Ge
    , 0x9d => Node::I32TruncSF32
    , 0x9e => Node::I32TruncSF64
    , 0x9f => Node::I32TruncUF32
    , 0xa0 => Node::I32TruncUF64
    , 0xa1 => Node::I32Wrap_i64
    , 0xa2 => Node::I64TruncSF32
    , 0xa3 => Node::I64TruncSF64
    , 0xa4 => Node::I64TruncUF32
    , 0xa5 => Node::I64TruncUF64
    , 0xa6 => Node::I64ExtendSI32
    , 0xa7 => Node::I64ExtendUI32
    , 0xa8 => Node::F32ConvertSI32
    , 0xa9 => Node::F32ConvertUI32
    , 0xaa => Node::F32ConvertSI64
    , 0xab => Node::F32ConvertUI64
    , 0xac => Node::F32DemoteF64
    , 0xad => Node::F32ReinterpretI32
    , 0xae => Node::F64ConvertSI32
    , 0xaf => Node::F64ConvertUI32
    , 0xb0 => Node::F64ConvertSI64
    , 0xb1 => Node::F64ConvertUI64
    , 0xb2 => Node::F64PromoteF32
    , 0xb3 => Node::F64ReinterpretI64
    , 0xb4 => Node::I32ReinterpretF32
    , 0xb5 => Node::I64ReinterpretF64

    , _    => Node::UNKNOWN
  }
}


fn to_ast_old(iter : &mut Iter<u8>) {
  loop {
    let (left, _) = iter.size_hint();
    if left == 0 {
      break;
    }
    let b = iter.next().unwrap();

    print!("{}={} ", b, match *b {
        0x00 => "nop".to_string()
      , 0x01 => "block".to_string()
      , 0x02 => "loop".to_string()
      , 0x03 => "if".to_string()
      , 0x04 => "else".to_string()
      , 0x05 => "select".to_string()
      , 0x06 => format!("(br ({}) {})", consume_vu1(iter), consume_vu32(iter))
      , 0x07 => format!("(br_if ({}) {})", consume_vu1(iter), consume_vu32(iter))
      , 0x08 => "br_table".to_string() //TODO: branch table
      , 0x09 => "return".to_string()
      , 0x0a => format!("(unreachable {})", consume_vu1(iter))
      , 0x0b => "drop".to_string()
      // 0x0c, 0x0d, 0x0e
      , 0x0f => "end".to_string()

      , 0x10 => format!("(i32.const 0x{0:x})", consume_vs32(iter))
      , 0x11 => format!("(i64.const 0x{0:x})", consume_vs64(iter))
      , 0x12 => format!("(f64.const {:e})", consume_f64(iter))
      , 0x13 => format!("(f32.const {:e})", consume_f32(iter))
      , 0x14 => format!("(get_local {})", consume_vu32(iter))
      , 0x15 => format!("(set_local {})", consume_vu32(iter))
      , 0x16 => format!("(call ({}) {})", consume_vu1(iter), consume_vu32(iter))
      , 0x17 => format!("(call_indirect ({}) {})", consume_vu1(iter), consume_vu32(iter))
      , 0x18 => format!("(call_import ({}) {})", consume_vu1(iter), consume_vu32(iter))
      , 0x19 => format!("(tee_local {})", consume_vu32(iter))

      , 0x20 => "i32.load8_s".to_string()
      , 0x21 => "i32.load8_u".to_string()
      , 0x22 => "i32.load16_s".to_string()
      , 0x23 => "i32.load16_u".to_string()
      , 0x24 => "i64.load8_s".to_string()
      , 0x25 => "i64.load8_u".to_string()
      , 0x26 => "i64.load16_s".to_string()
      , 0x27 => "i64.load16_u".to_string()
      , 0x28 => "i64.load32_s".to_string()
      , 0x29 => "i64.load32_u".to_string()
      , 0x2a => "i32.load".to_string()
      , 0x2b => "i64.load".to_string()
      , 0x2c => "f32.load".to_string()
      , 0x2d => "f64.load".to_string()
      , 0x2e => "i32.store8".to_string()
      , 0x2f => "i32.store16".to_string()
      , 0x30 => "i64.store8".to_string()
      , 0x31 => "i64.store16".to_string()
      , 0x32 => "i64.store32".to_string()
      , 0x33 => "i32.store".to_string()
      , 0x34 => "i64.store".to_string()
      , 0x35 => "f32.store".to_string()
      , 0x36 => "f64.store".to_string()
      // 0x37, 0x38
      , 0x39 => "grow_memory".to_string()
      // 0x3a
      , 0x3b => "current_memory".to_string()

      , 0x40 => "i32.add".to_string()
      , 0x41 => "i32.sub".to_string()
      , 0x42 => "i32.mul".to_string()
      , 0x43 => "i32.div_s".to_string()
      , 0x44 => "i32.div_u".to_string()
      , 0x45 => "i32.rem_s".to_string()
      , 0x46 => "i32.rem_u".to_string()
      , 0x47 => "i32.and".to_string()
      , 0x48 => "i32.or".to_string()
      , 0x49 => "i32.xor".to_string()
      , 0x4a => "i32.shl".to_string()
      , 0x4b => "i32.shr_u".to_string()
      , 0x4c => "i32.shr_s".to_string()
      , 0xb6 => "i32.rotr".to_string()
      , 0xb7 => "i32.rotl".to_string()
      , 0x4d => "i32.eq".to_string()
      , 0x4e => "i32.ne".to_string()
      , 0x4f => "i32.lt_s".to_string()
      , 0x50 => "i32.le_s".to_string()
      , 0x51 => "i32.lt_u".to_string()
      , 0x52 => "i32.le_u".to_string()
      , 0x53 => "i32.gt_s".to_string()
      , 0x54 => "i32.ge_s".to_string()
      , 0x55 => "i32.gt_u".to_string()
      , 0x56 => "i32.ge_u".to_string()
      , 0x57 => "i32.clz".to_string()
      , 0x58 => "i32.ctz".to_string()
      , 0x59 => "i32.popcnt".to_string()
      , 0x5a => "i32.eqz".to_string()
      , 0x5b => "i64.add".to_string()
      , 0x5c => "i64.sub".to_string()
      , 0x5d => "i64.mul".to_string()
      , 0x5e => "i64.div_s".to_string()
      , 0x5f => "i64.div_u".to_string()
      , 0x60 => "i64.rem_s".to_string()
      , 0x61 => "i64.rem_u".to_string()
      , 0x62 => "i64.and".to_string()
      , 0x63 => "i64.or".to_string()
      , 0x64 => "i64.xor".to_string()
      , 0x65 => "i64.shl".to_string()
      , 0x66 => "i64.shr_u".to_string()
      , 0x67 => "i64.shr_s".to_string()
      , 0xb8 => "i64.rotr".to_string()
      , 0xb9 => "i64.rotl".to_string()
      , 0x68 => "i64.eq".to_string()
      , 0x69 => "i64.ne".to_string()
      , 0x6a => "i64.lt_s".to_string()
      , 0x6b => "i64.le_s".to_string()
      , 0x6c => "i64.lt_u".to_string()
      , 0x6d => "i64.le_u".to_string()
      , 0x6e => "i64.gt_s".to_string()
      , 0x6f => "i64.ge_s".to_string()
      , 0x70 => "i64.gt_u".to_string()
      , 0x71 => "i64.ge_u".to_string()
      , 0x72 => "i64.clz".to_string()
      , 0x73 => "i64.ctz".to_string()
      , 0x74 => "i64.popcnt".to_string()
      , 0xba => "i64.eqz".to_string()
      , 0x75 => "f32.add".to_string()
      , 0x76 => "f32.sub".to_string()
      , 0x77 => "f32.mul".to_string()
      , 0x78 => "f32.div".to_string()
      , 0x79 => "f32.min".to_string()
      , 0x7a => "f32.max".to_string()
      , 0x7b => "f32.abs".to_string()
      , 0x7c => "f32.neg".to_string()
      , 0x7d => "f32.copysign".to_string()
      , 0x7e => "f32.ceil".to_string()
      , 0x7f => "f32.floor".to_string()
      , 0x80 => "f32.trunc".to_string()
      , 0x81 => "f32.nearest".to_string()
      , 0x82 => "f32.sqrt".to_string()
      , 0x83 => "f32.eq".to_string()
      , 0x84 => "f32.ne".to_string()
      , 0x85 => "f32.lt".to_string()
      , 0x86 => "f32.le".to_string()
      , 0x87 => "f32.gt".to_string()
      , 0x88 => "f32.ge".to_string()
      , 0x89 => "f64.add".to_string()
      , 0x8a => "f64.sub".to_string()
      , 0x8b => "f64.mul".to_string()
      , 0x8c => "f64.div".to_string()
      , 0x8d => "f64.min".to_string()
      , 0x8e => "f64.max".to_string()
      , 0x8f => "f64.abs".to_string()
      , 0x90 => "f64.neg".to_string()
      , 0x91 => "f64.copysign".to_string()
      , 0x92 => "f64.ceil".to_string()
      , 0x93 => "f64.floor".to_string()
      , 0x94 => "f64.trunc".to_string()
      , 0x95 => "f64.nearest".to_string()
      , 0x96 => "f64.sqrt".to_string()
      , 0x97 => "f64.eq".to_string()
      , 0x98 => "f64.ne".to_string()
      , 0x99 => "f64.lt".to_string()
      , 0x9a => "f64.le".to_string()
      , 0x9b => "f64.gt".to_string()
      , 0x9c => "f64.ge".to_string()
      , 0x9d => "i32.trunc_s/f32".to_string()
      , 0x9e => "i32.trunc_s/f64".to_string()
      , 0x9f => "i32.trunc_u/f32".to_string()
      , 0xa0 => "i32.trunc_u/f64".to_string()
      , 0xa1 => "i32.wrap/i64".to_string()
      , 0xa2 => "i64.trunc_s/f32".to_string()
      , 0xa3 => "i64.trunc_s/f64".to_string()
      , 0xa4 => "i64.trunc_u/f32".to_string()
      , 0xa5 => "i64.trunc_u/f64".to_string()
      , 0xa6 => "i64.extend_s/i32".to_string()
      , 0xa7 => "i64.extend_u/i32".to_string()
      , 0xa8 => "f32.convert_s/i32".to_string()
      , 0xa9 => "f32.convert_u/i32".to_string()
      , 0xaa => "f32.convert_s/i64".to_string()
      , 0xab => "f32.convert_u/i64".to_string()
      , 0xac => "f32.demote/f64".to_string()
      , 0xad => "f32.reinterpret/i32".to_string()
      , 0xae => "f64.convert_s/i32".to_string()
      , 0xaf => "f64.convert_u/i32".to_string()
      , 0xb0 => "f64.convert_s/i64".to_string()
      , 0xb1 => "f64.convert_u/i64".to_string()
      , 0xb2 => "f64.promote/f32".to_string()
      , 0xb3 => "f64.reinterpret/i64".to_string()
      , 0xb4 => "i32.reinterpret/f32".to_string()
      , 0xb5 => "i64.reinterpret/f64".to_string()

      , _    => "?".to_string()
    })
  }

  println!("");
}


fn consume_vu32(iter : &mut Iter<u8>) -> u32 {
  super::parsable_bytes::read_vu32(&mut || *iter.next().unwrap())
}


fn consume_vu1(iter : &mut Iter<u8>) -> u8 {
  super::parsable_bytes::read_vu1(&mut || *iter.next().unwrap())
}


fn consume_vu64(iter : &mut Iter<u8>) -> u64 {
  super::parsable_bytes::read_vu64(&mut || *iter.next().unwrap())
}


fn consume_vs32(iter : &mut Iter<u8>) -> i32 {
  super::parsable_bytes::read_vs32(&mut || *iter.next().unwrap())
}


fn consume_vs64(iter : &mut Iter<u8>) -> i64 {
  super::parsable_bytes::read_vs64(&mut || *iter.next().unwrap())
}


fn consume_f32(iter : &mut Iter<u8>) -> f32 {
  super::parsable_bytes::read_f32(&mut || *iter.next().unwrap())
}


fn consume_f64(iter : &mut Iter<u8>) -> f64 {
  super::parsable_bytes::read_f64(&mut || *iter.next().unwrap())
}


fn to_string(node : &Node) -> String {
  match node {
        &Node::Nop => format!("nop")
      , &Node::Block { ref children } => {
          match children.len() {
              0 => format!("()")
            , 1 => format!("({})", to_string(&children[0]))
            , 2 => format!("(\n\t{}\n\t{}\n)", to_string(&children[0]), to_string(&children[1]))
            , 3 => format!("(\n\t{}\n\t{}\n\t{}\n)", to_string(&children[0]), to_string(&children[1]), to_string(&children[2]))
            , 4 => format!("(\n\t{}\n\t{}\n\t{}\n\t{}\n)", to_string(&children[0]), to_string(&children[1]), to_string(&children[2]), to_string(&children[3]))
            , _ => format!("_({})", to_string(&children[0]))
          }
        }
      , &Node::Loop { ref children } => {
          match children.len() {
              0 => format!("(loop)")
            , _ => format!("(loop {})", to_string(&children[0]))
          }
        }
      , &Node::If { ref children } => format!("if")
      , &Node::Else { ref children } => format!("else")
      , &Node::Select => format!("select")
      , &Node::Br { ref argument_count, ref relative_depth } => format!("br")
      , &Node::BrIf { ref argument_count, ref relative_depth } => format!("br_if")
      , &Node::BrTable => format!("br_table")
      , &Node::Return => format!("return")
      , &Node::Unreachable => format!("unreachable")
      , &Node::Drop => format!("drop")
      , &Node::End => format!("end")
      , &Node::I32Const { ref value } => {
          format!("i32.const {}", value)
        }
      , &Node::I64Const { ref value } => {
          format!("i64.const {}", value)
        }
      , &Node::F64Const { ref value } => {
          format!("f64.const {}", value)
        }
      , &Node::F32Const { ref value } => {
          format!("f32.const {}", value)
        }
      , &Node::GetLocal { ref local_index } => format!("get_local")
      , &Node::SetLocal { ref local_index } => format!("set_local")
      , &Node::Call { ref argument_count, ref function_index } => {
          format!("call {}({})", function_index, argument_count)
        }
      , &Node::CallIndirect { ref argument_count, ref type_index } => format!("call_indirect")
      , &Node::CallImport { ref argument_count, ref import_index } => format!("call_import")
      , &Node::TeeLocal { ref local_index } => format!("tee_local")
      , &Node::I32Load8S => format!("i32.load8_s")
      , &Node::I32Load8U => format!("i32.load8_u")
      , &Node::I32Load16S => format!("i32.load16_s")
      , &Node::I32Load16U => format!("i32.load16_u")
      , &Node::I64Load8S => format!("i64.load8_s")
      , &Node::I64Load8U => format!("i64.load8_u")
      , &Node::I64Load16S => format!("i64.load16_s")
      , &Node::I64Load16U => format!("i64.load16_u")
      , &Node::I64Load32S => format!("i64.load32_s")
      , &Node::I64Load32U => format!("i64.load32_u")
      , &Node::I32Load => format!("i32.load")
      , &Node::I64Load => format!("i64.load")
      , &Node::F32Load => format!("f32.load")
      , &Node::F64Load => format!("f64.load")
      , &Node::I32Store8 => format!("i32.store8")
      , &Node::I32Store16 => format!("i32.store16")
      , &Node::I64Store8 => format!("i64.store8")
      , &Node::I64Store16 => format!("i64.store16")
      , &Node::I64Store32 => format!("i64.store32")
      , &Node::I32Store => format!("i32.store")
      , &Node::I64Store => format!("i64.store")
      , &Node::F32Store => format!("f32.store")
      , &Node::F64Store => format!("f64.store")
      , &Node::GrowMemory => format!("grow_memory")
      , &Node::CurrentMemory => format!("current_memory")
      , &Node::I32Add => format!("i32.add")
      , &Node::I32Sub => format!("i32.sub")
      , &Node::I32Mul => format!("i32.mul")
      , &Node::I32DivS => format!("i32.div_s")
      , &Node::I32DivU => format!("i32.div_u")
      , &Node::I32RemS => format!("i32.rem_s")
      , &Node::I32RemU => format!("i32.rem_u")
      , &Node::I32And => format!("i32.and")
      , &Node::I32Or => format!("i32.or")
      , &Node::I32Xor => format!("i32.xor")
      , &Node::I32Shl => format!("i32.shl")
      , &Node::I32ShrU => format!("i32.shr_u")
      , &Node::I32ShrS => format!("i32.shr_s")
      , &Node::I32Rotr => format!("i32.rotr")
      , &Node::I32Rotl => format!("i32.rotl")
      , &Node::I32Eq => format!("i32.eq")
      , &Node::I32Ne => format!("i32.ne")
      , &Node::I32LtS => format!("i32.lt_s")
      , &Node::I32LeS => format!("i32.le_s")
      , &Node::I32LtU => format!("i32.lt_u")
      , &Node::I32LeU => format!("i32.le_u")
      , &Node::I32GtS => format!("i32.gt_s")
      , &Node::I32GeS => format!("i32.ge_s")
      , &Node::I32GtU => format!("i32.gt_u")
      , &Node::I32GeU => format!("i32.ge_u")
      , &Node::I32Clz => format!("i32.clz")
      , &Node::I32Ctz => format!("i32.ctz")
      , &Node::I32Popcnt => format!("i32.popcnt")
      , &Node::I32Eqz => format!("i32.eqz")
      , &Node::I64Add => format!("i64.add")
      , &Node::I64Sub => format!("i64.sub")
      , &Node::I64Mul => format!("i64.mul")
      , &Node::I64DivS => format!("i64.div_s")
      , &Node::I64DivU => format!("i64.div_u")
      , &Node::I64RemS => format!("i64.rem_s")
      , &Node::I64RemU => format!("i64.rem_u")
      , &Node::I64And => format!("i64.and")
      , &Node::I64Or => format!("i64.or")
      , &Node::I64Xor => format!("i64.xor")
      , &Node::I64Shl => format!("i64.shl")
      , &Node::I64ShrU => format!("i64.shr_u")
      , &Node::I64ShrS => format!("i64.shr_s")
      , &Node::I64Rotr => format!("i64.rotr")
      , &Node::I64Rotl => format!("i64.rotl")
      , &Node::I64Eq => format!("i64.eq")
      , &Node::I64Ne => format!("i64.ne")
      , &Node::I64LtS => format!("i64.lt_s")
      , &Node::I64LeS => format!("i64.le_s")
      , &Node::I64LtU => format!("i64.lt_u")
      , &Node::I64LeU => format!("i64.le_u")
      , &Node::I64GtS => format!("i64.gt_s")
      , &Node::I64GeS => format!("i64.ge_s")
      , &Node::I64GtU => format!("i64.gt_u")
      , &Node::I64GeU => format!("i64.ge_u")
      , &Node::I64Clz => format!("i64.clz")
      , &Node::I64Ctz => format!("i64.ctz")
      , &Node::I64Popcnt => format!("i64.popcnt")
      , &Node::I64Eqz => format!("i64.eqz")
      , &Node::F32Add => format!("f32.add")
      , &Node::F32Sub => format!("f32.sub")
      , &Node::F32Mul => format!("f32.mul")
      , &Node::F32Div => format!("f32.div")
      , &Node::F32Min => format!("f32.min")
      , &Node::F32Max => format!("f32.max")
      , &Node::F32Abs => format!("f32.abs")
      , &Node::F32Neg => format!("f32.neg")
      , &Node::F32Copysign => format!("f32.copysign")
      , &Node::F32Ceil => format!("f32.ceil")
      , &Node::F32Floor => format!("f32.floor")
      , &Node::F32Trunc => format!("f32.trunc")
      , &Node::F32Nearest => format!("f32.nearest")
      , &Node::F32Sqrt => format!("f32.sqrt")
      , &Node::F32Eq => format!("f32.eq")
      , &Node::F32Ne => format!("f32.ne")
      , &Node::F32Lt => format!("f32.lt")
      , &Node::F32Le => format!("f32.le")
      , &Node::F32Gt => format!("f32.gt")
      , &Node::F32Ge => format!("f32.ge")
      , &Node::F64Add => format!("f64.add")
      , &Node::F64Sub => format!("f64.sub")
      , &Node::F64Mul => format!("f64.mul")
      , &Node::F64Div => format!("f64.div")
      , &Node::F64Min => format!("f64.min")
      , &Node::F64Max => format!("f64.max")
      , &Node::F64Abs => format!("f64.abs")
      , &Node::F64Neg => format!("f64.neg")
      , &Node::F64Copysign => format!("f64.copysign")
      , &Node::F64Ceil => format!("f64.ceil")
      , &Node::F64Floor => format!("f64.floor")
      , &Node::F64Trunc => format!("f64.trunc")
      , &Node::F64Nearest => format!("f64.nearest")
      , &Node::F64Sqrt => format!("f64.sqrt")
      , &Node::F64Eq => format!("f64.eq")
      , &Node::F64Ne => format!("f64.ne")
      , &Node::F64Lt => format!("f64.lt")
      , &Node::F64Le => format!("f64.le")
      , &Node::F64Gt => format!("f64.gt")
      , &Node::F64Ge => format!("f64.ge")
      , &Node::I32TruncSF32 => format!("i32.trunc_s/f32")
      , &Node::I32TruncSF64 => format!("i32.trunc_s/f64")
      , &Node::I32TruncUF32 => format!("i32.trunc_u/f32")
      , &Node::I32TruncUF64 => format!("i32.trunc_u/f64")
      , &Node::I32Wrap_i64 => format!("i32.wrap_i64")
      , &Node::I64TruncSF32 => format!("i64.trunc_s/f32")
      , &Node::I64TruncSF64 => format!("i64.trunc_s/f64")
      , &Node::I64TruncUF32 => format!("i64.trunc_u/f32")
      , &Node::I64TruncUF64 => format!("i64.trunc_u/f64")
      , &Node::I64ExtendSI32 => format!("i64.extend_s/i32")
      , &Node::I64ExtendUI32 => format!("i64.extend_u/i32")
      , &Node::F32ConvertSI32 => format!("f32.convert_s/i32")
      , &Node::F32ConvertUI32 => format!("f32.convert_u/i32")
      , &Node::F32ConvertSI64 => format!("f32.convert_s/i64")
      , &Node::F32ConvertUI64 => format!("f32.convert_u/i64")
      , &Node::F32DemoteF64 => format!("f32.demote/f64")
      , &Node::F32ReinterpretI32 => format!("f32.reinterpret/i32")
      , &Node::F64ConvertSI32 => format!("f64.convert_s/i32")
      , &Node::F64ConvertUI32 => format!("f64.convert_u/i32")
      , &Node::F64ConvertSI64 => format!("f64.convert_s/i64")
      , &Node::F64ConvertUI64 => format!("f64.convert_u/i64")
      , &Node::F64PromoteF32 => format!("f64.promote/f32")
      , &Node::F64ReinterpretI64 => format!("f64.reinterpret/i64")
      , &Node::I32ReinterpretF32 => format!("i32.reinterpret/f32")
      , &Node::I64ReinterpretF64 => format!("i64.reinterpret/f64")
      , &Node::UNKNOWN => format!("UNKNOWN")
  }
}


impl fmt::Display for Node {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", to_string(self))
  }
}
