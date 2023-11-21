use std::fmt;


pub struct ParsedUnit {
    pub name            : String
  , pub magic           : u32
  , pub version         : u32
  , pub types           : Vec<Type>
  , pub imports         : Vec<Import>
  , pub functions       : Vec<Function>
  , pub memory          : Option<Memory>
  , pub exports         : Vec<Export>
  , pub function_bodies : Vec<FunctionBody>
  , pub data            : Vec<Data>
  , pub table           : Vec<u64>
  , pub start           : u64
}


pub struct Type {
    pub form        : Form
  , pub parameters  : Vec<ValueType>
  , pub return_type : ValueType
}


pub struct Import {
    pub external_kind : ExternalKind
  , pub module        : String
  , pub name          : String
  , pub ftype_index   : Option<u8>
  , pub memory_type   : Option<MemoryType>
}


pub struct Function {
    pub ftype_index : u8
}


pub struct Memory {
    pub min      : u64
  , pub max      : u64
  , pub exported : bool
}


pub struct Export {
    pub function_index : u64
  , pub name           : String
}


pub struct FunctionBody {
    pub locals : Vec<ValueType>
  , pub body   : Vec<u8>
}


pub struct Data {
    pub address : u64
  , pub data    : Vec<u8>
}


pub enum Form {
  Function
}


pub enum ExternalKind {
    Function
  , Table
  , Memory
  , Global
  , UNKNOWN
}


#[derive(Copy,Clone)]
pub enum ValueType {
    Int32
  , Int64
  , Float32
  , Float64
  , UNKNOWN
  , None
}


pub struct MemoryType {
  limits : ResizableLimits
}


pub struct ResizableLimits {
    flags   : u8
  , initial : u64
  , maximum : u64
}


impl ParsedUnit {
  pub fn new(name : &String) -> ParsedUnit {
    ParsedUnit {
        name            : name.clone()
      , magic           : 0
      , version         : 0
      , types           : vec!()
      , imports         : vec!()
      , functions       : vec!()
      , memory          : None
      , exports         : vec!()
      , function_bodies : vec!()
      , data            : vec!()
      , table           : vec!()
      , start           : 0
    }
  }
}


impl fmt::Display for ParsedUnit {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ParsedUnit('{}') {{", self.name).unwrap();
    write!(f, "\n  magic number = {}", self.magic).unwrap();
    write!(f, "\n  version = {}", self.version).unwrap();
    write!(f, "\n  types").unwrap();
    let mut i = 0;
    for t in &self.types {
      write!(f, "\n    {} {}", i, t).unwrap();
      i += 1;
    }
    write!(f, "\n  imports").unwrap();
    i = 0;
    for imp in &self.imports {
      write!(f, "\n    {} {}", i, imp).unwrap();
      i += 1;
    }
    write!(f, "\n  functions").unwrap();
    i = 0;
    for func in &self.functions {
      write!(f, "\n    {} {}", i, func).unwrap();
      i += 1;
    }
    write!(f, "\n  memory: ").unwrap();
    match &self.memory.as_ref() {
        &Some(v) => { write!(f, "{}", v).unwrap(); }
      , &None => { write!(f, "None").unwrap(); }
    }
    write!(f, "\n  exports").unwrap();
    i = 0;
    for imp in &self.exports {
      write!(f, "\n    {} {}", i, imp).unwrap();
      i += 1;
    }
    write!(f, "\n  function_bodies").unwrap();
    i = 0;
    for imp in &self.function_bodies {
      write!(f, "\n    {} {}", i, imp).unwrap();
      i += 1;
    }
    write!(f, "\n  data").unwrap();
    i = 0;
    for d in &self.data {
      write!(f, "\n    {} {}", i, d).unwrap();
      i += 1;
    }
    write!(f, "\n  table").unwrap();
    i = 0;
    for fi in &self.table {
      write!(f, "\n    {} function = {}", i, fi).unwrap();
      i += 1;
    }
    write!(f, "\n  start function = {}", &self.start).unwrap();
    write!(f, "\n}}")
  }
}


impl fmt::Display for Type {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    f.debug_set().entries(self.parameters.iter()).finish().unwrap();
    write!(f, " : {}", self.return_type)
  }
}


impl fmt::Display for Import {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}::{} kind = {}", self.module, self.name, self.external_kind)
  }
}


impl fmt::Display for Function {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "type = {}", self.ftype_index)
  }
}


impl fmt::Display for Memory {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "min = {}, max = {}, exported = {}", self.min, self.max, self.exported)
  }
}


impl fmt::Display for Export {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{} function = {}", self.name, self.function_index)
  }
}


impl fmt::Display for FunctionBody {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "locals = ").unwrap();
    f.debug_set().entries(self.locals.iter()).finish().unwrap();
    write!(f, " body = ").unwrap();
    f.debug_set().entries(self.body.iter()).finish()
  }
}


impl fmt::Display for Data {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "address = {} data = ", self.address).unwrap();
    f.debug_set().entries(self.data.iter()).finish()
  }
}


/*
impl Clone for ValueType {
  impl Clone for Stats {
    fn clone(&self) -> ValueType {
      match
    }
  }
}*/


impl ValueType {
  pub fn decode(byte : u8) -> ValueType {
    match byte {
        0x01 => ValueType::Int32
      , 0x02 => ValueType::Int64
      , 0x03 => ValueType::Float32
      , 0x04 => ValueType::Float64
      , _    => ValueType::UNKNOWN
    }
  }
}


impl ExternalKind {
  pub fn decode(byte : u8) -> ExternalKind {
    match byte {
        0x01 => ExternalKind::Function
      , 0x02 => ExternalKind::Table
      , 0x03 => ExternalKind::Memory
      , 0x04 => ExternalKind::Global
      , _    => ExternalKind::UNKNOWN
    }
  }
}


impl fmt::Display for ValueType {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", match self {
        &ValueType::Int32   => "Int32"
      , &ValueType::Int64   => "Int64"
      , &ValueType::Float64 => "Float64"
      , &ValueType::Float32 => "Float32"
      , &ValueType::UNKNOWN => "UNKNOWN"
      , &ValueType::None    => "None"
    })
  }
}

impl fmt::Debug for ValueType {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self, f)
  }
}


impl fmt::Display for ExternalKind {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", match self {
        &ExternalKind::Function => "Function"
      , &ExternalKind::Table    => "Table"
      , &ExternalKind::Memory   => "Memory"
      , &ExternalKind::Global   => "Global"
      , &ExternalKind::UNKNOWN  => "UNKNOWN"
    })
  }
}

impl fmt::Debug for ExternalKind {
  fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
    fmt::Display::fmt(&self, f)
  }
}
