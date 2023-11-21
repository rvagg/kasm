extern crate byteorder;

use std::io::Cursor;
use self::byteorder::{LittleEndian, ReadBytesExt};

pub struct ParsableBytes {
    bytes       : Vec<u8>
  , pos         : usize
}


impl ParsableBytes {
  pub fn new(bytes : Vec<u8>) -> ParsableBytes {
    ParsableBytes {
        bytes : bytes
      , pos   : 0
    }
  }


  pub fn pos(&mut self) -> usize {
    self.pos
  }


  /*
  pub fn has_more(&mut self) -> bool {
    self.pos < self.bytes.len()
  }
  */


  pub fn has_at_least(&mut self, count : usize) -> bool {
    self.bytes.len() - self.pos >= count
  }


  pub fn skip(&mut self, len : usize) {
    self.pos += len;
  }


  pub fn skip_to(&mut self, pos : usize) {
    self.pos = pos;
  }


  pub fn read_byte(&mut self) -> u8 {
    let byte = self.bytes[self.pos];

    self.skip(1);
    byte
  }


  pub fn read_bytes(&mut self, len : usize) -> Vec<u8> {
    let mut vec = Vec::with_capacity(len);
    for i in 0..len {
      vec.push(self.bytes[self.pos + i]);
    }
    self.skip(len);
    vec
  }


  pub fn read_bool(&mut self) -> bool {
    self.read_byte() == 1
  }


  pub fn read_u32(&mut self) -> u32 {
    let mut result : u32 = 0;

    for i in 0..4 {
      result |= (self.bytes[self.pos + i] as u32) << 8 * i;
    }

    self.skip(4);
    result
  }


  // TODO: signed
  pub fn read_vu64(&mut self) -> u64 {
    read_vu64(&mut || self.read_byte())
  }


  pub fn read_vs64(&mut self) -> i64 {
    read_vs64(&mut || self.read_byte())
  }


  pub fn read_string(&mut self) -> String {
    let mut v = Vec::new();
    let mut i = 0;
    let len = self.read_vu64();

    loop {
      if i == len {
        break;
      }
      let b = self.read_byte(); 
      v.push(b);
      i += 1; // TODO: compact these two lines?
    }

    String::from_utf8(v).unwrap()
  }


/*
  fn have_remaining(self, size : usize) {
    //TODO: return error
    if self.bytes.len() - self.pos < size {
      println!("Not enough bytes left for {}", size);
    }
  }
  */
}


pub fn read_u8<F>(reader : &mut F) -> u8
    where F : FnMut() -> u8 {

  reader()
}


#[test]
fn test_read_u8() {
  assert_eq!(read_u8(&mut || 0b00000000), 0);
  assert_eq!(read_u8(&mut || 0b00000001), 1);
  assert_eq!(read_u8(&mut || 0b10000000), 128);
  assert_eq!(read_u8(&mut || 0b10000001), 129);
  assert_eq!(read_u8(&mut || 0b11111111), 255);
}


pub fn read_u16<F>(reader : &mut F) -> u16
    where F : FnMut() -> u8 {

  let hi = reader() as u16;
  let lo = reader() as u16;

  (hi << 8) + lo
}


#[test]
fn test_read_u16() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_u16(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0b00000000, 0b00000000 ]), 0);
  assert_eq!(read(vec![ 0b00000000, 0b00000001 ]), 1);
  assert_eq!(read(vec![ 0b00000001, 0b00000000 ]), 256);
  assert_eq!(read(vec![ 0b00000001, 0b00000001 ]), 257);
  assert_eq!(read(vec![ 0b00000000, 0b10000000 ]), 128);
  assert_eq!(read(vec![ 0b10000000, 0b00000000 ]), 32768);
  assert_eq!(read(vec![ 0b10000000, 0b10000000 ]), 32896);
  assert_eq!(read(vec![ 0b00000000, 0b11111111 ]), 255);
  assert_eq!(read(vec![ 0b11111111, 0b00000000 ]), 65280);
  assert_eq!(read(vec![ 0b11111111, 0b11111111 ]), 65535);
}


pub fn read_u32<F>(reader : &mut F) -> u32
    where F : FnMut() -> u8 {

  let hi = read_u16(reader) as u32;
  let lo = read_u16(reader) as u32;

  (hi << 16) + lo
}


#[test]
fn test_read_u32() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_u32(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 0);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000001 ]), 1);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000001, 0b00000000 ]), 256);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000001, 0b00000001 ]), 257);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b10000000 ]), 128);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b10000000, 0b00000000 ]), 32768);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b10000000, 0b10000000 ]), 32896);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b10000000, 0b00000000 ]), 32768);
  assert_eq!(read(vec![ 0b00000000, 0b10000000, 0b00000000, 0b00000000 ]), 8388608);
  assert_eq!(read(vec![ 0b00000000, 0b10000000, 0b10000000, 0b10000000 ]), 8421504);
  assert_eq!(read(vec![ 0b10000000, 0b00000000, 0b00000000, 0b00000000 ]), 2147483648);
  assert_eq!(read(vec![ 0b10000000, 0b10000000, 0b10000000, 0b10000000 ]), 2155905152);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b11111111, 0b00000000 ]), 65280);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b11111111, 0b11111111 ]), 65535);
  assert_eq!(read(vec![ 0b00000000, 0b11111111, 0b11111111, 0b11111111 ]), 16777215);
  assert_eq!(read(vec![ 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 4294967295);
}


pub fn read_u64<F>(reader : &mut F) -> u64
    where F : FnMut() -> u8 {

  let hi = read_u32(reader) as u64;
  let lo = read_u32(reader) as u64;

  (hi << 32) + lo
}


#[test]
fn test_read_u64() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_u64(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 0);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001 ]), 1);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001, 0b00000000 ]), 256);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001, 0b00000001 ]), 257);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000 ]), 128);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b00000000 ]), 32768);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b10000000 ]), 32896);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b00000000 ]), 32768);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b00000000, 0b00000000 ]), 8388608);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b10000000, 0b10000000 ]), 8421504);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b00000000, 0b00000000, 0b00000000 ]), 2147483648);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000 ]), 2155905152);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b10000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 549755813888);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b10000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 140737488355328);
  assert_eq!(read(vec![ 0b00000000, 0b10000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 36028797018963968);
  assert_eq!(read(vec![ 0b10000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000 ]), 9223372036854775808);
  assert_eq!(read(vec![ 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b10000000, 0b00000000 ]), 9259542123273814016);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b11111111, 0b00000000 ]), 65280);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b11111111, 0b11111111 ]), 65535);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b11111111, 0b11111111, 0b11111111 ]), 16777215);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 4294967295);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b00000000, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 1099511627775);
  assert_eq!(read(vec![ 0b00000000, 0b00000000, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 281474976710655);
  assert_eq!(read(vec![ 0b00000000, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 72057594037927935);
  assert_eq!(read(vec![ 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111 ]), 18446744073709551615);
}


fn read_vu<F>(reader : &mut F, size : usize) -> u64
    where F : FnMut() -> u8 {

  let mut result : u64 = 0;
  let max_bytes = ((size as f64) / 7_f64).ceil() as usize;

  for i in 0..max_bytes {
    let b = reader() as u32;
    result = result | ((b & 0x7f) << (7 * i)) as u64;
    if (b & 0x80) == 0 {
      break;
    }
  }
  result
}


pub fn read_vu64<F>(reader : &mut F) -> u64
    where F : FnMut() -> u8 {
  read_vu(reader, 64)
}


#[test]
fn test_read_vu64() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_vu64(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0 ]), 0);
  assert_eq!(read(vec![ 1 ]), 1);
  assert_eq!(read(vec![ 0b11100101, 0b10001110, 0b00100110 ]), 624485);
  assert_eq!(read(vec![ 0x7f ]), 127);
  assert_eq!(read(vec![ 0x80, 0x7f ]), 16256);
  assert_eq!(read(vec![ 0xb4, 0x07 ]), 0x3b4);
  assert_eq!(read(vec![ 0x8c, 0x08 ]), 0x40c);
  assert_eq!(read(vec![ 0xff, 0xff, 0xff, 0xff, 0xf ]), 0xffffffff);
  assert_eq!(read(vec![ 128, 128, 128, 128, 120 ]), 0x80000000);
}


pub fn read_vu32<F>(reader : &mut F) -> u32
    where F : FnMut() -> u8 {
  read_vu(reader, 32) as u32
}


#[test]
fn test_read_vu32() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_vu32(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0 ]), 0);
  assert_eq!(read(vec![ 1 ]), 1);
  assert_eq!(read(vec![ 0b11100101, 0b10001110, 0b00100110 ]), 624485);
  assert_eq!(read(vec![ 0x7f ]), 127);
  assert_eq!(read(vec![ 0x80, 0x7f ]), 16256);
  assert_eq!(read(vec![ 0xb4, 0x07 ]), 0x3b4);
  assert_eq!(read(vec![ 0x8c, 0x08 ]), 0x40c);
  assert_eq!(read(vec![ 0xff, 0xff, 0xff, 0xff, 0xf ]), 0xffffffff);
  assert_eq!(read(vec![ 128, 128, 128, 128, 120 ]), 0x80000000);
}


pub fn read_vu1<F>(reader : &mut F) -> u8
    where F : FnMut() -> u8 {
  read_vu(reader, 8) as u8
}


#[test]
fn test_read_vu1() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_vu1(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0 ]), 0);
  assert_eq!(read(vec![ 1 ]), 1);
}


fn read_vs<F>(reader : &mut F, size : usize) -> i64
    where F : FnMut() -> u8 {

  let mut result : i64 = 0;
  let mut shift = 0;
  let max_bytes = ((size as f64) / 7_f64).ceil() as usize;

  for _ in 0..max_bytes {
    let b = reader() as u32;
    result = result | (((b & 0x7f) as i64) << shift) as i64;
    shift = shift + 7;
    if (b & 0x80) == 0 {
      if (shift < size) && (b & 0x40) != 0 {
        result = result | - ((1 as i64) << shift) as i64;
      }
      break;
    }
  }
  result
}


pub fn read_vs64<F>(reader : &mut F) -> i64
    where F : FnMut() -> u8 {

  read_vs(reader, 64) as i64
}


pub fn read_vs32<F>(reader : &mut F) -> i32
    where F : FnMut() -> u8 {

  read_vs(reader, 32) as i32
}


#[test]
fn test_read_vs64() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_vs64(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0 ]), 0);
  assert_eq!(read(vec![ 1 ]), 1);
  assert_eq!(read(vec![ 0b11100101, 0b10001110, 0b00100110 ]), 624485);
  assert_eq!(read(vec![ 0xb4, 0x07 ]), 0x3b4);
  assert_eq!(read(vec![ 0x8c, 0x08 ]), 0x40c);
  assert_eq!(read(vec![ 0x7f ]), -1);
  assert_eq!(read(vec![ 0x80, 0x7f ]), -128);
  assert_eq!(read(vec![ 0b10011011, 0b11110001, 0b01011001 ]), -624485);
  assert_eq!(read(vec![ 128, 128, 128, 128, 128, 128, 128, 252, 255, 0 ]), 0x7ff8000000000000);
  assert_eq!(read(vec![ 128, 128, 128, 128, 128, 128, 128, 128, 128, 127 ]), 0x8000000000000000);
}


#[test]
fn test_read_vs32() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_vs32(&mut || *iter.next().unwrap() as u8)
  };

  assert_eq!(read(vec![ 0 ]), 0);
  assert_eq!(read(vec![ 1 ]), 1);
  assert_eq!(read(vec![ 0b11100101, 0b10001110, 0b00100110 ]), 624485);
  assert_eq!(read(vec![ 0xb4, 0x07 ]), 0x3b4);
  assert_eq!(read(vec![ 0x8c, 0x08 ]), 0x40c);
  assert_eq!(read(vec![ 0x7f ]), -1);
  assert_eq!(read(vec![ 0x80, 0x7f ]), -128);
  assert_eq!(read(vec![ 0b10011011, 0b11110001, 0b01011001 ]), -624485);
  // this is different as a 32 than a 64
  assert_eq!(read(vec![ 128, 128, 128, 128, 120 ]), 0x80000000);
}


pub fn read_f32<F>(reader : &mut F) -> f32
    where F : FnMut() -> u8 {

  let mut rdr = Cursor::new(vec![reader(), reader(), reader(), reader()]);
  rdr.read_f32::<LittleEndian>().unwrap()
}


#[test]
fn test_read_f32() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_f32(&mut || *iter.next().unwrap() as u8)
  };

  assert!(read(vec![ 0, 0, 192, 127 ]).is_nan());
  assert!(read(vec![ 0, 0, 192, 255 ]).is_nan()); // -nan
  assert_eq!(read(vec![ 0, 0, 0, 0 ]), 0.0);
  assert_eq!(read(vec![ 0, 0, 0, 128 ]), 0.0); // -0.0
  assert_eq!(read(vec![ 219, 15, 201, 64 ]), 6.28318548202514648);
  assert_eq!(read(vec![ 1, 0, 0, 0 ]), 1.4013e-45);
  assert_eq!(read(vec![ 0, 0, 128, 0 ]), 1.1754944e-38);
  assert_eq!(read(vec![ 255, 255, 127, 0 ]), 1.1754942e-38);
  assert_eq!(read(vec![ 255, 255, 127, 127 ]), 3.4028234e+38);
  assert_eq!(read(vec![ 249, 2, 21, 80 ]), 1.0e10);
}


pub fn read_f64<F>(reader : &mut F) -> f64
    where F : FnMut() -> u8 {

  let mut rdr = Cursor::new(vec![reader(), reader(), reader(), reader(), reader(), reader(), reader(), reader()]);
  rdr.read_f64::<LittleEndian>().unwrap()
}


#[test]
fn test_read_f64() {
  let read = | v : Vec<u8> | {
    let mut iter = v.iter();
    read_f64(&mut || *iter.next().unwrap() as u8)
  };

  assert!(read(vec![ 0, 0, 0, 0, 0, 0, 248, 127 ]).is_nan());
  assert!(read(vec![ 0, 0, 0, 0, 0, 0, 248, 255 ]).is_nan()); // -nan

  assert_eq!(read(vec![ 0, 0, 0, 0, 0, 0, 0, 0 ]), 0.0);
  assert_eq!(read(vec![ 0, 0, 0, 0, 0, 0, 0, 128 ]), 0.0); // -0
  assert_eq!(read(vec![ 24, 45, 68, 84, 251, 33, 25, 64 ]), 6.28318530717958623);
  assert_eq!(read(vec![ 1, 0, 0, 0, 0, 0, 0, 0 ]), 4.94066e-324);
  assert_eq!(read(vec![ 0, 0, 0, 0, 0, 0, 16, 0 ]), 2.2250738585072012e-308);
  assert_eq!(read(vec![ 255, 255, 255, 255, 255, 255, 15, 0 ]), 2.2250738585072011e-308);
  assert_eq!(read(vec![ 255, 255, 255, 255, 255, 255, 239, 127 ]), 1.7976931348623157e+308);
  assert_eq!(read(vec![ 125, 195, 148, 37, 173, 73, 178, 84 ]), 1.0e100);
}


