extern crate byteorder;
extern crate hex;

use self::byteorder::{LittleEndian, ReadBytesExt};
use std::io;

pub enum ReadStringError {
    Io(io::Error),
    Utf8(std::string::FromUtf8Error),
}

impl From<io::Error> for ReadStringError {
    fn from(err: io::Error) -> ReadStringError {
        ReadStringError::Io(err)
    }
}

impl From<std::string::FromUtf8Error> for ReadStringError {
    fn from(err: std::string::FromUtf8Error) -> ReadStringError {
        ReadStringError::Utf8(err)
    }
}

pub struct ParsableBytes {
    bytes: Vec<u8>,
    pos: usize,
}

impl ParsableBytes {
    pub fn new(bytes: Vec<u8>) -> ParsableBytes {
        ParsableBytes {
            bytes: bytes,
            pos: 0,
        }
    }
}

impl ParsableBytes {
    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn read_byte(&mut self) -> Result<u8, io::Error> {
        match self.next() {
            Some(byte) => Ok(byte),
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "no more bytes to read",
            )),
        }
    }

    pub fn has_at_least(&self, count: usize) -> bool {
        self.bytes.len() - self.pos >= count
    }

    pub fn remaining(&self) -> usize {
        self.bytes.len() - self.pos
    }

    pub fn skip(&mut self, len: usize) {
        self.pos += len;
    }

    pub fn skip_to(&mut self, pos: usize) {
        self.pos = pos;
    }

    pub fn read_bytes(&mut self, len: usize) -> Result<Vec<u8>, io::Error> {
        if self.pos + len > self.bytes.len() {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "not enough bytes to read",
            ));
        }
        let mut vec = Vec::with_capacity(len);
        for i in 0..len {
            vec.push(self.bytes[self.pos + i]);
        }
        self.skip(len);
        Ok(vec)
    }

    pub fn read_bool(&mut self) -> Result<bool, io::Error> {
        match self.next() {
            Some(byte) => Ok(byte == 1),
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "no more bytes to read",
            )),
        }
    }

    pub fn read_u32(&mut self) -> Result<u32, io::Error> {
        let bytes = self.read_bytes(4)?;
        if bytes.len() != 4 {
            return Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "not enough bytes to read",
            ));
        }

        let mut num = 0;
        for (i, byte) in bytes.iter().enumerate() {
            num |= (*byte as u32) << (i * 8);
        }
        Ok(num)
    }

    // TODO: signed
    pub fn read_vu64(&mut self) -> Result<u64, io::Error> {
        read_vu64(&mut || match self.next() {
            Some(byte) => Ok(byte),
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "no more bytes to read",
            )),
        })
    }

    pub fn read_vu32(&mut self) -> Result<u32, io::Error> {
        read_vu32(&mut || match self.next() {
            Some(byte) => Ok(byte),
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "no more bytes to read",
            )),
        })
    }

    pub fn read_vs64(&mut self) -> Result<i64, io::Error> {
        read_vs64(&mut || match self.next() {
            Some(byte) => Ok(byte),
            None => Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "no more bytes to read",
            )),
        })
    }

    pub fn read_string(&mut self) -> Result<String, io::Error> {
        let len = self.read_vu64()?;
        let mut v = Vec::with_capacity(len as usize);

        for _ in 0..len {
            match self.next() {
                Some(byte) => v.push(byte),
                None => {
                    return Err(io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "no more bytes to read",
                    ))
                }
            }
        }

        String::from_utf8(v)
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "invalid utf-8 sequence"))
    }
}

impl Iterator for ParsableBytes {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos < self.bytes.len() {
            let byte = self.bytes[self.pos];
            self.pos += 1;
            Some(byte)
        } else {
            None
        }
    }
}

/*
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
*/

/*
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
*/

/*
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
*/

fn read_vu<F>(reader: &mut F, size: usize) -> Result<u64, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    let mut result: u64 = 0;
    let max_bytes = ((size as f64) / 7_f64).ceil() as usize;

    for i in 0..max_bytes {
        let b = reader()? as u32;
        let value = (b & 0x7f) << (7 * i);
        if value > (u64::MAX - result) as u32 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "integer overflow",
            ));
        }
        result |= value as u64;
        if (b & 0x80) == 0 {
            break;
        }
    }

    Ok(result)
}

pub fn read_vu64<F>(reader: &mut F) -> Result<u64, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    read_vu(reader, 64)
}

#[cfg(test)]
fn next_byte(v: Vec<u8>) -> impl FnMut() -> Result<u8, io::Error> {
    let mut iter = v.into_iter();
    move || {
        iter.next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::UnexpectedEof, "no more bytes to read"))
    }
}

#[test]
fn test_read_vu64() {
    let read = |v: Vec<u8>| read_vu64(&mut next_byte(v)).expect("Failed to read u64");

    assert_eq!(read(vec![0]), 0);
    assert_eq!(read(vec![1]), 1);
    assert_eq!(read(vec![0b11100101, 0b10001110, 0b00100110]), 624485);
    assert_eq!(read(vec![0x7f]), 127);
    assert_eq!(read(vec![0x80, 0x7f]), 16256);
    assert_eq!(read(vec![0xb4, 0x07]), 0x3b4);
    assert_eq!(read(vec![0x8c, 0x08]), 0x40c);
    assert_eq!(read(vec![0xff, 0xff, 0xff, 0xff, 0xf]), 0xffffffff);
    assert_eq!(read(vec![128, 128, 128, 128, 120]), 0x80000000);
}

pub fn read_vu32<F>(reader: &mut F) -> Result<u32, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    read_vu(reader, 32).map(|v| v as u32)
}

#[test]
fn test_read_vu32() {
    let read = |v: Vec<u8>| read_vu32(&mut next_byte(v)).expect("Failed to read u32");

    assert_eq!(read(vec![0]), 0);
    assert_eq!(read(vec![1]), 1);
    assert_eq!(read(vec![0b11100101, 0b10001110, 0b00100110]), 624485);
    assert_eq!(read(vec![0x7f]), 127);
    assert_eq!(read(vec![0x80, 0x7f]), 16256);
    assert_eq!(read(vec![0xb4, 0x07]), 0x3b4);
    assert_eq!(read(vec![0x8c, 0x08]), 0x40c);
    assert_eq!(read(vec![0xff, 0xff, 0xff, 0xff, 0xf]), 0xffffffff);
    assert_eq!(read(vec![128, 128, 128, 128, 120]), 0x80000000);
}

pub fn read_vu1<F>(reader: &mut F) -> Result<u8, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    read_vu(reader, 8).map(|v| v as u8)
}

#[test]
fn test_read_vu1() {
    let read = |v: Vec<u8>| read_vu32(&mut next_byte(v)).expect("Failed to read vu1");

    assert_eq!(read(vec![0]), 0);
    assert_eq!(read(vec![1]), 1);
}

fn read_vs<F>(reader: &mut F, size: usize) -> Result<i64, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    let mut result: i64 = 0;
    let mut shift = 0;
    let max_bytes = ((size as f64) / 7_f64).ceil() as usize;

    for _ in 0..max_bytes {
        let b = reader()? as u32;
        result = result | (((b & 0x7f) as i64) << shift) as i64;
        shift = shift + 7;
        if (b & 0x80) == 0 {
            if (shift < size) && (b & 0x40) != 0 {
                result = result | -((1 as i64) << shift) as i64;
            }
            break;
        }
    }
    Ok(result)
}

pub fn read_vs64<F>(reader: &mut F) -> Result<i64, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    read_vs(reader, 64)
}

pub fn read_vs32<F>(reader: &mut F) -> Result<i32, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    read_vs(reader, 32).map(|v| v as i32)
}

#[test]
fn test_read_vs64() {
    let read = |v: Vec<u8>| read_vs64(&mut next_byte(v)).expect("Failed to read vs64");

    assert_eq!(read(vec![0]), 0);
    assert_eq!(read(vec![1]), 1);
    assert_eq!(read(vec![0b11100101, 0b10001110, 0b00100110]), 624485);
    assert_eq!(read(vec![0xb4, 0x07]), 0x3b4);
    assert_eq!(read(vec![0x8c, 0x08]), 0x40c);
    assert_eq!(read(vec![0x7f]), -1);
    assert_eq!(read(vec![0x80, 0x7f]), -128);
    assert_eq!(read(vec![0b10011011, 0b11110001, 0b01011001]), -624485);
    assert_eq!(
        read(vec![128, 128, 128, 128, 128, 128, 128, 252, 255, 0]),
        0x7ff8000000000000
    );
    assert_eq!(
        read(vec![128, 128, 128, 128, 128, 128, 128, 128, 128, 127]),
        (0x8000000000000000u64 as i64)
    );
}

#[test]
fn test_read_vs32() {
    let read = |v: Vec<u8>| read_vs32(&mut next_byte(v)).expect("Failed to read vs32");

    assert_eq!(read(vec![0]), 0);
    assert_eq!(read(vec![1]), 1);
    assert_eq!(read(vec![0b11100101, 0b10001110, 0b00100110]), 624485);
    assert_eq!(read(vec![0xb4, 0x07]), 0x3b4);
    assert_eq!(read(vec![0x8c, 0x08]), 0x40c);
    assert_eq!(read(vec![0x7f]), -1);
    assert_eq!(read(vec![0x80, 0x7f]), -128);
    assert_eq!(read(vec![0b10011011, 0b11110001, 0b01011001]), -624485);
    // this is different as a 32 than a 64
    assert_eq!(read(vec![128, 128, 128, 128, 120]), 0x80000000u32 as i32);
}

pub fn read_f32<F>(reader: &mut F) -> Result<f32, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    let mut buf = [0u8; 4];
    for i in 0..4 {
        buf[i] = reader()?;
    }
    let mut rdr = io::Cursor::new(buf);
    rdr.read_f32::<LittleEndian>()
}

#[test]
fn test_read_f32() {
    let read = |v: Vec<u8>| read_f32(&mut next_byte(v)).expect("Failed to read f32");

    assert!(read(vec![0, 0, 192, 127]).is_nan());
    assert!(read(vec![0, 0, 192, 255]).is_nan()); // -nan
    assert_eq!(read(vec![0, 0, 0, 0]), 0.0);
    assert_eq!(read(vec![0, 0, 0, 128]), 0.0); // -0.0
    assert_eq!(read(vec![219, 15, 201, 64]), 6.28318548202514648);
    assert_eq!(read(vec![1, 0, 0, 0]), 1.4013e-45);
    assert_eq!(read(vec![0, 0, 128, 0]), 1.1754944e-38);
    assert_eq!(read(vec![255, 255, 127, 0]), 1.1754942e-38);
    assert_eq!(read(vec![255, 255, 127, 127]), 3.4028234e+38);
    assert_eq!(read(vec![249, 2, 21, 80]), 1.0e10);
}

pub fn read_f64<F>(reader: &mut F) -> Result<f64, io::Error>
where
    F: FnMut() -> Result<u8, io::Error>,
{
    let mut buf = [0u8; 8];
    for i in 0..8 {
        buf[i] = reader()?;
    }
    let mut rdr = io::Cursor::new(buf);
    rdr.read_f64::<LittleEndian>()
}

#[test]
fn test_read_f64() {
    let read = |v: Vec<u8>| read_f64(&mut next_byte(v)).expect("Failed to read f64");

    assert!(read(vec![0, 0, 0, 0, 0, 0, 248, 127]).is_nan());
    assert!(read(vec![0, 0, 0, 0, 0, 0, 248, 255]).is_nan()); // -nan

    assert_eq!(read(vec![0, 0, 0, 0, 0, 0, 0, 0]), 0.0);
    assert_eq!(read(vec![0, 0, 0, 0, 0, 0, 0, 128]), 0.0); // -0
    assert_eq!(
        read(vec![24, 45, 68, 84, 251, 33, 25, 64]),
        6.28318530717958623
    );
    assert_eq!(read(vec![1, 0, 0, 0, 0, 0, 0, 0]), 4.94066e-324);
    assert_eq!(read(vec![0, 0, 0, 0, 0, 0, 16, 0]), 2.2250738585072012e-308);
    assert_eq!(
        read(vec![255, 255, 255, 255, 255, 255, 15, 0]),
        2.2250738585072011e-308
    );
    assert_eq!(
        read(vec![255, 255, 255, 255, 255, 255, 239, 127]),
        1.7976931348623157e+308
    );
    assert_eq!(read(vec![125, 195, 148, 37, 173, 73, 178, 84]), 1.0e100);
}
