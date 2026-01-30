//! WASI type definitions and error codes
//!
//! This module defines WASI preview1 error codes and helper types.
//!
//! # Specification
//!
//! See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#errno>

/// WASI error codes (errno values)
///
/// These are the standard WASI error codes as defined in the WASI preview1 specification.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#errno>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u32)]
pub enum WasiErrno {
    /// No error occurred
    Success = 0,
    /// Argument list too long
    TooBig = 1,
    /// Permission denied
    Access = 2,
    /// Address in use
    AddrInUse = 3,
    /// Address not available
    AddrNotAvail = 4,
    /// Address family not supported
    AfNoSupport = 5,
    /// Resource unavailable, or operation would block
    Again = 6,
    /// Connection already in progress
    Already = 7,
    /// Bad file descriptor
    BadF = 8,
    /// Bad message
    BadMsg = 9,
    /// Device or resource busy
    Busy = 10,
    /// Operation canceled
    Canceled = 11,
    /// No child processes
    Child = 12,
    /// Connection aborted
    ConnAborted = 13,
    /// Connection refused
    ConnRefused = 14,
    /// Connection reset
    ConnReset = 15,
    /// Resource deadlock would occur
    Deadlk = 16,
    /// Destination address required
    DestAddrReq = 17,
    /// Mathematics argument out of domain of function
    Dom = 18,
    /// Reserved
    Dquot = 19,
    /// File exists
    Exist = 20,
    /// Bad address
    Fault = 21,
    /// File too large
    Fbig = 22,
    /// Host is unreachable
    HostUnreach = 23,
    /// Identifier removed
    Idrm = 24,
    /// Illegal byte sequence
    Ilseq = 25,
    /// Operation in progress
    InProgress = 26,
    /// Interrupted function
    Intr = 27,
    /// Invalid argument
    Inval = 28,
    /// I/O error
    Io = 29,
    /// Socket is connected
    IsConn = 30,
    /// Is a directory
    IsDir = 31,
    /// Too many levels of symbolic links
    Loop = 32,
    /// File descriptor value too large
    Mfile = 33,
    /// Too many links
    Mlink = 34,
    /// Message too large
    MsgSize = 35,
    /// Reserved
    Multihop = 36,
    /// Filename too long
    NameTooLong = 37,
    /// Network is down
    NetDown = 38,
    /// Connection aborted by network
    NetReset = 39,
    /// Network unreachable
    NetUnreach = 40,
    /// Too many files open in system
    Nfile = 41,
    /// No buffer space available
    NoBufs = 42,
    /// No such device
    NoDev = 43,
    /// No such file or directory
    NoEnt = 44,
    /// Executable file format error
    NoExec = 45,
    /// No locks available
    NoLck = 46,
    /// Reserved
    NoLink = 47,
    /// Not enough space
    NoMem = 48,
    /// No message of the desired type
    NoMsg = 49,
    /// Protocol not available
    NoProtoOpt = 50,
    /// No space left on device
    NoSpc = 51,
    /// Function not supported
    NoSys = 52,
    /// The socket is not connected
    NotConn = 53,
    /// Not a directory or a symbolic link to a directory
    NotDir = 54,
    /// Directory not empty
    NotEmpty = 55,
    /// State not recoverable
    NotRecoverable = 56,
    /// Not a socket
    NotSock = 57,
    /// Not supported, or operation not supported on socket
    NotSup = 58,
    /// Inappropriate I/O control operation
    NoTty = 59,
    /// No such device or address
    Nxio = 60,
    /// Value too large to be stored in data type
    Overflow = 61,
    /// Previous owner died
    OwnerDead = 62,
    /// Operation not permitted
    Perm = 63,
    /// Broken pipe
    Pipe = 64,
    /// Protocol error
    Proto = 65,
    /// Protocol not supported
    ProtoNoSupport = 66,
    /// Protocol wrong type for socket
    Prototype = 67,
    /// Result too large
    Range = 68,
    /// Read-only file system
    Rofs = 69,
    /// Invalid seek
    Spipe = 70,
    /// No such process
    Srch = 71,
    /// Reserved
    Stale = 72,
    /// Connection timed out
    TimedOut = 73,
    /// Text file busy
    TxtBsy = 74,
    /// Cross-device link
    Xdev = 75,
    /// Extension: capabilities insufficient
    NotCapable = 76,
}

/// WASI file types
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#filetype>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum WasiFileType {
    /// The type of the file descriptor or file is unknown or is different from any of the other
    /// types specified.
    Unknown = 0,
    /// The file descriptor or file refers to a character device inode.
    CharacterDevice = 2,
    /// The file descriptor or file refers to a directory inode.
    Directory = 3,
    /// The file descriptor or file refers to a regular file inode.
    RegularFile = 4,
}

impl WasiErrno {
    /// Convert to u32 for WASI return values
    pub fn as_u32(self) -> u32 {
        self as u32
    }
}

impl From<WasiErrno> for u32 {
    fn from(errno: WasiErrno) -> u32 {
        errno as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_errno_values() {
        assert_eq!(WasiErrno::Success.as_u32(), 0);
        assert_eq!(WasiErrno::BadF.as_u32(), 8);
        assert_eq!(WasiErrno::Inval.as_u32(), 28);
        assert_eq!(WasiErrno::NoSys.as_u32(), 52);
    }

    #[test]
    fn test_errno_into_u32() {
        let errno: u32 = WasiErrno::Success.into();
        assert_eq!(errno, 0);
    }
}
