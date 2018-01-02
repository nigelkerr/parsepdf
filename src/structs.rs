extern crate nom;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub enum PdfVersion {
    Known { ver: Vec<u8> },
    Unknown
}

#[derive(Debug, PartialEq)]
pub enum PdfObject {
    Null,
    Boolean ( bool ),
    Integer (i64 ),
    Float (f64 ),
    Comment ( Vec<u8> ),
    String ( Vec<u8> ),
    Name( Vec<u8> ),
    Array( Vec<PdfObject> ),
    IndirectReference { number: u32, generation: u32 },
}