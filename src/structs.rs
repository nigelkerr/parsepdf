use std;
use std::fmt;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorCodes {
    NameNotStartWithSlash = 1024,
    ExpectedHexDigit,
    TooManyHexDigits,
    HexStringIncludesZeroByte,
    UnexpectedHexDecodingSituation,
    ByteValueOughtToHaveBeenHexEncoded,
    UnrecognizedEscapeSequence,
    ExpectedStringStart,
    ExpectedArrayStart,
    NoValidArrayContents,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PdfVersion {
    Known { ver: Vec<u8> },
    Unknown
}

#[derive(Debug, PartialEq, Clone)]
pub enum PdfObject {
    Null,
    Boolean ( bool ),
    Integer (i64 ),
    Float (f64 ),
    Comment ( Vec<u8> ),
    String ( Vec<u8> ),
    Name( Vec<u8> ),
    Array( Vec<PdfObject> ),
    Dictionary( NameKeyedMap ),
    IndirectReference { number: u32, generation: u32 },
}

#[derive(Debug, Clone)]
pub struct PdfError {
    pub desc: String,
}

impl fmt::Display for PdfError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.desc)
    }
}
impl std::error::Error for PdfError {
    fn description(&self) -> &str {
        &self.desc
    }
}

// only Name values as keys

#[derive(Debug, Clone, PartialEq)]
pub struct NameKeyedMap {
    map: HashMap<Vec<u8>,PdfObject>
}

impl NameKeyedMap {
    pub fn new() -> NameKeyedMap {
        NameKeyedMap {
            map: HashMap::new()
        }
    }

    pub fn insert(&mut self, k: PdfObject, v: PdfObject) -> Result<PdfObject, PdfError> {
        match k {
            PdfObject::Name(x) => {
                Ok(self.map.insert(x, v).unwrap())
            }
            _ => {
                Err(PdfError { desc: "key wasnt a PdfObject::Name".to_string() })
            }
        }
    }

    pub fn of(values: Vec<PdfObject>) -> NameKeyedMap {
        let mut map: NameKeyedMap = NameKeyedMap::new();

        for window in values.windows(2) {
            map.insert(window[0].clone(), window[1].clone());
        }

        map
    }
}