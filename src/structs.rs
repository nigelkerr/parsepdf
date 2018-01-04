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
    ExpectedDictionaryStart,
    ExpectedDictionaryValue,
    NoValidDictionaryContents,
    NonIntegerLengthInStreamDictionary,
    NegativeLengthInStreamDictionary,
    SomethingHorribleAboutStreamDictionary,
    CalledDictionaryAndGotSomethingElse,
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
    Comment ( Vec<u8> ),  // perhaps get rid of Comment and make it consumable like ws
    String ( Vec<u8> ),
    Name( Vec<u8> ),
    Array( Vec<PdfObject> ),
    Dictionary( NameKeyedMap ),
    Stream( NameKeyedMap, Vec<u8> ),
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

// only Name values acceptable as keys, and really a clone of the Name's
// Vec<u8> used as the key.

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

    pub fn insert(&mut self, k: PdfObject, v: PdfObject) -> Result<Option<PdfObject>, PdfError> {
        match k {
            PdfObject::Name(x) => {
                match self.map.insert(x, v) {
                    Some(z) => {
                        Ok(Some(z))
                    },
                    None => {
                        Ok(None)
                    }
                }
            }
            _ => {
                Err(PdfError { desc: "key wasnt a PdfObject::Name".to_string() })
            }
        }
    }

    pub fn of(values: Vec<PdfObject>) -> Result<Option<NameKeyedMap>, PdfError> {
        let mut map: NameKeyedMap = NameKeyedMap::new();

        for window in values.windows(2) {
            match map.insert(window[0].clone(), window[1].clone()) {
                Ok(_) => {},
                Err(x) => {
                    return Err(x)
                }
            }
        }

        Ok(Some(map))
    }

    pub fn get(&self, k: PdfObject) -> Result<Option<PdfObject>, PdfError>
    {
        match k {
            PdfObject::Name(x) => {
                match self.map.get(&x) {
                    Some(ref p) => {
                        Ok(Some((*p).clone()))
                    },
                    None => {
                        Ok(None)
                    }
                }
            },
            _ => {

                // we treat this as an error now: we could treat it as None-worthy...
                Err(PdfError { desc: "key wasnt a PdfObject::Name".to_string() })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn namekeyedmap_test() {
        match NameKeyedMap::new().get( PdfObject::Name(b"A"[..].to_owned()) ) {
            Ok(None) => {

            },
            _ => {
                assert_eq!(111, 0);
            }
        }

        match NameKeyedMap::of(  vec![PdfObject::Name(b"A"[..].to_owned()) ,  PdfObject::Name(b"B"[..].to_owned()) ] ) {
            Ok(Some(n)) => {

                match n.get(PdfObject::Name(b"A"[..].to_owned())) {

                    Ok(Some(x)) => {
                        assert_eq!(x, PdfObject::Name(b"B"[..].to_owned()));
                    },
                    _ => {
                        assert_eq!(114,0);
                    }
                }

            },
            _ => {
                assert_eq!(112, 0);
            }
        }

        match NameKeyedMap::of(  vec![PdfObject::Name(b"A"[..].to_owned()) ,
                                      PdfObject::Name(b"B"[..].to_owned()) ,
                                      PdfObject::Name(b"A"[..].to_owned()) ,
                                      PdfObject::Name(b"C"[..].to_owned()) ] ) {
            Ok(Some(n)) => {

                match n.get(PdfObject::Name(b"A"[..].to_owned())) {

                    Ok(Some(x)) => {
                        assert_eq!(x, PdfObject::Name(b"C"[..].to_owned()));
                    },
                    _ => {
                        assert_eq!(115,0);
                    }
                }

            },
            _ => {
                assert_eq!(112, 0);
            }
        }


        match NameKeyedMap::of(  vec![PdfObject::Name(b"A"[..].to_owned())  ] ) {
            Ok(Some(n)) => {

                match n.get(PdfObject::Name(b"A"[..].to_owned())) {

                    Ok(None) => {

                    },
                    _ => {
                        assert_eq!(117,0);
                    }
                }

            },
            _ => {
                assert_eq!(118, 0);
            }
        }


    }
}