use std;
use std::fmt;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::error::Error;
use std::io;



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
    FirstObjectNumberInXrefNotZero,
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
    IndirectReference { number: u32, generation: u16 },
}

#[derive(Debug)]
pub struct PdfError {
    pub desc: String,
    pub underlying: Option<Box<Error>>
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
    fn cause(&self) -> Option<&std::error::Error> {
        self.underlying.as_ref().map(|e| &**e)
    }
}

impl From<io::Error> for PdfError {
    fn from(o: io::Error) -> Self {
        PdfError {desc: "from a std::io::Error".to_string(), underlying:Some(Box::new(o))}
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
                Err(PdfError { desc: "key wasnt a PdfObject::Name".to_string(), underlying: None })
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
                Err(PdfError { desc: "key wasnt a PdfObject::Name".to_string(), underlying: None })
            }
        }
    }
}

/// contain the information represented in a given cross-reference table.
/// capturing what exactly the original representation was is not a goal,
/// capturing the information is.
#[derive(Debug, PartialEq, Eq)]
pub struct CrossReferenceTable {
    object_offsets: BTreeMap<u32, usize>,
    object_generations: BTreeMap<u32, u16>,
    free_objects: BTreeSet<u32>,
}

impl CrossReferenceTable {
    pub fn new() -> CrossReferenceTable {
        CrossReferenceTable {
            object_offsets: BTreeMap::new(),
            object_generations: BTreeMap::new(),
            free_objects: BTreeSet::new(),
        }
    }

    pub fn add_in_use(&mut self, number:u32, generation:u16, offset:usize) {
        self.object_generations.insert(number, generation);
        self.object_offsets.insert(number, offset);
    }
    pub fn add_free(&mut self, number:u32, generation: u16) {
        self.object_generations.insert(number, generation);
        self.free_objects.insert(number);
    }

    pub fn count_in_use(&self) -> usize {
        self.object_offsets.len()
    }
    pub fn count_free(&self) -> usize {
        self.free_objects.len()
    }

    pub fn in_use(&self) -> Vec<u32> {
        self.object_offsets.keys().cloned().collect()
    }
    pub fn free(&self) -> Vec<u32> {
        self.free_objects.iter().cloned().collect()
    }

    pub fn generation_of(&self, number: u32) -> Option<u16> {
        match self.object_generations.get(&number) {
            Some(ref x) => {
                Some((*x).clone())
            },
            _ => {
                None
            }
        }
    }
    pub fn offset_of(&self, number: u32) -> Option<usize> {
        match self.object_offsets.get(&number) {
            Some(ref x) => {
                Some((*x).clone())
            },
            _ => {
                None
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