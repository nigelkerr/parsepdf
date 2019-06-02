use nom::{
    branch::alt, bytes::complete::*, character::is_digit, combinator::*, error::*, sequence::*,
    Err, IResult, Needed,
};
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::str;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum PdfVersion {
    V1_0,
    V1_1,
    V1_2,
    V1_3,
    V1_4,
    V1_5,
    V1_6,
    V1_7,
    V2_0,
    Unknown,
}

impl From<&[u8]> for PdfVersion {
    fn from(i: &[u8]) -> Self {
        match i {
            b"1.0" => PdfVersion::V1_0,
            b"1.1" => PdfVersion::V1_1,
            b"1.2" => PdfVersion::V1_2,
            b"1.3" => PdfVersion::V1_3,
            b"1.4" => PdfVersion::V1_4,
            b"1.5" => PdfVersion::V1_5,
            b"1.6" => PdfVersion::V1_6,
            b"1.7" => PdfVersion::V1_7,
            b"2.0" => PdfVersion::V2_0,
            _ => PdfVersion::Unknown,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PdfObject {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(Vec<u8>),
    Name(Vec<u8>),
    Array(Vec<PdfObject>),
    Dictionary(NameMap),
    Stream(NameMap, Vec<u8>),
    IndirectReference { number: u32, generation: u16 },
}

#[derive(Debug)]
pub enum NameMapError {
    KeyNotPdfName,
    OddNumberOfItemsGiven,
}

impl fmt::Display for NameMapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NameMapError::KeyNotPdfName => write!(f, "Give only a PdfObject::Name as the key"),
            NameMapError::OddNumberOfItemsGiven => {
                write!(f, "Give only an even number of items to build a map")
            }
        }
    }
}

impl error::Error for NameMapError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NameMap {
    map: HashMap<Vec<u8>, PdfObject>,
}

impl NameMap {
    pub fn new() -> NameMap {
        NameMap {
            map: HashMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        k: PdfObject,
        v: PdfObject,
    ) -> Result<Option<PdfObject>, NameMapError> {
        match k {
            PdfObject::Name(x) => match self.map.insert(x, v) {
                Some(z) => Ok(Some(z)),
                None => Ok(None),
            },
            _ => Err(NameMapError::KeyNotPdfName),
        }
    }

    pub fn of(values: Vec<PdfObject>) -> Result<Option<NameMap>, NameMapError> {
        let mut map: NameMap = NameMap::new();

        if values.len() % 2 != 0 {
            return Err(NameMapError::OddNumberOfItemsGiven);
        }

        for window in values.chunks(2) {
            match map.insert(window[0].clone(), window[1].clone()) {
                Ok(_) => {}
                Err(x) => return Err(x),
            }
        }

        Ok(Some(map))
    }

    pub fn get(&self, k: PdfObject) -> Result<Option<PdfObject>, NameMapError> {
        match k {
            PdfObject::Name(x) => match self.map.get(&x) {
                Some(ref p) => Ok(Some((*p).clone())),
                None => Ok(None),
            },
            _ => {
                // we treat this as an error now: we could treat it as None-worthy...
                Err(NameMapError::KeyNotPdfName)
            }
        }
    }

    pub fn names(&self) -> Vec<PdfObject> {
        self.map
            .keys()
            .map(|vecu8| PdfObject::Name(vecu8.clone()))
            .collect()
    }
}

pub fn recognize_pdf_version(i: &[u8]) -> IResult<&[u8], PdfVersion> {
    preceded(
        tag(b"%PDF-"),
        map(
            alt((
                tag(b"1.0"),
                tag(b"1.1"),
                tag(b"1.2"),
                tag(b"1.3"),
                tag(b"1.4"),
                tag(b"1.5"),
                tag(b"1.6"),
                tag(b"1.7"),
                tag(b"2.0"),
            )),
            |s: &[u8]| PdfVersion::from(s),
        ),
    )(i)
}

pub fn recognize_pdf_line_end(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((tag(b"\r\n"), tag(b"\n"), tag(b"\r")))(i)
}

/// recognize a PDF ccomment as such, even though we will just discard them.
pub fn recognize_pdf_comment(i: &[u8]) -> IResult<&[u8], &[u8]> {
    preceded(
        tag(b"%"),
        terminated(
            take_till(|b| (b == b'\r' || b == b'\n')),
            recognize_pdf_line_end,
        ),
    )(i)
}

pub fn recognize_pdf_header(i: &[u8]) -> IResult<&[u8], PdfVersion> {
    match tuple((
        recognize_pdf_version,
        recognize_pdf_line_end,
        opt(recognize_pdf_comment),
    ))(i)
    {
        Ok((o, (pdf_version, _, _))) => return Ok((o, pdf_version)),
        Err(x) => return Err(x),
    }
}

pub fn recognize_pdf_null(i: &[u8]) -> IResult<&[u8], PdfObject> {
    map(tag(b"null"), |_| PdfObject::Null)(i)
}

pub fn recognize_pdf_boolean(i: &[u8]) -> IResult<&[u8], PdfObject> {
    alt((
        map(tag(b"true"), |_| PdfObject::Boolean(true)),
        map(tag(b"false"), |_| PdfObject::Boolean(false)),
    ))(i)
}

// because we trust.
fn bytes_to_i64(v: &[u8]) -> i64 {
    FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()
}

pub fn recognize_pdf_integer(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match tuple((
        opt(alt((map(tag(b"+"), |_| 1), map(tag(b"-"), |_| -1)))),
        map(take_while1(is_digit), |b| bytes_to_i64(b)),
    ))(i)
    {
        Ok((rest, (Some(sign), number))) => Ok((rest, PdfObject::Integer(number * sign))),
        Ok((rest, (None, number))) => Ok((rest, PdfObject::Integer(number))),
        Err(err) => Err(err),
    }
}

fn two_byte_slices_of_digits_to_f64(pre_digits: &[u8], post_digits: &[u8]) -> f64 {
    let mut full_number: Vec<u8> = Vec::new();
    full_number.extend_from_slice(pre_digits);
    full_number.extend_from_slice(b".");
    full_number.extend_from_slice(post_digits);
    return bytes_to_f64(&full_number);
}
// because we trust.
fn bytes_to_f64(v: &[u8]) -> f64 {
    FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()
}

/// Get a float per the wierd PDF rules that i dont trust std library functions
/// with for some reason.
pub fn recognize_pdf_float(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match alt((
        tuple((
            opt(alt((map(tag(b"+"), |_| 1f64), map(tag(b"-"), |_| -1f64)))),
            take_while(is_digit),
            tag(b"."),
            take_while1(is_digit),
        )),
        tuple((
            opt(alt((map(tag(b"+"), |_| 1f64), map(tag(b"-"), |_| -1f64)))),
            take_while1(is_digit),
            tag(b"."),
            take_while(is_digit),
        )),
    ))(i)
    {
        Ok((rest, (Some(sign), pre_digits, decimal_point, post_digits))) => Ok((
            rest,
            PdfObject::Float(sign * two_byte_slices_of_digits_to_f64(pre_digits, post_digits)),
        )),
        Ok((rest, (None, pre_digits, decimal_point, post_digits))) => Ok((
            rest,
            PdfObject::Float(two_byte_slices_of_digits_to_f64(pre_digits, post_digits)),
        )),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::AsBytes;

    macro_rules! header_test_success_macro {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, (expected_remainder, expected_value)) = $value;
                    match recognize_pdf_header(input) {
                        Ok((b, version)) => {
                            assert_eq!(expected_value, version);
                            assert_eq!(expected_remainder, b.as_bytes());
                        },
                        x => {
                            println!("x: {:#?}", x);
                            assert_eq!(1, 0);  // better way to do this part?
                        }
                    }
                }
            )*
        }
    }

    macro_rules! header_test_fail_macro {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected_err) = $value;
                    match recognize_pdf_header(input) {
                        Ok(x) => {
                            println!("x: {:#?}", x);
                            assert_eq!(2, 0);  // better way to do this part?
                        },
                        Err(e) => {
                            assert_eq!(expected_err, e);
                        },
                    }
                }
            )*
        }
    }

    header_test_success_macro! {
        h1: (b"%PDF-1.0\r\n", (b"", PdfVersion::V1_0)),
        h2: (b"%PDF-1.0\r\n%yoyoyo1\r\n", (b"", PdfVersion::V1_0)),
        h3: (b"%PDF-1.0\r\n99 0 obj\r\n", (b"99 0 obj\r\n", PdfVersion::V1_0)),
    }
    header_test_fail_macro! {
        hf1: (b"%PDF-3.0\r\n", nom::Err::Error((b"3.0\r\n".as_bytes(), nom::error::ErrorKind::Tag))),
    }

    #[test]
    fn name_map_test() {
        match NameMap::new().get(PdfObject::Name(b"A"[..].to_owned())) {
            Ok(None) => {}
            _ => {
                assert_eq!(3, 0);
            }
        }

        match NameMap::of(vec![
            PdfObject::Name(b"A"[..].to_owned()),
            PdfObject::Name(b"B"[..].to_owned()),
        ]) {
            Ok(Some(n)) => match n.get(PdfObject::Name(b"A"[..].to_owned())) {
                Ok(Some(x)) => {
                    assert_eq!(x, PdfObject::Name(b"B"[..].to_owned()));
                }
                _ => {
                    assert_eq!(4, 0);
                }
            },
            _ => {
                assert_eq!(5, 0);
            }
        }

        match NameMap::of(vec![
            PdfObject::Name(b"A"[..].to_owned()),
            PdfObject::Name(b"B"[..].to_owned()),
            PdfObject::Name(b"A"[..].to_owned()),
            PdfObject::Name(b"C"[..].to_owned()),
        ]) {
            Ok(Some(n)) => {
                match n.get(PdfObject::Name(b"A"[..].to_owned())) {
                    Ok(Some(x)) => {
                        assert_eq!(x, PdfObject::Name(b"C"[..].to_owned()));
                    }
                    _ => {
                        assert_eq!(6, 0);
                    }
                }

                assert_eq!(1, n.names().len());
            }
            _ => {
                assert_eq!(7, 0);
            }
        }

        match NameMap::of(vec![PdfObject::Name(b"A"[..].to_owned())]) {
            Ok(Some(_n)) => {
                assert_eq!(8, 0);
            }
            Err(_x) => {}
            _ => {
                assert_eq!(9, 0);
            }
        }
    }

    #[test]
    fn null_test() {
        assert_eq!(
            Ok((b"\r\n".as_bytes(), PdfObject::Null)),
            recognize_pdf_null(b"null\r\n")
        );
        assert_eq!(
            Err(nom::Err::Error((
                b"nu ll\r\n".as_bytes(),
                nom::error::ErrorKind::Tag
            ))),
            recognize_pdf_null(b"nu ll\r\n")
        );
        assert_eq!(
            Err(nom::Err::Error((
                b"nul\r\n".as_bytes(),
                nom::error::ErrorKind::Tag
            ))),
            recognize_pdf_null(b"nul\r\n")
        );
    }

    #[test]
    fn boolean_test() {
        assert_eq!(
            Ok((b"".as_bytes(), PdfObject::Boolean(true))),
            recognize_pdf_boolean(b"true")
        );
        assert_eq!(
            Ok((b"f".as_bytes(), PdfObject::Boolean(true))),
            recognize_pdf_boolean(b"truef")
        );
        assert_eq!(
            Ok((b"".as_bytes(), PdfObject::Boolean(false))),
            recognize_pdf_boolean(b"false")
        );
    }

    #[test]
    fn integers_test() {
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(123)),
            recognize_pdf_integer(b"123").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(-123)),
            recognize_pdf_integer(b"-123").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(0)),
            recognize_pdf_integer(b"0").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(0)),
            recognize_pdf_integer(b"-0").unwrap()
        ); // heh
    }

    macro_rules! float_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                match recognize_pdf_float(input) {
                    Ok((_b, PdfObject::Float(v))) => {
                        assert_relative_eq!(expected, v);
                    },
                    _ => {
                        assert_eq!(5, 0);
                    }
                }
            }
        )*
        }
    }

    float_test! {
        f1: (b"123.0",123.0),
        f2: (b"34.5",34.5),
        f3: (b"-3.62",-3.62),
        f4: (b"+123.6",123.6),
        f5: (b"4.",4.0),
        f6: (b"-.002",-0.002),
        f7: (b"0.0",0.0),
        f8: (b"-0.0",0.0),
        f9: (b"-.0",0.0),
        f10: (b"-0.",0.0),
    }
}
