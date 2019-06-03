use nom::{
    branch::alt, bytes::complete::*, character::*, combinator::*, error::*, sequence::*,
    multi::*,
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

#[inline]
pub fn is_pdf_whitespace(chr: u8) -> bool {
    (chr == 0x00 || chr == 0x09 || chr == 0x0A || chr == 0x0C || chr == 0x0D || chr == 0x20)
}

#[inline]
fn can_be_in_hexadecimal_string(chr: u8) -> bool {
    is_hex_digit(chr) || is_pdf_whitespace(chr)
}

#[inline]
pub fn from_hex(chr: u8) -> u8 {
    // heh
    if chr >= 0x30 && chr <= 0x39 {
        chr - 0x30
    } else if chr >= 0x41 && chr <= 0x46 {
        chr - 0x37
    } else {
        chr - 0x57
    }
}

// because we trust
fn vec_of_bytes_from_hex_string_literal(input: &[u8]) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input
        .iter()
        .filter(|&x| is_hex_digit(*x))
        .map(|&x| from_hex(x))
        .collect();

    for pair in filtered.chunks(2) {
        if pair.len() == 2 {
            result.push((pair[0] << 4) + pair[1]);
        } else if pair.len() == 1 {
            result.push(pair[0] << 4);
        }
    }

    result
}

pub fn recognize_pdf_hexidecimal_string(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        tag(b"<"),
        terminated(
            map(take_while(can_be_in_hexadecimal_string), |v| vec_of_bytes_from_hex_string_literal(v)),
            tag(b">")
        )
    )(i) {
        Ok((rest, v)) => { Ok((rest, PdfObject::String(v)))},
        Err(err) => { Err(err) },
    }
}

/// only those digits that can be the high order
/// third digit of a three-digit octal numeral
/// representing a value from 0 to 255 inclusive.
#[inline]
fn is_oct_high_digit(chr: u8) -> bool {
    chr >= 0x30 && chr <= 0x33
}

fn from_octal(i: u8) -> u8 {
    ( i - 0x30u8 )
}

fn three_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    match tuple((
        map(take_while_m_n(1usize, 1usize, is_oct_high_digit), |o: &[u8]| from_octal(o[0]) * 64),
        map(take_while_m_n(2usize, 2usize, is_oct_digit), |o: &[u8]| (from_octal(o[0]) * 8) + from_octal(o[1]))
    ))(i) {
        Ok((rest, (hi, lo))) => { Ok((rest, hi + lo))},
        Err(err) => { Err(err) },
    }
}

fn two_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    map(take_while_m_n(2usize, 2usize, is_oct_digit), |o: &[u8]| (from_octal(o[0]) * 8) + from_octal(o[1]))(i)
}

fn one_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    map(take_while_m_n(1usize, 1usize, is_oct_digit), |o: &[u8]| from_octal(o[0]))(i)
}

fn recognize_octal_value_from_string_literal(i: &[u8]) -> IResult<&[u8], u8> {
    println!("here 8 with {:#?}", i);
    alt((
        three_digit_octal,
        two_digit_octal,
        one_digit_octal
        ))(i)
}

fn recognize_valid_escapes_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 7 with {:#?}", i);
    match tuple((
        tag(b"\\"),
        alt((
            recognize_octal_value_from_string_literal,
            map(tag(b"n"), |_| 0x0au8),
            map(tag(b"r"), |_| 0x0du8),
            map(tag(b"t"), |_| 0x09u8),
            map(tag(b"b"), |_| 0x08u8),
            map(tag(b"f"), |_| 0x0cu8),
            map(tag(b"("), |_| 0x28u8),
            map(tag(b")"), |_| 0x29u8),
            map(tag(b"\\"), |_| 0x5cu8),
            ))
        ))(i) {
        Ok((rest, (bs, bv))) => { Ok((rest, vec![bv])) },
        Err(err) => { Err(err) },
    }
}

fn recognize_elidable_line_ending_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 6 with {:#?}", i);
    match tuple((
        tag(b"\\"),
        recognize_pdf_line_end,
    ))(i) {
        Ok((rest, (_,_))) => { Ok((rest, vec![])) },
        Err(err) => { Err(err) },
    }
}

fn recognize_line_ending_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 5 with {:#?}", i);
    match recognize_pdf_line_end(i) {
        Ok((rest, _)) => { Ok((rest, vec![0x0au8]))},
        Err(err) => { Err(err) },
    }
}

fn is_possible_in_string_literal(chr: u8) -> bool {
    chr != b'\\' && chr != b'(' && chr != b')'
}
fn recognize_bytes_possible_in_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 4 with {:#?}", i);
    map(take_while(is_possible_in_string_literal), |v: &[u8]| v.to_vec())(i)
}

fn recognize_empty_parens(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 3 with {:#?}", i);
    match tag("()")(i) {
        Ok((rest, empty_parens)) => {
            return Ok((rest, empty_parens.to_vec()))
        },
        Err(err) => { Err(err) },
    }
}

fn recognize_recursive_balanced_parenthetical_in_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 2 with {:#?}", i);
    match preceded(tag(b"("),
            terminated(recognize_string_literal_body, tag(b")")))(i) {
        Ok((rest, body)) => {
            let mut result_vec: Vec<u8> = Vec::new();
            result_vec.extend_from_slice(b"(");
            result_vec.extend_from_slice(&body);
            result_vec.extend_from_slice(b")");
            Ok((rest, result_vec))
        },
        Err(err) => { Err(err) },
    }
}

fn recognize_string_literal_body(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    println!("here 1 with {:#?}", i);
    fold_many0(
        alt((
            recognize_valid_escapes_from_string_literal,
            recognize_elidable_line_ending_from_string_literal,
            recognize_line_ending_from_string_literal,
            recognize_empty_parens,
            recognize_recursive_balanced_parenthetical_in_string_literal,
            recognize_bytes_possible_in_string_literal,
            )),
        Vec::new(),
        |mut acc: Vec<u8>, item| {
            acc.extend_from_slice(&item);
            acc
        }
    )(i)
}

pub fn recognize_pdf_literal_string(i: &[u8]) -> IResult<&[u8], PdfObject> {
    println!("here 0 with {:#?}", i);
    match preceded(
        tag(b"("),
        terminated(recognize_string_literal_body, tag(b")"))
    )(i) {
        Ok((rest, v)) => { Ok((rest, PdfObject::String(v))) },
        Err(err) => { Err(err) },
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
                        assert_eq!(10, 0);
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


    #[test]
    fn hexadecimal_string_test() {
        assert_eq!(
            Err(nom::Err::Error((b"->".as_bytes(), nom::error::ErrorKind::Tag))),
            recognize_pdf_hexidecimal_string(b"<a->")
        );
    }
    macro_rules! hexst {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_hexidecimal_string(input).unwrap() {
                        (_b, PdfObject::String(v)) => {
                            assert_eq!( v, expected );
                        },
                        _ => {
                            assert_eq!(11,0);
                        }
                    }
                }
            )*
        }
    }
    hexst! {
        hx1: (b"<abcdef0123456789>", vec![0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89]),
        hx2: (b"<abc def0123\n456789 >", vec![0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89]),
        hx3: (b"<ab>", vec![0xab]),
        hx3a: (b"<a\rb>", vec![0xab]),
        hx4: (b"<a>", vec![0xa0]),
        hx5: (b"<ab cd\nef1>", vec![0xab, 0xcd, 0xef, 0x10]),
    }

//    #[test]
//    fn octal_inside_string_literal_test() {
//        assert_eq!(Ok((b"9".as_bytes(), 32u8)), recognize_octal_value_from_string_literal(b"409"));
//        assert_eq!(Ok((b"4".as_bytes(), 36u8)), recognize_octal_value_from_string_literal(b"444"));
//        assert_eq!(Ok((b"8".as_bytes(), 1u8)), recognize_octal_value_from_string_literal(b"18"));
//        assert_eq!(Err(nom::Err::Error((b"999".as_bytes(), nom::error::ErrorKind::TakeWhileMN))), recognize_octal_value_from_string_literal(b"999"));
//    }



    macro_rules! tlsr {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_literal_string(input.as_bytes()) {
                        Ok((_b, PdfObject::String(v))) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },

                        Err(err) => {
                            println!("debug: {:#?}", err);
                            assert_eq!(13,0);
                        }
                        _ => {
                            assert_eq!(14,0);
                        }
                    }
                }
            )*
        }
    }

    tlsr! {
//        tlsr_00: ( b"(hiya)", b"hiya"),
//        tlsr_0: (b"(abcd)", b"abcd"),
//        tlsr_1: (b"(\\247)", b"\xA7"),
//
//        tlsr_2: (b"(a)", b"a"),
//        tlsr_3: (b"(This is a string)", b"This is a string"),
//        tlsr_4: (b"(Strings can contain newlines\nand such.)", b"Strings can contain newlines\nand such."),
//        tlsr_5: (b"(Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .)",
//                    b"Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) ."),
//        tlsr_6: (b"(The following is an empty string .)", b"The following is an empty string ."),
        tlsr_7: (b"()", b""),
//        tlsr_8: (b"(It has zero (0) length.)", b"It has zero (0) length."),
//
//        tlsr_9: (b"(These \\\rtwo strings \\\nare the same.)", b"These two strings are the same."),
//        tlsr_a: (b"(These two strings are the same.)", b"These two strings are the same."),
//
//        tlsr_b: (b"(This string has an end-of-line at the end of it.\n)", b"This string has an end-of-line at the end of it.\n"),
//        tlsr_c: (b"(So does this one.\\n)", b"So does this one.\n"),
//        tlsr_d: (b"(This string contains \\245two octal characters\\307.)", b"This string contains \xA5two octal characters\xC7."),
//        tlsr_e: (b"(\\0053)", b"\x053"),
//        tlsr_f: (b"(\\053)", b"+"),
//        tlsr_g: (b"(\\53)", b"+"),
//        tlsr_h: (b"(\\533)", b"+3"),
    }
}
