use nom::{
    branch::alt, bytes::complete::*, character::complete::digit1, character::*, combinator::*,
    error::context, multi::*, sequence::*, AsBytes, IResult,
};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::convert::TryFrom;
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

impl fmt::Display for PdfVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PdfVersion::Unknown => write!(f, "PDF Version Unknown"),
            PdfVersion::V1_0 => write!(f, "PDF Version 1.0"),
            PdfVersion::V1_1 => write!(f, "PDF Version 1.1"),
            PdfVersion::V1_2 => write!(f, "PDF Version 1.2"),
            PdfVersion::V1_3 => write!(f, "PDF Version 1.3"),
            PdfVersion::V1_4 => write!(f, "PDF Version 1.4"),
            PdfVersion::V1_5 => write!(f, "PDF Version 1.5"),
            PdfVersion::V1_6 => write!(f, "PDF Version 1.6"),
            PdfVersion::V1_7 => write!(f, "PDF Version 1.7"),
            PdfVersion::V2_0 => write!(f, "PDF Version 2.0"),
        }
    }
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
    Comment,
    Array(Vec<PdfObject>),
    Dictionary(NameMap),
    Stream(NameMap, Vec<u8>),
    IndirectReference { number: u32, generation: u16 },
}

impl fmt::Display for PdfObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            PdfObject::Null => write!(f, "PdfObject::Null"),
            PdfObject::Boolean(ref v) => write!(f, "PdfObject::Boolean({})", v),
            PdfObject::Integer(ref v) => write!(f, "PdfObject::Integer({})", v),
            PdfObject::Float(ref v) => write!(f, "PdfObject::Float({})", v),
            PdfObject::Comment => write!(f, "PdfObject::Comment"),
            PdfObject::String(ref v) => {
                // if we knew the encoding, we could use it, but alas
                write!(f, "PdfObject::String({:?})", &*v)
            }
            PdfObject::Name(ref v) => {
                // here we are directed to use utf-8 (end of § 7.3.5 and
                // Note 4 thereof)
                write!(
                    f,
                    "PdfObject::Name(/{})",
                    str::from_utf8(&*v).unwrap_or("not-utf-8")
                )
            }
            PdfObject::Array(ref v) => {
                writeln!(f, "PdfObject::Array[")?;
                for obj in v {
                    write!(f, "\t")?;
                    obj.fmt(f)?;
                    writeln!(f)?;
                }
                write!(f, "]")
            }
            PdfObject::Dictionary(ref nkm) => {
                writeln!(f, "PdfObject::Dictionary<<")?;
                for name in nkm.names() {
                    write!(f, "\t")?;
                    name.fmt(f)?;
                    write!(f, "\n\t\t")?;
                    nkm.get(name).unwrap().fmt(f)?;
                    writeln!(f)?;
                }
                write!(f, ">>")
            }
            PdfObject::Stream(ref nkm, ref _strm) => {
                writeln!(f, "PdfObject::Stream<<")?;
                for name in nkm.names() {
                    write!(f, "\t")?;
                    name.fmt(f)?;
                    write!(f, "\n\t\t")?;
                    nkm.get(name).unwrap().fmt(f)?;
                    writeln!(f)?;
                }
                write!(f, ">>")
            }
            PdfObject::IndirectReference {
                number: n,
                generation: g,
            } => write!(f, "PdfObject::IndirectReference({} {} R)", n, g),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PdfIndirectObject {
    pub number: u32,
    pub generation: u16,
    pub obj: PdfObject,
}

impl fmt::Display for PdfIndirectObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "PdfIndirectObject {} {}", self.number, self.generation)?;
        self.obj.fmt(f)
    }
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

#[derive(Debug, Clone, PartialEq, Default)]
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

    pub fn of(values: Vec<PdfObject>) -> Result<NameMap, NameMapError> {
        let mut map: NameMap = NameMap::new();

        if values.len() % 2 != 0 {
            return Err(NameMapError::OddNumberOfItemsGiven);
        }

        for chunk in values.chunks(2) {
            match map.insert(chunk[0].clone(), chunk[1].clone()) {
                Ok(_) => {}
                Err(x) => return Err(x),
            }
        }

        Ok(map)
    }

    pub fn get(&self, k: PdfObject) -> Option<PdfObject> {
        match k {
            PdfObject::Name(x) => match self.map.get(&x) {
                Some(ref p) => Some((*p).clone()),
                None => None,
            },
            _ => None,
        }
    }

    pub fn get2(&self, k: &[u8]) -> Option<PdfObject> {
        self.get(PdfObject::Name(k.to_owned()))
    }

    pub fn names(&self) -> Vec<PdfObject> {
        self.map
            .keys()
            .map(|vecu8| PdfObject::Name(vecu8.clone()))
            .collect()
    }

    pub fn contains_key(&self, k: PdfObject) -> bool {
        match k {
            PdfObject::Name(x) => self.map.contains_key(&x),
            _ => false,
        }
    }
    pub fn contains_key2(&self, k: &[u8]) -> bool {
        self.map.contains_key(k)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum XrefTableEntry2 {
    Free {
        number: u32,
        generation: u16,
    },
    Uncompressed {
        number: u32,
        generation: u16,
        offset: u64,
    },
    InStream {
        number: u32,
        in_object_number: u32,
        index_in_stream: u32,
    },
}

impl XrefTableEntry2 {
    pub fn get_number(&self) -> u32 {
        match *self {
            XrefTableEntry2::Free {
                number,
                generation: _generation,
            } => number,
            XrefTableEntry2::Uncompressed {
                number,
                generation: _generation,
                offset: _offset,
            } => number,
            XrefTableEntry2::InStream {
                number,
                in_object_number: _in_object_number,
                index_in_stream: _index_in_stream,
            } => number,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct XrefTable2 {
    free_objects: BTreeSet<u32>,
    free_object_generations: BTreeMap<u32, u16>,
    uncompressed_objects: BTreeSet<u32>,
    uncompressed_object_generations: BTreeMap<u32, u16>,
    uncompressed_object_offsets: BTreeMap<u32, u64>,
    in_stream_objects: BTreeSet<u32>,
    in_stream_objects_in_object: BTreeMap<u32, u32>,
    in_stream_objects_indexes: BTreeMap<u32, u32>,

    present: BTreeSet<u32>,
}

impl XrefTable2 {
    pub fn new() -> XrefTable2 {
        XrefTable2 {
            free_objects: BTreeSet::new(),
            free_object_generations: BTreeMap::new(),
            uncompressed_objects: BTreeSet::new(),
            uncompressed_object_generations: BTreeMap::new(),
            uncompressed_object_offsets: BTreeMap::new(),
            in_stream_objects: BTreeSet::new(),
            in_stream_objects_in_object: BTreeMap::new(),
            in_stream_objects_indexes: BTreeMap::new(),

            present: BTreeSet::new(),
        }
    }

    pub fn add(&mut self, xref_entry: XrefTableEntry2) -> bool {
        if self.present.contains(&xref_entry.get_number()) {
            return false;
        }
        self.present.insert(xref_entry.get_number());

        match xref_entry {
            XrefTableEntry2::Free { number, generation } => {
                self.free_objects.insert(number);
                self.free_object_generations.insert(number, generation);
            }
            XrefTableEntry2::Uncompressed {
                number,
                generation,
                offset,
            } => {
                self.uncompressed_objects.insert(number);
                self.uncompressed_object_generations
                    .insert(number, generation);
                self.uncompressed_object_offsets.insert(number, offset);
            }
            XrefTableEntry2::InStream {
                number,
                in_object_number,
                index_in_stream,
            } => {
                self.in_stream_objects.insert(number);
                self.in_stream_objects_in_object
                    .insert(number, in_object_number);
                self.in_stream_objects_indexes
                    .insert(number, index_in_stream);
            }
        }

        true
    }

    pub fn get(&self, obj_number: u32) -> Option<XrefTableEntry2> {
        if !self.present.contains(&obj_number) {
            return None;
        }

        if self.free_objects.contains(&obj_number) {
            return Some(XrefTableEntry2::Free {
                number: obj_number,
                generation: *self.free_object_generations.get(&obj_number).unwrap(),
            });
        }

        if self.uncompressed_objects.contains(&obj_number) {
            return Some(XrefTableEntry2::Uncompressed {
                number: obj_number,
                generation: *self
                    .uncompressed_object_generations
                    .get(&obj_number)
                    .unwrap(),
                offset: *self.uncompressed_object_offsets.get(&obj_number).unwrap(),
            });
        }

        if self.in_stream_objects.contains(&obj_number) {
            return Some(XrefTableEntry2::InStream {
                number: obj_number,
                in_object_number: *self.in_stream_objects_in_object.get(&obj_number).unwrap(),
                index_in_stream: *self.in_stream_objects_indexes.get(&obj_number).unwrap(),
            });
        }

        None
    }

    pub fn all_numbers(&self) -> Vec<u32> {
        self.present.iter().cloned().collect::<Vec<u32>>()
    }

    pub fn all_entries(&self) -> Vec<XrefTableEntry2> {
        let all_nums = self.all_numbers();
        let mut all_ents: Vec<XrefTableEntry2> = Vec::new();
        for obj_num in all_nums.iter() {
            let option = self.get(*obj_num);
            all_ents.push(option.unwrap())
        }
        all_ents
    }

    pub fn free(&self) -> Vec<u32> {
        self.free_objects.iter().cloned().collect::<Vec<u32>>()
    }

    pub fn in_use(&self) -> Vec<u32> {
        let mut tmp: Vec<u32> = self
            .uncompressed_objects
            .iter()
            .cloned()
            .collect::<Vec<u32>>();
        tmp.extend(self.in_stream_objects.iter());
        tmp
    }

    pub fn offset_of(&self, obj_number: u32) -> Option<u64> {
        if self.present.contains(&obj_number) && self.uncompressed_objects.contains(&obj_number) {
            return Some(*self.uncompressed_object_offsets.get(&obj_number).unwrap());
        }
        None
    }

    pub fn generation_of(&self, obj_number: u32) -> Option<u16> {
        if self.present.contains(&obj_number) && self.uncompressed_objects.contains(&obj_number) {
            return Some(
                *self
                    .uncompressed_object_generations
                    .get(&obj_number)
                    .unwrap(),
            );
        } else if self.present.contains(&obj_number) && self.free_objects.contains(&obj_number) {
            return Some(*self.free_object_generations.get(&obj_number).unwrap());
        } else if self.present.contains(&obj_number) && self.in_stream_objects.contains(&obj_number)
        {
            return Some(0);
        }
        None
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
            PdfVersion::from,
        ),
    )(i)
}

pub fn recognize_pdf_line_end(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((tag(b"\r\n"), tag(b"\n"), tag(b"\r")))(i)
}

/// recognize a PDF ccomment as such, even though we will just discard them.,
/// so that we have a PDF object to consume and return along with other
/// PDF objects from different combinators.
pub fn recognize_pdf_comment(i: &[u8]) -> IResult<&[u8], PdfObject> {
    map(
        preceded(
            pdf_whitespace,
            preceded(
                tag(b"%"),
                terminated(
                    take_till(|b| (b == b'\r' || b == b'\n')),
                    recognize_pdf_line_end,
                ),
            ),
        ),
        |_| PdfObject::Comment,
    )(i)
}

pub fn recognize_pdf_header(i: &[u8]) -> IResult<&[u8], PdfVersion> {
    match preceded(
        pdf_whitespace,
        tuple((
            recognize_pdf_version,
            recognize_pdf_line_end,
            opt(recognize_pdf_comment),
        )),
    )(i)
    {
        Ok((o, (pdf_version, _, _))) => Ok((o, pdf_version)),
        Err(x) => Err(x),
    }
}

pub fn recognize_pdf_null(i: &[u8]) -> IResult<&[u8], PdfObject> {
    preceded(pdf_whitespace, map(tag(b"null"), |_| PdfObject::Null))(i)
}

pub fn recognize_pdf_boolean(i: &[u8]) -> IResult<&[u8], PdfObject> {
    preceded(
        pdf_whitespace,
        alt((
            map(tag(b"true"), |_| PdfObject::Boolean(true)),
            map(tag(b"false"), |_| PdfObject::Boolean(false)),
        )),
    )(i)
}

// because we trust.  this irks me.
fn bytes_to_i64(v: &[u8]) -> i64 {
    FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()
}

// because we trust.  this irks me.
fn bytes_to_f64(v: &[u8]) -> f64 {
    FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()
}

macro_rules! digits_to {
    ($T:ident, $name:ident) => {
        pub fn $name(i: &[u8]) -> IResult<&[u8], $T> {
            match digit1(i) {
                Ok((rest, digits)) => {
                    let parsed_number = bytes_to_i64(digits);
                    match $T::try_from(parsed_number) {
                        Ok(v) => Ok((rest, v)),
                        Err(_x) => Err(nom::Err::Failure((i, nom::error::ErrorKind::TooLarge))),
                    }
                }
                Err(err) => Err(err),
            }
        }
    };
}

digits_to!(u8, digits_to_u8);
digits_to!(u16, digits_to_u16);
digits_to!(u32, digits_to_u32);
digits_to!(u64, digits_to_u64);
digits_to!(usize, digits_to_usize);

macro_rules! not_zero_padded_digits_to {
    ($T:ident, $name:ident) => {
        pub fn $name(i: &[u8]) -> IResult<&[u8], $T> {
            match recognize_digits_not_beginning_with_zero(i) {
                Ok((rest, digits)) => {
                    let parsed_number = bytes_to_i64(digits);
                    match $T::try_from(parsed_number) {
                        Ok(v) => Ok((rest, v)),
                        Err(_x) => Err(nom::Err::Failure((i, nom::error::ErrorKind::TooLarge))),
                    }
                }
                Err(err) => Err(err),
            }
        }
    };
}

not_zero_padded_digits_to!(u8, not_zero_padded_digits_to_u8);
not_zero_padded_digits_to!(u16, not_zero_padded_digits_to_u16);
not_zero_padded_digits_to!(u32, not_zero_padded_digits_to_u32);
not_zero_padded_digits_to!(u64, not_zero_padded_digits_to_u64);
not_zero_padded_digits_to!(usize, not_zero_padded_digits_to_usize);

pub fn recognize_pdf_integer(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        pdf_whitespace,
        tuple((
            opt(alt((map(tag(b"+"), |_| 1), map(tag(b"-"), |_| -1)))),
            map(take_while1(is_digit), bytes_to_i64),
        )),
    )(i)
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
    bytes_to_f64(&full_number)
}

/// Get a float per the wierd PDF rules that i dont trust std library functions
/// with for some reason.
pub fn recognize_pdf_float(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        pdf_whitespace,
        alt((
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
        )),
    )(i)
    {
        Ok((rest, (Some(sign), pre_digits, _decimal_point, post_digits))) => Ok((
            rest,
            PdfObject::Float(sign * two_byte_slices_of_digits_to_f64(pre_digits, post_digits)),
        )),
        Ok((rest, (None, pre_digits, _decimal_point, post_digits))) => Ok((
            rest,
            PdfObject::Float(two_byte_slices_of_digits_to_f64(pre_digits, post_digits)),
        )),
        Err(err) => Err(err),
    }
}

#[inline]
fn is_pdf_whitespace(chr: u8) -> bool {
    (chr == 0x00 || chr == 0x09 || chr == 0x0A || chr == 0x0C || chr == 0x0D || chr == 0x20)
}

fn pdf_whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(is_pdf_whitespace)(i)
}

#[inline]
fn is_pdf_delimiter(chr: u8) -> bool {
    (chr == 0x28
        || chr == 0x29
        || chr == 0x3c
        || chr == 0x3e
        || chr == 0x5b
        || chr == 0x5d
        || chr == 0x7b
        || chr == 0x7d
        || chr == 0x2f
        || chr == 0x25)
}

#[inline]
fn can_be_in_hexadecimal_string(chr: u8) -> bool {
    is_hex_digit(chr) || is_pdf_whitespace(chr)
}

#[inline]
fn hex_digit_to_byte_value(chr: u8) -> u8 {
    // heh
    if chr >= 0x30 && chr <= 0x39 {
        chr - 0x30
    } else if chr >= 0x41 && chr <= 0x46 {
        chr - 0x37
    } else {
        chr - 0x57
    }
}

// because we trust.  this irks me.
fn vec_of_bytes_from_hex_string_literal(input: &[u8]) -> Option<Vec<u8>> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input
        .iter()
        .filter(|&x| is_hex_digit(*x))
        .map(|&x| hex_digit_to_byte_value(x))
        .collect();

    for pair in filtered.chunks(2) {
        if pair.len() == 2 {
            result.push((pair[0] << 4) + pair[1]);
        } else if pair.len() == 1 {
            result.push(pair[0] << 4);
        }
    }

    Some(result)
}

pub fn recognize_asciihex_hexadecimal_string(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match preceded(
        pdf_whitespace,
        terminated(
            map(take_while(can_be_in_hexadecimal_string), |v| {
                vec_of_bytes_from_hex_string_literal(v)
            }),
            tag(b">"),
        ),
    )(i)
    {
        Ok((_rest, Some(v))) => Ok((_rest, v)),
        Ok((rest, None)) => Err(nom::Err::Error((rest, nom::error::ErrorKind::TooLarge))),
        Err(err) => Err(err),
    }
}

pub fn recognize_pdf_hexadecimal_string(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        pdf_whitespace,
        preceded(
            tag(b"<"),
            terminated(
                map(take_while(can_be_in_hexadecimal_string), |v| {
                    vec_of_bytes_from_hex_string_literal(v)
                }),
                tag(b">"),
            ),
        ),
    )(i)
    {
        Ok((rest, Some(v))) => Ok((rest, PdfObject::String(v))),
        Ok((rest, None)) => Err(nom::Err::Error((rest, nom::error::ErrorKind::TooLarge))),
        Err(err) => Err(err),
    }
}

#[inline]
fn can_be_in_ascii85_string(chr: u8) -> bool {
    is_ascii85_digit(chr) || is_pdf_whitespace(chr)
}

#[inline]
fn is_ascii85_digit(chr: u8) -> bool {
    (chr >= 0x21 && chr <= 0x75) || chr == 0x7A
}

fn vec_of_bytes_from_ascii85_string_literal(input: &[u8]) -> Option<Vec<u8>> {
    let c_85_4: u64 = 85 * 85 * 85 * 85;
    let c_85_3: u64 = 85 * 85 * 85;
    let c_85_2: u64 = 85 * 85;
    let c_85_1: u64 = 85;

    let addend: u8 = 0x21;
    let padding: u64 = u64::from(0x75u8 - addend);

    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input
        .iter()
        .filter(|&x| is_ascii85_digit(*x))
        .cloned()
        .collect();

    // it would be nice to have a for-loop over 5-byte chunks here, but the z
    // case, where it is a single byte, not 5, that makes 4 output bytes,
    // makes that hard.
    let mut pos = 0;
    let length = filtered.len();

    while pos < length {
        if filtered[pos] == 0x7A {
            // z special case
            result.push(0x0);
            result.push(0x0);
            result.push(0x0);
            result.push(0x0);
            pos += 1;
            continue;
        }

        let bytes_left = if (length - pos) > 5 { 5 } else { length - pos };
        if bytes_left < 2 {
            // ugh
            return None;
        }

        let mut bytes_needed = 4;
        let mut v: u64 = (u64::from(filtered[pos] - addend) * c_85_4)
            + (u64::from(filtered[pos + 1] - addend) * c_85_3);

        match bytes_left {
            2 => {
                bytes_needed = 1;
                v += (padding * c_85_2) + (padding * c_85_1) + padding;
            }
            3 => {
                bytes_needed = 2;
                v +=
                    (u64::from(filtered[pos + 2] - addend) * c_85_2) + (padding * c_85_1) + padding;
            }
            4 => {
                bytes_needed = 3;
                v += (u64::from(filtered[pos + 2] - addend) * c_85_2)
                    + (u64::from(filtered[pos + 3] - addend) as u64 * c_85_1)
                    + padding;
            }
            5 => {
                v += (u64::from(filtered[pos + 2] - addend) * c_85_2)
                    + (u64::from(filtered[pos + 3] - addend) * c_85_1)
                    + u64::from(filtered[pos + 4] - addend);
            }
            _ => {}
        }

        let mut shift: u64 = 32;
        for _ in 0..bytes_needed {
            shift -= 8;
            result.push((v >> shift) as u8);
        }

        pos += bytes_needed + 1;
    }

    Some(result)
}

pub fn recognize_ascii85_string(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match preceded(
        pdf_whitespace,
        terminated(
            map(take_while(can_be_in_ascii85_string), |v| {
                vec_of_bytes_from_ascii85_string_literal(v)
            }),
            tag(b"~>"),
        ),
    )(i)
    {
        Ok((rest, Some(v))) => Ok((rest, v)),
        Ok((rest, None)) => Err(nom::Err::Error((rest, nom::error::ErrorKind::TooLarge))),
        Err(err) => Err(err),
    }
}

fn below_128(chr: u8) -> bool {
    chr < 0x80
}

fn above_128(chr: u8) -> bool {
    chr > 0x80
}

fn rel_length_below_128(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let taken = map(take_while_m_n(1usize, 1usize, below_128), |v: &[u8]| v[0])(i)?;
    let (rest, length) = taken;
    match map(take((length + 1) as usize), |slice: &[u8]| slice.to_vec())(rest) {
        Ok((rest2, v)) => Ok((rest2, v)),
        Err(err) => Err(err),
    }
}

fn rel_length_above_128(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let taken = map(take_while_m_n(1usize, 1usize, above_128), |v: &[u8]| v[0])(i)?;
    if let (rest, length) = taken {
        match map(take(1usize), |v: &[u8]| {
            vec![v[0]; ((257 as u16) - u16::from(length)) as usize]
        })(rest)
        {
            Ok((rest2, v)) => {
                return Ok((rest2, v));
            }
            Err(err) => {
                return Err(err);
            }
        }
    } else {
        return Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge)));
    }
}

pub fn recognize_rle_sequence(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    terminated(
        fold_many0(
            alt((
                complete(rel_length_below_128),
                complete(rel_length_above_128),
            )),
            Vec::new(),
            |mut accum: Vec<u8>, item: Vec<u8>| {
                accum.extend(&item);
                accum
            },
        ),
        tag(b"\x80"),
    )(i)
}

/// only those digits that can be the high order
/// third digit of a three-digit octal numeral
/// representing a value from 0 to 255 inclusive.
#[inline]
fn is_oct_high_digit(chr: u8) -> bool {
    chr >= 0x30 && chr <= 0x33
}

fn from_octal(i: u8) -> u8 {
    (i - 0x30u8)
}

fn three_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    match tuple((
        map(
            take_while_m_n(1usize, 1usize, is_oct_high_digit),
            |o: &[u8]| from_octal(o[0]) * 64,
        ),
        map(
            take_while_m_n(2usize, 2usize, is_oct_digit),
            |o: &[u8]| (from_octal(o[0]) * 8) + from_octal(o[1]),
        ),
    ))(i)
    {
        Ok((rest, (hi, lo))) => Ok((rest, hi + lo)),
        Err(err) => Err(err),
    }
}

fn two_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    map(
        take_while_m_n(2usize, 2usize, is_oct_digit),
        |o: &[u8]| (from_octal(o[0]) * 8) + from_octal(o[1]),
    )(i)
}

fn one_digit_octal(i: &[u8]) -> IResult<&[u8], u8> {
    map(
        take_while_m_n(1usize, 1usize, is_oct_digit),
        |o: &[u8]| from_octal(o[0]),
    )(i)
}

fn recognize_octal_value_from_string_literal(i: &[u8]) -> IResult<&[u8], u8> {
    alt((three_digit_octal, two_digit_octal, one_digit_octal))(i)
}

fn recognize_valid_escapes_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
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
        )),
    ))(i)
    {
        Ok((rest, (_bs, bv))) => Ok((rest, vec![bv])),
        Err(err) => Err(err),
    }
}

fn recognize_elidable_line_ending_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match tuple((tag(b"\\"), recognize_pdf_line_end))(i) {
        Ok((rest, (_, _))) => Ok((rest, vec![])),
        Err(err) => Err(err),
    }
}

fn recognize_line_ending_from_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match recognize_pdf_line_end(i) {
        Ok((rest, _)) => Ok((rest, vec![0x0au8])),
        Err(err) => Err(err),
    }
}

fn is_possible_in_string_literal(chr: u8) -> bool {
    chr != b'\\' && chr != b'(' && chr != b')'
}

fn recognize_bytes_possible_in_string_literal(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take_while1(is_possible_in_string_literal), |v: &[u8]| {
        v.to_vec()
    })(i)
}

fn recognize_empty_parens(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match tag("()")(i) {
        Ok((rest, empty_parens)) => Ok((rest, empty_parens.to_vec())),
        Err(err) => Err(err),
    }
}

fn recognize_recursive_balanced_parenthetical_in_string_literal(
    i: &[u8],
) -> IResult<&[u8], Vec<u8>> {
    match preceded(
        tag(b"("),
        terminated(recognize_string_literal_body, tag(b")")),
    )(i)
    {
        Ok((rest, body)) => {
            let mut result_vec: Vec<u8> = Vec::new();
            result_vec.extend_from_slice(b"(");
            result_vec.extend_from_slice(&body);
            result_vec.extend_from_slice(b")");
            Ok((rest, result_vec))
        }
        Err(err) => Err(err),
    }
}

fn recognize_string_literal_body(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
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
        |mut acc: Vec<u8>, item: Vec<u8>| {
            acc.extend(item);
            acc
        },
    )(i)
}

pub fn recognize_pdf_literal_string(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        pdf_whitespace,
        preceded(
            tag(b"("),
            terminated(recognize_string_literal_body, tag(b")")),
        ),
    )(i)
    {
        Ok((rest, v)) => Ok((rest, PdfObject::String(v))),
        Err(err) => Err(err),
    }
}

fn recognize_name_hex_encoded_byte(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match map(
        preceded(tag(b"#"), take_while_m_n(2usize, 2usize, is_hex_digit)),
        |x: &[u8]| vec_of_bytes_from_hex_string_literal(x),
    )(i)
    {
        Ok((rest, Some(v))) => Ok((rest, v)),
        Ok((rest, None)) => Err(nom::Err::Error((rest, nom::error::ErrorKind::TooLarge))),
        Err(err) => Err(err),
    }
}

fn is_possible_in_name_unencoded(chr: u8) -> bool {
    chr != b'#' && !is_pdf_whitespace(chr) && !is_pdf_delimiter(chr) && (chr >= b'!' && chr <= b'~')
}

fn recognize_name_unencoded_byte(i: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take_while1(is_possible_in_name_unencoded), |u: &[u8]| {
        u.to_vec()
    })(i)
}

pub fn recognize_pdf_name(i: &[u8]) -> IResult<&[u8], PdfObject> {
    map(
        preceded(
            pdf_whitespace,
            preceded(
                tag(b"/"),
                fold_many1(
                    alt((
                        recognize_name_hex_encoded_byte,
                        recognize_name_unencoded_byte,
                    )),
                    Vec::new(),
                    |mut acc: Vec<u8>, item: Vec<u8>| {
                        acc.extend(item);
                        acc
                    },
                ),
            ),
        ),
        PdfObject::Name,
    )(i)
}

fn is_non_zero_digit(chr: u8) -> bool {
    chr >= 0x31 && chr <= 0x39
}

fn recognize_digits_not_beginning_with_zero(i: &[u8]) -> IResult<&[u8], &[u8]> {
    match preceded(
        pdf_whitespace,
        tuple((take_while1(is_non_zero_digit), take_while(is_digit))),
    )(i)
    {
        Ok((rest, (start, end))) => Ok((rest, &i[0..(start.len() + end.len())])),
        Err(err) => Err(err),
    }
}

pub fn recognize_pdf_indirect_reference(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        pdf_whitespace,
        tuple((
            not_zero_padded_digits_to_u32,
            tag(b" "),
            digits_to_u16,
            tag(b" R"),
        )),
    )(i)
    {
        Ok((rest, (object_number, _, object_generation, _))) => Ok((
            rest,
            PdfObject::IndirectReference {
                number: object_number,
                generation: object_generation,
            },
        )),
        Err(err) => Err(err),
    }
}

pub fn recognize_pdf_array(i: &[u8]) -> IResult<&[u8], PdfObject> {
    map(
        preceded(
            pdf_whitespace,
            delimited(
                tag(b"["),
                fold_many0(
                    alt((
                        recognize_pdf_indirect_reference,
                        recognize_pdf_integer,
                        recognize_pdf_float,
                        recognize_pdf_null,
                        recognize_pdf_boolean,
                        recognize_pdf_name,
                        recognize_pdf_comment,
                        recognize_pdf_hexadecimal_string,
                        recognize_pdf_literal_string,
                        recognize_pdf_array,
                        recognize_pdf_dictionary,
                    )),
                    Vec::new(),
                    move |mut acc: Vec<PdfObject>, item: PdfObject| {
                        match item {
                            PdfObject::Comment => {}
                            x => {
                                acc.push(x);
                            }
                        }
                        acc
                    },
                ),
                tuple((pdf_whitespace, tag(b"]"))),
            ),
        ),
        PdfObject::Array,
    )(i)
}

pub fn recognize_pdf_dictionary(i: &[u8]) -> IResult<&[u8], PdfObject> {
    let result = preceded(
        pdf_whitespace,
        delimited(
            tag(b"<<"),
            fold_many0(
                alt((
                    recognize_pdf_indirect_reference,
                    recognize_pdf_float,
                    recognize_pdf_integer,
                    recognize_pdf_null,
                    recognize_pdf_boolean,
                    recognize_pdf_name,
                    recognize_pdf_comment,
                    recognize_pdf_hexadecimal_string,
                    recognize_pdf_literal_string,
                    recognize_pdf_array,
                    recognize_pdf_dictionary,
                )),
                Vec::new(),
                move |mut acc: Vec<PdfObject>, item: PdfObject| {
                    match item {
                        PdfObject::Comment => {}
                        x => {
                            acc.push(x);
                        }
                    }
                    acc
                },
            ),
            tuple((pdf_whitespace, tag(b">>"))),
        ),
    )(i);
    match result {
        Ok((rest, vec_of_objects_for_namemap)) => match NameMap::of(vec_of_objects_for_namemap) {
            Ok(namemap) => Ok((rest, PdfObject::Dictionary(namemap))),
            _ => Err(nom::Err::Failure((i, nom::error::ErrorKind::Many0))),
        },
        Err(err) => Err(err),
    }
}

fn recognize_stream(i: &[u8], length: i64) -> IResult<&[u8], Vec<u8>> {
    // we have consumed the dictionary, our goal is to find stream,
    // the zero or more stream bytes, and endstream

    // if we have a greater than 0 length to guide us, use that.

    // if we have a zero length, do our best to figure out if we have
    // two line-ends between stream and endstream or not

    if length > 0 {
        match preceded(
            pdf_whitespace,
            tuple((
                alt((tag(b"stream\n"), tag(b"stream\r\n"))),
                take(length as usize),
                opt(recognize_pdf_line_end), // from examples
                tag(b"endstream"),
                pdf_whitespace,
            )),
        )(i)
        {
            Ok((rest, (_stream, stream_bytes, _line_end, _endstream, _ws))) => {
                return Ok((rest, stream_bytes.to_vec()));
            }
            Err(err) => {
                return Err(err);
            }
        }
    }

    if length == 0 {
        match preceded(
            pdf_whitespace,
            tuple((
                alt((tag(b"stream\n"), tag(b"stream\r\n"))),
                opt(recognize_pdf_line_end),
                tag(b"endstream"),
                pdf_whitespace,
            )),
        )(i)
        {
            Ok((rest, (_stream, _apparently_opt_line_end, _endstream, _ws))) => {
                return Ok((rest, vec![]));
            }
            Err(err) => {
                return Err(err);
            }
        }
    }

    // if we less than zero, do the silly seek forward looking for the first
    // occurrence of the endstream sequence.

    match preceded(
        pdf_whitespace,
        tuple((
            alt((tag(b"stream\n"), tag(b"stream\r\n"))),
            alt((
                take_until(b"\r\nendstream".as_bytes()),
                take_until(b"\rendstream".as_bytes()),
                take_until(b"\nendstream".as_bytes()),
            )),
            alt((
                tag(b"\r\nendstream"),
                tag(b"\rendstream"),
                tag(b"\nendstream"),
            )),
            pdf_whitespace,
        )),
    )(i)
    {
        Ok((rest, (_stream, stream_bytes, _line_end_and_endstream, _ws))) => {
            Ok((rest, stream_bytes.to_vec()))
        }
        Err(err) => Err(err),
    }
}

pub fn recognize_pdf_stream(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match recognize_pdf_dictionary(i) {
        Ok((rest, PdfObject::Dictionary(dictionary))) => match dictionary.get2(b"Length") {
            Some(PdfObject::Integer(length)) => match recognize_stream(rest, length) {
                Ok((rest2, stream_bytes_vec)) => {
                    Ok((rest2, PdfObject::Stream(dictionary, stream_bytes_vec)))
                }
                Err(err) => Err(err),
            },
            Some(PdfObject::IndirectReference {
                number: _n,
                generation: _g,
            }) => match recognize_stream(rest, -1) {
                Ok((rest2, stream_bytes_vec)) => {
                    Ok((rest2, PdfObject::Stream(dictionary, stream_bytes_vec)))
                }
                Err(err) => Err(err),
            },
            None => Ok((rest, PdfObject::Dictionary(dictionary))),
            _ => Err(nom::Err::Failure((i, nom::error::ErrorKind::Many0))),
        },
        Err(err) => Err(err),
        _ => Err(nom::Err::Failure((i, nom::error::ErrorKind::Many0))),
    }
}

pub fn recognize_pdf_indirect_object(i: &[u8]) -> IResult<&[u8], PdfIndirectObject> {
    match preceded(
        pdf_whitespace,
        tuple((
            not_zero_padded_digits_to_u32,
            tag(b" "),
            digits_to_u16,
            tag(b" obj"),
            pdf_whitespace,
            alt((
                recognize_pdf_indirect_reference,
                recognize_pdf_integer,
                recognize_pdf_float,
                recognize_pdf_null,
                recognize_pdf_boolean,
                recognize_pdf_name,
                recognize_pdf_comment,
                recognize_pdf_hexadecimal_string,
                recognize_pdf_literal_string,
                recognize_pdf_array,
                recognize_pdf_stream,
            )),
            pdf_whitespace,
            tag(b"endobj"),
            pdf_whitespace,
        )),
    )(i)
    {
        Ok((rest, (object_number, _, object_generation, _, _, object_itself, _, _, _))) => Ok((
            rest,
            PdfIndirectObject {
                number: object_number,
                generation: object_generation,
                obj: object_itself,
            },
        )),
        Err(err) => Err(err),
    }
}

fn recognize_pdf_cross_reference_entry(i: &[u8]) -> IResult<&[u8], XrefTableEntry2> {
    match tuple((
        map(take_while_m_n(10usize, 10usize, is_digit), digits_to_u64),
        tag(b" "),
        map(take_while_m_n(5usize, 5usize, is_digit), digits_to_u16),
        tag(b" "),
        map(
            alt((tag(b"n"), tag(b"f"))),
            |in_use_or_not_bytes: &[u8]| in_use_or_not_bytes[0] == b'n',
        ),
        alt((tag(b"\r\n"), tag(b" \r"), tag(b" \n"))),
    ))(i)
    {
        Ok((rest, (offset, _, generation, _, in_use, _))) => {
            if let Err(err) = offset {
                return Err(err);
            }
            if let Err(err) = generation {
                return Err(err);
            }

            if in_use {
                Ok((
                    rest,
                    XrefTableEntry2::Uncompressed {
                        number: 0u32,
                        generation: generation.unwrap().1,
                        offset: offset.unwrap().1,
                    },
                ))
            } else {
                Ok((
                    rest,
                    XrefTableEntry2::Free {
                        number: 0u32,
                        generation: generation.unwrap().1,
                    },
                ))
            }
        }
        Err(err) => Err(err),
    }
}

fn set_object_number(xref_entry: &XrefTableEntry2, object_number: u32) -> XrefTableEntry2 {
    match xref_entry {
        XrefTableEntry2::Free {
            number: _number,
            generation,
        } => XrefTableEntry2::Free {
            number: object_number,
            generation: *generation,
        },
        XrefTableEntry2::Uncompressed {
            number: _number,
            generation,
            offset,
        } => XrefTableEntry2::Uncompressed {
            number: object_number,
            generation: *generation,
            offset: *offset,
        },
        XrefTableEntry2::InStream {
            number: _number,
            in_object_number,
            index_in_stream,
        } => XrefTableEntry2::InStream {
            number: object_number,
            in_object_number: *in_object_number,
            index_in_stream: *index_in_stream,
        },
    }
}

fn recognize_pdf_cross_reference_subsection(i: &[u8]) -> IResult<&[u8], Vec<XrefTableEntry2>> {
    match tuple((
        digits_to_u32,
        tag(b" "),
        digits_to_usize,
        recognize_pdf_line_end,
    ))(i)
    {
        Ok((rest, (start_number, _, how_many_entries, _))) => {
            match fold_many_m_n(
                how_many_entries,
                how_many_entries,
                recognize_pdf_cross_reference_entry,
                Vec::new(),
                |mut acc: Vec<XrefTableEntry2>, item| {
                    acc.push(item);
                    acc
                },
            )(rest)
            {
                Ok((rest, vec_of_entries)) => {
                    let mut object_number: u32 = start_number;
                    let mut final_vec: Vec<XrefTableEntry2> = Vec::new();
                    for xref_entry in &vec_of_entries {
                        final_vec.push(set_object_number(xref_entry, object_number));
                        object_number += 1;
                    }

                    Ok((rest, final_vec))
                }
                Err(err) => Err(err),
            }
        }
        Err(err) => Err(err),
    }
}

fn recognize_pdf_old_style_cross_reference_section(i: &[u8]) -> IResult<&[u8], XrefTable2> {
    match preceded(
        alt((tag(b"xref\r\n"), tag(b"xref\r"), tag(b"xref\n"))),
        tuple((
            many1(recognize_pdf_cross_reference_subsection),
            opt(pdf_whitespace),
        )),
    )(i)
    {
        Ok((rest, (vec_of_subsections, _))) => {
            let mut xref: XrefTable2 = XrefTable2::new();

            for subsection in &vec_of_subsections {
                for xref_entry in subsection {
                    xref.add(xref_entry.clone());
                }
            }
            Ok((rest, xref))
        }
        Err(err) => Err(err),
    }
}

fn recognize_pdf_old_style_trailer(i: &[u8]) -> IResult<&[u8], PdfObject> {
    match preceded(
        tuple((tag(b"trailer"), pdf_whitespace)),
        tuple((recognize_pdf_dictionary, opt(pdf_whitespace))),
    )(i)
    {
        Ok((rest, (trailer_dict, _))) => Ok((rest, trailer_dict)),
        Err(err) => Err(err),
    }
}

fn recognize_old_style_cross_reference(i: &[u8]) -> IResult<&[u8], (XrefTable2, PdfObject)> {
    tuple((
        recognize_pdf_old_style_cross_reference_section,
        recognize_pdf_old_style_trailer,
    ))(i)
}

//// we will not reflect to the caller that we actually have an indirect stream
//// object here, and maybe that's okay for purposes.  we can also access it as
//// a normal indirect object (probably once we've decoded this stream and made
//// an xref table...).
//fn recognize_xrefstm_cross_reference(i: &[u8]) -> IResult<&[u8], (XrefTable2, PdfObject)> {
//    match recognize_pdf_indirect_object(i) {
//        Ok((rest, PdfIndirectObject { number: number, generation: generation, obj: PdfObject::Stream(name_map, data) })) => {
//
//            let size: u32 = name_map.get2(b"Size".as_bytes())
//
//            // think about this /W and contents as a test case for validation patterns ugh
//            if !name_map.contains_key2(b"W".as_bytes()) {
//                return Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge)));
//            }
//            let mut w_params: Vec<i64> = Vec::new();
//            match name_map.get2(b"W".as_bytes()) {
//                Some(PdfObject::Array(vec_of_three_integers)) => {
//                    if vec_of_three_integers.len() != 3 {
//                        return Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge)));
//                    }
//                    for item in vec_of_three_integers.iter() {
//                        match item {
//                            PdfObject::Integer(integer) => { w_params.push(*integer); }
//                            _ => { return Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge))); }
//                        }
//                    }
//                }
//                _ => {
//                    return Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge)));
//                }
//            }
//
//            // because the relevant values in this map are required to be direct,
//            // we dont need to fiddle with this map.
//            let decoded_data = crate::filters::decode(&data, &name_map);
//            // now /W tells us what to do with these bytes
//
//            if decoded_data.len()
//
//        }
//        Ok((_rest, unexpected)) => {
//            println!("expected indirect stream object, got {:#?} instead", unexpected);
//            // TODO compose errors better ugh
//            Err(nom::Err::Error((i, nom::error::ErrorKind::TooLarge)))
//        }
//        Err(err) => { Err(err) }
//    }
//}

/// Given that we are started at an offset that a startxref
/// refers to, recognize either the old_style or xrefstm style
/// cross-reference and trailer together.
pub fn recognize_pdf_cross_reference(i: &[u8]) -> IResult<&[u8], (XrefTable2, PdfObject)> {
    context(
        "recognize_pdf_cross_reference",
        //            alt((
        recognize_old_style_cross_reference,
        //        recognize_xrefstm_cross_reference,
        //    ))
    )(i)
}

pub fn recognize_pdf_startxref(i: &[u8]) -> IResult<&[u8], u64> {
    match tuple((
        tag(b"startxref"),
        recognize_pdf_line_end,
        not_zero_padded_digits_to_u64,
        recognize_pdf_line_end,
        opt(recognize_pdf_comment), // ought we assert more here?  comments generally useless.
    ))(i)
    {
        Ok((rest, (_, _, startxref_offset, _, _))) => Ok((rest, startxref_offset)),
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
            None => {}
            _ => {
                assert_eq!(3, 0);
            }
        }

        match NameMap::of(vec![
            PdfObject::Name(b"A"[..].to_owned()),
            PdfObject::Name(b"B"[..].to_owned()),
        ]) {
            Ok(n) => match n.get(PdfObject::Name(b"A"[..].to_owned())) {
                Some(x) => {
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
        ]) {
            Ok(n) => match n.get2(b"A") {
                Some(x) => {
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
            Ok(n) => {
                match n.get(PdfObject::Name(b"A"[..].to_owned())) {
                    Some(x) => {
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
            Ok(_n) => {
                assert_eq!(8, 0);
            }
            Err(_x) => {}
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
            Err(nom::Err::Error((
                b"->".as_bytes(),
                nom::error::ErrorKind::Tag
            ))),
            recognize_pdf_hexadecimal_string(b"<a->")
        );
    }
    macro_rules! hexst {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_hexadecimal_string(input).unwrap() {
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

    #[test]
    fn octal_inside_string_literal_test() {
        assert_eq!(
            Ok((b"9".as_bytes(), 32u8)),
            recognize_octal_value_from_string_literal(b"409")
        );
        assert_eq!(
            Ok((b"4".as_bytes(), 36u8)),
            recognize_octal_value_from_string_literal(b"444")
        );
        assert_eq!(
            Ok((b"8".as_bytes(), 1u8)),
            recognize_octal_value_from_string_literal(b"18")
        );
        assert_eq!(
            Err(nom::Err::Error((
                b"999".as_bytes(),
                nom::error::ErrorKind::TakeWhileMN
            ))),
            recognize_octal_value_from_string_literal(b"999")
        );
    }

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
        tlsr_00: ( b"(hiya)", b"hiya"),
        tlsr_0: (b"(abcd)", b"abcd"),
        tlsr_1: (b"(\\247)", b"\xA7"),

        tlsr_2: (b"(a)", b"a"),
        tlsr_3: (b"(This is a string)", b"This is a string"),
        tlsr_4: (b"(Strings can contain newlines\nand such.)", b"Strings can contain newlines\nand such."),
        tlsr_5: (b"(Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .)",
                    b"Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) ."),
        tlsr_6: (b"(The following is an empty string .)", b"The following is an empty string ."),
        tlsr_7: (b"()", b""),
        tlsr_8: (b"(It has zero (0) length.)", b"It has zero (0) length."),

        tlsr_9: (b"(These \\\rtwo strings \\\nare the same.)", b"These two strings are the same."),
        tlsr_a: (b"(These two strings are the same.)", b"These two strings are the same."),

        tlsr_b: (b"(This string has an end-of-line at the end of it.\n)", b"This string has an end-of-line at the end of it.\n"),
        tlsr_c: (b"(So does this one.\\n)", b"So does this one.\n"),
        tlsr_d: (b"(This string contains \\245two octal characters\\307.)", b"This string contains \xA5two octal characters\xC7."),
        tlsr_e: (b"(\\0053)", b"\x053"),
        tlsr_f: (b"(\\053)", b"+"),
        tlsr_g: (b"(\\53)", b"+"),
        tlsr_h: (b"(\\533)", b"+3"),
    }

    macro_rules! name_object_result_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_name(input.as_bytes()) {
                        Ok((_b, PdfObject::Name( v ))) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },
                        x => {
                            println!("wut: {:#?}", x);
                            assert_eq!(15, 0);
                        }
                    }
                }
            )*
        }
    }

    name_object_result_test! {
        nort_1: (b"/Name1",b"Name1"),
        nort_2: (b"/ASomewhatLongerName",b"ASomewhatLongerName"),
        nort_3:(b"/A;Name_With-Various***Characters?",b"A;Name_With-Various***Characters?"),
        nort_5: (b"/1.2",b"1.2"),
        nort_6: (b"/$$",b"$$"),
        nort_7: (b"/@pattern",b"@pattern"),
        nort_8: (b"/.notdef",b".notdef"),
        nort_9: (b"/Lime#20Green",b"Lime Green"),
        nort_10: (b"/paired#28#29parentheses",b"paired()parentheses"),
        nort_11: (b"/The_Key_of_F#23_Minor",b"The_Key_of_F#_Minor"),
        nort_12: (b"/A#42",b"AB"),
        nort_13: (b"/#2F",b"/"),
        nort_15: (b"/abcd[",b"abcd"),
        nort_16: (b"/this that", b"this"),
    }

    #[test]
    fn name_fail_cases() {
        assert_eq!(
            Err(nom::Err::Error((
                b"".as_bytes(),
                nom::error::ErrorKind::Many1
            ))),
            recognize_pdf_name(b"/")
        );
        assert_eq!(
            Err(nom::Err::Error((
                b" ".as_bytes(),
                nom::error::ErrorKind::Many1
            ))),
            recognize_pdf_name(b"/ ")
        );
        assert_eq!(
            Err(nom::Err::Error((
                b"/".as_bytes(),
                nom::error::ErrorKind::Many1
            ))),
            recognize_pdf_name(b"//")
        );
    }

    #[test]
    fn indirect_reference_test() {
        assert_eq!(
            (
                b" ".as_bytes(),
                PdfObject::IndirectReference {
                    number: 95,
                    generation: 1,
                }
            ),
            recognize_pdf_indirect_reference(b"95 1 R ".as_bytes()).unwrap()
        );
        assert_eq!(
            (
                b"\n".as_bytes(),
                PdfObject::IndirectReference {
                    number: 95,
                    generation: 100,
                }
            ),
            recognize_pdf_indirect_reference(b"95 100 R\n".as_bytes()).unwrap()
        );
        assert_eq!(
            (
                b"".as_bytes(),
                PdfObject::IndirectReference {
                    number: 96,
                    generation: 100,
                }
            ),
            recognize_pdf_indirect_reference(b"96 100 R".as_bytes()).unwrap()
        );
        assert_eq!(
            Err(nom::Err::Error((
                b"09 1 R".as_bytes(),
                nom::error::ErrorKind::TakeWhile1
            ))),
            recognize_pdf_indirect_reference(b"09 1 R".as_bytes())
        );

        assert_eq!(
            Err(nom::Err::Failure((
                b"1928356 R".as_bytes(),
                nom::error::ErrorKind::TooLarge
            ))),
            recognize_pdf_indirect_reference(b"9 1928356 R".as_bytes())
        );
    }

    macro_rules! recognized_array_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_array(input.as_bytes()) {
                        Ok((_b, v)) => {
                            assert_eq!(v, expected);
                        },
                        x => {
                            println!("fail 16: {:#?}", x );
                            assert_eq!(16, 0);
                        }
                    }
                }
            )*
        }
    }

    recognized_array_object_test! {
        raot_1: (b"[1 2 3]",
            PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3) ])),
        raot_2: (b"[1 2 [3]]",
            PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_3: (b"[1 2[3]]", PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_5: (b"[/1 2[3]]", PdfObject::Array( vec![PdfObject::Name(b"1"[..].to_owned()), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_6: (b"[/12[3]] ]", PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_7: (b"[/12 [3]]", PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_8: (b"[       /12 \n1\r\r2    [ ] ]",
                                PdfObject::Array( vec![PdfObject::Name(b"12"[..].to_owned()),
                                PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![ ])
                                    ])),
        raot_8a: (b"[ ]",
                                PdfObject::Array( vec![])),

        raot_8b: (b"[]",
                                PdfObject::Array( vec![])),

        raot_9: (b"[1%yo\r %yo\n2%hiya\r\n [3]]",
                                PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),
        raot_10: (b"[%yo\r1%yo\r %yo\n2%hiya\r\n [3]]",
                                PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),
                                    PdfObject::Array( vec![PdfObject::Integer(3) ])
                                    ])),

        raot_12: (b"[<09 ab> /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::String(b"\x09\xAB"[..].to_owned()),
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),
        raot_13: (b"[99 0 R /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::IndirectReference { number: 99, generation: 0 },
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),
        raot_14: (b"[99 0 R 100 2 R  ]",
                                PdfObject::Array( vec![
                                    PdfObject::IndirectReference { number: 99, generation: 0 },
                                    PdfObject::IndirectReference { number: 100, generation: 2 },
                                ])),

        raot_11: (b"[(hiya) /yo 1 2 3]",
                                PdfObject::Array( vec![
                                    PdfObject::String(b"hiya"[..].to_owned()),
                                    PdfObject::Name(b"yo"[..].to_owned()),
                                    PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3)
                                ])),
       raot_15: (b"[<</a[1 2 3]>> <</b 1 2 R>>]",
            PdfObject::Array(vec![
                PdfObject::Dictionary( NameMap::of(vec![
                    PdfObject::Name( b"a"[..].to_owned()),
                    PdfObject::Array( vec![PdfObject::Integer(1), PdfObject::Integer(2),PdfObject::Integer(3) ])
                ]).unwrap() ),

                PdfObject::Dictionary( NameMap::of(vec![
                    PdfObject::Name( b"b"[..].to_owned()),
                    PdfObject::IndirectReference { number: 1, generation: 2 }
                ]).unwrap() ),
            ])
        ),

        raot_16: (b"[/a/b/c]", PdfObject::Array( vec![PdfObject::Name(b"a"[..].to_owned()),
                                    PdfObject::Name(b"b"[..].to_owned()),
                                    PdfObject::Name(b"c"[..].to_owned()),
                                    ])),
    }

    macro_rules! recognized_dict_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;

                    match recognize_pdf_dictionary(input.as_bytes()) {
                        Ok((_b, v)) => {
                            assert_eq!(v, expected);
                        },
                        _ => {
                            assert_eq!(11, 0);
                        }
                    }


                }
            )*
        }
    }

    recognized_dict_object_test! {
        rd_0: (b"<</Length 6 0 R/Filter /FlateDecode>>",
                PdfObject::Dictionary(
                    NameMap::of(
                        vec![
                            PdfObject::Name(b"Length"[..].to_owned()),
                            PdfObject::IndirectReference { number: 6, generation: 0},
                            PdfObject::Name(b"Filter"[..].to_owned()),
                            PdfObject::Name(b"FlateDecode"[..].to_owned()),
                        ]
                    ).unwrap()
                )),
        rd_1: (b"<< >>", PdfObject::Dictionary( NameMap::new() )),
        rd_1a: (b"<<>>", PdfObject::Dictionary( NameMap::new() )),
        rd_2: (b"<</yo%yo\n1>>", PdfObject::Dictionary( NameMap::of(
                                    vec![
                                        PdfObject::Name( b"yo"[..].to_owned()),
                                        PdfObject::Integer( 1 )
                                    ]
                                ).unwrap() )),
        rd_3: (b"<</a<</a<</a [1]>>>>>>",

                    PdfObject::Dictionary(
                        NameMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::Dictionary(
                                                NameMap::of(
                                                    vec![
                                                        PdfObject::Name( b"a"[..].to_owned()),
                                                        PdfObject::Array( vec![PdfObject::Integer(1) ])
                                                    ]
                                                ).unwrap()
                                            )
                                        ]
                                    ).unwrap()
                                )
                            ]
                        ).unwrap()
                    )

                ),
        rd_4: (b"<</a<</a<</a 1>>>>>>",
                    PdfObject::Dictionary(
                        NameMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::Dictionary(
                                                NameMap::of(
                                                    vec![
                                                        PdfObject::Name( b"a"[..].to_owned()),
                                                        PdfObject::Integer(1)
                                                    ]
                                                ).unwrap()
                                            )
                                        ]
                                    ).unwrap()
                                )
                            ]
                        ).unwrap()
                    )
        ),
        rd_5: (b"<</a<</a<abcdef>>>>>",
                    PdfObject::Dictionary(
                        NameMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::String( b"\xAB\xCD\xEF"[..].to_owned() )
                                        ]
                                    ).unwrap()
                                )
                            ]
                        ).unwrap()
                    )

        ),
        rd_6: (b"<</a<</a%yo\n<abcdef>>>>>",
                    PdfObject::Dictionary(
                        NameMap::of(
                            vec![
                                PdfObject::Name( b"a"[..].to_owned()),
                                PdfObject::Dictionary(
                                    NameMap::of(
                                        vec![
                                            PdfObject::Name( b"a"[..].to_owned()),
                                            PdfObject::String( b"\xAB\xCD\xEF"[..].to_owned() )
                                        ]
                                    ).unwrap()
                                )
                            ]
                        ).unwrap()
                    )
        ),
        rd_7: (b"<< /BG2 /Default /OP true /OPM 1 /SA false /SM 0.02 /Type /ExtGState /UCR2 /Default /op true >>\n",
            PdfObject::Dictionary(
                NameMap::of(
                    vec![
                        PdfObject::Name( b"BG2"[..].to_owned()),
                        PdfObject::Name( b"Default"[..].to_owned()),
                        PdfObject::Name( b"OP"[..].to_owned()),
                        PdfObject::Boolean(true),
                        PdfObject::Name( b"OPM"[..].to_owned()),
                        PdfObject::Integer(1),
                        PdfObject::Name( b"SA"[..].to_owned()),
                        PdfObject::Boolean(false),
                        PdfObject::Name( b"SM"[..].to_owned()),
                        PdfObject::Float(0.02),
                        PdfObject::Name( b"Type"[..].to_owned()),
                        PdfObject::Name( b"ExtGState"[..].to_owned()),
                        PdfObject::Name( b"UCR2"[..].to_owned()),
                        PdfObject::Name( b"Default"[..].to_owned()),
                        PdfObject::Name( b"op"[..].to_owned()),
                        PdfObject::Boolean(true),
                    ]
                ).unwrap()
            )
        ),
    }

    #[test]
    fn test_recognized_dict_object_errors() {
        assert_eq!(
            Err(nom::Err::Failure((
                b"<</yo%yo\n >>".as_bytes(),
                nom::error::ErrorKind::Many0
            ))),
            recognize_pdf_dictionary(b"<</yo%yo\n >>".as_bytes())
        );
        assert_eq!(
            Err(nom::Err::Failure((
                b"<</yo>>".as_bytes(),
                nom::error::ErrorKind::Many0
            ))),
            recognize_pdf_dictionary(b"<</yo>>".as_bytes())
        );
    }

    #[test]
    fn test_recognize_stream() {
        // the indication from samples in the spec ( § 8.9.5.4 Alternate Images )
        // suggests that there is not necessarily an additional EOL in this instance.
        assert_eq!(
            recognize_stream(b" stream\r\nendstream", 0),
            Ok((b"".as_bytes(), vec![]))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--\r\nendstream  ", 16),
            Ok((b"".as_bytes(), b"__--__--__--__--"[..].to_owned()))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--\nendstream  ", 16),
            Ok((b"".as_bytes(), b"__--__--__--__--"[..].to_owned()))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--", 16),
            Err(nom::Err::Error((
                b"__--__--".as_bytes(),
                nom::error::ErrorKind::Eof
            )))
        );

        assert_eq!(
            recognize_stream(b" stream\n__--__--__--__--", 16),
            Err(nom::Err::Error((
                b"".as_bytes(),
                nom::error::ErrorKind::Tag
            )))
        );
    }

    macro_rules! recognized_stream_object_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match recognize_pdf_stream(input.as_bytes()) {
                        Ok((_rest, x)) => {
                            assert_eq!(x, expected);

                        },
                        x => {
                            println!("err is: {:?}", x);
                            assert_eq!(150, 0);
                        }
                    }
                }
            )*
        }
    }

    recognized_stream_object_test! {
        rsot_1: (b"<< >>", PdfObject::Dictionary( NameMap::new() )), // because if you dont have a Length you're just a dictionary
        rsot_2: (b"<< /Length 20 >>\nstream\n01234567890123456789\nendstream\n",
            PdfObject::Stream(
                NameMap::of( vec![PdfObject::Name(b"Length"[..].to_owned()), PdfObject::Integer(20)] ).unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_3: (b"<< /Length 20 >>\r\nstream\r\n01234567890123456789\r\nendstream\r\n",
            PdfObject::Stream(
                NameMap::of( vec![PdfObject::Name(b"Length"[..].to_owned()), PdfObject::Integer(20)] ).unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_4: (b"<</Length 10 0 R/Filter /FlateDecode>>\nstream\n01234567890123456789\nendstream\n",
            PdfObject::Stream(
                NameMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_4a: (b"<</Length 10 0 R/Filter /FlateDecode>>\nstream\r\n01234567890123456789\r\nendstream\n",
            PdfObject::Stream(
                NameMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap(),
                b"01234567890123456789"[..].to_owned()
            )
        ),
        rsot_4b: (b"<</Length 10 0 R/Filter /FlateDecode>>\nstream\r\n01234567890123456789\r\n\r\nendstream\n",
            PdfObject::Stream(
                NameMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap(),
                b"01234567890123456789\r\n"[..].to_owned()
            )
        ),
        rsot_5: (b"<</Length 10 0 R/Filter /FlateDecode>>\r\nstream\r\n0123456\n7890123456789\r\nendstream\r\n",
            PdfObject::Stream(
                NameMap::of(
                    vec![
                        PdfObject::Name(b"Length"[..].to_owned()),
                        PdfObject::IndirectReference{ number: 10, generation: 0},
                        PdfObject::Name(b"Filter"[..].to_owned()),
                        PdfObject::Name(b"FlateDecode"[..].to_owned()),
                    ] ).unwrap(),
                b"0123456\n7890123456789"[..].to_owned()
            )
        ),
    }

    #[test]
    fn test_indirect_object() {
        assert_eq!(
            PdfIndirectObject {
                number: 1 as u32,
                generation: 0 as u16,
                obj: PdfObject::String(b"xyzzy"[..].to_owned()),
            },
            recognize_pdf_indirect_object(b"1 0 obj\n(xyzzy)endobj".as_bytes())
                .unwrap()
                .1
        );
    }

    #[test]
    fn test_xref_subsection_entry() {
        assert_eq!(
            Ok((
                b"".as_bytes(),
                XrefTableEntry2::Uncompressed {
                    number: 0u32,
                    offset: 200u64,
                    generation: 1u16,
                }
            )),
            recognize_pdf_cross_reference_entry(b"0000000200 00001 n \n")
        );
        assert_eq!(
            Ok((
                b"".as_bytes(),
                XrefTableEntry2::Free {
                    number: 0u32,
                    generation: 3u16,
                }
            )),
            recognize_pdf_cross_reference_entry(b"0000000400 00003 f \n")
        );
        assert_eq!(
            Err(nom::Err::Failure((
                b"99999".as_bytes(),
                nom::error::ErrorKind::TooLarge
            ))),
            recognize_pdf_cross_reference_entry(b"0000000200 99999 n \n")
        );
    }

    #[test]
    fn test_xref_subsection() {
        assert_eq!(
            Ok((
                b"".as_bytes(),
                vec![
                    XrefTableEntry2::Uncompressed {
                        number: 5,
                        generation: 2,
                        offset: 1,
                    },
                    XrefTableEntry2::Uncompressed {
                        number: 6,
                        generation: 6,
                        offset: 99,
                    },
                ]
            )),
            recognize_pdf_cross_reference_subsection(
                b"5 2\n0000000001 00002 n \n0000000099 00006 n \n"
            )
        );

        assert_eq!(
            Ok((
                b"0000000999 00008 n \n".as_bytes(),
                vec![
                    XrefTableEntry2::Uncompressed {
                        number: 5,
                        generation: 2,
                        offset: 1,
                    },
                    XrefTableEntry2::Uncompressed {
                        number: 6,
                        generation: 6,
                        offset: 99,
                    },
                ]
            )),
            recognize_pdf_cross_reference_subsection(
                b"5 2\n0000000001 00002 n \n0000000099 00006 n \n0000000999 00008 n \n"
            )
        );

        assert_eq!(
            Err(nom::Err::Error((
                b"0000000001 00002 n \n0000000099 00006 n \n0000000999 00008 n \n".as_bytes(),
                nom::error::ErrorKind::ManyMN
            ))),
            recognize_pdf_cross_reference_subsection(
                b"5 4\n0000000001 00002 n \n0000000099 00006 n \n0000000999 00008 n \n"
            )
        );
    }

    #[test]
    fn test_xref_section() {
        let (_bytes, xref) = recognize_pdf_old_style_cross_reference_section(
            b"xref\r\n0 6\r\n0000000003 65535 f \n0000000017 00000 n \n0000000081 00000 n \n0000000000 00007 f \n0000000331 00000 n \n0000000409 00000 n \n"
        ).unwrap();

        assert_eq!(4usize, xref.in_use().len());
        assert_eq!(2usize, xref.free().len());

        for in_use_obj_num in xref.in_use() {
            assert!(
                in_use_obj_num == 1
                    || in_use_obj_num == 2
                    || in_use_obj_num == 4
                    || in_use_obj_num == 5
            );
            assert_eq!(Some(0u16), xref.generation_of(in_use_obj_num));
        }
        for free_obj_num in xref.free() {
            assert!(free_obj_num == 0 || free_obj_num == 3);
            if free_obj_num == 0 {
                assert_eq!(Some(65535u16), xref.generation_of(free_obj_num));
            }
            if free_obj_num == 3 {
                assert_eq!(Some(7u16), xref.generation_of(free_obj_num));
            }
        }

        assert_eq!(Some(17u64), xref.offset_of(1));
        assert_eq!(Some(81u64), xref.offset_of(2));
        assert_eq!(Some(331u64), xref.offset_of(4));
        assert_eq!(Some(409u64), xref.offset_of(5));

        let (_bytes, xref2) = recognize_pdf_old_style_cross_reference_section(
            b"xref\n0 1\n0000000000 65535 f \n3 1\n0000025325 00000 n \n23 2\n0000025518 00002 n \n0000025635 00000 n \n30 1\n0000025777 00000 n \n"
        ).unwrap();

        assert_eq!(4usize, xref2.in_use().len());
        assert_eq!(1usize, xref2.free().len());

        for in_use_obj_num in xref2.in_use() {
            assert!(
                in_use_obj_num == 3
                    || in_use_obj_num == 23
                    || in_use_obj_num == 24
                    || in_use_obj_num == 30
            );
        }

        assert_eq!(Some(0u16), xref2.generation_of(3));
        assert_eq!(Some(2u16), xref2.generation_of(23));
        assert_eq!(Some(0u16), xref2.generation_of(24));
        assert_eq!(Some(0u16), xref2.generation_of(30));

        for free_obj_num in xref2.free() {
            assert!(free_obj_num == 0);
            if free_obj_num == 0 {
                assert_eq!(Some(65535u16), xref2.generation_of(free_obj_num));
            }
        }

        assert_eq!(Some(25325u64), xref2.offset_of(3));
        assert_eq!(Some(25518u64), xref2.offset_of(23));
        assert_eq!(Some(25635u64), xref2.offset_of(24));
        assert_eq!(Some(25777u64), xref2.offset_of(30));
    }

    #[test]
    fn test_trailer() {
        assert_eq!(
            Ok((
                b"startxref\n18799\n%%EOF\n".as_bytes(),
                PdfObject::Dictionary(
                    NameMap::of(
                        vec![
                            PdfObject::Name(b"Size"[..].to_owned()),
                            PdfObject::Integer(22),
                            PdfObject::Name(b"Root"[..].to_owned()),
                            PdfObject::IndirectReference { number: 2, generation: 0 },
                            PdfObject::Name(b"Info"[..].to_owned()),
                            PdfObject::IndirectReference { number: 1, generation: 0 },
                            PdfObject::Name(b"ID"[..].to_owned()),
                            PdfObject::Array(
                                vec![
                                    PdfObject::String(b"\x81\xb1\x4a\xaf\xa3\x13\xdb\x63\xdb\xd6\xf9\x81\xe4\x9f\x94\xf4"[..].to_owned()),
                                    PdfObject::String(b"\x81\xb1\x4a\xaf\xa3\x13\xdb\x63\xdb\xd6\xf9\x81\xe4\x9f\x94\xf4"[..].to_owned())
                                ]
                            ),
                        ]
                    ).unwrap()
                )
            )),
            recognize_pdf_old_style_trailer(b"trailer\n<</Size 22\n/Root 2 0 R\n/Info 1 0 R\n/ID [<81b14aafa313db63dbd6f981e49f94f4>\n<81b14aafa313db63dbd6f981e49f94f4>\n] >>\nstartxref\n18799\n%%EOF\n")
        );
    }
}
