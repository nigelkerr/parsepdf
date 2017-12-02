#[macro_use]
extern crate nom;
#[macro_use]
extern crate approx;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::ops::Range;
use std::ops::RangeFrom;
use std::ops::RangeTo;
use std::str::FromStr;

// parse a pdf file, per ISO 32000-2_2017(en)

#[derive(Debug, PartialEq, Eq)]
pub enum PdfVersion {
    Known { ver: Vec<u8> },
    Unknown
}

#[derive(Debug, PartialEq, Eq)]
pub enum PdfObject {
    Null,
    IndirectReference { number: u32, version: u32 },
}

// which of these is better?  how will i figure that out?

named!(pub pdf_line_ending_by_macro<&[u8],&[u8]>,
    alt!(
        complete!(tag!(b"\r\n")) |
        complete!(tag!(b"\r")) |
        complete!(tag!(b"\n"))
    )
);

pub fn pdf_line_ending(input: &[u8]) -> nom::IResult<&[u8], &[u8]> {
    // this here _ => is amounting to the complete! behavior above, it seems.
    match input.compare("\r\n") {
        CompareResult::Ok => Done(input.slice(2..), input.slice(0..2)),
        _ => match input.compare("\r") {
            CompareResult::Ok => Done(input.slice(1..), input.slice(0..1)),
            _ => match input.compare("\n") {
                CompareResult::Ok => Done(input.slice(1..), input.slice(0..1)),
                _ => Error(error_position!(ErrorKind::CrLf, input)),
            }
        }
    }
}

#[inline]
pub fn is_not_line_end_chars(chr: u8) -> bool {
    (chr != b'\n' && chr != b'\r')
}

// comments are going to by my undoing, given where all they can occur ( § 7.2.4 )

named!(pub recognize_comment<&[u8],&[u8]>,
    recognize!(
        tuple!(
            tag!(b"%"),
            take_while!( is_not_line_end_chars ),
            pdf_line_ending
        )
    )
);

fn byte_vec_from_comment(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();

    // skip the single % starting the comment.
    for item in input.iter().skip(1) {
        if is_not_line_end_chars(*item) {
            result.push(*item);
        }
    }

    Ok(result)
}

// just the bytes of the comment itself, not the delimiting
named!(pub comment<&[u8],Vec<u8> >,
    map_res!( recognize_comment, byte_vec_from_comment )
);

named!(pdf_version<&[u8],&[u8]>,
    alt!(
        tag!(b"1.0") |
        tag!(b"1.1") |
        tag!(b"1.2") |
        tag!(b"1.3") |
        tag!(b"1.4") |
        tag!(b"1.5") |
        tag!(b"1.6") |
        tag!(b"1.7") |
        tag!(b"2.0")
    )
);

named!(pub pdf_magic<&[u8],PdfVersion>,
    do_parse!(
        tag!(b"%PDF-") >>
        ver_bytes: pdf_version >>
        pdf_line_ending_by_macro >>
        ( PdfVersion::Known{ ver: ver_bytes.to_vec() } )
    )
);

// § 7.5.2

named!(pub pdf_header<&[u8],PdfVersion>,
    do_parse!(
        ver: pdf_magic >>
        opt!( comment ) >>
        ( ver )
    )
);

// § 7.3.2
named!(pub recognize_boolean<&[u8],&[u8]>,
    recognize!(alt!( tag!(b"true") | tag!(b"false")))
);

named!(pub boolean<&[u8],bool>,
    map_res!(map_res!( recognize_boolean , str::from_utf8 ), FromStr::from_str)
);

// § 7.3.3

named!(maybe_signed_integer<&[u8],(Option<&[u8]>, &[u8])>,
    pair!(
        opt!(alt!(tag_s!("+") | tag_s!("-"))),
        nom::digit
    )
);
named!(pub recognize_signed_integer<&[u8],&[u8]>,
    recognize!(
        maybe_signed_integer
    )
);
named!(pub signed_integer<&[u8],i64>,
    map_res!( map_res!( recognize_signed_integer, str::from_utf8 ), FromStr::from_str )
);


named!(maybe_signed_float_ap<&[u8],(Option<&[u8]>,&[u8],&[u8],Option<&[u8]>)>,
    tuple!(
        opt!(alt!(tag!(b"+") | tag!(b"-"))),
        digit,
        tag!(b"."),
        opt!(complete!(digit))
    )
);

named!(maybe_signed_float_pp<&[u8],(Option<&[u8]>,&[u8],&[u8],Option<&[u8]>)>,
    tuple!(
        opt!(alt!(tag!(b"+") | tag!(b"-"))),
        tag!(b"."),
        digit,
        opt!(complete!(digit))
    )
);

named!(pub recognize_signed_float<&[u8],&[u8]>,
    recognize!(
        alt!(
            complete!( maybe_signed_float_ap ) |
            complete!( maybe_signed_float_pp )
        )
    )
);


named!(pub signed_float<&[u8],f64>,
    map_res!( map_res!( recognize_signed_float, str::from_utf8 ), FromStr::from_str )
);


// "Table 1 -- White space characters"
#[inline]
pub fn is_pdf_whitespace(chr: u8) -> bool {
    (chr == 0x00 ||
        chr == 0x09 ||
        chr == 0x0A ||
        chr == 0x0C ||
        chr == 0x0D ||
        chr == 0x20
    )
}

// § 7.3.4.3 Hexadecimal Strings
// return here a slice of the final value

#[inline]
fn can_be_in_hexadecimal_string(chr: u8) -> bool {
    is_hex_digit(chr) ||
        is_pdf_whitespace(chr)
}

named!(maybe_hexadecimal_string<&[u8],&[u8]>,
    delimited!(
        tag!("<"),
        take_while!( can_be_in_hexadecimal_string ),
        tag!(">")
    )
);

named!(pub recognize_hexadecimal_string<&[u8],&[u8]>,
    recognize!( maybe_hexadecimal_string )
);

#[inline]
fn from_hex(chr: u8) -> u8 {
    // heh
    if chr >= 0x30 && chr <= 0x39 {
        chr - 0x30
    } else if chr >= 0x41 && chr <= 0x46 {
        chr - 0x37
    } else {
        chr - 0x57
    }
}

// we are trusting that what came before worked out...
fn byte_vec_from_hexadecimal_string(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();
    let filtered: Vec<u8> = input.iter().filter(
        |&x| nom::is_hex_digit(*x)
    ).map(|&x| from_hex(x)).collect();

    for pair in filtered.chunks(2) {
        match pair.len() {
            2 => { result.push((pair[0] << 4) + pair[1]); }
            1 => { result.push((pair[0] << 4)); }
            _ => return Err(nom::ErrorKind::Custom(1111)),
        }
    }

    Ok(result)
}

named!(pub hexadecimal_string<&[u8],Vec<u8>>,
    map_res!( recognize_hexadecimal_string, byte_vec_from_hexadecimal_string)
);


// § 7.3.4.2 Literal Strings ugh


/// given the current state, could we have more octal digits
/// if we'd had this much octal so far?
fn could_have_more_octal_digits(v: i32, digits: usize) -> bool {
    let mut retval: bool = false;

    if digits < 3 {
        retval = true;
    }

    if v > 31 {
        // which is to say, greater than octal 37
        retval = false;
    }

    retval
}

/// determine if we have a valid pdf literal string, and
/// return the full sequence of bytes from opening paren
/// to closing paren.
/// no treatment of leading or following whitespace, handle
/// that outside.
/// we're going to need to have a crassly similar loop for
/// taking this byte-sequence and understanding it as a
/// character sequence.
pub fn recognize_literal_string<T>(input: T) -> IResult<T, T> where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength,
    <T as InputIter>::Item: AsChar {
    let input_length = input.input_len();
    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }

    // what depth of balanced unescaped parens are we at?
    // starts our iteration at 0, and it ought to be 0
    // when we end (so, the delimiting paren is the first
    // and last we encounter and manipulate this variable
    // for...)
    let mut parens_depth: usize = 0;

    // did we just see an escaping backslash?
    let mut was_escape_char: bool = false;

    // how to represent the octals?  we want to accept the
    // longest valid octal, even in the face of the escape
    // sequence being followed by other digits
    let mut current_octal_value: i32 = -1;
    let mut number_octal_digits: usize = 0;

    for (idx, item) in input.iter_indices() {
        let chr = item.as_char();

        if was_escape_char {
            match chr {
                '\n' | '\r' => {
                    // glam! we care more on deserializing
                }
                'n' | 'r' | 't' | 'b' | 'f' | '(' | ')' | '\\' => {
                    // glam! we'll do the right thing on deserializing
                }
                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' => {
                    number_octal_digits = 1;
                    current_octal_value = (chr as u8 - '0' as u8) as i32;
                }
                _ => {
                    // anything outside the above gets us an error
                    return Error(error_position!(ErrorKind::Custom(3333), input));
                }
            }
            was_escape_char = false;
            continue;
        }

        if number_octal_digits > 0 {
            if could_have_more_octal_digits(current_octal_value, number_octal_digits) &&
                (chr >= '0' && '7' <= chr) {
                number_octal_digits += 1;
                current_octal_value = (current_octal_value << 3) + (chr as u8 - '0' as u8) as i32;
                continue;
            } else {
                // glam! reset octal
                number_octal_digits = 0;
                current_octal_value = -1;
            }
        }

        match chr {
            '(' => {
                parens_depth += 1;
            }
            ')' => {
                parens_depth -= 1;
                if parens_depth == 0 {
                    return Done(input.slice(idx + 1..), input.slice(0..idx + 1));
                }
            }
            '\\' => {
                was_escape_char = true;
            }
            _ => {
                // glam! arbitrary 8-bit values accepted here.
            }
        }
    }

    // i can see how we could get here with the _ branch, but
    // not the 0 branch
    match parens_depth {
        0 => {}
        _ => { return Incomplete(Needed::Unknown); }
    }

    // ... how would we ever get here?
    Done(input.slice(input_length..), input)
}

/// make a sequence of bytes from the raw byte sequence of
/// the PDF literal string.
fn byte_vec_from_literal_string(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Err(ErrorKind::Custom(6666));
    }

    // what depth of balanced unescaped parens are we at?
    // starts our iteration at 0, and it ought to be 0
    // when we end (so, the delimiting paren is the first
    // and last we encounter and manipulate this variable
    // for...)
    let mut parens_depth: usize = 0;

    // did we just see an escaping backslash?
    let mut was_escape_char: bool = false;
    let mut was_escaped_carriage_return: bool = false;

    // how to represent the octals?  we want to accept the
    // longest valid octal, even in the face of the escape
    // sequence being followed by other digits
    let mut current_octal_value: i32 = -1;
    let mut number_octal_digits: usize = 0;

    for (_idx, item) in input.iter_indices() {
        let chr = *item;

        if was_escaped_carriage_return {
            was_escaped_carriage_return = false;
            match chr {
                b'\n' => {
                    continue;
                }
                _ => {
                    // glam! this wasnt involved in line ending and can proceed
                }
            }
        }

        if was_escape_char {
            match chr {
                b'\n' => {
                    // glam! push nothing and carry on
                }
                b'\r' => {
                    // we have to worry about \r vs \r\n here,
                    // they both should be rendered into the result as \n
                    was_escaped_carriage_return = true;
                }
                b'n' => {
                    result.push(b'\n')
                }
                b'r' => {
                    result.push(b'\r')
                }
                b't' => {
                    result.push(b'\t')
                }
                b'b' => {
                    result.push(0x08 as u8)
                }
                b'f' => {
                    result.push(0x0c as u8)
                }
                b'(' | b')' | b'\\' => {
                    result.push(chr);
                }
                b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                    number_octal_digits = 1;
                    current_octal_value = (chr as u8 - '0' as u8) as i32;
                }
                _ => {
                    // anything outside the above gets us an error
                    return Err(ErrorKind::Custom(4444));
                }
            }
            was_escape_char = false;
            continue;
        }

        if number_octal_digits > 0 {
            if could_have_more_octal_digits(current_octal_value, number_octal_digits) &&
                (chr == b'0' || chr == b'1' || chr == b'2' || chr == b'3' || chr == b'4' || chr == b'5' || chr == b'6' || chr == b'7') {
                number_octal_digits += 1;
                current_octal_value = (current_octal_value << 3) + ((chr as u8 - '0' as u8) as i32);
                continue;
            } else {
                // glam! reset octal
                result.push(current_octal_value as u8);
                number_octal_digits = 0;
                current_octal_value = -1;
            }
        }

        match chr {
            b'(' => {
                if parens_depth > 0 {
                    result.push(chr);
                }
                parens_depth += 1;
            }
            b')' => {
                parens_depth -= 1;
                if parens_depth > 0 {
                    result.push(chr);
                }
                if parens_depth == 0 {
                    break;
                }
            }
            b'\\' => {
                was_escape_char = true;
            }
            _ => {
                result.push(chr);
            }
        }
    }

    // i can see how we could get here with the _ branch, but
    // not the 0 branch
    match parens_depth {
        0 => {}
        _ => { return Err(ErrorKind::Custom(5555)); }
    }

    Ok(result)
}

named!(pub literal_string<&[u8],Vec<u8>>,
    map_res!( recognize_literal_string, byte_vec_from_literal_string )
);

// § 7.3.5 Name objects

/// recognize a Name object, returning the sequence of the entire Name
/// object and its leading /.
pub fn recognize_name_object<T>(input: T) -> IResult<T, T> where
    T: Slice<Range<usize>> + Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
    T: InputIter + InputLength,
    <T as InputIter>::Item: AsChar {
    let input_length = input.input_len();
    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }

    //
    let mut first_iteration: bool = true;

    // was the previous character the number_sign, after which
    // we should expect exactly two hex digits ?
    let mut was_number_sign: bool = false;
    let mut count_hex_digits: usize = 0;
    let mut current_hex_value: u8 = 0;

    for (idx, item) in input.iter_indices() {
        let chr = item.as_char();

        // did we start right?
        if first_iteration {
            match chr {
                '/' => {
                    first_iteration = false;
                    continue;
                }
                _ => {
                    return Error(error_position!(ErrorKind::Custom(7777), input));
                }
            }
        }

        if was_number_sign {
            match count_hex_digits {
                0 | 1 => {
                    match is_hex_digit(chr as u8) {
                        true => {
                            current_hex_value = (current_hex_value << 4) + from_hex(chr as u8);
                            count_hex_digits += 1;

                            if count_hex_digits > 1 {
                                current_hex_value = 0;
                                count_hex_digits = 0;
                                was_number_sign = false;
                            }
                            continue;
                        }
                        false => {
                            return Error(error_position!(ErrorKind::Custom(9999), input));
                        }
                    }
                }
                _ => {
                    return Error(error_position!(ErrorKind::Custom(8888), input));
                }
            }
        }

        match chr {
            '#' => {
                was_number_sign = true;
                continue;
            }
            '\x00' => {
                return Error(error_position!(ErrorKind::Custom(33333), input));
            }
            '\n' | '\r' | '\t' | ' ' | '\x0C' | '/' | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '%' => {
                // unescaped whitespace and unescaped delimiters ends the name
                return Done(input.slice(idx..), input.slice(0..idx));
            }
            '\x21' ... '\x7e' => {
                // glam!
            }
            _ => {
                // these ought to have been # encoded
                return Error(error_position!(ErrorKind::Custom(22222), input));
            }
        }
    }

    if was_number_sign {
        return Incomplete(Needed::Unknown);
    }

    Done(input.slice(input_length..), input)
}

fn byte_vec_from_name_object(input: &[u8]) -> Result<Vec<u8>, nom::ErrorKind> {
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Err(ErrorKind::Custom(55555));
    }

    let mut first_iteration: bool = true;

    // was the previous character the number_sign, after which
    // we should expect exactly two hex digits ?
    let mut was_escape_char: bool = false;
    let mut count_hex_digits: usize = 0;
    let mut current_hex_value: u8 = 0;

    for (_idx, item) in input.iter_indices() {
        let chr = *item;

        // did we start right?
        if first_iteration {
            match chr {
                b'/' => {
                    first_iteration = false;
                    continue;
                }
                _ => {
                    return Err(ErrorKind::Custom(77777));
                }
            }
        }

        if was_escape_char {
            match count_hex_digits {
                0 | 1 => {
                    match is_hex_digit(chr as u8) {
                        true => {
                            current_hex_value = (current_hex_value << 4) + from_hex(chr as u8);
                            count_hex_digits += 1;

                            if count_hex_digits > 1 {
                                result.push(current_hex_value);
                                current_hex_value = 0;
                                count_hex_digits = 0;
                                was_escape_char = false;
                            }
                            continue;
                        }
                        false => {
                            return Err(ErrorKind::Custom(99999));
                        }
                    }
                }
                _ => {
                    return Err(ErrorKind::Custom(88888));
                }
            }
        }

        match chr {
            b'#' => {
                was_escape_char = true;
                continue;
            }
            b'\x00' => {
                return Err(ErrorKind::Custom(111111));
            }
            b'\n' | b'\r' | b'\t' | b' ' | b'\x0C' | b'/' | b'(' | b')' | b'<' | b'>' | b'[' | b']' | b'{' | b'}' | b'%' => {
                // unescaped whitespace or delimiters end the name
                return Ok(result);
            }
            b'\x21' ... b'\x7e' => {
                result.push(chr);
                // glam!
            }
            _ => {
                // these ought to have been # encoded
                return Err(ErrorKind::Custom(222222));
            }
        }
    }

    // we expect that we get called only if the recognizer
    // succeeded, so here it is an error to be dangling.
    if was_escape_char {
        return Err(ErrorKind::Custom(66666));
    }

    Ok(result)
}

/// return the name itself expanded to un-escaped form
named!(pub name_object<&[u8],Vec<u8>>,
    map_res!( recognize_name_object, byte_vec_from_name_object )
);

named!(pub recognize_null_object<&[u8],&[u8]>,
    recognize!(tag!(b"null"))
);

named!(pub null_object<&[u8],PdfObject>,
    do_parse!(
        recognize_null_object >>
        (PdfObject::Null)
    )
);

// surely there's a better way...
named!(non_zero_positive_int_not_padded<&[u8], &[u8] >,
    recognize!(
        pair!(
            alt!(
                tag!(b"1") |
                tag!(b"2") |
                tag!(b"3") |
                tag!(b"4") |
                tag!(b"5") |
                tag!(b"6") |
                tag!(b"7") |
                tag!(b"8") |
                tag!(b"9")
            ),
            opt!( complete!(nom::digit ) )
        )
    )
);

named!(maybe_indirect_reference<&[u8], (&[u8],&[u8],&[u8],&[u8]) >,
    tuple!(
        non_zero_positive_int_not_padded,
        tag!(b" "),
        complete!(nom::digit),
        tag!(b" R")
    )
);

named!(pub recognize_indirect_reference<&[u8],&[u8]>,
    recognize!(
        maybe_indirect_reference
    )
);

#[inline]
fn dec_u32(input: &[u8]) -> u32 {
    let mut res = 0u32;

    for &e in input {
        let digit = e as char;
        let value = digit.to_digit(10).unwrap_or(0);
        res = value + (res * 10);
    }
    res
}

named!(pub indirect_reference<&[u8],PdfObject>,
    do_parse!(
        number_bytes: non_zero_positive_int_not_padded >>
        tag!(b" ") >>
        version_bytes: recognize!(digit) >>
        tag!(b" R") >>
        (
            PdfObject::IndirectReference{ number: dec_u32(number_bytes), version: dec_u32(version_bytes) }
        )
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pdf_indirect_reference_test() {
        assert_eq!(b"95 0 R".as_bytes(), recognize_indirect_reference(b"95 0 R".as_bytes()).to_result().unwrap());
        assert_eq!(b"95 0 R".as_bytes(), recognize_indirect_reference(b"95 0 R ".as_bytes()).to_result().unwrap());
        assert_eq!(b"9 1 R".as_bytes(), recognize_indirect_reference(b"9 1 R".as_bytes()).to_result().unwrap());
        assert_eq!(Err(nom::Err::Position(nom::ErrorKind::Alt, [48, 57, 32, 49, 32, 82].as_bytes())), recognize_indirect_reference(b"09 1 R".as_bytes()).to_result());
        assert_eq!(PdfObject::IndirectReference { number: 95, version: 0 },
                   indirect_reference(b"95 0 R ".as_bytes()).to_result().unwrap());
        assert_eq!(PdfObject::IndirectReference { number: 95, version: 100 },
                   indirect_reference(b"95 100 R ".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_null_test() {
        match null_object(b"null").to_result().unwrap() {
            PdfObject::Null => {}
            PdfObject::IndirectReference { number: _, version: _ } => {
                assert_eq!(3,0);
            },
        }
        match null_object(b"nul") {
            Incomplete(_) => {}
            _ => {
                assert_eq!(1, 0);
            }
        }
        match null_object(b"mul") {
            Error(_) => {}
            _ => {
                assert_eq!(2, 0);
            }
        }
    }

    #[test]
    fn pdf_boolean_test() {
        assert_eq!(true, boolean(b"true ").to_result().unwrap());
        assert_eq!(false, boolean(b"false").to_result().unwrap());
    }

    #[test]
    fn pdf_magic_test() {
        assert_eq!(PdfVersion::Known { ver: b"1.0".to_vec() }, pdf_magic(b"%PDF-1.0\r\n").to_result().unwrap());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Tag, &[98u8, 108u8, 97u8, 104u8][..]),
                   pdf_magic(b"blah").to_result().unwrap_err());
        assert_eq!(nom::Err::Position(nom::ErrorKind::Alt, &[51u8, 46u8, 48u8, 13u8][..]),
                   pdf_magic(b"%PDF-3.0\r").to_result().unwrap_err());
    }

    #[test]
    fn pdf_linendings_test() {
        assert_eq!(b"\r".as_bytes(), pdf_line_ending(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\r\n".as_bytes(), pdf_line_ending(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\n".as_bytes(), pdf_line_ending(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_linendings_by_macro_test() {
        assert_eq!(b"\r".as_bytes(), pdf_line_ending_by_macro(b"\rdd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\r\n".as_bytes(), pdf_line_ending_by_macro(b"\r\ndd".as_bytes()).to_result().unwrap());
        assert_eq!(b"\n".as_bytes(), pdf_line_ending_by_macro(b"\ndd".as_bytes()).to_result().unwrap());
    }

    #[test]
    fn pdf_comments_test() {
        assert_eq!(comment(b"%hiya\n").to_result().unwrap(), b"hiya".as_bytes());
        assert_eq!(comment("%なななな\n".as_bytes()).to_result().unwrap(), "なななな".as_bytes());
    }

    #[test]
    fn pdf_header_test() {
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"1.0".to_vec() }),
                   pdf_header(b"%PDF-1.0\r ".as_bytes()));
        assert_eq!(nom::IResult::Done(b" ".as_bytes(), PdfVersion::Known { ver: b"2.0".to_vec() }),
                   pdf_header("%PDF-2.0\r%なななな\n ".as_bytes()));
    }

    #[test]
    fn integers_test() {
        assert_eq!(123, signed_integer(b"123").to_result().unwrap());
        assert_eq!(-123, signed_integer(b"-123").to_result().unwrap());
        assert_eq!(0, signed_integer(b"0").to_result().unwrap());
        assert_eq!(0, signed_integer(b"-0").to_result().unwrap()); // heh
    }
    macro_rules! float_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                assert_relative_eq!(expected, signed_float(input).to_result().unwrap());
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
    }


    #[test]
    fn hexadecimal_string_test() {
        assert_eq!(
            b"abcdef0123456789".as_bytes(),
            maybe_hexadecimal_string(b"<abcdef0123456789>".as_bytes()).to_result().unwrap()
        );

        assert_eq!(
            b"abc def0123\n456789 ".as_bytes(),
            maybe_hexadecimal_string(b"<abc def0123\n456789 >".as_bytes()).to_result().unwrap()
        );

        assert_eq!(
            nom::Err::Position(nom::ErrorKind::Tag, &[45u8, 62u8][..]),
            maybe_hexadecimal_string(b"<a->".as_bytes()).to_result().unwrap_err()
        );

        assert_eq!(
            vec![0xab],
            hexadecimal_string(b"<ab>".as_bytes()).to_result().unwrap()
        );

        assert_eq!(
            vec![0xab, 0xcd, 0xef],
            hexadecimal_string(b"<ab cd\nef>".as_bytes()).to_result().unwrap()
        );

        assert_eq!(
            vec![0xab, 0xcd, 0xef, 0x10],
            hexadecimal_string(b"<ab cd\nef1>".as_bytes()).to_result().unwrap()
        );
    }


    macro_rules! lsrt {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let expected = $value;
                    assert_eq!(expected,
                        recognize_literal_string(expected).to_result().unwrap());
                }
            )*
        }
    }

    macro_rules! lsit {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(expected,
                        recognize_literal_string(input).unwrap_inc());
                }
            )*
        }
    }


    macro_rules! lset {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(expected,
                        recognize_literal_string(input));
                }
            )*
        }
    }


    lsrt! {
        lsrt_2: b"(a)".as_bytes(),
        lsrt_3: b"(This is a string)".as_bytes(),
        lsrt_4: b"(Strings can contain newlines\nand such.)".as_bytes(),
        lsrt_5: b"(Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .)".as_bytes(),
        lsrt_6: b"(The following is an empty string .)".as_bytes(),
        lsrt_7: b"()".as_bytes(),
        lsrt_8: b"(It has zero (0) length.)".as_bytes(),

        // but we wont test equivalence til we deserialize...
        lsrt_9: b"(These \\\rtwo strings \\\nare the same.)".as_bytes(),
        lsrt_a: b"(These two strings are the same.)".as_bytes(),

        lsrt_b: b"(This string has an end-of-line at the end of it.\n)".as_bytes(),
        lsrt_c: b"(So does this one.\\n)".as_bytes(),
        lsrt_d: b"(This string contains \\245two octal characters\\307.)".as_bytes(),
        lsrt_e: b"(\\0053)".as_bytes(),
        lsrt_f: b"(\\053)".as_bytes(),
        lsrt_g: b"(\\53)".as_bytes(),
    }

    lsit! {
        lsit_1: (b"(abcde".as_bytes(), Needed::Unknown),
        lsit_2: (b"(abc()".as_bytes(), Needed::Unknown),
        lsit_3: (b"(abc\\".as_bytes(), Needed::Unknown),
    }

    lset! {
        lset_1: (b"(abc\\80)".as_bytes(), Error(nom::Err::Position(ErrorKind::Custom(3333), b"(abc\\80)".as_bytes()))),
    }

    macro_rules! tlsr {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(
                        literal_string(input).to_result().unwrap(),
                        expected
                    );
                }
            )*
        }
    }

    tlsr! {
        tlsr_0: (b"(abcd)".as_bytes(), b"abcd".as_bytes()),
        tlsr_1: (b"(\\247)".as_bytes(), b"\xA7".as_bytes()),

        tlsr_2: (b"(a)".as_bytes(), b"a".as_bytes()),
        tlsr_3: (b"(This is a string)".as_bytes(), b"This is a string".as_bytes()),
        tlsr_4: (b"(Strings can contain newlines\nand such.)".as_bytes(), b"Strings can contain newlines\nand such.".as_bytes()),
        tlsr_5: (b"(Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .)".as_bytes(),
                    b"Strings can contain balanced parentheses ()\nand special characters ( * ! & } ^ %and so on) .".as_bytes()),
        tlsr_6: (b"(The following is an empty string .)".as_bytes(), b"The following is an empty string .".as_bytes()),
        tlsr_7: (b"()".as_bytes(), b"".as_bytes()),
        tlsr_8: (b"(It has zero (0) length.)".as_bytes(), b"It has zero (0) length.".as_bytes()),

        tlsr_9: (b"(These \\\rtwo strings \\\nare the same.)".as_bytes(), b"These two strings are the same.".as_bytes()),
        tlsr_a: (b"(These two strings are the same.)".as_bytes(), b"These two strings are the same.".as_bytes()),

        tlsr_b: (b"(This string has an end-of-line at the end of it.\n)".as_bytes(), b"This string has an end-of-line at the end of it.\n".as_bytes()),
        tlsr_c: (b"(So does this one.\\n)".as_bytes(), b"So does this one.\n".as_bytes()),
        tlsr_d: (b"(This string contains \\245two octal characters\\307.)".as_bytes(), b"This string contains \xA5two octal characters\xC7.".as_bytes()),
        tlsr_e: (b"(\\0053)".as_bytes(), b"\x053".as_bytes()),
        tlsr_f: (b"(\\053)".as_bytes(), b"\x2B".as_bytes()),
        tlsr_g: (b"(\\53)".as_bytes(), b"\x2B".as_bytes()),
        tlsr_h: (b"(\\533)".as_bytes(), b"\x2B3".as_bytes()),
    }

    macro_rules! name_object_result_test {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(name_object(input).to_result().unwrap(),
                        expected);
                }
            )*
        }
    }

    name_object_result_test! {
        nort_1: (b"/Name1".as_bytes(),b"Name1".as_bytes()),
        nort_2: (b"/ASomewhatLongerName".as_bytes(),b"ASomewhatLongerName".as_bytes()),
        nort_3:(b"/A;Name_With-Various***Characters?".as_bytes(),b"A;Name_With-Various***Characters?".as_bytes()),
        nort_5: (b"/1.2".as_bytes(),b"1.2".as_bytes()),
        nort_6: (b"/$$".as_bytes(),b"$$".as_bytes()),
        nort_7: (b"/@pattern".as_bytes(),b"@pattern".as_bytes()),
        nort_8: (b"/.notdef".as_bytes(),b".notdef".as_bytes()),
        nort_9: (b"/Lime#20Green".as_bytes(),b"Lime Green".as_bytes()),
        nort_10: (b"/paired#28#29parentheses".as_bytes(),b"paired()parentheses".as_bytes()),
        nort_11: (b"/The_Key_of_F#23_Minor".as_bytes(),b"The_Key_of_F#_Minor".as_bytes()),
        nort_12: (b"/A#42".as_bytes(),b"AB".as_bytes()),
        nort_13: (b"/#2F".as_bytes(),b"/".as_bytes()),
        nort_14: (b"/".as_bytes(),b"".as_bytes()),
        nort_15: (b"/abcd[".as_bytes(),b"abcd".as_bytes()),
    }
}

