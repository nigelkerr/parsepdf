extern crate nom;

use nom::*;
use nom::digit;
use nom::ErrorKind;
use nom::IResult::*;
use std::str;
use std::str::FromStr;

use structs::PdfObject;

named!(pub pdf_line_ending_by_macro<&[u8],&[u8]>,
    alt!(
        complete!(tag!(b"\r\n")) |
        complete!(tag!(b"\r")) |
        complete!(tag!(b"\n"))
    )
);

#[inline]
pub fn is_not_line_end_chars(chr: u8) -> bool {
    (chr != b'\n' && chr != b'\r')
}

// § 7.3.2
named!(pub boolean<&[u8],PdfObject>,
    do_parse!(
        v: map_res!(map_res!(alt!(
                tag!(b"true") |
                tag!(b"false")
                ), str::from_utf8), FromStr::from_str)
        >>
        (PdfObject::Boolean ( v ) )
    )
);

// § 7.3.3

named!(maybe_signed_integer<&[u8],(Option<&[u8]>, &[u8])>,
    pair!(
        opt!(alt!(tag_s!("+") | tag_s!("-"))),
        nom::digit
    )
);
named!(recognize_signed_integer<&[u8],&[u8]>,
    recognize!(
        maybe_signed_integer
    )
);
named!(pub signed_integer<&[u8],PdfObject>,
    do_parse!(
        v: recognize_signed_integer
        >>
        (PdfObject::Integer ( FromStr::from_str(str::from_utf8(v).unwrap()).unwrap() ) )
    )
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

named!(recognize_signed_float<&[u8],&[u8]>,
    recognize!(
        alt!(
            complete!( maybe_signed_float_ap ) |
            complete!( maybe_signed_float_pp )
        )
    )
);

named!(pub signed_float<&[u8],PdfObject>,
    do_parse!(
        v: recognize_signed_float
        >>
        (PdfObject::Float ( FromStr::from_str(str::from_utf8(v).unwrap()).unwrap()))
    )
);

// comments § 7.2.4

// comments are going to by my undoing, given where all they can occur

named!(pub recognize_comment<&[u8],&[u8]>,
    recognize!(
        tuple!(
            tag!(b"%"),
            take_while!( is_not_line_end_chars ),
            pdf_line_ending_by_macro
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
named!(pub comment<&[u8],PdfObject>,
    do_parse!(
        v: map_res!( recognize_comment, byte_vec_from_comment )
        >>
        ( PdfObject::Comment ( v ) )
    )
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

named!(pub recognize_some_ws<&[u8],&[u8]>,
    recognize!(
        take_while!(is_pdf_whitespace)
    )
);

pub fn skip_whitespace(input: &[u8]) -> usize {
    let ws_iresult = recognize_some_ws(input);
    match ws_iresult {
        Done(_, recognized) => {
            return recognized.len();
        }
        _ => {
            return 0;
        }
    }
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
    do_parse!(
        v: map_res!( maybe_hexadecimal_string, byte_vec_from_hexadecimal_string)
        >>
        ( PdfObject::String( v ) )
    )
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
fn recognize_literal_string(input: &[u8]) -> IResult<&[u8], &[u8]>
{
    let input_length = input.input_len();
    if input_length == 0 {
        return Incomplete(Needed::Unknown);
    }

    let mut index: usize = 0;

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

    'outer:
        while index < input_length {
        if parens_depth == 0 {
            index += skip_whitespace(&input[index..]);
            if input[index] == b'(' {
                parens_depth += 1;
                index += 1;
                continue 'outer;
            } else {
                return Error(error_position!(ErrorKind::Custom(23456),input));
            }
        }

        if was_escape_char {
            match input[index] {
                b'\n' | b'\r' => {
                    // glam! we care more on deserializing
                }
                b'n' | b'r' | b't' | b'b' | b'f' | b'(' | b')' | b'\\' => {
                    // glam! we'll do the right thing on deserializing
                }
                b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                    number_octal_digits = 1;
                    current_octal_value = (input[index] - b'0') as i32;
                }
                _ => {
                    // anything outside the above gets us an error
                    return Error(error_position!(ErrorKind::Custom(3333), input));
                }
            }
            was_escape_char = false;
            index += 1;
            continue 'outer;
        }

        if number_octal_digits > 0 {
            if could_have_more_octal_digits(current_octal_value, number_octal_digits) &&
                (input[index] >= b'0' && b'7' <= input[index]) {
                number_octal_digits += 1;
                current_octal_value = (current_octal_value << 3) + (input[index] - b'0') as i32;
                index += 1;
                continue 'outer;
            } else {
                // glam! reset octal
                number_octal_digits = 0;
                current_octal_value = -1;
            }
        }

        match input[index] {
            b'(' => {
                parens_depth += 1;
            }
            b')' => {
                parens_depth -= 1;
                if parens_depth == 0 {
                    return Done(input.slice(index + 1..), input.slice(0..index + 1));
                }
            }
            b'\\' => {
                was_escape_char = true;
            }
            _ => {
                // glam! arbitrary 8-bit values accepted here.
            }
        }
        index += 1;



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
    do_parse!(
        v: map_res!( recognize_literal_string, byte_vec_from_literal_string )
        >>
        ( PdfObject::String( v ) )
    )
);
