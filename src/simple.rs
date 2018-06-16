extern crate nom;

use nom::digit;
use nom::ErrorKind;
use nom::*;
use std::str;
use std::str::FromStr;

use structs::CrossReferenceTable;
use structs::ErrorCodes;
use structs::PdfObject;
use structs::PdfVersion;

use nesting::dictionary_object;

named!(pub pdf_line_ending_by_macro<&[u8],&[u8]>,
    alt!(
        complete!(tag!(b"\r\n")) |
        complete!(tag!(b"\r")) |
        complete!(tag!(b"\n"))
    )
);

named!(pdf_version<&[u8],&[u8]>,
    re_bytes_find!("^(1\\.[01234567]|2\\.0)")
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

named!(pub signed_integer<&[u8],PdfObject>,
    do_parse!(
        v: re_bytes_capture!(r"^([-+]?[0-9]+)")
        >>
        (PdfObject::Integer ( FromStr::from_str(str::from_utf8(v[0]).unwrap()).unwrap() ) )
    )
);

named!(pub signed_float<&[u8],PdfObject>,
    do_parse!(
        v: re_bytes_capture!(r"^([-+]?([0-9]*\.[0-9]+|[0-9]+\.[0-9]*))")
        >>
        (PdfObject::Float ( FromStr::from_str(str::from_utf8(v[0]).unwrap()).unwrap()))
    )
);

// comments § 7.2.4

// comments are going to by my undoing, given where all they can occur.
// the comment does not include the line-ending itself, just up to it.

named!(pub recognize_comment<&[u8],&[u8]>,
    recognize!(
        tuple!(
            tag!(b"%"),
            take_while!( is_not_line_end_chars )
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
    (chr == 0x00 || chr == 0x09 || chr == 0x0A || chr == 0x0C || chr == 0x0D || chr == 0x20)
}

named!(pub recognize_some_ws<&[u8],&[u8]>,
    recognize!(
        re_bytes_find!("^(\x00|\x09|\x0a|\x0c|\x0d|\x20)*")
    )
);

pub fn skip_whitespace(input: &[u8]) -> usize {
    let ws_iresult = recognize_some_ws(input);
    match ws_iresult {
        Ok((_, recognized)) => {
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
    is_hex_digit(chr) || is_pdf_whitespace(chr)
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
    let filtered: Vec<u8> = input
        .iter()
        .filter(|&x| nom::is_hex_digit(*x))
        .map(|&x| from_hex(x))
        .collect();

    for pair in filtered.chunks(2) {
        if pair.len() == 2 {
            result.push((pair[0] << 4) + pair[1]);
        } else if pair.len() == 1 {
            result.push(pair[0] << 4);
        }
    }

    Ok(result)
}

named!(pub hexadecimal_string<&[u8],PdfObject>,
    do_parse!(
        v: map_res!( maybe_hexadecimal_string, byte_vec_from_hexadecimal_string)
        >>
        ( PdfObject::String( v ) )
    )
);

named!(pub bare_hexadecimal_string<&[u8],PdfObject>,
    do_parse!(
        v: map_res!( take_while!( can_be_in_hexadecimal_string ), byte_vec_from_hexadecimal_string )
        >>
        ( PdfObject::String( v ))
    )
);

// § 7.3.4.2 Literal Strings

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

/// determine if we have a valid pdf literal string,
/// make a sequence of decoded data bytes from the raw byte sequence of
/// the PDF literal string.
fn recognize_literal_string(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Err(Err::Incomplete(Needed::Unknown));
    }

    // are we inside the string yet?
    let mut inside: bool = false;

    // what depth of balanced unescaped parens are we at?
    // starts our iteration at 0, and it ought to be 0
    // when we end (so, the delimiting paren is the first
    // and last we encounter and manipulate this variable
    // for...)
    let mut parens_depth: usize = 0;

    // did we just see an escaping backslash?
    let mut was_escape_char: bool = false;
    let mut was_escaped_carriage_return: bool = false;

    // we want to accept the
    // longest valid octal, even in the face of the escape
    // sequence being followed by other digits
    let mut current_octal_value: i32 = -1;
    let mut number_octal_digits: usize = 0;
    let mut index = 0;

    'outer: for (idx, item) in input.iter_indices() {
        let chr = item;
        index = idx;

        if !inside {
            if chr != b'(' {
                return Err(Err::Error(error_position!(
                    input,
                    ErrorKind::Custom(ErrorCodes::ExpectedStringStart as u32)
                )));
            }
            inside = true;
            // and then head to the switch below.  re-write this.
        }

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
                    // they both MUST be rendered into the result as \n
                    was_escaped_carriage_return = true;
                }
                b'n' => result.push(b'\n'),
                b'r' => result.push(b'\r'),
                b't' => result.push(b'\t'),
                b'b' => result.push(0x08 as u8),
                b'f' => result.push(0x0c as u8),
                b'(' | b')' | b'\\' => {
                    result.push(chr);
                }
                b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
                    number_octal_digits = 1;
                    current_octal_value = (chr as u8 - '0' as u8) as i32;
                }
                _ => {
                    // anything outside the above gets us an error
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::UnrecognizedEscapeSequence as u32)
                    )));
                }
            }
            was_escape_char = false;
            continue;
        }

        if number_octal_digits > 0 {
            if could_have_more_octal_digits(current_octal_value, number_octal_digits)
                && (chr == b'0' || chr == b'1' || chr == b'2' || chr == b'3' || chr == b'4'
                    || chr == b'5' || chr == b'6' || chr == b'7')
            {
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
                    break 'outer;
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
        _ => {
            return Err(Err::Incomplete(Needed::Unknown));
        }
    }
    // +1 because the last index we look at is the final ) itself
    Ok((&input[index + 1..], result))
}

named!(pub literal_string<&[u8],PdfObject>,
    do_parse!(
        v: recognize_literal_string
        >>
        ( PdfObject::String( v ) )
    )
);

// null object § 7.3.9

named!(pub recognize_null_object<&[u8],&[u8]>,
    recognize!(tag!(b"null"))
);

named!(pub null_object<&[u8],PdfObject>,
    do_parse!(
        recognize_null_object >>
        (PdfObject::Null)
    )
);

// § 7.3.5 Name objects

/// recognize a Name object, returning a vec of the bytes of the decoded form of the name.
pub fn recognize_name_object(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result: Vec<u8> = Vec::new();

    let input_length = input.input_len();
    if input_length == 0 {
        return Err(Err::Incomplete(Needed::Unknown));
    }

    //
    let mut first_iteration: bool = true;

    // was the previous character the number_sign, after which
    // we should expect exactly two hex digits ?
    let mut was_number_sign: bool = false;
    let mut count_hex_digits: usize = 0;
    let mut current_hex_value: u8 = 0;

    for (idx, item) in input.iter_indices() {
        let chr = item;

        // did we start right?
        if first_iteration {
            match chr {
                b'/' => {
                    first_iteration = false;
                    continue;
                }
                _ => {
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::NameNotStartWithSlash as u32)
                    )));
                }
            }
        }

        if was_number_sign {
            match count_hex_digits {
                0 | 1 => match is_hex_digit(chr as u8) {
                    true => {
                        current_hex_value = (current_hex_value << 4) + from_hex(chr as u8);
                        count_hex_digits += 1;

                        if count_hex_digits > 1 {
                            result.push(current_hex_value);
                            current_hex_value = 0;
                            count_hex_digits = 0;
                            was_number_sign = false;
                        }
                        continue;
                    }
                    false => {
                        return Err(Err::Error(error_position!(
                            input,
                            ErrorKind::Custom(ErrorCodes::ExpectedHexDigit as u32)
                        )));
                    }
                },
                // how will we ever get here?
                _ => {
                    return Err(Err::Error(error_position!(
                        input,
                        ErrorKind::Custom(ErrorCodes::TooManyHexDigits as u32)
                    )));
                }
            }
        }

        match chr {
            b'#' => {
                was_number_sign = true;
                continue;
            }
            b'\x00' => {
                return Err(Err::Error(error_position!(
                    input,
                    ErrorKind::Custom(ErrorCodes::HexStringIncludesZeroByte as u32)
                )));
            }
            b'\n' | b'\r' | b'\t' | b' ' | b'\x0C' | b'/' | b'(' | b')' | b'<' | b'>' | b'['
            | b']' | b'{' | b'}' | b'%' => {
                // unescaped whitespace and unescaped delimiters end the name
                return Ok((input.slice(idx..), result));
            }
            b'\x21'...b'\x7e' => {
                result.push(chr);
                // glam!
            }
            _ => {
                // these ought to have been # encoded
                return Err(Err::Error(error_position!(
                    input,
                    ErrorKind::Custom(ErrorCodes::ByteValueOughtToHaveBeenHexEncoded as u32)
                )));
            }
        }
    }

    if was_number_sign {
        return Err(Err::Incomplete(Needed::Unknown));
    }

    // ... wut?  i guess we get here if the end of
    // current input is indistinguishable from
    // the end of a name possibly.
    Ok((&input[input_length..], result))
}

/// return the name itself expanded to un-escaped form
named!(pub name_object<&[u8],PdfObject>,
    do_parse!(
        v: recognize_name_object
        >>
        ( PdfObject::Name( v ) )
    )
);

// indirect references § 7.3.10

named!(pub indirect_reference<&[u8],PdfObject>,
    do_parse!(
        num: map_res!(map_res!(re_bytes_find!(r"^[123456789]\d*"), str::from_utf8), FromStr::from_str) >>
        tag!(b" ") >>
        gen: map_res!(map_res!(recognize!(digit), str::from_utf8), FromStr::from_str) >>
        tag!(b" R") >>
        (
            PdfObject::IndirectReference{ number: num, generation: gen }
        )
    )
);

// § 7.5.4 Cross-reference table

#[inline]
fn number_from_digits(digits: &[u8]) -> u64 {
    FromStr::from_str(str::from_utf8(digits).unwrap()).unwrap()
}

pub fn xref_table(input: &[u8]) -> IResult<&[u8], CrossReferenceTable> {
    // starts with xref
    // then 1 or more cross-ref subsections
    // no explicit closer, just on to the next thing which ain't this here.

    match re_bytes_find!(input, r"^xref(\r\n|\r|\n)") {
        Ok((rest, _xref)) => {
            let mut xrt: CrossReferenceTable = CrossReferenceTable::new();

            let mut linput = rest;
            let mut first: bool = true;

            'outer: loop {
                match re_bytes_capture!(linput, r"^(0|[123456789]\d*) ([123456789]\d*)(\r\n|\r|\n)")
                {
                    Ok((rest2, vec2)) => {
                        // unwrapping here feels safe given the matching
                        let start: u32 = number_from_digits(vec2[1]) as u32;

                        if first && start != 0 {
                            return Err(Err::Error(error_position!(
                                input,
                                ErrorKind::Custom(
                                    ErrorCodes::FirstObjectNumberInXrefNotZero as u32
                                )
                            )));
                        }
                        first = false;

                        let how_many: usize = number_from_digits(vec2[2]) as usize;
                        linput = rest2;

                        for itr in 0..how_many {
                            match re_bytes_capture!(
                                linput,
                                r"^(\d{10}) (\d{5}) ([nf])( \r| \n|\r\n)"
                            ) {
                                Ok((rest3, vec3)) => {
                                    let num0: u64 = number_from_digits(vec3[1]);
                                    let num1: u16 = number_from_digits(vec3[2]) as u16;
                                    let in_use: bool = vec3[3][0] == b'n';

                                    if in_use {
                                        xrt.add_in_use(start + (itr as u32), num1, num0 as usize);
                                    } else {
                                        xrt.add_free(start + (itr as u32), num1);
                                        // we would do some inspecting of num0 here if the current
                                        // spec cared anymore: the old-style linked list and the
                                        // new style mean we kind of dont care anymore.
                                    }

                                    linput = rest3;
                                }
                                Err(Err::Incomplete(whatever)) => {
                                    return Err(Err::Incomplete(whatever));
                                }
                                Err(err) => {
                                    return Err(err);
                                }
                            }
                        }
                    }
                    Err(Err::Incomplete(whatever)) => {
                        return Err(Err::Incomplete(whatever));
                    }
                    Err(err) => {
                        if !first {
                            return Ok((linput, xrt));
                        }
                        return Err(err);
                    }
                }
            }
        }
        Err(Err::Incomplete(whatever)) => {
            return Err(Err::Incomplete(whatever));
        }
        Err(err) => return Err(err),
    }
}

// § 7.5.5 File trailer
// this here is independent of how we *locate* the trailer, which needs to be
// implemented of.

// get the trailer Dictionary and the offset of the xref we need to look for.

pub fn file_trailer(input: &[u8]) -> IResult<&[u8], (PdfObject, usize)> {
    // need to make these patterns stricter
    match re_bytes_find!(input, r"^\s*trailer\s*(\r\n|\r|\n)") {
        Ok((rest, _trailer)) => match dictionary_object(rest) {
            Ok((rest2, dictionary)) => match re_bytes_capture!(
                rest2,
                r"^\s*startxref\s*(\r\n|\r|\n)([123456789]\d*)\s*(\r\n|\r|\n)%%EOF\s*"
            ) {
                Ok((rest3, vec3)) => {
                    let startxref = number_from_digits(vec3[2]) as usize;
                    return Ok((rest3, (dictionary, startxref)));
                }
                Err(Err::Incomplete(whatever)) => {
                    return Err(Err::Incomplete(whatever));
                }
                Err(err) => {
                    return Err(err);
                }
            },
            Err(Err::Incomplete(whatever)) => {
                return Err(Err::Incomplete(whatever));
            }
            Err(err) => {
                return Err(err);
            }
        },
        Err(Err::Incomplete(whatever)) => {
            return Err(Err::Incomplete(whatever));
        }
        Err(err) => {
            return Err(err);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn indirect_reference_test() {
        assert_eq!(
            (
                b" ".as_bytes(),
                PdfObject::IndirectReference {
                    number: 95,
                    generation: 1
                }
            ),
            indirect_reference(b"95 1 R ".as_bytes()).unwrap()
        );
        assert_eq!(
            (
                b"\n".as_bytes(),
                PdfObject::IndirectReference {
                    number: 95,
                    generation: 100
                }
            ),
            indirect_reference(b"95 100 R\n".as_bytes()).unwrap()
        );
        assert_eq!(
            (
                b"".as_bytes(),
                PdfObject::IndirectReference {
                    number: 96,
                    generation: 100
                }
            ),
            indirect_reference(b"96 100 R".as_bytes()).unwrap()
        );
        assert_eq!(
            Err(Err::Error(Context::Code(
                b"09 1 R".as_bytes(),
                nom::ErrorKind::RegexpFind
            ))),
            indirect_reference(b"09 1 R".as_bytes())
        );
    }

    #[test]
    fn null_test() {
        match null_object(b"null").unwrap() {
            (_b, PdfObject::Null) => {}
            _ => {
                assert_eq!(3, 0);
            }
        }
        match null_object(b"nul") {
            Err(Err::Incomplete(_)) => {}
            _ => {
                assert_eq!(1, 0);
            }
        }
        match null_object(b"mul") {
            Err(_) => {}
            _ => {
                assert_eq!(2, 0);
            }
        }
    }

    #[test]
    fn boolean_test() {
        assert_eq!(
            (b" ".as_bytes(), PdfObject::Boolean(true)),
            boolean(b"true ").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Boolean(false)),
            boolean(b"false").unwrap()
        );
    }

    #[test]
    fn magic_test() {
        assert_eq!(
            (
                b"".as_bytes(),
                PdfVersion::Known {
                    ver: b"1.0".to_vec()
                }
            ),
            pdf_magic(b"%PDF-1.0\r\n").unwrap()
        );
        assert_eq!(
            Err(Err::Error(Context::Code(
                &[98u8, 108u8, 97u8, 104u8][..],
                nom::ErrorKind::Tag
            ))),
            pdf_magic(b"blah")
        );
        assert_eq!(
            Err(Err::Error(Context::Code(
                &b"3.0\r"[..],
                nom::ErrorKind::RegexpFind
            ))),
            pdf_magic(b"%PDF-3.0\r")
        );
    }

    #[test]
    fn linendings_by_macro_test() {
        assert_eq!(
            (b"dd".as_bytes(), b"\r".as_bytes()),
            pdf_line_ending_by_macro(b"\rdd".as_bytes()).unwrap()
        );
        assert_eq!(
            (b"dd".as_bytes(), b"\r\n".as_bytes()),
            pdf_line_ending_by_macro(b"\r\ndd".as_bytes()).unwrap()
        );
        assert_eq!(
            (b"dd".as_bytes(), b"\n".as_bytes()),
            pdf_line_ending_by_macro(b"\ndd".as_bytes()).unwrap()
        );
    }

    #[test]
    fn comments_test() {
        match comment(b"%hiya\n").unwrap() {
            (_b, PdfObject::Comment(v)) => {
                assert_eq!(v, "hiya".as_bytes());
            }
            _ => {
                assert_eq!(6, 0);
            }
        }
        match comment("%なななな\n".as_bytes()).unwrap() {
            (_b, PdfObject::Comment(v)) => {
                assert_eq!(v, "なななな".as_bytes());
            }
            _ => {
                assert_eq!(6, 0);
            }
        }
    }

    #[test]
    fn header_test() {
        assert_eq!(
            Ok((
                b" ".as_bytes(),
                PdfVersion::Known {
                    ver: b"1.0".to_vec()
                }
            )),
            pdf_header(b"%PDF-1.0\r ".as_bytes())
        );
        assert_eq!(
            Ok((
                b"\n ".as_bytes(),
                PdfVersion::Known {
                    ver: b"2.0".to_vec()
                }
            )),
            pdf_header("%PDF-2.0\r%なななな\n ".as_bytes())
        );
    }

    #[test]
    fn integers_test() {
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(123)),
            signed_integer(b"123").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(-123)),
            signed_integer(b"-123").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(0)),
            signed_integer(b"0").unwrap()
        );
        assert_eq!(
            (b"".as_bytes(), PdfObject::Integer(0)),
            signed_integer(b"-0").unwrap()
        ); // heh
    }
    macro_rules! float_test {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (input, expected) = $value;
                match signed_float(input).unwrap() {
                    (_b, PdfObject::Float(v)) => {
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
    }

    #[test]
    fn hexadecimal_string_test() {
        assert_eq!(
            Err(Err::Error(Context::Code(
                &[45u8, 62u8][..],
                nom::ErrorKind::Tag
            ))),
            hexadecimal_string(b"<a->".as_bytes())
        );
    }
    macro_rules! hexst {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match hexadecimal_string(input).unwrap() {
                        (_b, PdfObject::String(v)) => {
                            assert_eq!( v, expected );
                        },
                        _ => {
                            assert_eq!(7,0);
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

    macro_rules! lsit {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let input = $value;
                    match literal_string(input.as_bytes()) {
                        Err(Err::Incomplete(_)) => {}
                        _ => {
                            assert_eq!(9, 0);
                        }
                    }
                }
            )*
        }
    }

    lsit! {
        lsit_1: b"(abcde",
        lsit_2: b"(abc()",
        lsit_3: b"(abc\\",
    }
    macro_rules! lset {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    assert_eq!(expected,
                        literal_string(input.as_bytes()));
                }
            )*
        }
    }

    lset! {
        lset_1: (
            b"(abc\\80)",
            Err(Err::Error(error_position!(b"(abc\\80)".as_bytes(),ErrorKind::Custom(ErrorCodes::UnrecognizedEscapeSequence as u32))))
        ),
    }

    macro_rules! tlsr {
        ($($name:ident: $value:expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (input, expected) = $value;
                    match literal_string(input.as_bytes()) {
                        Ok((_b, PdfObject::String(v))) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },
                        _ => {
                            assert_eq!(9,0);
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
                    match name_object(input.as_bytes()) {
                        Ok((_b, PdfObject::Name( v ))) => {
                            assert_eq!(expected[..].to_owned(), v);
                        },
                        _ => {
                            assert_eq!(10, 0);
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
        nort_14: (b"/",b""),
        nort_15: (b"/abcd[",b"abcd"),
    }

    #[test]
    fn test_ws_recognize() {
        assert_eq!(0, recognize_some_ws(b"a ".as_bytes()).unwrap().1.len());
        assert_eq!(0, recognize_some_ws(b"".as_bytes()).unwrap().1.len());
        assert_eq!(2, recognize_some_ws(b" \t".as_bytes()).unwrap().1.len());
        assert_eq!(
            9,
            recognize_some_ws(b" \t \r\n \n \r".as_bytes())
                .unwrap()
                .1
                .len()
        );
    }

    #[test]
    fn test_xref_table_recognize() {
        let x2 = b"xref\n0 6\n0000000003 65535 f \n0000000017 00000 n \n0000000081 00000 n \n0000000000 00007 f \n0000000331 00000 n \n0000000409 00000 n \ntrailer";

        match xref_table(x2[..].as_bytes()) {
            Ok((_rest, xrt)) => {
                assert_eq!(4, xrt.count_in_use());
                assert_eq!(2, xrt.count_free());
                assert_eq!(vec![1, 2, 4, 5], xrt.in_use());
                assert_eq!(vec![0, 3], xrt.free());
                assert_eq!(Some(65535), xrt.generation_of(0));
                assert_eq!(Some(0), xrt.generation_of(1));
                assert_eq!(Some(0), xrt.generation_of(2));
                assert_eq!(Some(7), xrt.generation_of(3));
                assert_eq!(Some(0), xrt.generation_of(4));
                assert_eq!(Some(0), xrt.generation_of(5));
                assert_eq!(None, xrt.offset_of(0));
                assert_eq!(Some(17), xrt.offset_of(1));
                assert_eq!(Some(81), xrt.offset_of(2));
                assert_eq!(None, xrt.offset_of(3));
                assert_eq!(Some(331), xrt.offset_of(4));
                assert_eq!(Some(409), xrt.offset_of(5));
            }
            _ => {
                assert_eq!(151, 0);
            }
        }

        let x3 =  b"xref\n0 1\n0000000000 65535 f \n3 1\n0000025325 00000 n \n23 2\n0000025518 00002 n \n0000025635 00000 n \n30 1\n0000025777 00000 n \ntrailer";
        match xref_table(x3[..].as_bytes()) {
            Ok((_rest, xrt)) => {
                assert_eq!(4, xrt.count_in_use());
                assert_eq!(1, xrt.count_free());
                assert_eq!(vec![3, 23, 24, 30], xrt.in_use());
                assert_eq!(vec![0], xrt.free());
                assert_eq!(Some(65535), xrt.generation_of(0));
                assert_eq!(Some(0), xrt.generation_of(3));
                assert_eq!(Some(2), xrt.generation_of(23));
                assert_eq!(Some(0), xrt.generation_of(24));
                assert_eq!(Some(0), xrt.generation_of(30));

                assert_eq!(None, xrt.offset_of(0));
                assert_eq!(Some(25325), xrt.offset_of(3));
                assert_eq!(Some(25518), xrt.offset_of(23));
                assert_eq!(Some(25635), xrt.offset_of(24));
                assert_eq!(Some(25777), xrt.offset_of(30));

                assert_eq!(None, xrt.offset_of(252));
                assert_eq!(None, xrt.offset_of(1024));
                assert_eq!(None, xrt.generation_of(252));
                assert_eq!(None, xrt.generation_of(1024));
            }
            _ => {
                assert_eq!(152, 0);
            }
        }
    }

    #[test]
    fn test_basic_trailer() {
        let trailer = b"trailer\n<</Size 22/Root 2 0 R/Info 1 0 R/ID [<81b14aafa313db63dbd6f981e49f94f4><81b14aafa313db63dbd6f981e49f94f4>]>>\nstartxref\n18799\n%%EOF\n";

        match file_trailer(&trailer[..]) {
            Ok((_rest, (dict, offset))) => {
                assert_eq!(18799 as usize, offset);
                match dict {
                    PdfObject::Dictionary(nkm) => {
                        assert_eq!(
                            PdfObject::Integer(22),
                            nkm.get(PdfObject::Name(b"Size"[..].to_owned()))
                                .unwrap()
                                .unwrap()
                        );
                    }
                    _ => {
                        assert_eq!(200, 0);
                    }
                }
            }
            _ => {
                assert_eq!(201, 0);
            }
        }
    }

}
